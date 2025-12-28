

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(recipes)
  library(bslib)
  library(RANN)  # ensures dependency for step_impute_knn is installed on shinyapps
})

# --------- Load models and data from the app directory (no parent paths) ---------
read_required_rds <- function(path) {
  if (!file.exists(path)) stop(sprintf("Required file not found: %s", path), call. = FALSE)
  readRDS(path)
}

outcome.3.logistic <- read_required_rds("models/outcome_3_logistic.rds")
claimsize.3.gamma  <- read_required_rds("models/claimsize_3_gamma.rds")
data               <- read_required_rds("data/training_data.rds")

# --------- Helpers ---------
get_base_vars <- function(fit) {
  rn <- rownames(attr(terms(fit), "factors"))
  rn[!(rn %in% c("(Intercept)", as.character(formula(fit))[2]))]
}

prep_newdata_for <- function(newdata, fit) {
  nd <- newdata
  if (!is.null(fit$xlevels)) {
    for (nm in names(fit$xlevels)) if (nm %in% names(nd)) {
      nd[[nm]] <- factor(nd[[nm]], levels = fit$xlevels[[nm]])
    }
  }
  droplevels(nd)
}

# Variables from your summary image (collect them ALL if present in data)
vars_from_image <- c(
  "ID","AGE","GENDER","DRIVING_EXPERIENCE","EDUCATION","CREDIT_SCORE",
  "VEHICLE_OWNERSHIP","VEHICLE_YEAR","MARRIED","CHILDREN","ANNUAL_MILEAGE",
  "VEHICLE_TYPE","SPEEDING_VIOLATIONS","PAST_ACCIDENTS"
  # OUTCOME excluded (target)
)
ui_vars <- intersect(vars_from_image, names(data))

# Ensure characters are factors in the training reference
char_as_factor <- names(Filter(is.character, data[ui_vars]))
if (length(char_as_factor)) data[char_as_factor] <- lapply(data[char_as_factor], factor)

# Determine types
is_num <- vapply(data[ui_vars], is.numeric, logical(1))
num_vars_all <- names(which(is_num))
fac_vars_all <- setdiff(ui_vars, num_vars_all)

# yes/no binaries
binary_vars <- intersect(c("MARRIED","CHILDREN","VEHICLE_OWNERSHIP"), ui_vars)

# Labels
label_map <- c(
  ID                  = "Policy holder ID",
  AGE                 = "Age band",
  GENDER              = "Gender",
  DRIVING_EXPERIENCE  = "Driving experience",
  EDUCATION           = "Highest education level",
  CREDIT_SCORE        = "Credit score (0–1)",
  VEHICLE_OWNERSHIP   = "Does the policyholder own the vehicle?",
  VEHICLE_YEAR        = "Vehicle year (before/after 2015)",
  MARRIED             = "Is the policyholder married?",
  CHILDREN            = "Does the policyholder have children?",
  ANNUAL_MILEAGE      = "Annual mileage",
  VEHICLE_TYPE        = "Vehicle type",
  SPEEDING_VIOLATIONS = "Number of speeding violations",
  PAST_ACCIDENTS      = "number of prior accidents"   # updated wording
)
pretty_label <- function(var) {
  if (var %in% names(label_map)) return(label_map[[var]])
  lbl <- gsub("_", " ", var); lbl <- tolower(lbl)
  gsub("\\b([a-z])", "\\U\\1", lbl, perl = TRUE)
}

# Numeric ranges and integer-like detection
num_range <- function(v) {
  x <- data[[v]]
  c(min = suppressWarnings(min(x, na.rm = TRUE)),
    max = suppressWarnings(max(x, na.rm = TRUE)))
}
ranges <- setNames(lapply(num_vars_all, num_range), num_vars_all)
is_integerish <- function(x) all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)
int_like <- setNames(vapply(num_vars_all, function(v) is_integerish(data[[v]]), logical(1)), num_vars_all)

# kNN imputer for numeric covariates (neighbors = 5), excluding ID
num_impute_vars <- setdiff(num_vars_all, "ID")
imputer_prep <- NULL; medians <- NULL
if (length(num_impute_vars)) {
  rec <- recipe(~ ., data = data[, num_impute_vars, drop = FALSE]) |>
    step_impute_knn(all_predictors(), neighbors = 5)
  imputer_prep <- prep(rec, training = data[, num_impute_vars, drop = FALSE], retain = TRUE)
  medians <- vapply(data[, num_impute_vars, drop = FALSE], function(x) median(x, na.rm = TRUE), numeric(1))
}
impute_numeric_row <- function(row_df) {
  if (!length(num_impute_vars)) return(row_df)
  baked <- bake(imputer_prep, new_data = row_df[, num_impute_vars, drop = FALSE])
  row_df[, num_impute_vars] <- baked
  for (v in num_impute_vars) if (is.na(row_df[[v]])) row_df[[v]] <- medians[[v]]
  row_df
}

# Models' predictor set (for prediction; we still collect everything)
pred_vars <- union(get_base_vars(outcome.3.logistic), get_base_vars(claimsize.3.gamma))

# --------- UI (red theme) ---------
ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#dc3545"),
  titlePanel("Pure Premium Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Provide policy and driver information"),
      helpText("Leave numeric fields blank to impute via kNN trained on your data. For yes/no questions, select an option."),
      lapply(ui_vars, function(v) {
        if (v == "ID") {
          return(textInput(inputId = "in__ID", label = pretty_label("ID"), value = ""))
        }
        if (v %in% binary_vars) {
          return(selectInput(paste0("in__", v), pretty_label(v), choices = c("yes" = 1, "no" = 0), selected = 0))
        }
        if (v %in% num_vars_all) {
          r <- ranges[[v]]
          step_val <- if (v == "CREDIT_SCORE") 0.001 else if (int_like[[v]]) 1 else NA
          return(numericInput(
            inputId = paste0("in__", v),
            label   = pretty_label(v),
            value   = NA,
            min     = 0,  # enforce non-negative in UI
            max     = if (is.finite(r["max"])) r["max"] else NA,
            step    = step_val
          ))
        }
        lvls <- levels(as.factor(data[[v]]))
        selectInput(paste0("in__", v), pretty_label(v), choices = lvls, selected = lvls[1])
      }),
      actionButton("compute", "Calculate Pure Premium", class = "btn-danger"),
      width = 4
    ),
    mainPanel(
      h4("Results"),
      tableOutput("results"),
      tags$hr(),
      h5("Numeric inputs after imputation"),
      tableOutput("imputed"),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  
  make_new_row <- reactive({
    vals <- lapply(ui_vars, function(v) input[[paste0("in__", v)]])
    names(vals) <- ui_vars
    df <- as.data.frame(vals, stringsAsFactors = FALSE)
    
    for (v in ui_vars) {
      if (v == "ID") {
        df[[v]] <- as.character(df[[v]])
      } else if (v %in% binary_vars) {
        df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
      } else if (v %in% num_vars_all) {
        df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
      } else {
        df[[v]] <- factor(df[[v]], levels = levels(as.factor(data[[v]])))
      }
    }
    df
  })
  
  # Validate non-negative numeric inputs; if any negative, block and show message
  validate_non_negative <- function(row_df) {
    nums <- setdiff(num_vars_all, character(0))
    if (!length(nums)) return(TRUE)
    all(vapply(nums, function(v) all(is.na(row_df[[v]]) | row_df[[v]] >= 0), logical(1)))
  }
  
  obs <- eventReactive(input$compute, {
    new_row <- make_new_row()
    
    validate(need(validate_non_negative(new_row), "please input positive numbers only"))
    
    new_row_imp <- impute_numeric_row(new_row)
    
    nd_logit <- prep_newdata_for(new_row_imp, outcome.3.logistic)
    nd_gamma <- prep_newdata_for(new_row_imp, claimsize.3.gamma)
    
    p_claim <- as.numeric(predict(outcome.3.logistic, newdata = nd_logit, type = "response"))
    sev_hat <- as.numeric(predict(claimsize.3.gamma,   newdata = nd_gamma, type = "response"))
    pure_p  <- p_claim * sev_hat
    
    list(row_used = new_row_imp, p_claim = p_claim, sev_hat = sev_hat, pure_premium = pure_p)
  }, ignoreInit = TRUE)
  
  output$results <- renderTable({
    o <- obs(); if (is.null(o)) return(NULL)
    data.frame(
      Metric = c("Probability of claim",
                 "Conditional severity",
                 "pure premium = probability of claim x conditional severity"),
      Value  = signif(c(o$p_claim, o$sev_hat, o$pure_premium), 6),
      check.names = FALSE
    )
  })
  
  output$imputed <- renderTable({
    o <- obs(); if (is.null(o) || !length(num_impute_vars)) return(NULL)
    as.data.frame(o$row_used[, num_impute_vars, drop = FALSE])
  }, rownames = TRUE)
}

shinyApp(ui, server)