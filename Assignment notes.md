Plan:
- split data into training and validation 
	- conventional 80-30 split
	- split data before imputing missing values to ensure no data leakage occurs
	- great difficulty with using PCA imputation of values, since packages did not support using the same PCA model used to impute train data to also be used on the test data. 
	- opted for knn imputation as an alternative
	- only using numerical columns to impute!
- Explore data
	- With missing cells, we impute them using randomforests to take full advantage of the correlation between covariates
- Find
	- high leverage points(?)
	- outliers
	- multicolinearity
	- heteroskedasticity
- Subset selection
- Model number of claims variable as GLM (poisson?)
- Model claim amount variable
- test models
	- crucial to avoid data leakage: when testing, impute missing values based only on the train data





## Claim size modelling
- gamma with log link
**In the "Justification" section:** "A **log link** was chosen for the Gamma severity model. This choice is motivated by two key factors: **(1)** it ensures that all predicted claim severities are positive, which is a necessary real-world constraint; and **(2)** it models the effect of risk characteristics as multiplicative (e.g., a 10% increase), which is more aligned with actuarial pricing principles than an additive model. An identity link was rejected as it could produce nonsensical negative predictions, while an inverse link was rejected due to its lack of a clear business interpretation."
- WHy impute values
For your report, you could phrase it like this: _"The raw dataset contained missing values in several key predictor variables. To avoid the significant loss of statistical power and the potential introduction of sampling bias that would result from listwise deletion, a multiple imputation strategy using Principal Component Analysis (or Random Forest) was employed. This approach ensures that all observations contribute to the final model and that the model is trained on a dataset that is more representative of the entire policyholder population."_
- What to do about heteroskedasticity
The good news is that you are likely already using the correct models to handle this. The key is to explain _why_ in your report. **For your Gamma GLM:** The primary reason actuaries use a **Gamma GLM with a log link** is precisely because it is designed to handle the kind of heteroskedasticity you're seeing. The variance of the Gamma distribution is not constant; it is proportional to the square of the mean (Var(Y)∝μ2). This mathematical property naturally models the "funnel shape" where the variance increases as the predicted claim size increases.
 **Action:** In your report, state that the Gamma GLM was chosen specifically to account for the heteroskedasticity inherent in claim severity data. Your residual plot provides the evidence that this was a correct and necessary choice.
- why cooks distance
opted for cooks distance as it combines the outlier and high leverage status of points into one metric
- comparing dispersion of gamma model with and without influential points
  "Finally, the model's dispersion parameter was compared before and after the removal of the 96 influential points. The dispersion remained unchanged (approximately 1.24), indicating that these points were not the root cause of the model's overdispersion. This confirmed that the observed overdispersion is a general feature of the dataset. Given that these points represent legitimate high-risk profiles and their removal does not improve the overall model fit, the final decision was made to retain them to ensure an unbiased and robust pricing model."

- Gamma model had lower RMSE , while lognormal model had a slightly lower MAE. However, in calculating 

# Claim counts modelling
- Possible models
- Logistic regression
	- binomial with loglink function
	- why? easy to interpret, as is in log odds scale
		- exponentiation to get how much a unit increase in the variable would increase the odds of a claim
- binomial with probit link
	- still good, but slightly worse AIC and AUC


"To select the final model for predicting claim occurrence, a range of methods were evaluated, including traditional GLMs (with logit and probit links) and more complex machine learning models such as Support Vector Machines (SVM) and Random Forest. While the machine learning models, particularly Random Forest, achieved a marginally higher predictive accuracy as measured by AUC, they were ultimately not selected due to their 'black box' nature, which makes it difficult to isolate the specific impact of each risk factor.

For a pricing task, model transparency and the ability to explain results to stakeholders are paramount. Therefore, the **logit link GLM (logistic regression) was selected for the final model due to its superior interpretability**. The ability to convert logit coefficients into odds ratios allows for clear and direct business insights, such as stating that a one-unit increase in a predictor changes the odds of a claim by a specific percentage. This level of transparency was deemed more valuable than the minor uplift in predictive power offered by the less interpretable machine learning alternatives."