
# loss size index function

lsplot <- function(data){
  # calculating loss size index for each n and putting it into a df
  ls <- cumsum(sort(data))/sum(data)
  plot(1:length(data)/length(data),
       ls, xlab = "number of claims (in 100%)",
       ylab = "empirical loss size index function")
  abline(h = 0.2, v = 0.8)
}


# mean excess function
mef <- function(x, u) {
  mefvector <- c()
  for (i in u) {
    mefvector <- c(mefvector, sum(pmax(sort(x)- i, 0))/length(x[x >i]))
  }
  return(mefvector)
}

# coding RIGHT censored data
coderightcens <- function(exit, death){
  censdata <- cbind.data.frame(left = exit, right = exit)
  censdata$right[death == 0] <- NA # right censoring those who didnt die
  return(censdata)
}


# negative log likelihood
negloglik <- function(pdf, cdf, param, x, deduct, limitI) {
  # Function returns the negative log likelihood of the censored and
  # truncated dataset. Each data point's contribution to the log
  # likelihood depends on the theoretical distribution pdf and cdf
  # and also the deductible and limit values to adjust for truncation
  # and censoring
  PL <- do.call(cdf, c(list(q = deduct), param)) # prob. below deductible
  PX <- do.call(cdf, c(list(q = x), param)) # prob. above x
  fX <- do.call(pdf, c(list(x = x), param)) # density at x
  lik.contr <- ifelse(limitI, log(1 - PX), log(fX)) - log(1 - PL)
  return(-sum(lik.contr))
}

# Plotting values, logged values and empirical ranks
empiricalrankplot <- function(data){
  par(mfrow = c(1, 3), pty = "s")
  plot(data, pch = 20, cex = 0.5, main = "Original")
  plot(log(data), pch = 20, cex = 0.5, main = "Log values")
  plot(copula::pobs(data)[, 1], copula::pobs(data)[,
                                                   2], pch = 20, cex = 0.5, xlab = paste("rank of", names(data)[1]),
       ylab = paste("rank of", names(data)[1]),
       main = "Empirical ranks")
}

# plotting copula simulations
copulasim <- function(obj){
  x = rCopula(4000, obj)
  par(mfrow = c(1, 2))
  plot(x, main = "simulations", xlab = "u", ylab = "v", pch = ".",
       col = "blue")
  contour(obj, pCopula, main = "CDF", xlab = "u", ylab = "v")
}
