# Jackknife for the mean
mean.jackknife <- function(a_vector) {
  n <- length(a_vector)
  jackknife.ests <- vector(length=n)
  for (omitted.point in 1:n) {
    jackknife.ests[omitted.point] <- mean(a_vector[-omitted.point])
  }
  variance.of.ests <- var(jackknife.ests)
  jackknife.var <- ((n-1)^2/n)*variance.of.ests
  jackknife.stderr <- sqrt(jackknife.var)
  return(jackknife.stderr)
}

-----------------------------------------------
# Jackknife forGammaParameters
gamma.est <- function(the_data) {
  m <- mean(the_data)
  v <- var(the_data)
  a <- m^2/v
  s <- v/m
  return(c(a=a,s=s))
}

gamma.jackknife <- function(a_vector) {
  n <- length(a_vector)
  jackknife.ests <- matrix(NA,nrow=2,ncol=n)
  rownames(jackknife.ests) = c("a"
                               ,
                               "s")
  for (omitted.point in 1:n) {
    fit <- gamma.est(a_vector[-omitted.point])
    jackknife.ests["a"
                   ,omitted.point] <- fit["a"]
    jackknife.ests["s"
                   ,omitted.point] <- fit["s"]
  }
  variance.of.ests <- apply(jackknife.ests,1,var)
  jackknife.vars <- ((n-1)^2/n)*variance.of.ests
  jackknife.stderrs <- sqrt(jackknife.vars)
  return(jackknife.stderrs)
}

-----------------------------------------------
# Jackknife for linear regression coefficients
  jackknife.lm <- function(df,formula,p) {
    n <- nrow(df)
    jackknife.ests <- matrix(0,nrow=p,ncol=n)
    for (omit in 1:n) {
      new.coefs <- lm(as.formula(formula),data=df[-omit,])$coefficient
      jackknife.ests[,omit] <- new.coefs
    }
    variance.of.ests <- apply(jackknife.ests,1,var)
    jackknife.var <- ((n-1)^2/n)*variance.of.ests
    jackknife.stderr <- sqrt(jackknife.var)
    return(jackknife.stderr)
  }
  
-----------------------------------------------
# Refactoring the Jackknife
  ## The Common Operation
omit.case <- function(the_data,omitted_point) {
  data_dims <- dim(the_data)
  if (is.null(data_dims) || (length(data_dims)==1)) {
    return(the_data[-omitted_point])
  } else {
    return(the_data[-omitted_point,])
  }
}
  
  ## TheGeneral Operation
jackknife <- function(estimator,the_data) {
  if (is.null(dim(the_data))) { n <- length(the_data) }
  else { n <- nrow(the_data) }
  omit_and_est <- function(omit) {
    estimator(omit.case(the_data,omit))
  }
  jackknife.ests <- matrix(sapply(1:n, omit_and_est), ncol=n)
  var.of.reestimates <- apply(jackknife.ests,1,var)
  jackknife.var <- ((n-1)^2/n)* var.of.reestimates
  jackknife.stderr <- sqrt(jackknife.var)
  return(jackknife.stderr)
}


  