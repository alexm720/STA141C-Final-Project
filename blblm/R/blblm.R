#' @aliases blblm-package
#' @import purrr
#' @import stats
#' @import furrr
#' @importFrom utils capture.output
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' @title Bag of little Bootstraps for lm
#' @description This functions aims to improve the results produced by the lm function with the use of blb. The functions allows
#' for the user to set the number of splits they want in their data along with the number of times they want to bootstrap their data.
#' This function also allows for parallelization. NOTE: set up workers before running function if you're planning to use parallelization.
#' e.g plan(multiprocess, workers = 4).
#' @param formula An object of class "formula". Can also be seen as a symbolic description of a model that is to be fit
#'
#' @param data A dataframe containing the data used in formula.
#' @param m  Represents the amount of times the data given is split.
#' @param B Represents the amount of times the data is run on bootstrap
#' @param parallel A boolean which denotes whether or not the user would like to use parallelization. If TRUE, blb will be run with
#' the maximum number of threads available. If FALSE, then a single thread will be used.
#' @export
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
blblm <- function(formula, data, m = 10, B = 5000, parallel = FALSE) {
  if(parallel == TRUE)
  {
    data_list <- split_data(data,m)
    estimates<- furrr::future_map(data_list, ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
    class(res) <- "blblm"
    invisible(res)
  }

  else{
    data_list <- split_data(data, m)
    estimates <- map(data_list, ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
    class(res) <- "blblm"
    invisible(res)
  }
}


#' split data into m parts of approximated equal sizes
#' @title Data split
#' @description splits data in m parts that are approximately equal sizes.
#' @param data dataframe
#' @param m number of times to split the dataframe
split_data <- function(data, m) {

    idx <- sample.int(m, nrow(data), replace = TRUE)
    data %>% split(idx)

}


#' compute the estimates
#' @title BootStrap estimates
#' @description Computes the estimates B number of times and returns the results
#' @param formula formula of given fit
#' @param data data as a dataframe
#' @param n size of data
#' @param B number of times to be bootstrapped
lm_each_subsample <- function(formula, data, n,B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.

      environment(formula) <- environment()
      m <- model.frame(formula, data)
      X <- model.matrix(formula, m)
      y <- model.response(m)
      ret = replicate(B, lm1(X, y, n), simplify = FALSE)
      return(ret)


}


#' compute the regression estimates for a blb dataset
#' @title List of coefficients and sigma
#' @description calls blbcoef and blbsigma and returns a list of both coefficients and sigma values
#' @param X  data split in m ways as a matrix
#' @param y response values split in m ways
#' @param n size of data
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#' @title Coefficients for blblm model
#' @description calls coef function to compute coefficients
#' @param fit A fit blblm Model
blbcoef <- function(fit) {
    coef(fit)
}


#' compute sigma from fit
#' @title sigma
#' @description call returns the sigma value of the given fit model
#' @param fit A fit model
#'
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}



#' @title Blblm Model
#' @description Prints the Model formula used to fit your data using the blblm function .
#' @param x A fit blblm Model
#' @param ... Argument that is not predefined
#' @export
#' @method print blblm
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' print(fit)
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @title Sigma for blb fit model
#' @description This function returns the sigma value of a fit model using the blblm function.
#' @param object A fit blb Model
#' @param confidence If this parameter is set to TRUE, the confidence interval of the sigma value will be returned. Else is set the FALSE, a
#' single value for the sigma will be returned.
#' @param level This is the level of confidence that is used to calculate your confidence interval.When confidence is set to TRUE,
#' this value can be changed to alter your confidence interval.
#' @param ... Argument that is not predefined
#' @export
#' @method sigma blblm
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' sigma(fit)
#' sigma(fit, confidence = TRUE, level = 0.95)
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' @title Coefficients for blblm model
#' @description This function returns the coefficients when given your fit model produced from the blblm function
#' @param object A fit blblm model
#' @param ... Argument that is not predefined
#' @export
#' @method coef blblm
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000)
#' coef(fit)
coef.blblm <- function(object, ...) {

  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())

}

#' @title Confidence Interval for blblm model
#' @description This function is used to compute the confidence interval for a given fit blblm model
#' @param object A fit blblm model
#' @param parm parameters set to compute your confidence interval
#' @param level The level of confidence to computer your confidence interval. Default level set to 0.96
#' @param ... Argument that is not predefined
#' @export
#' @method confint blblm
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 10000)
#' confint(fit, c("wt", "hp"),level = 0.95)
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p))
             %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @title Predicting on blblm model
#' @description Given a fit blblm model, this function predicts on new data using the already fit model.
#' @param object A fit blblm model
#' @param new_data New data set to be predicted on using model obtained from object
#' @param confidence If set to TRUE, confidence interval of prediction is returned
#' @param level If confidence is set to TRUE, level can be adjusted for wanted confidence interval
#' @param ... Argument that is not predefined
#' @export
#' @method predict blblm
#' @examples
#' fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 10000)
#' predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


#' @title Mean of confidence interval
#' @description computes mean of confidence interval from bootstrap
#' @param x fit data
#' @param level confidence level
#'
#' @export

mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

#' @title Mean
#' @description computes the mean
#' @param .x data
#' @param .f function
#' @param ... Argument that is not predefined
#' @export
map_mean <- function(.x, .f, ...) {
    (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

#' @title cbind data
#' @description cbinds data
#' @param .x data
#' @param .f function
#' @param ... Argument that is not predefined
#'
#' @export
map_cbind <- function(.x, .f, ...) {
    map(.x, .f, ...) %>% reduce(cbind)
}

#' @title rbind data
#' @description rbinds data
#' @param .x data
#' @param .f function
#' @param ... Argument that is not predefined
#'
#' @export
map_rbind <- function(.x, .f, ...){
    map(.x, .f, ...) %>% reduce(rbind)

}
