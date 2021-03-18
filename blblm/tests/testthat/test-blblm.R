
test_that("Coefficient Results are similar for Parallel vs Single Core",{
  library(furrr)
  plan(multiprocess,workers = 12)
  set.seed(121)
  n = 5000
  y = rnorm(n,mean = 10,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .25)
  z= 0.3 * y + rnorm(n, mean = 0, sd = .25)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =1000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =1000,parallel = TRUE)
  expect_equal(coef(fit1),coef(fit2),tolerance = 0.005)

  set.seed(3)
  n = 10000
  y = rnorm(n,mean = 5,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .2)
  z= 0.1 * y + rnorm(n, mean = 0, sd = .2)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =10000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =10000,parallel = TRUE)
  expect_equal(coef(fit1),coef(fit2),tolerance = 0.005)


  set.seed(8)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_equal(coef(fit1),coef(fit2),tolerance = 2)


  set.seed(833)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000, parallel = TRUE)
  expect_equal(coef(fit1),coef(fit2),tolerance = .7)



})



test_that("Sigma Results are similar for Parallel vs Single Core",{
  library(furrr)
  plan(multiprocess,workers = 12)
  set.seed(121)
  n = 5000
  y = rnorm(n,mean = 10,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .25)
  z= 0.3 * y + rnorm(n, mean = 0, sd = .25)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =1000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =1000,parallel = TRUE)
  expect_equal(sigma(fit1),sigma(fit2),tolerance = 0.005)

  set.seed(3)
  n = 10000
  y = rnorm(n,mean = 5,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .2)
  z= 0.1 * y + rnorm(n, mean = 0, sd = .2)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =10000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =10000,parallel = TRUE)
  expect_equal(sigma(fit1),sigma(fit2),tolerance = 0.005)


  set.seed(8)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_equal(sigma(fit1),sigma(fit2),tolerance = 2)


  set.seed(833)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000, parallel = TRUE)
  expect_equal(sigma(fit1),sigma(fit2),tolerance = .7)


})





test_that("Confidence Interval Results are similar for Parallel vs Single Core",{
  library(furrr)
  plan(multiprocess,workers = 12)
  set.seed(121)
  n = 5000
  y = rnorm(n,mean = 10,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .25)
  z= 0.3 * y + rnorm(n, mean = 0, sd = .25)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =1000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =1000,parallel = TRUE)
  expect_equal(confint(fit1),confint(fit2),tolerance = 0.005)

  set.seed(3)
  n = 10000
  y = rnorm(n,mean = 5,sd = 2)
  x = .5 * y + rnorm(n, mean = 0, sd = .2)
  z= 0.1 * y + rnorm(n, mean = 0, sd = .2)
  df = data.frame(y =y , x =x , z= z)
  fit1 = blblm(y~x+z,data = df,m = 10, B =10000)
  fit2 = blblm(y~x+z,data = df,m = 10, B =10000,parallel = TRUE)
  expect_equal(confint(fit1),confint(fit2),tolerance = 0.005)


  set.seed(8)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_equal(confint(fit1),confint(fit2),tolerance = 2)


  set.seed(833)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000, parallel = TRUE)
  expect_equal(confint(fit1),confint(fit2),tolerance = .7)


})


predict


test_that("Predict Results are similar for Parallel vs Single Core",{
  library(furrr)
  plan(multiprocess,workers = 12)
  set.seed(8)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_equal(predict(fit1, data.frame(wt = c(2.5, 3), hp = c(150, 170))),
               predict(fit2, data.frame(wt = c(2.5, 3), hp = c(150, 170))),tolerance = 2)


  set.seed(833)
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000)
  fit2 = blblm(mpg ~ wt * hp, data = mtcars, m = 2, B = 10000, parallel = TRUE)
  expect_equal(predict(fit1, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE),
              predict(fit2, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE),tolerance = 2)


})












