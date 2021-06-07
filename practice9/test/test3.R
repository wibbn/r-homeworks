if ('testthat' %in% rownames(installed.packages()) == FALSE) {
  install.packages('testthat')
}

library(testthat)
source("../R/funcs3.R", chdir=TRUE)

context('Tests for third part - equations solves')

test_that('SIM: Simple tests', {
  expect_equal(SIM(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1)), c(2, 3), tolerance = 1e-6)
  expect_equal(SIM(rbind(c(1, 0), c(0, 1)), c(2, 3), rnorm(2)), c(2, 3), tolerance = 1e-6)
  
  expect_equal(SIM(rbind(c(2, 0), c(0, 2)), c(4, 6), c(1, 1)), c(2, 3), tolerance = 1e-6)
  expect_equal(SIM(rbind(c(2, 0), c(0, 2)), c(4, 6), rnorm(2)), c(2, 3), tolerance = 1e-6)
})


test_that('SIM: arguments boundary cases', {
  expect_error(SIM(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3)))
  expect_error(SIM(c(1, 2, 3), 1, c(1, 2, 3)))
  expect_error(SIM(c(1, 2, 3), c(1, 2, 3), 1))
  
  expect_error(SIM(rbind(c(1, 0), c(0, 1)), c(1, 2, 3), c(1, 1)))
  expect_error(SIM(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1, 1)))
  
  expect_error(SIM(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1), count = 0))
  expect_error(SIM(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1), eps = -1))
  
  expect_error(SIM(rbind(c(2, 1), c(1, -2)), c(2, 3), c(1, 1)))
  expect_error(SIM(rbind(c(10, 2, -1), c(-2, -6, -1), c(1, -3, 12)), c(5, 24.42, 36), rnorm(3)))
})