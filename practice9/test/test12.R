if ('testthat' %in% rownames(installed.packages()) == FALSE) {
  install.packages('testthat')
}

library(testthat)
source("../R/funcs1.R", chdir=TRUE)
source("../R/funcs2.R", chdir=TRUE)

context('Tests for Alter-Johns function and stuff')

test_that("arifm: Simple test for arifm proportion", {
  expect_equal(arifm(c(1, 2, 3, 4, 5)), c(0, 0, 0))
  expect_equal(arifm(c(1, 2, 1, 2, 1)), c(-0.2876821, 0.2876821, -0.2876821), tolerance=1e-7)
  expect_equal(arifm(c(10, 20, 40, 80, 160)), c(0.1495317, 0.1790482, 0.1986707), tolerance=1e-7)
})

test_that("geom: Simple test for geom proportion", {
  expect_equal(geom(c(1, 2, 3, 4, 5)), c(-0.06453852, -0.04082199, -0.02817088), tolerance=1e-7)
  expect_equal(geom(c(1, 2, 1, 2, 1)), c(-0.5753641, 0.5753641, -0.5753641), tolerance=1e-7)
  expect_equal(geom(c(1, 2, 4, 8, 16)), c(0.11778304, 0.10536052, 0.07696104), tolerance=1e-7)
})

test_that("garm: Simple test for garm proportion", {
  expect_equal(garm(c(1, 2, 3, 4, 5)), c(-0.06453852, -0.04082199, -0.02817088), tolerance=1e-7)
  expect_equal(garm(c(100, 500, 300, 10, 5)), c(-1.1673886, -2.2889801, 0.2831371), tolerance=1e-7)
  expect_equal(garm(c(1, 20, 400, 8000, 160000)), c(-1.306718, -2.215008, -2.300357), tolerance=1e-7)
})

test_that('out_of_trend: It works (equality to proportions)', {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5)), arifm(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2), arifm(c(1, 2, 3, 4, 5), dt = 2))
  
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = 'Geom'), geom(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2, method = 'Geom'), geom(c(1, 2, 3, 4, 5), dt = 2))
  
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = 'Garm'), garm(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2, method = 'Garm'), garm(c(1, 2, 3, 4, 5), dt = 2))
})

test_that('out_of_trend: Arguments boundary cases', {
  expect_error(out_of_trend(10))
  expect_error(out_of_trend(c()))
  expect_error(out_of_trend(c(1)))
  expect_error(out_of_trend(c(1, 2)))
  
  expect_error(out_of_trend(c(1, 2, 3, 4, 5), dt = 3))
})

test_that('alter_johns: Result format test (length of resulting vector)', {
  for (i in 2:100) {
    expect_equal(length(alter_johns(runif(i))), i - 1)
  }

  expect_true(is.double(alter_johns(rep(1, 100))))
})

test_that('alter_johns: Return value tests', {
  expect_equal(alter_johns(c(1, 2, 3)), c(1, 2))
  expect_equal(alter_johns(c(0, 1, 2)), c(1, 2))
  
  expect_equal(alter_johns(c(1, 2, 3, 4, 50, 6)), c(18.6, 13.25, 18, 26.5, 5))
  expect_equal(alter_johns(c(1, 20, 300, 4000)), c(1333, 2139.5, 3999))
  expect_equal(alter_johns(c(2.5, 1, 3.5)), c(2, 1))
  
  for (i in 1:100) {
    expect_equal(alter_johns(rep(i, 10)), rep(0, 9))
  }
})

test_that('alter_johns: arguments boundary cases', {
  expect_error(alter_johns('HELP'))
  expect_error(alter_johns(c('H', 'E', 'L', 'P', '!!!')))
  
  expect_error(alter_johns(list(1, 2, 3)))
  
  expect_error(alter_johns(c(1)))
  
  expect_true(all(is.na(alter_johns(c(2.5, 1, 3.5, NA)))))
})