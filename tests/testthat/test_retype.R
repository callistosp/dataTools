context("Test retype()")
library(dataTools)
library(testthat)

test_that("basic typecast test",{
  t <- data.frame(let=c("a", "b", "c"), num=c("1", "2", "3"), stringsAsFactors = F)
  m <- data.frame(let=c("a", "b", "c"), num=c(1, 2, 3), stringsAsFactors = F)
  t <- retype(t, num, numeric)
  expect_identical(t, m)
})

test_that("multiple typecast test",{
  t <- data.frame(let=c("a", "b", "c"), num=c("1", "2", "3"), stringsAsFactors = F)
  m <- data.frame(let=as.factor(c("a", "b", "c")), num=c(1, 2, 3), stringsAsFactors = F)
  t <- retype(t, num, numeric, let, factor)
  expect_identical(t, m)
})

test_that("no input",{
  t <- data.frame(let=c("a", "b", "c"), num=c("1", "2", "3"), stringsAsFactors = F)
  p <- retype(t)
  expect_identical(t, p)
})

test_that("odd number of arguments", {
  t <- data.frame(let=c("a", "b", "c"), num=c("1", "2", "3"), stringsAsFactors = F)
  m <- data.frame(let=as.factor(c("a", "b", "c")), num=c(1, 2, 3), stringsAsFactors = F)
  t <- retype(t, num, numeric, let)
  expect_identical(t, m)
})
