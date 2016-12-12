# Unit test two way functions

test_that("linear implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- linear(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- linearCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("quadratic implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- quadratic(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- quadraticCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

# test_that("cubic implementations are equal", {
#   set.seed(1)
#   x <- runif(10)
#   set.seed(1)
#   y <- cubic(x, 3, 10, 30, 10)
#   set.seed(1)
#   ycpp <- cubicCpp(x, 3, 10, 30, 10)
#   expect_equal(y, ycpp)
# })

# test_that("qroot implementations are equal", {
#   set.seed(1)
#   x <- runif(10)
#   set.seed(1)
#   y <- qroot(x, 3, 10, 30, 10)
#   set.seed(1)
#   ycpp <- qrootCpp(x, 3, 10, 30, 10)
#   expect_equal(y, ycpp)
# })

test_that("exponential implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- exponential2(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- exponentialCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp, tolerance = .000001)
})

test_that("Natural log implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- logE(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- logECpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})