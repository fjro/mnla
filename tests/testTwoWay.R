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

test_that("Sigmoid implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sigmoid(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sigmoidCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Step implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- step(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- stepCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

# test_that("Spike implementations are equal", {
#   set.seed(1)
#   x <- runif(10)
#   set.seed(1)
#   y <- spike(x, 3, 10, 30, 10)
#   set.seed(1)
#   ycpp <- spikeCpp(x, 3, 10, 30, 10)
#   expect_equal(y, ycpp)
# })

test_that("Sine low implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sinLow(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sinLowCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Sine high implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sinHigh(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sinHighCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Linear periodic implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- linearPeriodic(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- linearPeriodicCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Varying freq implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- varyingFreq(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- varyingFreqCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

# test_that("Circle implementations are equal", {
#   set.seed(1)
#   x <- runif(10)
#   set.seed(1)
#   y <- circle(x, 3, 10, 30, 10)
#   set.seed(1)
#   ycpp <- circleCpp(x, 3, 10, 30, 10)
#   expect_equal(y, ycpp)
# })

# test_that("X shaped implementations are equal", {
#   set.seed(1)
#   x <- runif(10)
#   set.seed(1)
#   y <- xShaped(x, 3, 10, 30, 10)
#   set.seed(1)
#   ycpp <- xShapedCpp(x, 3, 10, 30, 10)
#   expect_equal(y, ycpp)
# })