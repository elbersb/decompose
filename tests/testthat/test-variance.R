library("data.table")
library("decompose")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# should have a test with missing values

data <- data.table(y = rnorm(1000), x = sample(1:5, 1000, replace = TRUE), wt = runif(1000))
decompose_variance(data, y ~ x)
decompose_variance(data, y ~ x, weight = "wt")
