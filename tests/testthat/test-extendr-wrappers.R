# Test a highly composite number and a prime number
test_that("factor works", {
  expect_equal(factors(2520, FALSE), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 18, 20, 21, 24, 28, 30, 35, 36, 40, 42, 45, 56, 60, 63, 70, 72, 84, 90, 105, 120, 126, 140, 168, 180, 210, 252, 280, 315, 360, 420, 504, 630, 840, 1260))
  expect_equal(factors(5040, TRUE), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 24, 28, 30, 35, 36, 40, 42, 45, 48, 56, 60, 63, 70, 72, 80, 84, 90, 105, 112, 120, 126, 140, 144, 168, 180, 210, 240, 252, 280, 315, 336, 360, 420, 504, 560, 630, 720, 840, 1008, 1260, 1680, 2520, 5040))
  expect_equal(factors(113, FALSE), 1)
  expect_equal(factors(97, TRUE), c(1, 97))
})

# Test digital sum
test_that("digital sum works", {
  expect_equal(digital_sum(12345678), 9)
  expect_equal(digital_sum(1e10), 1)
})

# Test polygon area
test_that("polygon area approximates a circle", {
  theta <- seq(0, 2 * pi, length.out = 500)
  x <- cos(theta)
  y <- sin(theta)
  expect_equal(round(polygon_area(x, y), 2), round(pi, 2))
})

# Test replace after
test_that("replace after works", {
  expect_equal(replace_after("hello world", " ", " there"), "hello there")
  expect_equal(replace_after("hello world", "ll", "lp me"), "help me")
  expect_equal(replace_after("hello world", "o", ""), "hell")
})

# Test replace before
test_that("replace before works", {
  expect_equal(replace_before("hello world", " ", "there"), "thereworld")
  expect_equal(replace_before("hello world", "llo", "help me"), "help me world")
  expect_equal(replace_before("hello world", "o", ""), "rld")
})
