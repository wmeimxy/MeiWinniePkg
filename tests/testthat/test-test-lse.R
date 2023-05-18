test_that("finite output", {
  expect_equal(is.finite(log_summed_exps(c(1:2000))), TRUE)
})

test_that("test1 1", {
  expect_equal(round(log_summed_exps(c(1:2000)), digits = 3), 2000.459)
})

test_that("warning output", {
  m <- c(1:4) %>% matrix(nrow = 2, ncol = 2)
  expect_warning(log_summed_exps(m))
})

test_that("test 2", {
  expect_equal(log_summed_exps(c(1:10)), log(sum(exp(1:10))))
})

test_that("test 3", {
  expect_equal(log_summed_exps(c(10:1)), log(sum(exp(1:10))))
})
