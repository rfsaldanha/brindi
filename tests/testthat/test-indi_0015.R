test_that("indi_0015 works with mun res", {
  res <- indi_0015(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5500)
})

test_that("indi_0015 works with mun res and month aggregation", {
  res <- indi_0015(agg = "mun_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 67100)
})

test_that("indi_0015 works with mun res and week aggregation", {
  res <- indi_0015(agg = "mun_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 296500)
})

test_that("indi_0015 works with mun res and multiple years", {
  res <- indi_0015(agg = "mun_res", ano = c(2013, 2015))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 11100)
})
