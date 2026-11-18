# mun res

test_that("indi_0016 works with mun res", {
  res <- indi_0016(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5500)
})

test_that("indi_0016 works with mun res and month aggregation", {
  res <- indi_0016(agg = "mun_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 67100)
})

test_that("indi_0016 works with mun res and week aggregation", {
  res <- indi_0016(agg = "mun_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 296500)
})

test_that("indi_0016 works with mun res and multiple years", {
  res <- indi_0016(agg = "mun_res", ano = c(2013, 2015))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 11100)
})

# uf res

test_that("indi_0016 works with uf res", {
  res <- indi_0016(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 27)
})

test_that("indi_0016 works with uf res and month aggregation", {
  res <- indi_0016(agg = "uf_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 320)
})

test_that("indi_0016 works with uf res and week aggregation", {
  res <- indi_0016(agg = "uf_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1430)
})

test_that("indi_0016 works with uf res and multiple years", {
  res <- indi_0016(agg = "uf_res", ano = c(2013, 2015))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 50)
})

# regsaude 449 res

test_that("indi_0016 works with regsaude 449 res", {
  res <- indi_0016(agg = "regsaude_449_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 440)
})

test_that("indi_0016 works with regsaude 449 res and month aggregation", {
  res <- indi_0016(agg = "regsaude_449_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5300)
})

test_that("indi_0016 works with regsaude 449 res and week aggregation", {
  res <- indi_0016(agg = "regsaude_449_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 23840)
})

test_that("indi_0016 works with regsaude 449 res and multiple years", {
  res <- indi_0016(agg = "regsaude_449_res", ano = c(2013, 2015))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 890)
})
