test_that("indi_0001 works with mun res", {
  res <- indi_0001(agg = "mun_res", ano = 2013)
  res <- indi_0001_adj(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("indi_0001 works with mun ocor", {
  res <- indi_0001(agg = "mun_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("indi_0001 works with uf res", {
  res <- indi_0001(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0001 works with uf ocor", {
  res <- indi_0001(agg = "uf_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0001 works with regsaude res", {
  res <- indi_0001(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0001 works with regsaude ocor", {
  res <- indi_0001(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0001 works with mun res and month", {
  res <- indi_0001(agg = "mun_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 33000)
})

test_that("indi_0001 works with mun res and week", {
  res <- indi_0001(agg = "mun_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 63000)
})

test_that("indi_0001 works with mun res and multiple years", {
  res <- indi_0001(agg = "mun_res", agg_time = "year", ano = c(2012, 2013))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 10000)
})
