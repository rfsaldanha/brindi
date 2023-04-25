test_that("indi_0004 works with mun res", {
  res <- indi_0004(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("indi_0004 works with mun ocor", {
  res <- indi_0004(agg = "mun_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("indi_0004 works with uf res", {
  res <- indi_0004(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0004 works with uf ocor", {
  res <- indi_0004(agg = "uf_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0004 works with regsaude res", {
  res <- indi_0004(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0004 works with regsaude ocor", {
  res <- indi_0004(agg = "regsaude_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0004 works with uf res and month aggregation", {
  res <- indi_0004(agg = "uf_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 320)
})

test_that("indi_0004 works with uf res and week aggregation", {
  res <- indi_0004(agg = "uf_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1300)
})

test_that("indi_0004 works with uf res and multiple years", {
  res <- indi_0004(agg = "uf_res", ano = c(2011, 2013))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 50)
})
