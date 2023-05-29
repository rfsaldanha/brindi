test_that("indi_0002 works with mun res", {
  res <- indi_0002(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("indi_0002 works with mun ocor", {
  res <- indi_0002(agg = "mun_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("indi_0002 works with uf res", {
  res <- indi_0002(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0002 works with uf ocor", {
  res <- indi_0002(agg = "uf_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0002 works with regsaude res", {
  res <- indi_0002(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 360)
})

test_that("indi_0002 works with regsaude ocor", {
  res <- indi_0002(agg = "regsaude_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 360)
})

test_that("indi_0002 works with regsaude 449 res", {
  res <- indi_0002(agg = "regsaude_449_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 440)
})

test_that("indi_0002 works with regsaude 449 ocor", {
  res <- indi_0002(agg = "regsaude_449_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 440)
})

test_that("indi_0002 works with uf and month aggregation res", {
  res <- indi_0002(agg = "uf_res", agg_time = "month", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 300)
})

test_that("indi_0002 works with uf and week aggregation res", {
  res <- indi_0002(agg = "uf_res", agg_time = "week", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1300)
})

test_that("indi_0002 works with mun_res and multiple years res", {
  res <- indi_0002(agg = "mun_res", ano = c(2013, 2014))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 10400)
})

test_that("indi_0002 works with uf_res and multiple years res", {
  res <- indi_0002(agg = "uf_res", ano = c(2013, 2014))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 50)
})
