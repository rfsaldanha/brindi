test_that("indi_0001 works with mun res", {
  res <- indi_0001(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("indi_0001 works with uf res", {
  res <- indi_0001(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})
