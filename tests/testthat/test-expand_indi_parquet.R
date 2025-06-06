test_that("expand_indi_parquet works", {
  tmpdir <- tempdir()

  expand_indi_parquet(
    agg = "uf_res",
    agg_time = "year",
    anos = 2010,
    dir = tmpdir,
    indi = "indi_0001",
    pop_source = "datasus",
    adjust_rates = FALSE
  )

  res <- arrow::read_parquet(paste0(
    tmpdir,
    "/",
    "indi_0001_uf_res_year",
    ".parquet"
  ))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})
