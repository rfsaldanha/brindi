test_that("expand_indi_parquet works", {
  tmpdir <- tempdir()

  expand_indi_parquet(
    agg = "uf_res",
    agg_time = "year",
    anos = 2010,
    dir = tmpdir,
    indi = "indi_0001"
  )

  res <- arrow::read_parquet(paste0(tmpdir, "/", "indi_0001", ".parquet"))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})
