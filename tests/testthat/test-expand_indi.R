test_that("indi_expand works with several arguments", {
  res <- expand_indi(
    agg = c("uf_res", "uf_ocor"),
    agg_time = c("year", "month"),
    anos = c(2010, 2011),
    pop_source = "datasus",
    adjust_rates = FALSE,
    indi_fun = "indi_0001_adj"
  )

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1400)
})
