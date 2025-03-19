test_that("expand_indi_sqlite works", {
  tmp_db <- tempfile()

  expand_indi_sqlite(
    agg = c("uf_res", "uf_ocor"),
    agg_time = c("year", "month"),
    anos = c(2011, 2012),
    indi = c("indi_0001"),
    pop_source = "datasus",
    adjust_rates = FALSE,
    db = tmp_db
  )

  expect_gt(file.size(tmp_db), 0)
})
