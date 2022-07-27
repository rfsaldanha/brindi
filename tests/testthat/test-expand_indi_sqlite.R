test_that("expand_indi_sqlite works", {

  tmp_db <- tempfile()

  expand_indi_sqlite(
    agg = c("uf_res", "uf_ocor"),
    anos = 2011:2013,
    db = tmp_db
  )

  expect_gt(file.size(tmp_db), 0)
})
