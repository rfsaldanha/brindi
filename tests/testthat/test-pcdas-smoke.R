test_that("PCDaS SIM helper returns data", {
  res <- .brindi_get_sim(
    agg = "uf_res",
    agg_time = "year",
    ano = 2013,
    cid_like = "I"
  )

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 0)
})

test_that("PCDaS SIH helper returns data", {
  res <- .brindi_get_sih(
    agg = "uf_res",
    agg_time = "year",
    ano = 2013,
    more_filters = "DIAG_PRINC LIKE 'A00%'"
  )

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 0)
})

test_that("new SIM and SIH indicators are callable", {
  sim_res <- indi_0022(agg = "uf_res", ano = 2013)
  sih_res <- indi_0028(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(sim_res)[1])
  expect_equal("tbl_df", class(sih_res)[1])
  expect_gt(nrow(sim_res), 0)
  expect_gt(nrow(sih_res), 0)
})
