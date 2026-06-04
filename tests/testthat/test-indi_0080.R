test_that("indi_0080 has expected signature", {
  expect_true(is.function(indi_0080))
  expect_true(all(c("agg","agg_time","ano","multi","decimals","pop_source","pcdas_token","adjust_rates") %in% names(formals(indi_0080))))
})
