test_that("indi_0064 has expected signature", {
  expect_true(is.function(indi_0064))
  expect_true(all(c("agg","agg_time","ano","multi","decimals","pop_source","pcdas_token","adjust_rates") %in% names(formals(indi_0064))))
})
