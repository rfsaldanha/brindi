test_that("generic pcdas query works", {
  res <- generic_pcdas_query(sql_query = "SELECT N_AIH, DT_INTER, PROC_REA FROM \"datasus-sih\" LIMIT 100")

  expect_equal(nrow(res), 100)
})
