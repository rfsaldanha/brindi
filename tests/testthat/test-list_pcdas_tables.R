test_that("list pcdas tables works", {
  res <- list_pcdas_tables()

  expect_equal("character", class(res))
})
