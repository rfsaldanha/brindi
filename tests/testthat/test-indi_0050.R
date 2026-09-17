testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_PNADC_INTEGRATION_TESTS"), "true"),
  "PNADc integration tests require BRINDI_RUN_PNADC_INTEGRATION_TESTS=true"
)

# brasil

testthat::test_that("indi_0050 works with brasil", {
  res <- indi_0050(
    agg = "brasil",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0050")
  testthat::expect_equal(res$ano, 2023)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# uf

testthat::test_that("indi_0050 works with uf", {
  res <- indi_0050(
    agg = "uf",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf)), 27)
  testthat::expect_true(all(res$nome == "indi_0050"))
  testthat::expect_true(all(res$ano == 2023))
  testthat::expect_true(all(res$agg == "uf"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# multiple years

testthat::test_that("indi_0050 works with multiple years", {
  res <- indi_0050(
    agg = "uf",
    ano = c(2022, 2023)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 54)
  testthat::expect_equal(sort(unique(res$ano)), c(2022, 2023))
  testthat::expect_true(all(res$nome == "indi_0050"))
  testthat::expect_true(all(res$agg == "uf"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# invalid aggregation

testthat::test_that("indi_0050 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0050(
      agg = "mun_res",
      ano = 2023
    ),
    "`agg` must be either 'brasil' or 'uf'.",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0050 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0050(
      agg = "brasil",
      agg_time = "month",
      ano = 2023
    ),
    "PNAD Continua-based indicators are annual.",
    fixed = TRUE
  )
})
