testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_PNADC_INTEGRATION_TESTS"), "true"),
  "PNADc integration tests require BRINDI_RUN_PNADC_INTEGRATION_TESTS=true"
)

# brasil

testthat::test_that("indi_0052 works with brasil", {
  res <- indi_0052(
    agg = "brasil",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0052")
  testthat::expect_equal(res$ano, 2023)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))

  # IBGE published approximately 4.2% for Brazil in 2023.
  # A tolerance is used to avoid brittleness after microdata revisions.
  testthat::expect_equal(
    res$valor,
    4.2,
    tolerance = 0.3
  )
})

# uf

testthat::test_that("indi_0052 works with uf", {
  res <- indi_0052(
    agg = "uf",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf)), 27)
  testthat::expect_true(all(res$nome == "indi_0052"))
  testthat::expect_true(all(res$ano == 2023))
  testthat::expect_true(all(res$agg == "uf"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# multiple years

testthat::test_that("indi_0052 works with multiple years", {
  res <- indi_0052(
    agg = "uf",
    ano = c(2022, 2023)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 54)
  testthat::expect_equal(sort(unique(res$ano)), c(2022, 2023))
  testthat::expect_true(all(res$nome == "indi_0052"))
  testthat::expect_true(all(res$agg == "uf"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# invalid aggregation

testthat::test_that("indi_0052 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0052(
      agg = "mun_res",
      ano = 2023
    ),
    "`agg` must be either 'brasil' or 'uf'.",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0052 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0052(
      agg = "brasil",
      agg_time = "month",
      ano = 2023
    ),
    "PNAD Continua-based indicators are annual.",
    fixed = TRUE
  )
})

# unavailable module year

testthat::test_that("indi_0052 rejects years without child-labour module", {
  testthat::expect_error(
    indi_0052(
      agg = "brasil",
      ano = 2021
    ),
    "indi_0052 currently supports the PNAD Continua child-labour module",
    fixed = TRUE
  )
})
