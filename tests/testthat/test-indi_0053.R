testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_PNADC_INTEGRATION_TESTS"), "true"),
  "PNADc integration tests require BRINDI_RUN_PNADC_INTEGRATION_TESTS=true"
)

# brasil

testthat::test_that("indi_0053 works with brasil", {
  res <- indi_0053(
    agg = "brasil",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0053")
  testthat::expect_equal(res$ano, 2023)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor > 0))

  # RIPSA reports approximately 3.6 for Brazil in 2023.
  # A tolerance is used to accommodate revisions in PNADc microdata/weights.
  testthat::expect_equal(
    res$valor,
    3.6,
    tolerance = 0.2
  )
})

# uf

testthat::test_that("indi_0053 works with uf", {
  res <- indi_0053(
    agg = "uf",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf)), 27)
  testthat::expect_true(all(res$nome == "indi_0053"))
  testthat::expect_true(all(res$ano == 2023))
  testthat::expect_true(all(res$agg == "uf"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor > 0))
})

# capitals

testthat::test_that("indi_0053 works with capitals", {
  res <- indi_0053(
    agg = "capital",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$capital)), 27)
  testthat::expect_true(all(res$nome == "indi_0053"))
  testthat::expect_true(all(res$ano == 2023))
  testthat::expect_true(all(res$agg == "capital"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor > 0))
})

# multiple years and RIPSA interview rule

testthat::test_that("indi_0053 works with multiple years", {
  res <- indi_0053(
    agg = "brasil",
    ano = c(2022, 2023)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 2)
  testthat::expect_equal(sort(unique(res$ano)), c(2022, 2023))
  testthat::expect_true(all(res$nome == "indi_0053"))
  testthat::expect_true(all(res$agg == "brasil"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor > 0))
})

# invalid aggregation

testthat::test_that("indi_0053 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0053(
      agg = "mun_res",
      ano = 2023
    ),
    "`agg` must be one of: 'brasil', 'uf', or 'capital'.",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0053 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0053(
      agg = "brasil",
      agg_time = "month",
      ano = 2023
    ),
    "PNAD Continua-based indicators are annual.",
    fixed = TRUE
  )
})

# unsupported years

testthat::test_that("indi_0053 rejects years before PNAD Continua", {
  testthat::expect_error(
    indi_0053(
      agg = "brasil",
      ano = 2011
    ),
    "indi_0053 supports PNAD Continua annual microdata from 2012 onward.",
    fixed = TRUE
  )
})
