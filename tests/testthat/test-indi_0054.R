testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_SIDRA_INTEGRATION_TESTS"), "true"),
  "SIDRA integration tests require BRINDI_RUN_SIDRA_INTEGRATION_TESTS=true"
)

# brasil

testthat::test_that("indi_0054 works with brasil", {
  res <- indi_0054(
    agg = "brasil",
    ano = c(2020, 2021)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0054")
  testthat::expect_equal(res$ano_ini, 2020)
  testthat::expect_equal(res$ano_fim, 2021)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_true(is.finite(res$valor))

  # Table 6579 gives approximately 0.74% annual population growth
  # for Brazil between 2020 and 2021.
  testthat::expect_equal(
    res$valor,
    0.74,
    tolerance = 0.05
  )
})

# uf res

testthat::test_that("indi_0054 works with uf res", {
  res <- indi_0054(
    agg = "uf_res",
    ano = c(2020, 2021)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf_res)), 27)
  testthat::expect_true(all(res$nome == "indi_0054"))
  testthat::expect_true(all(res$ano_ini == 2020))
  testthat::expect_true(all(res$ano_fim == 2021))
  testthat::expect_true(all(res$agg == "uf_res"))
  testthat::expect_true(all(is.finite(res$valor)))
})

# mun res

testthat::test_that("indi_0054 works with mun res", {
  res <- indi_0054(
    agg = "mun_res",
    ano = c(2020, 2021)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_gt(nrow(res), 5500)
  testthat::expect_equal(
    length(unique(res$mun_res)),
    nrow(res)
  )
  testthat::expect_true(all(res$nome == "indi_0054"))
  testthat::expect_true(all(res$ano_ini == 2020))
  testthat::expect_true(all(res$ano_fim == 2021))
  testthat::expect_true(all(res$agg == "mun_res"))
  testthat::expect_true(all(is.finite(res$valor)))
})

# multi-year reference period

testthat::test_that("indi_0054 calculates geometric annual growth for multi-year periods", {
  res <- indi_0054(
    agg = "brasil",
    ano = c(2019, 2021)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$ano_ini, 2019)
  testthat::expect_equal(res$ano_fim, 2021)
  testthat::expect_true(is.finite(res$valor))
})

# invalid aggregation

testthat::test_that("indi_0054 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0054(
      agg = "regsaude_449_res",
      ano = c(2020, 2021)
    ),
    "`agg` must be one of: 'brasil', 'uf_res', or 'mun_res'.",
    fixed = TRUE
  )
})

# invalid year vector

testthat::test_that("indi_0054 requires two years", {
  testthat::expect_error(
    indi_0054(
      agg = "brasil",
      ano = 2021
    ),
    "`ano` must contain exactly two valid years",
    fixed = TRUE
  )
})

# reversed period

testthat::test_that("indi_0054 rejects reversed periods", {
  testthat::expect_error(
    indi_0054(
      agg = "brasil",
      ano = c(2021, 2020)
    ),
    "The final year in `ano` must be greater than the initial year.",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0054 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0054(
      agg = "brasil",
      agg_time = "month",
      ano = c(2020, 2021)
    ),
    "Population growth is annual.",
    fixed = TRUE
  )
})
