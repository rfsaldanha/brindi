testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_SIDRA_INTEGRATION_TESTS"), "true"),
  "SIDRA integration tests require BRINDI_RUN_SIDRA_INTEGRATION_TESTS=true"
)

# brasil - total

testthat::test_that("indi_0058 works with brasil", {
  res <- indi_0058(
    agg = "brasil",
    ano = 2024
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0058")
  testthat::expect_equal(res$ano, 2024)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_equal(res$sexo, "total")
  testthat::expect_equal(
    res$valor,
    44.3,
    tolerance = 0.1
  )
})

# brasil - 2023

testthat::test_that("indi_0058 works with 2023", {
  res <- indi_0058(
    agg = "brasil",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(
    res$valor,
    45.6,
    tolerance = 0.1
  )
})

# brasil - masculino

testthat::test_that("indi_0058 works with masculino", {
  res <- indi_0058(
    agg = "brasil",
    ano = 2024,
    sexo = "masculino"
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$sexo, "masculino")
  testthat::expect_true(is.finite(res$valor))
  testthat::expect_true(res$valor >= 0 & res$valor <= 100)
})

# brasil - feminino

testthat::test_that("indi_0058 works with feminino", {
  res <- indi_0058(
    agg = "brasil",
    ano = 2024,
    sexo = "feminino"
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$sexo, "feminino")
  testthat::expect_true(is.finite(res$valor))
  testthat::expect_true(res$valor >= 0 & res$valor <= 100)
})

# uf res

testthat::test_that("indi_0058 works with uf res", {
  res <- indi_0058(
    agg = "uf_res",
    ano = 2024
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf_res)), 27)
  testthat::expect_true(all(res$nome == "indi_0058"))
  testthat::expect_true(all(res$ano == 2024))
  testthat::expect_true(all(res$agg == "uf_res"))
  testthat::expect_true(all(res$sexo == "total"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor >= 0 & res$valor <= 100))
})

# multiple years

testthat::test_that("indi_0058 works with multiple years", {
  res <- indi_0058(
    agg = "uf_res",
    ano = c(2023, 2024)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 54)
  testthat::expect_equal(
    sort(unique(res$ano)),
    c(2023, 2024)
  )
  testthat::expect_true(all(res$nome == "indi_0058"))
  testthat::expect_true(all(res$agg == "uf_res"))
  testthat::expect_true(all(res$sexo == "total"))
  testthat::expect_true(all(is.finite(res$valor)))
})

# unavailable year

testthat::test_that("indi_0058 rejects unavailable years", {
  testthat::expect_error(
    indi_0058(
      agg = "brasil",
      ano = 2021
    ),
    "indi_0058 currently supports SIDRA table 7269 for years",
    fixed = TRUE
  )
})

# invalid aggregation

testthat::test_that("indi_0058 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0058(
      agg = "mun_res",
      ano = 2024
    ),
    "`agg` must be either 'brasil' or 'uf_res'.",
    fixed = TRUE
  )
})

# invalid sex

testthat::test_that("indi_0058 rejects unsupported sex", {
  testthat::expect_error(
    indi_0058(
      agg = "brasil",
      ano = 2024,
      sexo = "outro"
    ),
    "`sexo` must be one of",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0058 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0058(
      agg = "brasil",
      agg_time = "month",
      ano = 2024
    ),
    "Proportion without basic education is annual.",
    fixed = TRUE
  )
})
