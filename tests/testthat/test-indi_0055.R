testthat::skip_if_not(
  identical(Sys.getenv("BRINDI_RUN_IBGE_INTEGRATION_TESTS"), "true"),
  "IBGE integration tests require BRINDI_RUN_IBGE_INTEGRATION_TESTS=true"
)

# brasil - total

testthat::test_that("indi_0055 works with brasil", {
  res <- indi_0055(
    agg = "brasil",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$nome, "indi_0055")
  testthat::expect_equal(res$ano, 2023)
  testthat::expect_equal(res$agg, "brasil")
  testthat::expect_equal(res$sexo, "total")
  testthat::expect_equal(
    res$valor,
    76.4,
    tolerance = 0.1
  )
})

# brasil - masculino

testthat::test_that("indi_0055 works with masculino", {
  res <- indi_0055(
    agg = "brasil",
    ano = 2023,
    sexo = "masculino"
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$sexo, "masculino")
  testthat::expect_equal(
    res$valor,
    73.1,
    tolerance = 0.1
  )
})

# brasil - feminino

testthat::test_that("indi_0055 works with feminino", {
  res <- indi_0055(
    agg = "brasil",
    ano = 2023,
    sexo = "feminino"
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$sexo, "feminino")
  testthat::expect_equal(
    res$valor,
    79.7,
    tolerance = 0.1
  )
})

# uf res

testthat::test_that("indi_0055 works with uf res", {
  res <- indi_0055(
    agg = "uf_res",
    ano = 2023
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 27)
  testthat::expect_equal(length(unique(res$uf_res)), 27)
  testthat::expect_true(all(res$nome == "indi_0055"))
  testthat::expect_true(all(res$ano == 2023))
  testthat::expect_true(all(res$agg == "uf_res"))
  testthat::expect_true(all(res$sexo == "total"))
  testthat::expect_true(all(is.finite(res$valor)))
  testthat::expect_true(all(res$valor > 0))
})

# multiple years

testthat::test_that("indi_0055 works with multiple years", {
  res <- indi_0055(
    agg = "uf_res",
    ano = c(2022, 2023)
  )

  testthat::expect_equal("tbl_df", class(res)[1])
  testthat::expect_equal(nrow(res), 54)
  testthat::expect_equal(
    sort(unique(res$ano)),
    c(2022, 2023)
  )
  testthat::expect_true(all(res$nome == "indi_0055"))
  testthat::expect_true(all(res$agg == "uf_res"))
  testthat::expect_true(all(res$sexo == "total"))
  testthat::expect_true(all(is.finite(res$valor)))
})

# invalid aggregation

testthat::test_that("indi_0055 rejects unsupported aggregation", {
  testthat::expect_error(
    indi_0055(
      agg = "mun_res",
      ano = 2023
    ),
    "`agg` must be either 'brasil' or 'uf_res'.",
    fixed = TRUE
  )
})

# invalid sex

testthat::test_that("indi_0055 rejects unsupported sex", {
  testthat::expect_error(
    indi_0055(
      agg = "brasil",
      ano = 2023,
      sexo = "outro"
    ),
    "`sexo` must be one of",
    fixed = TRUE
  )
})

# invalid year

testthat::test_that("indi_0055 rejects unsupported year", {
  testthat::expect_error(
    indi_0055(
      agg = "brasil",
      ano = 1999
    ),
    "indi_0055 supports years from 2000 to 2070",
    fixed = TRUE
  )
})

# invalid time aggregation

testthat::test_that("indi_0055 rejects non-annual time aggregation", {
  testthat::expect_error(
    indi_0055(
      agg = "brasil",
      agg_time = "month",
      ano = 2023
    ),
    "Life expectancy at birth is annual.",
    fixed = TRUE
  )
})
