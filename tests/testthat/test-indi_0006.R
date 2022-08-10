test_that("indi_0006 works with mun res", {
  res <- indi_0006(agg = "mun_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 2400)
})

test_that("indi_0006 works with mun ocor", {
  res <- indi_0006(agg = "mun_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 2400)
})

test_that("indi_0006 works with uf res", {
  res <- indi_0006(agg = "uf_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0006 works with uf ocor", {
  res <- indi_0006(agg = "uf_ocor", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0006 works with regsaude res", {
  res <- indi_0006(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("indi_0006 works with regsaude ocor", {
  res <- indi_0006(agg = "regsaude_res", ano = 2013)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

