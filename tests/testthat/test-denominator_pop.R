test_that("denominator pop works with mun_res", {

  res <- denominator_pop(agg = "mun_res")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 123000)
})

test_that("denominator pop works with mun_ocor", {

  res <- denominator_pop(agg = "mun_ocor")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 123000)
})

test_that("denominator pop works with uf_res", {

  res <- denominator_pop(agg = "uf_res")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 500)
})

test_that("denominator pop works with uf_ocor", {

  res <- denominator_pop(agg = "uf_ocor")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 500)
})


test_that("denominator pop works with regsaude_res", {

  res <- denominator_pop(agg = "regsaude_res")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 7900)
})

test_that("denominator pop works with regsaude_ocor", {

  res <- denominator_pop(agg = "regsaude_ocor")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 7900)
})
