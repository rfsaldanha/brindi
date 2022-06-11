test_that("get sim with mun works", {
  res <- get_sim(agg = "mun_res", ano = 2010)

  expect_equal("data.frame", class(res))
  expect_gt(nrow(res), 5000)
})

test_that("get sim with uf works", {
  res <- get_sim(agg = "uf_res", ano = 2010)

  expect_equal("data.frame", class(res))
  expect_gt(nrow(res), 20)
})
