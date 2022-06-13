test_that("get sim with mun residence works", {
  res <- get_sim(agg = "mun_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("get sim with uf residence works", {
  res <- get_sim(agg = "uf_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with reg saude residence works", {
  res <- get_sim(agg = "regsaude_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 30)
})

test_that("get sim with mun occurence works", {
  res <- get_sim(agg = "mun_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5000)
})

test_that("get sim with uf occurence works", {
  res <- get_sim(agg = "uf_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with reg saude occurence works", {
  res <- get_sim(agg = "regsaude_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 30)
})

test_that("get sim with uf and sex works", {
  res <- get_sim(agg = "uf_res", ano = 2010, sexo = "Masculino")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and age less than works", {
  res <- get_sim(agg = "uf_res", ano = 2010, idade_a = 10)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and age greater than works", {
  res <- get_sim(agg = "uf_res", ano = 2010, idade_b = 10)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and age interval than works", {
  res <- get_sim(agg = "uf_res", ano = 2010, idade_a = 10, idade_b = 30)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and cid like works", {
  res <- get_sim(agg = "uf_res", ano = 2010, cid_like = "I")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and cid in works", {
  res <- get_sim(agg = "uf_res", ano = 2010, cid_in = c("I219", "B342", "R98"))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with uf and cid in works", {
  res <- get_sim(agg = "uf_res", ano = 2010, cid_in = cid_seq("I01", "I10"))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sim with mun and all filters works", {
  res <- get_sim(agg = "mun_res", ano = 2010, sexo = "Feminino", idade_a = 20, idade_b = 50, cid_like = "I2", cid_in = c("I219", "B342", "R98"))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1000)
})

test_that("get sim with uf and other filters works", {
  res <- get_sim(agg = "uf_res", ano = 2010, more_filters = "def_raca_cor = 'Preta' AND def_est_civil = 'Casado'")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})
