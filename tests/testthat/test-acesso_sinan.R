test_that("acesso_sinan() handles ano_final smaller than ano_inicio", {
  expect_no_error(
    acesso_sinan(
      Fonte = "SINAN-DENG",
      UF = "BR",
      ano_inicio = 2021,
      ano_final = 2020
    )
  )
})
