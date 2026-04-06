test_that("acesso_datasus() rejects invalid Fonte", {
  expect_error(
    acesso_datasus(
      Fonte = "INVALIDA",
      UF = "RJ",
      ano_inicio = 2020,
      ano_final = 2020
    ),
    regexp = "Família.*não mapeada|get_ftp_dir"
  )
})

test_that("acesso_datasus() rejects ano_final smaller than ano_inicio", {
  expect_error(
    acesso_datasus(
      Fonte = "SINASC-DN",
      UF = "MG",
      ano_inicio = 2021,
      ano_final = 2020
    ),
    regexp = "ano|ano_final|ano_inicio|final.*menor"
  )
})

test_that("acesso_datasus() accepts monthly SINASC call", {
  skip_if_offline()

  res <- acesso_datasus(
    Fonte = "SINASC-DN",
    UF = "MG",
    ano_inicio = 2019,
    ano_final = 2019,
    mes_inicial = "01",
    mes_final = "06",
    quiet = TRUE
  )

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
  expect_gte(nrow(res), 0)
})

test_that("acesso_datasus() accepts e-SUS call", {
  skip_if_offline()

  res <- acesso_datasus(
    Fonte = "e-SUS",
    UF = "BR",
    ano_inicio = 2023,
    ano_final = 2023,
    quiet = TRUE
  )

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
  expect_gte(nrow(res), 0)
})
