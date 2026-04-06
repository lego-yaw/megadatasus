test_that("clean_table() rejects invalid fonte", {
  df <- data.frame(
    IDADE = 10,
    SEXO = "M",
    stringsAsFactors = FALSE
  )

  expect_error(
    clean_table(df, fonte = "INVALIDA"),
    regexp = "fonte|Fonte|inválid|não reconhecida|não suportada"
  )
})

test_that("clean_table() accepts SIM-DO and returns data.frame-like object", {
  df <- data.frame(
    DTOBITO = "2020-01-01",
    IDADE = "025",
    SEXO = "M",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    res <- clean_table(df, fonte = "SIM-DO")
  })

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
  expect_equal(nrow(res), nrow(df))
})

test_that("clean_table() accepts SINASC-DN and returns data.frame-like object", {
  df <- data.frame(
    DTNASC = "2020-01-01",
    SEXO = "M",
    PESO = "3200",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    res <- clean_table(df, fonte = "SINASC-DN")
  })

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
  expect_equal(nrow(res), nrow(df))
})

test_that("clean_table() handles empty data frame", {
  df <- data.frame()

  expect_no_error({
    res <- clean_table(df, fonte = "SIM-DO")
  })

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
  expect_equal(nrow(res), 0)
})

test_that("clean_table() preserves row count for SIM-DO", {
  df <- data.frame(
    DTOBITO = c("2020-01-01", "2020-01-02"),
    IDADE = c("025", "030"),
    SEXO = c("M", "F"),
    stringsAsFactors = FALSE
  )

  res <- clean_table(df, fonte = "SIM-DO")

  expect_equal(nrow(res), 2)
})

test_that("clean_table() preserves row count for SINASC-DN", {
  df <- data.frame(
    DTNASC = c("2020-01-01", "2020-01-02"),
    SEXO = c("M", "F"),
    PESO = c("3200", "2800"),
    stringsAsFactors = FALSE
  )

  res <- clean_table(df, fonte = "SINASC-DN")

  expect_equal(nrow(res), 2)
})
