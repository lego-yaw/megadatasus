test_that("describe_df() works with minimal SINAN data", {
  df_ex <- data.frame(
    SINAN_UF = "RJ",
    DT_NOTIFIC = "20240101",
    CS_SEXO = "M",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    res <- describe_df(df_ex)
  })

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
  expect_true(ncol(res) >= 1)
})

test_that("describe_df() works when sistema is provided", {
  df_ex <- data.frame(
    SINAN_UF = "RJ",
    DT_NOTIFIC = "20240101",
    CS_SEXO = "M",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    res <- describe_df(df_ex, sistema = "SINAN")
  })

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
})

test_that("describe_df() returns a non-empty tabular structure", {
  df_ex <- data.frame(
    SINAN_UF = "RJ",
    DT_NOTIFIC = "20240101",
    CS_SEXO = "M",
    stringsAsFactors = FALSE
  )

  res <- describe_df(df_ex)

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
  expect_true(ncol(res) >= 1)
})

test_that("describe_df() handles repeated and missing values without error", {
  df_ex <- data.frame(
    SINAN_UF = c("RJ", "RJ", NA),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    res <- describe_df(df_ex)
  })

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
})

test_that("describe_df() handles empty data frame", {
  df_ex <- data.frame()

  expect_no_error({
    res <- describe_df(df_ex)
  })

  expect_true(is.data.frame(res))
})

test_that("describe_df() works for multiple columns", {
  df_ex <- data.frame(
    SINAN_UF = c("RJ", "SP"),
    CS_SEXO = c("M", "F"),
    IDADE = c(30, 40),
    stringsAsFactors = FALSE
  )

  res <- describe_df(df_ex)

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
  expect_true(ncol(res) >= 1)
})

test_that("describe_df() returns consistent output class", {
  df_ex <- data.frame(
    SINAN_UF = "RJ",
    stringsAsFactors = FALSE
  )

  res <- describe_df(df_ex)

  expect_true(is.data.frame(res))
})
