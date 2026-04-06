test_that("Datasus_info() returns a tabular object for valid sources", {
  skip_if_offline()

  fontes <- c("SIM", "SINASC-DN")

  for (f in fontes) {
    expect_no_error({
      res <- Datasus_info(f)
    })

    expect_true(
      is.data.frame(res) || data.table::is.data.table(res),
      info = paste("Fonte testada:", f)
    )
  }
})

test_that("Datasus_info() supports refresh argument", {
  skip_if_offline()

  expect_no_error({
    res <- Datasus_info("SIM", refresh = TRUE)
  })

  expect_true(is.data.frame(res) || data.table::is.data.table(res))
})
