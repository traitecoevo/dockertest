context("Misc")

test_that("Support functions", {
  expect_that(is_mac(), is_a("logical"))
  expect_that(is_mac(), equals(Sys.info()[["sysname"]] == "Darwin"))
})
