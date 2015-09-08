context("Misc")

test_that("Support functions", {
  expect_that(is_mac(), is_a("logical"))
  expect_that(is_mac(), equals(Sys.info()[["sysname"]] == "Darwin"))
})

test_that("parse launch", {
  f <- function(x) {
    parse_main_args(strsplit(x, " +")[[1]][-1])[c("type", "--", "args")]
  }

  res <- f("dockertest launch --dry-run test -- R -a")
  expect_that(res$type, equals("test"))
  expect_that(res$"--", is_true())
  expect_that(res$args, equals(c("R", "-a")))

  res <- f("dockertest launch --dry-run -- R -a")
  expect_that(res$type, is_null())
  expect_that(res$"--", is_true())
  expect_that(res$args, equals(c("R", "-a")))

  res <- f("dockertest launch --dry-run")
  expect_that(res$type, is_null())
  expect_that(res$"--", is_false())
  expect_that(res$args, equals(list()))

  res <- f("dockertest launch --dry-run test")
  expect_that(res$type, equals("test"))
  expect_that(res$"--", is_false())
  expect_that(res$args, equals(list()))

  res <- f("dockertest launch --dry-run test --")
  expect_that(res$type, equals("test"))
  expect_that(res$"--", is_true())
  expect_that(res$args, equals(list()))

  res <- f("dockertest launch --dry-run --")
  expect_that(res$type, equals(NULL))
  expect_that(res$"--", is_true())
  expect_that(res$args, equals(list()))
})
