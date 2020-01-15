
df <- data.frame(
  x = letters[1:10],
  `a n` = 1:10,
  `a prop` = (1:10) / 10,
  `a deff` = round(pi, 2),
  `b n` = 10:1,
  `b prop` = (10:1) / 10,
  `b deff` = round(pi * 2, 2),
  check.names = FALSE
)

test_that("rename redundant works as advertized", {
  expect_named(rename_redundant(df, "%" = prop), c("x", "a n", "%", "a deff", "b n", "%", "b deff"))
})

test_that("augment redundant works as advertized", {
  expect_named(augment_redundant(df, " (n)" = " n", "(%)" = prop), c("x", "a (n)", "a (%)", "a deff", "b (n)", "b (%)", "b deff"))
})
