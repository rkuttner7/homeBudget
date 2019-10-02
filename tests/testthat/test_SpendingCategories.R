context("SpendingCategories")


test_that("str_detect_any returns TRUE if any pattern matches a substring in string", {
  expect_equal(str_detect_any(string = c("yes a", "no b"), pattern = "a"),
               c(TRUE, FALSE))
  expect_equal(str_detect_any(string = c("yes a", "YES b", "no c"), pattern = c("a", "b")),
               c(TRUE, TRUE, FALSE))
  expect_equal(str_detect_any(string = c("yes aPLUS-STUFF", "no b"), pattern = "a"),
               c(TRUE, FALSE))
  fruit <- c("apple", "banana", "pear", "pinapple")
  expect_equal(str_detect_any(fruit, "a"), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(str_detect_any(fruit, "^a"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(str_detect_any(fruit, "a$"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(str_detect_any(fruit, "[lr]"), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(str_detect_any(fruit, c("^a", "a$")), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(str_detect_any(fruit, c("apple", "r")), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(str_detect_any(fruit, "apple|r"), c(TRUE, FALSE, TRUE, TRUE))
})

Description <- c('rent 2019 march', 'Happy Co. gas 20190601',
                 'New restaurant')
myCategories <- list(housing = c('rent', 'Hardware Inc'),
                     transportation = 'gas')

test_that("addCategories fails when description matches multiple categories", {
  expect_equal(addCategories(Description = Description,
                             myCategories),
               c("housing", "transportation", "MISSING"))
})

test_that("addCategories fails when description matches multiple categories", {
  expect_error(addCategories(Description = c("gas rent"), myCategories))

})
