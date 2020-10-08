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


test_that("addCategories matches two distinct substrings", {
  similar_patterns_fail <- list(Housing = c("TARGET .*[NORTHGATE]"),
                           Grocery = c("TARGET T"))
  similar_patterns <- list(Housing = "(?=.*TARGET)(?=.*NORTHGATE)",
                           Grocery = c("TARGET T"))


  expect_error(addCategories(Description = "TARGET T- 302 NE North Seattle WA            03/23",
                             similar_patterns_fail))
  expect_silent(addCategories(Description = c("TARGET T- 302 NE North Seattle WA            03/23",
                                              "TARGET SOMETHING    NORTHGATE BLAH"),
                              similar_patterns))
  expected <- c("Grocery", "Housing")
  expect_equal(addCategories(Description = c("TARGET T- 302 NE North Seattle WA            03/23",
                                             "TARGET SOMETHING    NORTHGATE BLAH"),
                             similar_patterns), expected)

  # matche 2 distinct phrases
  longer_patterns <- list(right = "(?=.*KITCHEN COLLECTION)(?=.*LINCOLN CITY OR)")

  expect_equal(addCategories(Description = c("KITCHEN COLLECTION something LINCOLN CITY OR",
                                             "LINCOLN CITY OR"),
                             longer_patterns),
               expected = c("right", "MISSING"))
})
