context("quarterlyIncome")


test_that("quarterly_intervals returns number of quarters relative to cutoff", {
  expect_equal(length(quarterly_intervals(end = as.Date("2019-12-31"))), 4)
  expect_equal(length(quarterly_intervals(end = as.Date("2019-12-31"),
                      cutoff = as.Date("2019-06-01"))), 2)
  expect_equal(length(quarterly_intervals(end = as.Date("2019-12-31"),
                                          cutoff = as.Date("2019-06-05"),
                                          inclPartial = FALSE)), 1)
})

int1 <- lubridate::interval(start = as.Date(c("2019-01-01", "2019-02-01")),
                            end = as.Date(c("2019-03-01", "2019-05-01")))
int2 <- lubridate::interval(start = as.Date("2019-03-02"),
                            end = as.Date("2019-03-11"))

test_that("length_overlap counts the number of days intersecting the intervals", {
  expect_equal(length_overlap(interval1 = int1, interval2 = int2,
                              units = "days"),
               c(0, 10))
})

test_that("percentOverlap returns the percent of overlap of intersecting intervals", {
  expect_equal(percentOverlap(interval1 = int1, interval2 = int2,
                              units = "days"),
               c(0, 10/as.numeric(diff(as.Date(c("2019-02-01", "2019-05-01"))))))
})
