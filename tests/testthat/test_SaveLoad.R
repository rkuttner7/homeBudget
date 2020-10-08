context("SaveLoad")


test_that("read_csvCreditCard reads csv into a data.table", {

  expect_s3_class(read_csvCreditCard(file = system.file("extdata",
                                                         "Chase.csv",
                                                         package = "homeBudget",
                                                         mustWork = TRUE),
                                      Date = "Posting Date",
                                      Amount = "Amount",
                                      Description = "Description",
                                      company = "Chase"),
                  c("data.table", "data.frame"))

  expect_s3_class(read_csvCreditCard(file = system.file("extdata",
                                                         "AmEx.csv",
                                                         package = "homeBudget",
                                                         mustWork = TRUE),
                                      Date = "Date", Amount = "Amount",
                                      Description = "Description",
                                      company = "American Express"),
                  c("data.table", "data.frame"))


  expect_s3_class(read_csvCreditCard(file = "AmEx_data2.csv",
                                      Date = "Date", Amount = "Amount",
                                      Description = "Description",
                                      company = "American Express"),
                  c("data.table", "data.frame"))

})



test_that("appendCreditCardData returns row bind of input data.tables", {

  expect_true(nrow(appendCreditCardData(data.table::data.table(PostingDate='a',
                                                           Amount=1,
                                                           Description='a'),
                                         data.table::data.table(PostingDate='b',
                                                                Amount=2,
                                                            Description='b')))==2)
})
