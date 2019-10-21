
# Internal data
#  pre-computed data table for reporting income statements
incomeExample <- data.table::data.table(DateStart = as.Date(c("2019-06-01", "2019-06-01")),
                                        DateEnd = as.Date(c("2019-08-30", "2019-07-01")),
                                        Amount = c(8762.21, 5332.15),
                                        category = c("Company 1 & position title",
                                                     "Company 2 & position title"))

usethis::use_data(incomeExample)
