
# Internal data
#  pre-computed data table for testing functions

# load raw csv data
# expect a csv for each company listed in read_csvCreditCard()
# expect the csv to start with company name listed in read_csvCreditCard()
# so that files are read in the proper order relative to argument `company`.
outputDir <- system.file("extdata",
                         package = "homeBudget")
files <- sort(file.path(outputDir, list.files(outputDir)))
companies <- sort(eval(formals(homeBudget::read_csvCreditCard)$company))

creditCardExample <- data.table::rbindlist(l =
                                 Map(homeBudget::read_csvCreditCard,
                                       file = files,
                                       company = companies),
                                 use.names = TRUE, fill = TRUE)

usethis::use_data(creditCardExample)
