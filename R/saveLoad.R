
#' @title readCreditCard_Chase
#' @description
#'   Read in Chase specific csv.
#' @importFrom data.table :=
#' @keywords internal
#' @inheritParams read_csvCreditCard
readCreditCard_Chase <- function(file){
  # define any unbound variables locally within a function
  Amount <- Details <- PostingDate <- Type <- NULL

  dataCredit <- data.table::fread(file = file,
                                 sep = ",", header = TRUE, skip = 0)

  # Confirm naming convention from banks has not changed
  # and essential columns exist
  needCols <- c("Posting Date", "Amount", "Description")
  if(! all(needCols %in% names(dataCredit))){
    missingCol <- toString(shQuote(setdiff(needCols,
                          names(dataCredit))))
    stop(gettextf("CSV is missing expected columns:\n  {%s}.",
                  missingCol, domain = NA),
         call. = FALSE)
  }

  # remove white space between each column name
  newNames <- gsub(pattern = " ", replacement =  "",
                   x = names(dataCredit),
                   fixed = TRUE)
  data.table::setnames(dataCredit,
                       old = names(dataCredit),
                       new = newNames)

  # only include records related to spending
  dataCredit <- dataCredit[Details=='DEBIT' & !(Type %in% c("ACCT_XFER", "ATM"))]


  dataCredit[ , ':='(PostingDate = lubridate::mdy(PostingDate))]

  # spending is changed from negative to positive for consistency w/AmEx and clarity
  dataCredit[ , ':='(Amount = -1*Amount)]
  dataCredit
}

#' @title read_csvCreditCard_AmEx
#' @description
#'   Read in American Express specific csv.
#' @importFrom data.table :=
#' @keywords internal
#' @inheritParams read_csvCreditCard
read_csvCreditCard_AmEx <- function(file){
  # define any unbound variables locally within a function
  Amount <- Description <- PostingDate <-  NULL

  # -- set column 'Phone Number' to type character to avoid error in View() when
  #    record is missing.
  dataCredit <- data.table::fread(file = file,
                                     sep = ",", header = TRUE, skip = 0,
                                     colClasses = list(character = 'Phone Number'),
                                     stringsAsFactors = FALSE)

  # Confirm naming convention from banks has not changed
  # and essential columns exist
  needCols <- c("Date", "Amount", "Appears On Your Statement As")
  if(! all(needCols %in% names(dataCredit))){
    missingCol <- toString(shQuote(setdiff(needCols,
                                           names(dataCredit))))
    stop(gettextf("CSV is missing expected columns:\n  {%s}.",
                  missingCol, domain = NA),
         call. = FALSE)
  }

  # remove white space between each column name
  newNames <- gsub(pattern = " ", replacement =  "",
                   x = names(dataCredit),
                   fixed = TRUE)

  # Modify Names for consitency
  newNames[newNames  %in% c("Date", "Description", "Type",
                            "AppearsOnYourStatementAs",
                            "AdditionalInformation")] <-
    c("PostingDate", "Company", "Details", "Description", "AdditionalInfo")

  data.table::setnames(dataCredit,
                       old = names(dataCredit),
                       new = newNames)

  # only include records related to spending
  # -- remove paying off American Express bill from expense report
  dataCredit <- dataCredit[!(Description=="ONLINE PAYMENT - THANK YOU" &
                                     Amount < 0)]

  dataCredit[ , ':='(PostingDate = lubridate::mdy(PostingDate))]
  dataCredit
}

#' @title read_csvCreditCard
#' @description
#'   Load raw credit card csv data into R as a data.table
#'   and set variables to proper type for analysis.
#' @param file the name of the file path which the data are to be
#'   read from.
#' @param company the name of company responsible for the
#'   credit card. Choose among: {Chase, American Express}
#' @examples
#'   \dontrun{
#'     read_csvCreditCard(file = "/budget/data/someChaseStatement.csv",
#'                        company = "Chase")
#'    }
#' @return data.table of new credit card transactions
#'
#' @export
read_csvCreditCard <- function(file,
                              company = c("Chase", "American Express")){
  if(missing(company)) {
    null_options <- toString(shQuote(eval(formals(sys.function())$company)))
    stop(gettextf("You need to specify a value for `company`.\n  Choose one of {%s}.",
                  null_options, domain = NA))
  }
  company <-  match.arg(company)

  switch(stringr::str_replace(string = company,
                              pattern = " ",
                              replacement = ""),
         Chase = readCreditCard_Chase(file),
         AmericanExpress = read_csvCreditCard_AmEx(file),
         stop(gettextf("csv load function not found for `company` \"%s\".",
                       company, domain = NA))
         )
}

#' @title loadCreditCard
#' @description
#'   Load the credit card history data \code{creditCardHistory}
#'   saved in the R object \code{creditCardHistory.rda}.
#' @param path the path to the folder for the saved R credit card
#' @examples
#'   \dontrun{
#'     loadCreditCard(path = "/budget/data")
#'    }
#' @return data.table of saved credit card history
#'
#' @export
loadCreditCard <- function(path){
  # define any unbound variables locally within a function
  creditCardHistory <-  NULL

  if(file.exists(file.path(path, "creditCardHistory.rda"))){
    load(file = file.path(path, "creditCardHistory.rda"),
         envir = environment())
  } else{
    stop("File `creditCardHistory.rda` not found. Check `path`.")
  }
  creditCardHistory
}

#' @title saveCreditCard
#' @description
#'   Save the history of Credit Card data to \code{creditCardHistory.rda}.
#'   Overwrites previous save file.
#' @param path the path to the folder for the saved R credit card
#' @param creditCardHistory data.table of entire credit card history
#' @section Warning:
#'  Be sure that your previous home budget data has been properly loaded
#'  and appended before saving to disk as the contents will be overwritten
#'  with the new file.
#' @examples
#'  \dontrun{
#'   saveCreditCard(path = "/budget/data",
#'                  creditCardHistory = myDataSet)
#'  }
#' @return data.table of saved credit card history
#'
#' @export
saveCreditCard <- function(path, creditCardHistory){
  if(! data.table::is.data.table(creditCardHistory)) stop("Expecting data-set to be type data.table")
    save(file = file.path(path, "creditCardHistory.rda"),
         list = "creditCardHistory")
}

#' @title appendCreditCardData
#' @description
#'   Combine a sequence of data.tables by row to
#'   append new credit card data to the previous credit card
#'   data.
#' @details
#'   Duplicate records based on matching \code{'PostingDate', 'Amount', 'Description'}
#'   are removed to avoid double counting.
#' @param ... data.table(s) of credit card records
#'
#' @export
appendCreditCardData <- function(...){
  dataList <- list(...)

  stopifnot(unlist(lapply(dataList,
                          data.table::is.data.table)))

  dtdata <- data.table::rbindlist(l = dataList,
                                  use.names = TRUE, fill = TRUE)

  # remove duplicates if there is overlap between old and new data-sets.
  unique(dtdata,
         by = c('PostingDate', 'Amount', 'Description'))
}

