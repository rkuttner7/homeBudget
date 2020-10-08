

#' @title modifyCreditCard_Chase
#' @description
#'   Modify Chase to filter to only records related to spending.
#'   Change the expense value from negative to positive for consistency.
#' @importFrom data.table :=
#' @keywords internal
#' @inheritParams read_csvCreditCard
modifyCreditCard_Chase <- function(dataCredit){
  Details <- Amount <- Type <- NULL

  # only include records related to spending
  dataCredit <- dataCredit[Details=='DEBIT' &
                             #!(Type %in% c("ACCT_XFER", "ATM"))
                           Type == "DEBIT_CARD"
                           ]

  # spending is changed from negative to positive for consistency w/AmEx and clarity
  dataCredit[ , ':='(Amount = -1*Amount)]
  dataCredit
}

#' @title modifyCreditCard_AmEx
#' @description
#'   Modify American Express to exclude monthly payments.
#' @keywords internal
#' @inheritParams read_csvCreditCard
modifyCreditCard_AmEx <- function(dataCredit){
  Description <- Amount <- NULL

  # only include records related to spending
  # -- remove paying off American Express bill from expense report
  dataCredit <- dataCredit[!(Description=="ONLINE PAYMENT - THANK YOU" &
                               Amount < 0)]
  dataCredit <- dataCredit[!(Description=="CUSTOMER SERVICE PAYMENT THANK YOU")]

  dataCredit
}


#' @title read_csvCreditCard
#' @description
#'   Load raw credit card csv data into R as a data.table
#'   and set variables to proper type for analysis.
#' @param file the name of the file path which the data are to be
#'   read from.
#' @param Date name of the csv column header for the posting transaction date
#' @param Amount name of the csv column header for the transaction amount
#' @param Description name of the csv column header purchase description
#' @param company the name of company responsible for the
#'   credit card. Choose among: {Chase, American Express}
#' @examples
#'   \dontrun{
#'     read_csvCreditCard(file = "/budget/data/someChaseStatement.csv",
#'                        Date = "Posting Date",
#'                        Amount = "Amount",
#'                        Description = "Description",
#'                        company = "Chase")
#'    }
#' @return data.table of new credit card transactions
#'
#' @importFrom data.table :=
#' @export
read_csvCreditCard <- function(file,
                                Date,
                                Amount,
                                Description,
                                company = c("Chase", "American Express")){
  # define any unbound variables locally within a function
  PostingDate <- NULL

  # column final variable names
  col_names <- c("PostingDate", "Amount", "Description")
  col_names_initial <- c(Date, Amount, Description)

  # -- set column 'Phone Number' to type character to avoid error in View() when
  #    record is missing.
  # dataCredit <- data.table::fread(file = file,
  #                                 sep = ",", header = TRUE, skip = 0,
  #                                 #colClasses = list(character = 'Phone Number'),
  #                                 stringsAsFactors = FALSE)
  dataCredit <- data.table::as.data.table(readr::read_csv(file = file))

  # Confirm naming convention from banks has not changed
  # and essential columns exist
  if(! all(col_names_initial %in% names(dataCredit))){
    missingCol <- toString(shQuote(setdiff(col_names_initial,
                                           names(dataCredit))))
    stop(gettextf("CSV is missing expected columns:\n\t%s",
                  paste(missingCol, collapse = "\n\t"),
                  domain = NA),
         call. = FALSE)
  }

  # Modify Names for consitency
  fixNames <- setdiff(col_names_initial, col_names)
  fixNames_i <- which(! col_names_initial %in% col_names)

  if(length(fixNames) > 0){
    # Name conflicts due to other column already having prefered variable name
    conflictNames <- intersect(col_names[fixNames_i], names(dataCredit))
    if(length(conflictNames) > 0) {
      conflictNames_new <- paste0(conflictNames, "_2")
      } else conflictNames_new <- c()

    data.table::setnames(dataCredit,
                         old = c(fixNames, conflictNames),
                         new = c(col_names[fixNames_i], conflictNames_new))
  }

  # remove white space between each column name
  newNames <- gsub(pattern = " ", replacement =  "",
                   x = names(dataCredit),
                   fixed = TRUE)
  if(length(setdiff(newNames, names(dataCredit))) > 0){
    data.table::setnames(dataCredit,
                         old = names(dataCredit),
                         new = newNames)
  }

  dataCredit[ , ':='(PostingDate = lubridate::mdy(PostingDate))]

  company <-  match.arg(company)

  dataCreditMod <- switch(stringr::str_replace(string = company,
                              pattern = " ",
                              replacement = ""),
         Chase = modifyCreditCard_Chase(dataCredit),
         AmericanExpress = modifyCreditCard_AmEx(dataCredit),
         stop(gettextf("modification function not found for `company` \"%s\".",
                       company, domain = NA)) )
  dataCreditMod
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

