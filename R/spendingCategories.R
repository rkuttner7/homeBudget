

#' @title str_detect_any
#' @description
#' Check if any pattern is a substring of the character vector.
#' Returns a logical vector indicating if any pattern
#' is contained within.
#' @param string character vector; Input vector.
#' @param pattern character vector; substring or pattern to search for.
#' @seealso \code{\link[stringr:str_detect]{stringr::str_detect()}}
#' @return logical vector of same length as input \code{string}.
str_detect_any <- function(string, pattern) {
  pattern <- paste(pattern, collapse = "|")
  stringr::str_detect(string = string, pattern)
}


#' @title addCategories
#' @description
#'  For each budget category check if in any pattern is a
#'  match with the \code{Description}.
#'  Return the name of the category that matches.
#' @details
#'  If none of the categories contain a pattern that matches
#'  then return \code{MISSING}. A \code{Description} is not allowed to
#'  belong to multiple categories. The first instance of such
#'  a problem will cause an error to be thrown with the first
#'  \code{Description} and all the categories it has been matched
#'  with.
#' @param Description character vector
#' @param myCategories list; each named element contains a character
#'    vector of all known patterns defining that category.
#' @examples
#'   Description <- c('rent 2019 march',
#'                    'Happy Co. gas 20190601',
#'                    'New restaurant')
#'   myCategories <- list(housing = c('rent', 'Hardware Inc'),
#'                        transportation = 'gas')
#'   addCategories(Description = Description, myCategories)
#' \dontrun{
#'   addCategories(Description = c("gas rent"), myCategories)
#' }
#' @return
#'   character vector
#'
#' @export
addCategories <- function(Description, myCategories){
  matchCatgDF <- as.data.frame(Map(f = str_detect_any,
                                 string = list(Description),
                                 pattern = myCategories))
  names(matchCatgDF) <- names(myCategories)

  MULTP <- (rowSums(matchCatgDF) > 1L)
  if(any(MULTP)){
    rowNumMultp <- which(MULTP)[1]
    colCatgMultp <- which(as.logical(matchCatgDF[rowNumMultp, ]))
    catgMultp <- names(matchCatgDF)[colCatgMultp]
    # give first example of description matching multiple categories
    stop(gettextf("Multiple matching categories at position %i to:\n   %s",
                  rowNumMultp,
                  paste(catgMultp, collapse = ", "),
                  domain = NA))
  }

  final <- rep("MISSING", length(Description))
  for(catg in names(matchCatgDF)){
    indexrow <- which(matchCatgDF[[catg]])
    final[indexrow] <- catg
  }
  final
}
