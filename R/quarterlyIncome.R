#' @title quarterly_intervals
#' @description
#'   Find the four quarters for the year.
#' @param end date ending of last quarter
#' @param cutoff date cutoff to exclude later quarters
#' @param inclPartial logical to include partialy complete quarters
#'   due to cutoff (default is TRUE)
#' @return
#'   lubridate intervals
#' @examples
#' quarterly_intervals(end = as.Date("2019-12-31"))
#' quarterly_intervals(end = as.Date("2019-12-31"),
#'                     cutoff = as.Date("2019-06-05"))
#' quarterly_intervals(end = as.Date("2019-12-31"),
#'                     cutoff = as.Date("2019-06-05"), inclPartial = FALSE)
#'
#' @export
quarterly_intervals <- function(end, cutoff = NULL, inclPartial = TRUE){
  all_qrt <- rev(seq(from = end, by = "-1 quarter", length.out = 5))
  # full 4 quarters
  n_periods <- length(all_qrt)-1
  origin_period <- all_qrt[1:n_periods]+1
  cutoff_period <- all_qrt[(1:n_periods)+1]
  if(!is.null(cutoff) && cutoff < end & !inclPartial){
    cutoff_period <- cutoff_period[cutoff_period <= cutoff]
    n_periods <- length(cutoff_period)
    if(n_periods==0) stop("No quarters start before cutoff")
    origin_period <- origin_period[1:n_periods]
  } else if(!is.null(cutoff) && cutoff < end & inclPartial){
    origin_period <- origin_period[origin_period < cutoff]
    n_periods <- length(origin_period)
    if(n_periods==0) stop("No quarters end before cutoff")
    cutoff_period <- pmin(cutoff_period[1:n_periods], cutoff)
  }
  lubridate::interval(start = origin_period,
                      end = cutoff_period)
}


#' @title quarterly_income
#' @description
#'  Find the dollars earned over the time period defined in the
#'  interval for each salary position in the data.table.
#' @param dt data.table of income data. Must include columns
#'    `Amount` the earning for that interval, `DateStart` the start
#'    date for earning that salary, `DateEnd` the end date for earning
#'    that salary.
#' @param intervals lubridate interval object with specified start and end dates.
#' @param units character string. Units in which the results are desired.
#' @rdname overlapAmount
#' @importFrom data.table .SD
#' @seealso
#'   \code{\link[lubridate:interval]{name}}
#' @examples
#'   qrt_int <- quarterly_intervals(end = as.Date("2019-12-31"),
#'                                  cutoff = as.Date("2019-09-01"))
#'   quarterly_income(homeBudget::incomeExample, qrt_int)
#'
#' @export
quarterly_income <- function(dt, intervals, units = "days"){
  period <- data.table::frank(lubridate::int_start(intervals),
                              ties.method = "dense")
  qrt_inc <- data.table::rbindlist(l = Map(f = overlapAmount, list(dt),
                                           intervals, period,
                                list(units)))
}

overlapAmount <- function(dt, interval, period, units){
  # define any unbound variables locally within a function
  Amount <- DateStart <- DateEnd <- NULL

  stopifnot(c("Amount", "DateStart", "DateEnd") %in% names(dt))

  mycols <- setdiff(names(dt), "Amount")
  # drop rows that do not overlap the time window
  dt[ , c(list("Amount" = Amount *
                 percentOverlap(lubridate::interval(start = DateStart,
                                                    end = DateEnd),
                                interval, units),
               "interval_start" = lubridate::int_start(interval),
               "interval_end" = lubridate::int_end(interval),
               "period" = period),
          .SD), .SDcols = mycols][Amount > 0]
}

#' @title length_overlap
#' @description
#'   Count the amount of overlap between two date intervals.
#' @param interval1,interval2 lubridate interval object with
#'    specified start and end dates.
#' @inheritParams quarterly_income
#' @examples
#' int1 <- lubridate::interval(start = as.Date(c("2019-01-01", "2019-02-01")),
#'                             end = as.Date(c("2019-03-01", "2019-05-01")))
#' int2 <- lubridate::interval(start = as.Date("2019-03-02"),
#'                             end = as.Date("2019-04-02"))
#' length_overlap(interval1 = int1, interval2 = int2, units = "days")
#'
#' @export
length_overlap <- function(interval1, interval2, units){
  len <- as.numeric(lubridate::as.duration(lubridate::intersect(interval1,
                                                                interval2)),
                    units)+1
  len[is.na(len)] <- 0
  len
}

#' @title percentOverlap
#' @description
#'   The proportion of interval 1 that overlaps the intersection
#'   between the two intervals.
#' @param interval1,interval2 lubridate interval object with
#'    specified start and end dates.
#' @inheritParams quarterly_income
#'
#' @export
percentOverlap <- function(interval1, interval2, units){
  length_overlap(interval1, interval2, units) /
    as.numeric(lubridate::as.duration(interval1), units)
}


