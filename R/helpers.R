#' Calculate percentage response
#'
#' Calculate the percentage response between two activity scores.
#'
#' @param score1 numeric First score.
#' @param score2 numeric Second (more recent) score.
#' @param ignore boolean ignore incorrect values and return NA.
#'
#' @return Percentage response
#'
#' @examples
#' percentage_response(score1 = 13, score2 = 3)
#' percentage_response(score1 = c(266, 150), score2 = c(32, 170))
#'
#' @export
percentage_response <- function(score1, score2, ignore = TRUE) {
  score1 <- suppressWarnings(as.numeric(score1))
  score2 <- suppressWarnings(as.numeric(score2))

  if(!.equal_lengths(score1, score2)) {
    stop("score1 and score2 are not the same length.")
  }

  if(any(is.na(score1) | score1 < 0)) {
    if(ignore) {
      score1[ is.na(score1) | score1 < 0 ] <- NA
    } else {
      stop("score1 must be greater than 0.")
    }
  }

  if(any(is.na(score2) | score2 < 0)) {
    if(ignore) {
      score2[ is.na(score2) | score2 < 0 ] <- NA
    } else {
      stop("score2 must be greater than 0.")
    }
  }

  response <- round(100 - ((score2 / score1) * 100),1)
  return(response)
}

#' Test for objects of type "Date"
#'
#' Checks if an object inherits class Date. Used prior to calculating durations
#' from two dates.
#'
#' @param x object to be tested.
#'
#' @examples
#' date <- as.Date("2010-01-01")
#'
#' is_date(date)
#'
#' @export
is_date <- function(x) inherits(x, 'Date')

#' Calculate the duration between two dates in days.
#'
#' Calculates how the duration in days between two dates.
#'
#' @param date1 date
#' @param date2 date
#'
#' @examples
#' date1 <- as.Date("2010-01-01")
#' date2 <- as.Date("2010-02-13")
#'
#' dates_to_duration(date1, date2)
#'
#' @export
dates_to_duration <- function(date1, date2) {
  if(is.na(date1) || is.na(date2)) {
    return(NA)
  }

  if (!is_date(date1) || !is_date(date2)) {
    stop("Non-date object found.")
  }

  duration <- as.numeric(date2-date1)

  return(duration)
}

.equal_lengths <- function(...) {
  args <- list(...)
  return(var(lengths(args)) == 0)
}
