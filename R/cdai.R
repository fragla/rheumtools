#' Calculate CDAI score
#'
#' Calculate the Clinical Disease Activity Index.
#'
#' @param tjc numeric Tender 28 joint count (0 to 28).
#' @param sjc numeric Swollen 28 joint count (0 to 28).
#' @param ptgh numeric Patient Global Health VAS score (0 to 10cm).
#' @param phgh numeric Physician Global Health VAS score (0 to 10cm).
#' @param digits numeric specifying the number of decimal places. Defaults to 0.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return CDAI score.
#'
#' @examples
#' cdai_score(tjc = 4, sjc = 5, ptgh = 5, phgh = 6)
#' cdai_score(tjc = 4, sjc = 5, ptgh = 10, phgh = 8)
#'
#' @export
cdai_score <- function(tjc, sjc, ptgh, phgh, digits = 0, ignore = TRUE) {
  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  ptgh <- suppressWarnings(as.numeric(ptgh))
  phgh <- suppressWarnings(as.numeric(phgh))

  if(!.equal_lengths(tjc, sjc, ptgh, phgh)) {
    stop("tjc, sjc, ptgh and phgh are not the same length.")
  }

  if(any(is.na(tjc) | !tjc %in% 0:28)) {
    if(ignore) {
      tjc[ is.na(tjc) | !tjc %in% 0:28 ] <- NA
    } else {
      stop("Tender Joint Count must be between 0 and 28.")
    }
  }

  if(any(is.na(sjc) | !sjc %in% 0:28)) {
    if(ignore) {
      sjc[ is.na(sjc) | !sjc %in% 0:28 ] <- NA
    } else {
      stop("Swollen Joint Count must be between 0 and 28")
    }
  }

  if(any(is.na(ptgh) | ptgh < 0 | ptgh > 10)) {
    if(ignore) {
      ptgh[ is.na(ptgh) | ptgh < 0 | ptgh > 10 ] <- NA
    } else {
      stop("Patient global VAS must be between 0 and 10.")
    }
  }

  if(any(is.na(phgh) | phgh < 0 | phgh > 10)) {
    if(ignore) {
      phgh[ is.na(phgh) | phgh < 0 | phgh > 10 ] <- NA
    } else {
      stop("Physician global VAS must be between 0 and 10.")
    }
  }

  round(tjc + sjc + ptgh + phgh, digits) #range is 0 to 76.
}


#' Calculate CDAI activity level
#'
#' Calculate the Clinical Disease Activity Index Level for a CDAI score. Remission: >= 0 and
#' <= 2.8; Low: > 2.8 and <= 10; Moderate: > 10 and <= 22; High: > 22.
#'
#' @param cdai numeric CDAI score.
#' @param ignore boolean ignore incorrect cdai values and return NA.
#' @return CDAI level.
#'
#' @references \url{http://www.ncbi.nlm.nih.gov/pubmed/16273793}
#'
#' @examples
#' cdai_classification(12)
#' cdai_classification(c(2, 42))
#'
#' @export
cdai_classification <- function(cdai, ignore = TRUE) {
  cdai <- suppressWarnings(as.numeric(cdai))

  if(any(is.na(cdai) | cdai < 0)) {
    if(ignore) {
      cdai[ is.na(cdai) | cdai < 0 ] <- NA
    } else {
      stop("CDAI score must be greater than 0.")
    }
  }

  activity <- rep(NA_character_, length(cdai))
  activity[cdai >= 0] <- "Remission"
  activity[cdai > 2.8] <- "Low"
  activity[cdai > 10] <- "Moderate"
  activity[cdai > 22] <- "High"

  return(activity)
}

#' Calculate CDAI response
#'
#' Calculate the CDAI 50/70/85 for a CDAI percentage response.
#'
#' @param response numeric CDAI percentage response.
#' @param ignore boolean ignore incorrect CDAI value types and return NA.
#'
#' @return CDAI response classification
#'
#' @examples
#' cdai_response(76.9)
#' cdai_response(c(88.0, -13.3))
#'
#' @export
cdai_response <- function(response, ignore = TRUE) {
  response <- suppressWarnings(as.numeric(response))

  if(any(is.na(response))) {
    if(!ignore) {
      stop("CDAI response score must be a numeric.")
    }
  }

  cat <- rep(NA, length(response))
  cat[response < 50] <- "None"
  cat[response >= 50] <- "Minor"
  cat[response >= 70] <- "Moderate"
  cat[response >= 85] <- "Major"
  return(cat)
}
