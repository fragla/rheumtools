#' Calculate SDAI score
#'
#' Calculate the Simple Disease Activity Index.
#'
#' @param tjc numeric Tender 28 joint count (0 to 28).
#' @param sjc numeric Swollen 28 joint count (0 to 28).
#' @param ptgh numeric Patient Global Health VAS score (0 to 10cm).
#' @param phgh numeric Physician Global Health VAS score (0 to 10cm).
#' @param crp numeric C-reactive protein (CRP) in mg/dl.
#' @param digits numeric specifying the number of decimal places. Defaults to 0.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return SDAI score.
#'
#' @references \url{http://www.ncbi.nlm.nih.gov/pubmed/16273793}
#'
#' @examples
#' sdai_score(tjc = 4, sjc = 5, ptgh = 5, phgh = 6, crp=7)
#' sdai_score(tjc = 4, sjc = 5, ptgh = 10, phgh = 8, crp=3)
#'
#' @export
sdai_score <- function(tjc, sjc, ptgh, phgh, crp, digits = 0, ignore = TRUE) {
  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  ptgh <- suppressWarnings(as.numeric(ptgh))
  phgh <- suppressWarnings(as.numeric(phgh))
  crp <- suppressWarnings(as.numeric(crp))

  if(!.equal_lengths(tjc, sjc, ptgh, phgh, crp)) {
    stop("tjc, sjc, ptgh, phgh and crp are not the same length.")
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

  if(any(is.na(crp) | crp < 0)) {
    if(ignore) {
      phgh[ (!is.na(crp) & crp < 0) ] <- NA
    } else {
      stop("CRP value must be greater than 0.")
    }
  }

  round(tjc + sjc + ptgh + phgh + crp, digits) #range is 0 to 76.
}


#' Calculate SDAI activity level
#'
#' Calculate the Simple Disease Activity Index Level for a SDAI score. Remission: >= 0 and
#' <= 3.3; Low: > 3.3 and <= 11; Moderate: > 11 and <= 26; High: > 26.
#'
#'
#' @param sdai numeric SDAI score.
#' @param ignore boolean ignore incorrect sdai values and return NA.
#' @return SDAI level.
#' @examples
#' sdai_classification(12)
#' sdai_classification(c(2, 42))
#'
#' @export
sdai_classification <- function(sdai, ignore = TRUE) {
  sdai <- suppressWarnings(as.numeric(sdai))

  if(any(is.na(sdai) | sdai < 0)) {
    if(ignore) {
      sdai[ is.na(sdai) | sdai < 0 ] <- NA
    } else {
      stop("SDAI score must be greater than 0.")
    }
  }

  activity <- rep(NA_character_, length(sdai))
  activity[sdai >= 0] <- "Remission"
  activity[sdai > 3.3] <- "Low"
  activity[sdai > 11] <- "Moderate"
  activity[sdai > 26] <- "High"

  return(activity)
}
