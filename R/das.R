#' Calculate DAS28-CRP scores
#'
#' Calculate the Disease Activity Score 28 (DAS28-CRP) for Rheumatoid
#'  Arthritis with CRP.
#'
#' @param tjc numeric Tender 28 joint count (0 to 28).
#' @param sjc numeric Swollen 28 joint count (0 to 28).
#' @param crp numeric C-reactive protein (CRP) in mg/l.
#' @param ptgh numeric Patient Global Health VAS score (0 to 100mm).
#' @param digits numeric specifying the number of decimal places. Defaults to 2.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return DAS28-CRP score.
#'
#' @examples
#' das_28_crp_score(tjc = 4, sjc = 5, crp = 7, ptgh = 50)
#' das_28_crp_score(tjc = 28, sjc = 28, crp = 10, ptgh = 100)
#'
#' @export
das_28_crp_score <- function(tjc, sjc, crp, ptgh, digits = 2, ignore=TRUE) {
  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  crp <- suppressWarnings(as.numeric(crp))
  ptgh <- suppressWarnings(as.numeric(ptgh))

  if(!.equal_lengths(tjc, sjc, crp, ptgh)) {
    stop("tjc, sjc, crp and ptgh are not the same length.")
  }

  if(any(is.na(tjc) | tjc < 0 | tjc > 28)) {
    if(ignore) {
      tjc[ is.na(tjc) | tjc < 0 | tjc > 28 ] <- NA
    } else {
      stop("Tender Joint Count must be between 0 and 28.")
    }
  }

  if(any(is.na(sjc) | sjc < 0 | sjc > 28)) {
    if(ignore) {
      sjc[ is.na(sjc) | sjc < 0 | sjc > 28 ] <- NA
    } else {
      stop("Swollen Joint Count must be between 0 and 28.")
    }
  }

  if(any(is.na(crp) | crp < 0)) {
    if(ignore) {
      crp[ is.na(crp) | crp < 0 ] <- NA
    } else {
      stop("CRP value must be greater than 0.")
    }
  }

  if(any(is.na(ptgh) | ptgh < 0 | ptgh > 100)) {
    if(ignore) {
      ptgh[ is.na(ptgh) | ptgh < 0 | ptgh > 100 ] <- NA
    } else {
      stop("Patient global VAS must be between 0 and 100.")
    }
  }

  round(0.56 * sqrt(tjc) + 0.28 * sqrt(sjc) + 0.36 * log(crp + 1) + 0.014 * ptgh + 0.96, digits)

}

#' Calculate DAS28-ESR scores
#'
#' Calculate the Disease Activity Score 28 (DAS28-ESR) for Rheumatoid
#'  Arthritis with ESR.
#'
#' @param tjc numeric Tender 28 joint count (0 to 28).
#' @param sjc numeric Swollen 28 joint count (0 to 28).
#' @param esr numeric Erythrocyte Sedimentation Rate (ESR) in mm/hour.
#' @param ptgh numeric Patient Global Health VAS score (0 to 100mm).
#' @param digits numeric specifying the number of decimal places. Defaults to 2.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return DAS28-ESR score.
#'
#' @examples
#' das_28_esr_score(tjc = 4, sjc = 5, esr = 7, ptgh = 50)
#' das_28_esr_score(tjc = 28, sjc = 28, esr = 10, ptgh = 100)
#'
#' @export
das_28_esr_score <- function(tjc, sjc, esr, ptgh, digits = 2, ignore = TRUE) {

  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  esr <- suppressWarnings(as.numeric(esr))
  ptgh <- suppressWarnings(as.numeric(ptgh))

  if(!.equal_lengths(tjc, sjc, esr, ptgh)) {
    stop("tjc, sjc, esr and ptgh are not the same length.")
  }

  if(any(is.na(tjc) | tjc < 0 | tjc > 28)) {
    if(ignore) {
      tjc[ is.na(tjc) | tjc < 0 | tjc > 28 ] <- NA
    } else {
      stop("Tender Joint Count must be between 0 and 28.")
    }
  }

  if(any(is.na(sjc) | sjc < 0 | sjc > 28)) {
    if(ignore) {
      sjc[ is.na(sjc) | sjc < 0 | sjc > 28 ] <- NA
    } else {
      stop("Swollen Joint Count must be between 0 and 28.")
    }
  }

  if(any(is.na(esr) | esr < 0)) {
    if(ignore) {
      esr[ is.na(esr) | esr < 0 ] <- NA
    } else {
      stop("ESR value must be greater than 0.")
    }
  }

  if(any(is.na(ptgh) | ptgh < 0 | ptgh > 100)) {
    if(ignore) {
      ptgh[ is.na(ptgh) | ptgh < 0 | ptgh > 100 ] <- NA
    } else {
      stop("Patient global VAS must be between 0 and 100.")
    }
  }

  round(0.56 * sqrt(tjc) + 0.28 * sqrt(sjc) + 0.70 * log(esr) + 0.014 * ptgh, digits)

}

#' Calculate DAS activity level
#'
#' Calculate the Disease Activity Level for a DAS28 score. Remission: >= 0 and
#' < 2.6; Low: >= 2.6 and < 3.2; Moderate: >= 3.2 and <= 5.1; High: > 5.1.
#'
#' @param das numeric DAS28 score.
#' @param ignore boolean ignore incorrect das values and return NA.
#'
#' @return DAS28 activity level.
#'
#' @examples
#' das_28_classification(1.80)
#' das_28_classification(c(5.11, 4.25))
#'
#' @export
das_28_classification <- function(das, ignore = TRUE) {
  das <- suppressWarnings(as.numeric(das))

  if(any(is.na(das) | das < 0)) {
    if(ignore) {
      das[ is.na(das) | das < 0 ] <- NA
    } else {
      stop("DAS28 score must be greater than 0.")
    }
  }

  activity <- rep(NA_character_, length(das))
  activity[das >= 0] <- "Remission"
  activity[das >= 2.6] <- "Low"
  activity[das > 3.2] <- "Moderate"
  activity[das > 5.1] <- "High"

  return(activity)
}

#' Calculate EULAR response
#'
#' Calculate the EULAR response between two DAS28 scores.
#'
#' @param das1 numeric first DAS28 score
#' @param das2 numeric follow-up DAS28 score
#' @param ignore boolean ignore incorrect das values and return NA.
#'
#' @return EULAR response classification
#'
#' @examples
#' eular_ra_response(das1 = 5.21, das2 = 1.8)
#' eular_ra_response(das1 = c(4.3, 3.93), das2 = c(5.13, 1.1))
#'
#' @export
eular_ra_response <- function(das1, das2, ignore = TRUE) {

  das1 <- suppressWarnings(as.numeric(das1))
  das2 <- suppressWarnings(as.numeric(das2))

  if(!.equal_lengths(das1, das2)) {
    stop("das1 and das2 are not the same length.")
  }

  if(any(is.na(das1) | das1 < 0)) {
    if(ignore) {
      das1[ is.na(das1) | das1 < 0 ] <- NA
    } else {
      stop("das1 must be greater than 0.")
    }
  }

  if(any(is.na(das2) | das2 < 0)) {
    if(ignore) {
      das2[ is.na(das2) | das2 < 0 ] <- NA
    } else {
      stop("das2 must be greater than 0.")
    }
  }

  response <- rep(NA_character_, length(das1))

  response[which(das2 <= 3.2 & (das1 - das2) > 1.2)] <- "Good"
  response[which(das2 <= 3.2 & (das1 - das2) > 0.6 & (das1 - das2) <= 1.2)] <- "Moderate"
  response[which(das2 <= 3.2 & (das1 - das2) <= 0.6)] <- "No response"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1 - das2) > 1.2)] <- "Moderate"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1 - das2) > 0.6 & (das1 - das2) <= 1.2)] <- "Moderate"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1 - das2) <= 0.6)] <- "No response"
  response[which(das2 > 5.1 & (das1 - das2) > 1.2)] <- "Moderate"
  response[which(das2 > 5.1 & (das1 - das2) > 0.6 & (das1 - das2) <= 1.2)] <- "No response"
  response[which(das2 > 5.1 & (das1 - das2) <= 0.6)] <- "No response"

  return(response)
}
