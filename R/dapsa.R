#' Calculate DAPSA score
#'
#' Calculate the Disease Activity Index for Psoriatic Arthritis.
#'
#' @param tjc numeric Tender 68 joint count (0 to 68).
#' @param sjc numeric Swollen 66 joint count (0 to 66).
#' @param pain numeric Pain VAS (0 to 10cm).
#' @param ptgh numeric Patient Global Health VAS score (0 to 10cm).
#' @param crp numeric C-reactive protein (CRP) in mg/dl.
#' @param digits numeric specifying the number of decimal places. Defaults to 0.
#' @param crp_unit character specifying either 'mg/L' or 'mg/dL'.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return DAPSA score.
#'
#' @examples
#' dapsa_score(tjc = 4, sjc = 5, pain = 2.5, ptgh = 5, crp = 7)
#' dapsa_score(tjc = 28, sjc = 28, pain = 10, ptgh = 10, crp = 10)
#'
#' @export
dapsa_score <- function(tjc, sjc, pain, ptgh, crp, digits = 0, crp_unit, ignore = TRUE) {

  if(is.na(crp_unit) || !crp_unit %in% c('mg/L', 'mg/dL')){
    stop("crp_units must be one of 'mg/L' or 'mg/dL'")
  }

  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  crp <- suppressWarnings(as.numeric(crp))
  pain <- suppressWarnings(as.numeric(pain))
  ptgh <- suppressWarnings(as.numeric(ptgh))

  if(!.equal_lengths(tjc, sjc, pain, ptgh, crp)) {
    stop("tjc, sjc, pain, ptgh and crp are not the same length.")
  }

  if(any(is.na(tjc) | tjc < 0 | tjc > 68)) {
    if(ignore) {
      tjc[ is.na(tjc) | tjc < 0 | tjc > 68 ] <- NA
    } else {
      stop("Tender Joint Count must be between 0 and 68.")
    }
  }

  if(any(is.na(sjc) | sjc < 0 | sjc > 66)) {
    if(ignore) {
      sjc[ is.na(sjc) | sjc < 0 | sjc > 66 ] <- NA
    } else {
      stop("Swollen Joint Count must be between 0 and 66")
    }
  }

  if(any(is.na(crp) | crp < 0)) {
    if(ignore) {
      crp[ is.na(crp) | crp < 0 ] <- NA
    } else {
      stop("CRP value must be ≥ 0.")
    }
  }

  if(any(is.na(pain) | pain < 0 | pain > 10)) {
    if(ignore) {
      pain[ is.na(pain) | pain < 0 | pain > 10 ] <- NA
    } else {
      stop("Patient Pain VAS must be between 0 and 10.")
    }
  }

  if(any(is.na(ptgh) | ptgh < 0 | ptgh > 10)) {
    if(ignore) {
      ptgh[ is.na(ptgh) | ptgh < 0 | ptgh > 10 ] <- NA
    } else {
      stop("Patient global VAS must be between 0 and 10.")
    }
  }

  if (crp_unit == "mg/L") {
    crp <- crp / 10
  }

  if (any(crp > 10, na.rm = TRUE)) {
    warning("Some CRP values > 10 mg/dL detected; confirm units (common mix-up with mg/L).", call. = FALSE)
  }

  dapsa <- round(tjc + sjc + pain + ptgh + crp, digits = digits)

  return(dapsa)
}

#' Calculate Clinical DAPSA score
#'
#' Calculate the Clinical Disease Activity Index for Psoriatic Arthritis.
#'
#' @param tjc numeric Tender 68 joint count (0 to 68).
#' @param sjc numeric Swollen 66 joint count (0 to 66).
#' @param pain numeric Pain VAS (0 to 10cm).
#' @param ptgh numeric Patient Global Health VAS score (0 to 10cm).
#' @param digits numeric specifying the number of decimal places. Defaults to 0.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return Clinical DAPSA score.
#'
#' @examples
#' clinical_dapsa_score(tjc = 4, sjc = 5, pain = 2.5, ptgh = 5)
#' clinical_dapsa_score(tjc = 28, sjc = 28, pain = 10, ptgh = 10)
#'
#' @export
clinical_dapsa_score <- function(tjc, sjc, pain, ptgh, digits = 0, ignore = TRUE) {

  tjc <- suppressWarnings(as.numeric(tjc))
  sjc <- suppressWarnings(as.numeric(sjc))
  pain <- suppressWarnings(as.numeric(pain))
  ptgh <- suppressWarnings(as.numeric(ptgh))

  if(!.equal_lengths(tjc, sjc, pain, ptgh)) {
    stop("tjc, sjc, pain and ptgh are not the same length.")
  }

  if(any(is.na(tjc) | tjc < 0 | tjc > 68)) {
    if(ignore) {
      tjc[ is.na(tjc) | tjc < 0 | tjc > 68 ] <- NA
    } else {
      stop("Tender Joint Count must be between 0 and 68.")
    }
  }

  if(any(is.na(sjc) | sjc < 0 | sjc > 66)) {
    if(ignore) {
      sjc[ is.na(sjc) | sjc < 0 | sjc > 66 ] <- NA
    } else {
      stop("Swollen Joint Count must be between 0 and 66")
    }
  }

  if(any(is.na(pain) | pain < 0 | pain > 10)) {
    if(ignore) {
      pain[ is.na(pain) | pain < 0 | pain > 10 ] <- NA
    } else {
      stop("Patient Pain VAS must be between 0 and 10.")
    }
  }

  if(any(is.na(ptgh) | ptgh < 0 | ptgh > 10)) {
    if(ignore) {
      ptgh[ is.na(ptgh) | ptgh < 0 | ptgh > 10 ] <- NA
    } else {
      stop("Patient global VAS must be between 0 and 10.")
    }
  }

  dapsa <- round(tjc + sjc + pain + ptgh, digits = digits)

  return(dapsa)
}

#' Calculate DAPSA activity level scores
#'
#' Calculate the Disease Activity Level for a DAPSA score. Remission: >= 0 and
#' <= 4; Low: > 4 and <= 14; Moderate: > 14 and <= 28; High: > 28.
#'
#' @param dapsa numeric DAPSA score.
#' @param ignore boolean ignore incorrect DAPSA values and return NA.
#'
#' @return DAPSA activity level.
#'
#' @examples
#' dapsa_classification(13)
#' dapsa_classification(266)
#'
#' @export
dapsa_classification <- function(dapsa, ignore = TRUE) {
  dapsa <- suppressWarnings(as.numeric(dapsa))

  if(any(is.na(dapsa) | dapsa < 0)) {
    if(ignore) {
      dapsa[ is.na(dapsa) | dapsa < 0 ] <- NA
    } else {
      stop("DAPSA score must be greater than 0.")
    }
  }

  activity <- rep(NA, length(dapsa))
  activity[dapsa <= 4] <- "Remission"
  activity[dapsa > 4] <- "Low"
  activity[dapsa > 14] <- "Moderate"
  activity[dapsa > 28] <- "High"

  return(activity)

}

#' Calculate DAPSA response
#'
#' Calculate the DAPSA response for a DAPSA percentage response.
#'
#' @param response numeric DAPSA percentage response.
#' @param ignore boolean ignore incorrect DAPSA value types and return NA.
#'
#' @return DAPSA response classification
#'
#' @examples
#' dapsa_response(76.9)
#' dapsa_response(c(88.0, -13.3))
#'
#' @export
dapsa_response <- function(response, ignore = TRUE) {
  response <- suppressWarnings(as.numeric(response))

  if(any(is.na(response))) {
    if(!ignore) {
      stop("DAPSA response score must be a numeric.")
    }
  }

  cat <- rep(NA, length(response))
  cat[response < 50] <- "None"
  cat[response >= 50] <- "Minor"
  cat[response >= 75] <- "Moderate"
  cat[response >= 85] <- "Major"
  return(cat)
}
