#' Classify Erythrocyte Sedimentation Rate
#'
#' Classify Erythrocyte Sedimentation Rate (ESR) as either positive or negative
#' based on ESR, age and sex.
#'
#' @param esr numeric Erythrocyte Sedimentation Rate mm/hr.
#' @param age numeric Age in years.
#' @param sex character Sex - Male or Female.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return ESR classification - Positive or Negative
#'
#' @examples
#' esr_classification(esr = 16, age = 25, sex = "Male")
#' esr_classification(esr = 10, age = 55, sex = "female")
#'
#' @source Tishkowski K, Gupta V. Erythrocyte Sedimentation Rate. 2023 Apr 23.
#' In: StatPearls [Internet]. Treasure Island (FL): StatPearls Publishing;
#' 2025 Jan–. PMID: 32491417.
#'
#' @export
esr_classification <- function(esr, age, sex, ignore = TRUE) {
  esr <- suppressWarnings(as.numeric(esr))
  age <- suppressWarnings(as.numeric(age))
  sex <- suppressWarnings(tolower(as.character(sex)))

  if(!.equal_lengths(esr, age, sex)) {
    stop("esr, age and sex are not the same length.")
  }

  if(any(is.na(esr) | esr < 0 )) {
    if(ignore) {
      esr[ is.na(esr) | esr < 0 ] <- NA
    } else {
      stop("ESR must be >= 0.")
    }
  }

  if(any(is.na(age) | age < 0 )) {
    if(ignore) {
      age[ is.na(age) | age < 0 ] <- NA
    } else {
      stop("Age must be greater than 0.")
    }
  }

  if(any(is.na(sex) | !sex %in% c("male", "female"))) {
    if(ignore) {
      sex[ is.na(sex) | !sex %in% c("male", "female") ] <- NA
    } else {
      stop("Sex must be Male or Female.")
    }
  }

  esr_status <- rep(NA_character_, length(esr))
  esr_status[esr <= 15 & age <= 50 & sex == "male"] <- "Negative"
  esr_status[esr <= 20 & age <= 50 & sex == "female"] <- "Negative"
  esr_status[esr <= 20 & age > 50 & sex == "male"] <- "Negative"
  esr_status[esr <= 30 & age > 50 & sex == "female"] <- "Negative"
  esr_status[esr > 15 & age <= 50 & sex == "male"] <- "Positive"
  esr_status[esr > 20 & age <= 50 & sex == "female"] <- "Positive"
  esr_status[esr > 20 & age > 50 & sex == "male"] <- "Positive"
  esr_status[esr > 30 & age > 50 & sex == "female"] <- "Positive"

  return(esr_status)
}

#' Classify C-reactive protein
#'
#' Classify C-reactive protein (CRP) as either positive or negative
#' based on CRP value.
#'
#' @param crp numeric C-reactive protein (mg/dL)
#' @param cutoff CRP cutoff for positive/negative classicication.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return CRP classification - Positive or Negative
#'
#' @examples
#' crp_classification(crp = 16)
#' crp_classification(crp = 12, cutoff = 10)
#'
#' @source Nehring SM, Goyal A, Patel BC. C Reactive Protein. 2023 Jul 10.
#' In: StatPearls [Internet]. Treasure Island (FL): StatPearls Publishing;
#' 2025 Jan–. PMID: 28722873.
#'
#' @export
crp_classification <- function(crp, cutoff = NULL, ignore = TRUE) {

  crp <- suppressWarnings(as.numeric(crp))

  if(any(is.na(crp) | crp < 0 )) {
    if(ignore) {
      crp[ is.na(crp) | crp < 0 ] <- NA
    } else {
      stop("CRP must be >= 0.")
    }
  }

  if(!is.null(cutoff)) {
    crp_status <- .cutoff_classification(crp, cutoff = cutoff)
  } else {
    crp_status <- .multi_classification(crp)
  }

  return(crp_status)
}

.multi_classification <- function(crp) {

  crp_status <- rep(NA_character_, length(crp))
  crp_status[crp < 0.3] <- "Normal"
  crp_status[crp >= 0.3 & crp < 1] <- "Normal or minor elevation"
  crp_status[crp >= 1 & crp < 10] <- "Moderate elevation"
  crp_status[crp >= 10] <- "Marked elevation"
  crp_status[crp >= 50] <- "Severe elevation"

  return(crp_status)
}

.cutoff_classification <- function(crp, cutoff) {

  crp_status <- rep(NA_character_, length(crp))
  crp_status[ which(crp < cutoff)] <- "Negative"
  crp_status[ which(crp >= cutoff)] <- "Positive"

  return(crp_status)
}
