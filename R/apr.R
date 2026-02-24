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
#' Classifies C-reactive protein (CRP) values into standard or hs-CRP categories,
#' or using a custom numeric cutoff.
#'
#' @param crp numeric C-reactive protein (mg/dL)
#' @param cutoff optional numeric cutoff for binary classification. If provided, `mode` is ignored.
#' @param crp_unit character 'mg/L' or 'mg/dL'. Required unless `cutoff` is provided.
#' @param mode character specifying either 'standard' (default) or 'hs' classification. Required unless cutoff is provided.
#' @param ignore boolean If TRUE, negative or NA CRP values are set to NA. If FALSE, an error is thrown.
#'
#' @return CRP classification - Positive or Negative OR Normal, Normal or minor
#' elevation, Moderate elevation, Marked elevation, Severe elevation
#'
#' @examples
#' # Standard CRP in mg/L
#' crp_classification(c(2, 8, 25, 120), crp_unit = "mg/L")
#'
#' # hs-CRP in mg/L
#' crp_classification(c(0.5, 2, 5, 12), crp_unit = "mg/L", mode = "hs")
#'
#' # custom cutoff
#' crp_classification(crp = 12, cutoff = 10)
#'
#' @source
#' Singh B, Goyal A, Patel BC. C-Reactive Protein: Clinical Relevance and Interpretation. [Updated 2025 May 3].
#' In: StatPearls [Internet]. Treasure Island (FL): StatPearls Publishing; 2025 Jan-.
#' Available from: https://www.ncbi.nlm.nih.gov/books/NBK441843/
#'
#' Pearson TA, et al. "Markers of inflammation and cardiovascular disease:
#' application to clinical and public health practice." *Circulation*, 2003;107:499–511.
#'
#' Clinical CRP thresholds (standard CRP):
#' Normal <0.3 mg/dL, Normal or minor 0.3–1 mg/dL, Moderate 1–10 mg/dL,
#' Marked 10–50 mg/dL, Severe >50 mg/dL.
#'
#' hs-CRP thresholds (cardiovascular risk):
#' Low risk <1 mg/L, Average risk 1–3 mg/L, High risk >3mg/L
#' >10 mg/L (may indicate acute inflammation; repeat test recommended).
#'
#' @export
crp_classification <- function(crp, cutoff = NULL, crp_unit = NULL, mode = c("standard", "hs"), ignore = TRUE) {

  mode <- match.arg(mode)

  if(is.null(crp_unit) && is.null(cutoff)){
    stop("Either crp_unit or cutoff must be provided.")
  }

  # validate cutoff (if provided)
  if (!is.null(cutoff)) {

    cutoff <- suppressWarnings(as.numeric(cutoff))

    if (is.na(cutoff) || length(cutoff) != 1) {
      stop("cutoff must be a single numeric value.")
    }

    if (cutoff < 0) {
      stop("cutoff must be >= 0.")
    }
  }

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
    if (mode =="standard") {
      crp_status <- .multi_classification(crp, crp_unit)
    } else if (mode == "hs") {
      crp_status <- .hs_classification(crp, crp_unit)
    }
  }

  return(crp_status)
}

.multi_classification <- function(crp, crp_unit) {

  if(is.null(crp_unit) || !crp_unit %in% c('mg/L', 'mg/dL')){
    stop("crp_units must be one of 'mg/L' or 'mg/dL'")
  }

  if (crp_unit == "mg/L") {
    crp <- crp / 10
  }

  crp_status <- rep(NA_character_, length(crp))

  crp_status[crp < 0.3] <- "Normal"
  crp_status[crp >= 0.3 & crp < 1] <- "Normal or minor elevation"
  crp_status[crp >= 1 & crp < 10] <- "Moderate elevation"
  crp_status[crp >= 10 & crp < 50] <- "Marked elevation"
  crp_status[crp >= 50] <- "Severe elevation"

  return(crp_status)
}

.hs_classification <- function(crp, crp_unit) {

  if (is.null(crp_unit) || !crp_unit %in% c("mg/L", "mg/dL")) {
    stop("crp_unit must be one of 'mg/L' or 'mg/dL'")
  }

  # Ensure everything in mg/L (hs-CRP thresholds are in mg/L)
  if (crp_unit == "mg/dL") {
    crp <- crp * 10
  }

  # Issue warning for very high values
  if (any(crp > 10, na.rm = TRUE)) {
    warning("hs-CRP > 10 mg/L detected: may indicate acute inflammation. Consider repeating after resolution.")
  }

  crp_status <- rep(NA_character_, length(crp))

  crp_status[crp < 1] <- "Low risk"
  crp_status[crp >= 1 & crp <= 3] <- "Average risk"
  crp_status[crp > 3] <- "High risk"

  return(crp_status)
}

.cutoff_classification <- function(crp, cutoff) {

  crp_status <- rep(NA_character_, length(crp))
  crp_status[ which(crp < cutoff)] <- "Negative"
  crp_status[ which(crp >= cutoff)] <- "Positive"

  return(crp_status)
}
