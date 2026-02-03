#' Calculate Body Mass Index
#'
#' Calculate Body Mass Index using weight and height.
#'
#' @param weight numeric weight.
#' @param height numeric height.
#' @param metric logical metric units (kilograms/metres) rather than imperial
#' (pounds/inches) Default is TRUE.
#' @param digits numeric the number of decimal places. Defaults to 1.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return Body Mass Index
#'
#' @examples
#' body_mass_index(weight = 70, height = 1.75)
#'
#' @export
body_mass_index <- function(weight, height, metric = TRUE, digits = 1, ignore = TRUE) {
  weight <- suppressWarnings(as.numeric(weight))
  height <- suppressWarnings(as.numeric(height))

  if(!.equal_lengths(weight, height)) {
    stop("height and weight are not the same length.")
  }

  if(any(is.na(height) | height < 0)) {
    if(ignore) {
      height[ is.na(height) | height < 0 ] <- NA
    } else {
      stop("Height must be be greater than 0.")
    }
  }

  if(any(!is.na(height) & metric & height > 2.5)) {
    warning("Check heights are not in centimetres or inches.", call. = FALSE)
  }

  if(any(is.na(weight) | weight < 0)) {
    if(ignore) {
      weight[ is.na(weight) | weight < 0 ] <- NA
    } else {
      stop("Weight must be be greater than 0.")
    }
  }

  bmi <- weight / height^2

  if(!metric) {
    bmi <- bmi * 703
  }

  round(bmi, digits = digits)
}

#' Classify Body Mass Index
#'
#' Classify Body Mass Index as Underweight, Normal, Overweight or Obese.
#'
#' @param bmi numeric Body Mass Index.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return BMI classification
#'
#' @examples
#' body_mass_index_classification(22.9)
#'
#' @export
body_mass_index_classification <- function(bmi, ignore = TRUE) {
  bmi <- suppressWarnings(as.numeric(bmi))

  if(any(is.na(bmi))) {
    if(!ignore) {
      stop("BMI must be a numeric.")
    }
  }

  classif <- rep(NA, length(bmi))
  classif[bmi < 18.5] <- "Underweight"
  classif[bmi >= 18.5 & bmi < 25] <- "Normal"
  classif[bmi >= 25 & bmi < 30] <- "Overweight"
  classif[bmi >= 30 ] <- "Obese"
  return(classif)
}
