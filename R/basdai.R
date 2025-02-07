#' Calculate BASDAI scores
#'
#' Calculate the Bath Ankylosing Spondylitis Disease Activity Index scores.
#' Questions scored from 0 (none) to 10 (very severe).
#'
#' @param q1 numeric question 1: How would you describe the overall level of
#' fatigue/tiredness you have experienced?
#' @param q2 numeric question 2: How would you describe the overall level of
#' AS neck, back or hip pain you have had?
#' @param q3 numeric question 3: How would you describe the overall level of
#' pain/swelling in joints other than neck, back or hips you have had?
#' @param q4 numeric question 4: How would you describe the overall level of
#' discomfort you have had from any areas tender to touch or pressure?
#' @param q5 numeric question 5: How would you describe the overall level of
#' morning stiffness you have had from the time you wake up?
#' @param q6 numeric question 6: How long does your morning stiffness last from
#' the time you wake up?
#' @param digits numeric the number of decimal places. Defaults to 1.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return Body Mass Index
#'
#' @references \url{https://pubmed.ncbi.nlm.nih.gov/7699630/}
#'
#' @examples
#' basdai_score(q1 = 1, q2 = 2, q3 = 3, q4 = 4, q5 = 5, q6 = 10)
#'
#' @export
basdai_score <- function (q1, q2, q3, q4, q5, q6, digits = 1, ignore = TRUE) {
  q1 <- suppressWarnings(as.numeric(q1))
  q2 <- suppressWarnings(as.numeric(q2))
  q3 <- suppressWarnings(as.numeric(q3))
  q4 <- suppressWarnings(as.numeric(q4))
  q5 <- suppressWarnings(as.numeric(q5))
  q6 <- suppressWarnings(as.numeric(q6))

  if(!.equal_lengths(q1, q2, q3, q4, q5, q6)) {
    stop("Question answers are not the same length.")
  }

  if(any(is.na(q1) | q1 < 0 | q1 > 10)) {
    if(ignore) {
      q1[ is.na(q1) | q1 < 0 | q1 > 10 ] <- NA
    } else {
      stop("Q1 must be between 0 and 10.")
    }
  }

  if(any(is.na(q2) | q2 < 0 | q2 > 10)) {
    if(ignore) {
      q2[ is.na(q2) | q2 < 0 | q2 > 10 ] <- NA
    } else {
      stop("Q2 must be between 0 and 10.")
    }
  }

  if(any(is.na(q3) | q3 < 0 | q3 > 10)) {
    if(ignore) {
      q3[ is.na(q3) | q3 < 0 | q3 > 10 ] <- NA
    } else {
      stop("Q3 must be between 0 and 10.")
    }
  }

  if(any(is.na(q4) | q4 < 0 | q4 > 10)) {
    if(ignore) {
      q4[ is.na(q4) | q4 < 0 | q4 > 10 ] <- NA
    } else {
      stop("Q4 must be between 0 and 10.")
    }
  }

  if(any(is.na(q5) | q5 < 0 | q5 > 10)) {
    if(ignore) {
      q5[ is.na(q5) | q5 < 0 | q5 > 10 ] <- NA
    } else {
      stop("Q5 must be between 0 and 10.")
    }
  }

  if(any(is.na(q6) | q6 < 0 | q6 > 10)) {
    if(ignore) {
      q6[ is.na(q6) | q6 < 0 | q6 > 10 ] <- NA
    } else {
      stop("Q6 must be between 0 and 10.")
    }
  }

  round(((q1 + q2 + q3 + q4) + ((q5 + q6) / 2)) / 5, digits)
}
