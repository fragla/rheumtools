#' Calculate DLQI score
#'
#' Calculate the Dermatology Life Quality Index score. The DLQI is a seven item
#' questionnaire relating to skin problems. For all questions with the exception
#' of question 10 the possible answers are "Very much", "A lot", "A little",
#' "Not at all" and "Not relevant". Question 7 is a two-part question where the
#' possible answers for the first part are "Yes", "No" or "Not relevant". The
#' second is only answered if the first part is "No". The possible answers to
#' the second part are "A lot", "A little" and "Not at all". Valid answers to
#' question 7 for this function are "Yes", "Not relevant", "A lot", "A little"
#' and "Not at all".
#'
#' @param q1 character Answer to DLQI question 1
#' @param q2 character Answer to DLQI question 2
#' @param q3 character Answer to DLQI question 3
#' @param q4 character Answer to DLQI question 4
#' @param q5 character Answer to DLQI question 5
#' @param q6 character Answer to DLQI question 6
#' @param q7 character Answer to DLQI question 7
#' @param q8 character Answer to DLQI question 8
#' @param q9 character Answer to DLQI question 9
#' @param q10 character Answer to DLQI question 10
#' @param ignore boolean ignore incorrect dlqi parameters and return NA.
#'
#' @return DLQI classification level.
#'
#' @examples
#' dlqi_score("Very much", "A lot", "A little","Not at all", "Not relevant",
#'             "A lot", "Yes", "Very much", "A lot", "A little")
#'
#' @export
dlqi_score <- function(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, ignore = TRUE) {
  if(!.equal_lengths(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)) {
    stop("Answers to questions 1 to 10 are not the same length.")
  }

  dlqi.cats <- dlqi_main_categories()
  dlqi.q7.cats <- dlqi_q7_categories()

  q1 <- unname(dlqi.cats[tolower(q1)])
  q2 <- unname(dlqi.cats[tolower(q2)])
  q3 <- unname(dlqi.cats[tolower(q3)])
  q4 <- unname(dlqi.cats[tolower(q4)])
  q5 <- unname(dlqi.cats[tolower(q5)])
  q6 <- unname(dlqi.cats[tolower(q6)])
  q7 <- unname(dlqi.q7.cats[tolower(q7)])
  q8 <- unname(dlqi.cats[tolower(q8)])
  q9 <- unname(dlqi.cats[tolower(q9)])
  q10 <- unname(dlqi.cats[tolower(q10)])

  if(any(is.na(q1) | ! q1 %in% 0:3)) {
    if(ignore) {
      q1[ is.na(q1) | ! q1 %in% 0:3 ] <- NA
    } else {
      stop("Q1 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q2) | ! q2 %in% 0:3)) {
    if(ignore) {
      q2[ is.na(q2) | ! q2 %in% 0:3 ] <- NA
    } else {
      stop("Q2 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q3) | ! q3 %in% 0:3)) {
    if(ignore) {
      q3[ is.na(q3) | ! q3 %in% 0:3 ] <- NA
    } else {
      stop("Q3 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q4) | ! q4 %in% 0:3)) {
    if(ignore) {
      q4[ is.na(q4) | ! q4 %in% 0:3 ] <- NA
    } else {
      stop("Q4 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q5) | ! q5 %in% 0:3)) {
    if(ignore) {
      q5[ is.na(q5) | ! q5 %in% 0:3 ] <- NA
    } else {
      stop("Q5 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q6) | ! q6 %in% 0:3)) {
    if(ignore) {
      q6[ is.na(q6) | ! q6 %in% 0:3 ] <- NA
    } else {
      stop("Q6 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q7) | ! q7 %in% 0:3)) {
    if(ignore) {
      q7[ is.na(q7) | ! q7 %in% 0:3 ] <- NA
    } else {
      stop("Q7 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q8) | ! q8 %in% 0:3)) {
    if(ignore) {
      q8[ is.na(q8) | ! q8 %in% 0:3 ] <- NA
    } else {
      stop("Q8 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q9) | ! q9 %in% 0:3)) {
    if(ignore) {
      q9[ is.na(q9) | ! q9 %in% 0:3 ] <- NA
    } else {
      stop("Q9 must be in the range 0 and 3.")
    }
  }

  if(any(is.na(q10) | ! q10 %in% 0:3)) {
    if(ignore) {
      q10[ is.na(q10) | ! q10 %in% 0:3 ] <- NA
    } else {
      stop("Q10 must be in the range 0 and 3.")
    }
  }

  total <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  return(total)
}

dlqi_main_categories <- function() {
  return(c("very much" = 3, "a lot" = 2, "a little" = 1, "not at all" = 0, "not relevant" = 0))
}

dlqi_q7_categories <- function() {
  return(c("yes" = 3, "a lot" = 2, "a little" = 1, "not at all" = 0, "not relevant" = 0))
}

#' Calculate DLQI classification
#'
#' Calculate the classification level for Dermatology Life Quality Index
#'
#' 0-1:   no effect at all on patient's life
#' 2-5:   small effect on patient's life
#' 6-10:  moderate effect on patient's life
#' 11-20: very large effect on patient's life
#' 21-30: extremely large effect on patient's life
#'
#' @param dlqi numeric DLQI score in the range 0 to 30.
#' @param ignore boolean ignore incorrect dlqi scores and return NA.
#'
#' @return DLQI classification level.
#'
#' @examples
#' dlqi_classification(17)
#' dlqi_classification(c(2, 25))
#'
#' @export
dlqi_classification <- function(dlqi, ignore = TRUE) {

  dlqi <- suppressWarnings(as.numeric(dlqi))

  if(any(is.na(dlqi) | !dlqi %in% 0:30)) {
    if(ignore) {
      dlqi[ is.na(dlqi) | !dlqi %in% 0:30 ] <- NA
    } else {
      stop("DLQI score must be an integer in the range 0 to 30.")
    }
  }

  classif <- rep(NA_character_, length(dlqi))
  classif[dlqi >= 0] <- "no effect at all on patient's life"
  classif[dlqi >= 2] <- "small effect on patient's life"
  classif[dlqi >= 6] <- "moderate effect on patient's life"
  classif[dlqi >= 11] <- "very large effect on patient's life"
  classif[dlqi >= 21] <- "extremely large effect on patient's life"

  return(classif)
}
