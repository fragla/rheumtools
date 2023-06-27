#psaid_12_score(8, 10, 6, 6, 6, 9, 9, 5, 10, 8, 6, 1) example = 7.3
# The PsAID score gives a number between 0 and 10. A higher score on the PsAID indicates more impact of the disease.
# A score below 4 out of 10 is considered a patient-acceptable status.
# A change of 3 or more points is considered relevant absolute change.

#' Calculate PSAID-12 score
#'
#' Calculate the EULAR Psoriatic Arthritis Impact of Disease 12 item score.
#'
#' @param pain numeric Pain due PsA during the last week (0: none to 10: extreme).
#' @param fatigue numeric. Level of the fatigue due PsA during the last week (0: no fatigue to 10: totally exhausted).
#' @param skin numeric Skin problems due PsA during the last week (0: none to 10: extreme).
#' @param work numeric Difficulties participating fully in work and/or leisure activities due to PsA during the last week (0: none to 10: extreme).
#' @param functional numeric. Difficulty doing daily physical activities due PsA during the last week (0: no difficulty to 10: extreme difficulty).
#' @param discomfort numeric. Feeling of discomfort and annoyance with everyday tasks due to PsA during the last week (0: none to 10: extreme).
#' @param sleep numeric. Sleep difficulties (i.e. resting at night) due to PsA during the last week (0: no difficulty to 10: extreme difficulty).
#' @param coping numeric. Coping (manage, deal, make do) with PsA during the last week (0: very well to 10: very poorly).
#' @param anxiety numeric. Level of anxiety, fear and uncertainty due to your PsA during the last week (0: none to 10: extreme).
#' @param shame numeric. Level of embarrassment and/or shame due to appearance experienced during the last week (0: none to 10: extreme).
#' @param social numeric. Difficulties participating fully in social activities due to PsA during the last week (0: none to 10: extreme).
#' @param depression numeric. Level of depression due to PsA experienced during the last week (0: none to 10: extreme).
#' @param digits numeric specifying the number of decimal places. Defaults to 1.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return PSAID-12 score.
#'
#' @examples
#' psaid_12_score(8, 10, 6, 6, 6, 9, 9, 5, 10, 8, 6, 1)
#'
#' @export
psaid_12_score <- function(pain, fatigue, skin, work, functional, discomfort, sleep, coping, anxiety, shame, social, depression, digits=1, ignore=TRUE) {
 ##If one value missing impute using mean.
  pain <- suppressWarnings(as.numeric(pain))
  fatigue <- suppressWarnings(as.numeric(fatigue))
  skin <- suppressWarnings(as.numeric(skin))
  work <- suppressWarnings(as.numeric(work))
  functional <- suppressWarnings(as.numeric(functional))
  discomfort <- suppressWarnings(as.numeric(discomfort))
  sleep <- suppressWarnings(as.numeric(sleep))
  coping <- suppressWarnings(as.numeric(coping))
  anxiety <- suppressWarnings(as.numeric(anxiety))
  shame <- suppressWarnings(as.numeric(shame))
  social <- suppressWarnings(as.numeric(social))
  depression <- suppressWarnings(as.numeric(depression))

  if(!.equal_lengths(pain, fatigue, skin, work, functional, discomfort, sleep, coping, anxiety, shame, social, depression)) {
    stop("pain, fatigue, skin, work, functional, discomfort, sleep, coping, shame, social and depression are not the same length.")
  }

  if(any(is.na(pain) | ! pain %in% 0:10)) {
    if(ignore) {
      pain[ which(!is.na(pain) & !pain %in% 0:10) ] <- NA
    } else {
      stop("Pain item must be between 0 and 10.")
    }
  }

  if(any(is.na(fatigue) | ! fatigue %in% 0:10)) {
    if(ignore) {
      fatigue[ which(!is.na(fatigue) & !fatigue %in% 0:10) ] <- NA
    } else {
      stop("Fatigue item must be between 0 and 10.")
    }
  }

  if(any(is.na(skin) | ! skin %in% 0:10)) {
    if(ignore) {
      skin[ which(!is.na(skin) & !skin %in% 0:10) ] <- NA
    } else {
      stop("Skin item must be between 0 and 10.")
    }
  }

  if(any(is.na(work) | ! work %in% 0:10)) {
    if(ignore) {
      work[ which(!is.na(work) & !work %in% 0:10) ] <- NA
    } else {
      stop("Work item must be between 0 and 10.")
    }
  }

  if(any(is.na(functional) | ! functional %in% 0:10)) {
    if(ignore) {
      functional[ which(!is.na(functional) & !functional %in% 0:10) ] <- NA
    } else {
      stop("Functional item must be between 0 and 10.")
    }
  }

  if(any(is.na(discomfort) | ! discomfort %in% 0:10)) {
    if(ignore) {
      discomfort[ which(!is.na(discomfort) & !discomfort %in% 0:10) ] <- NA
    } else {
      stop("Discomfort item must be between 0 and 10.")
    }
  }

  if(any(is.na(sleep) | ! sleep %in% 0:10)) {
    if(ignore) {
      sleep[ which(!is.na(sleep) & !sleep %in% 0:10) ] <- NA
    } else {
      stop("Sleep item must be between 0 and 10.")
    }
  }

  if(any(is.na(coping) | ! coping %in% 0:10)) {
    if(ignore) {
      coping[ which(!is.na(coping) & !coping %in% 0:10) ] <- NA
    } else {
      stop("Coping item must be between 0 and 10.")
    }
  }

  if(any(is.na(anxiety) | ! anxiety %in% 0:10)) {
    if(ignore) {
      anxiety[ which(!is.na(anxiety) & !anxiety %in% 0:10) ] <- NA
    } else {
      stop("Anxiety item must be between 0 and 10.")
    }
  }

  if(any(is.na(shame) | ! shame %in% 0:10)) {
    if(ignore) {
      shame[ which(!is.na(shame) & !shame %in% 0:10) ] <- NA
    } else {
      stop("Shame item must be between 0 and 10.")
    }
  }

  if(any(is.na(social) | ! social %in% 0:10)) {
    if(ignore) {
      social[ which(!is.na(social) & !social %in% 0:10) ] <- NA
    } else {
      stop("Social item must be between 0 and 10.")
    }
  }

  if(any(is.na(depression) | ! depression %in% 0:10)) {
    if(ignore) {
      depression[ which(!is.na(depression) & !depression %in% 0:10) ] <- NA
    } else {
      stop("Depression item must be between 0 and 10.")
    }
  }

  score <- pain * 3 + fatigue * 2 + skin * 2 + work * 2 +
    functional * 2 + discomfort * 2 + sleep * 2 + coping +
    anxiety + shame + social + depression

  round(score / 20, digits)
}

#psaid_9_score(8, 10, 6, 6, 6, 9, 9, 5, 10) example =7.7
# The PsAID score gives a number between 0 and 10. A higher score on the PsAID indicates more impact of the disease.
# A score below 4 out of 10 is considered a patient-acceptable status.
# A change of 3 or more points is considered relevant absolute change.

#' Calculate PSAID-9 score
#'
#' Calculate the EULAR Psoriatic Arthritis Impact of Disease nine item score.
#'
#' @param pain numeric Pain due PsA during the last week (0: none to 10: extreme).
#' @param fatigue numeric. Level of the fatigue due PsA during the last week (0: no fatigue to 10: totally exhausted).
#' @param skin numeric Skin problems due PsA during the last week (0: none to 10: extreme).
#' @param work numeric Difficulties participating fully in work and/or leisure activities due to PsA during the last week (0: none to 10: extreme).
#' @param functional numeric. Difficulty doing daily physical activities due PsA during the last week (0: no difficulty to 10: extreme difficulty).
#' @param discomfort numeric. Feeling of discomfort and annoyance with everyday tasks due to PsA during the last week (0: none to 10: extreme).
#' @param sleep numeric. Sleep difficulties (i.e. resting at night) due to PsA during the last week (0: no difficulty to 10: extreme difficulty).
#' @param coping numeric. Coping (manage, deal, make do) with PsA during the last week (0: very well to 10: very poorly).
#' @param anxiety numeric. Level of anxiety, fear and uncertainty due to your PsA during the last week (0: none to 10: extreme).
#' @param digits numeric specifying the number of decimal places. Defaults to 1.
#' @param ignore boolean ignore incorrect parameter values and return NA.
#'
#' @return PSAID-9 score.
#'
#' @examples
#' psaid_9_score(8, 10, 6, 6, 6, 9, 9, 5, 10)
#'
#' @export
psaid_9_score <- function(pain, fatigue, skin, work, functional, discomfort, sleep, coping, anxiety, digits=1, ignore=TRUE) {
  ##If one value missing impute using mean.
  pain <- suppressWarnings(as.numeric(pain))
  fatigue <- suppressWarnings(as.numeric(fatigue))
  skin <- suppressWarnings(as.numeric(skin))
  work <- suppressWarnings(as.numeric(work))
  functional <- suppressWarnings(as.numeric(functional))
  discomfort <- suppressWarnings(as.numeric(discomfort))
  sleep <- suppressWarnings(as.numeric(sleep))
  coping <- suppressWarnings(as.numeric(coping))
  anxiety <- suppressWarnings(as.numeric(anxiety))

  if(!.equal_lengths(pain, fatigue, skin, work, functional, discomfort, sleep, coping, anxiety)) {
    stop("pain, fatigue, skin, work, functional, discomfort, sleep, coping and anxiety are not the same length.")
  }

  if(any(is.na(pain) | ! pain %in% 0:10)) {
    if(ignore) {
      pain[ which(!is.na(pain) & !pain %in% 0:10) ] <- NA
    } else {
      stop("Pain item must be between 0 and 10.")
    }
  }

  if(any(is.na(fatigue) | ! fatigue %in% 0:10)) {
    if(ignore) {
      fatigue[ which(!is.na(fatigue) & !fatigue %in% 0:10) ] <- NA
    } else {
      stop("Fatigue item must be between 0 and 10.")
    }
  }

  if(any(is.na(skin) | ! skin %in% 0:10)) {
    if(ignore) {
      skin[ which(!is.na(skin) & !skin %in% 0:10) ] <- NA
    } else {
      stop("Skin item must be between 0 and 10.")
    }
  }

  if(any(is.na(work) | ! work %in% 0:10)) {
    if(ignore) {
      work[ which(!is.na(work) & !work %in% 0:10) ] <- NA
    } else {
      stop("Work item must be between 0 and 10.")
    }
  }

  if(any(is.na(functional) | ! functional %in% 0:10)) {
    if(ignore) {
      functional[ which(!is.na(functional) & !functional %in% 0:10) ] <- NA
    } else {
      stop("Functional item must be between 0 and 10.")
    }
  }

  if(any(is.na(discomfort) | ! discomfort %in% 0:10)) {
    if(ignore) {
      discomfort[ which(!is.na(discomfort) & !discomfort %in% 0:10) ] <- NA
    } else {
      stop("Discomfort item must be between 0 and 10.")
    }
  }

  if(any(is.na(sleep) | ! sleep %in% 0:10)) {
    if(ignore) {
      sleep[ which(!is.na(sleep) & !sleep %in% 0:10) ] <- NA
    } else {
      stop("Sleep item must be between 0 and 10.")
    }
  }

  if(any(is.na(coping) | ! coping %in% 0:10)) {
    if(ignore) {
      coping[ which(!is.na(coping) & !coping %in% 0:10) ] <- NA
    } else {
      stop("Coping item must be between 0 and 10.")
    }
  }

  if(any(is.na(anxiety) | ! anxiety %in% 0:10)) {
    if(ignore) {
      anxiety[ which(!is.na(anxiety) & !anxiety %in% 0:10) ] <- NA
    } else {
      stop("Anxiety item must be between 0 and 10.")
    }
  }

  score <- pain * 0.174 + fatigue * 0.131 + skin * 0.121 +
    work  * 0.110 + functional * 0.107 + discomfort * 0.098 +
    sleep * 0.089 + coping * 0.087 + anxiety * 0.085

  return(round(score, digits))
}
