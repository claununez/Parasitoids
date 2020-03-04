#' Calculate body mass from wing length in Ichneumonidae insects
#'
#' @description ichneumonidae_wwl helps to estimate the body mass of insects 
#' of the Ichneumonidae family considering the length of the forewing.
#'
#' @param wing_length (numeric) Vector of wing length measurements.
#' @param units_wing_length (character) Units of the wing length measurements. 
#' Options are millimeters = "mm", centimeters = "cm", or inches = "in".
#' @param sex_or_strategy (character) Sex or parasitism strategy of the specimens.
#' Options are: unknown = "unknown", males = "m", females = "f", 
#' idiobiont = "idi", or koinobiont = "koi". Default = "unknown"; see details.
#' 
#'
#' @return
#' A vector of weight values in milligrams (mg).
#'
#' @details 
#' Citation:
#' 
#' To cite the use of this function in scientific papers use:
#' 
#' Mazón, M., C. Nuñez-Penichet, M.E. Cobos (in review). Relationship between 
#' body mass and forewing length in Neotropical Ichneumonidae (Insecta: 
#' Hymenoptera). Neotropical Entomology.
#' 
#' Calculations are made based on the equations described below. Argument 
#' \code{sex_or_strategy} allows definitions of sex or parasitism strategy, but 
#' not combinations of both categories. If "unknown" (the default) is defined,
#' a formula obtaine for data combining information of a large list of specimens
#' is used.
#' 
#' Equations used:
#' 
#' unknown:  y = exp(-2.40998 + (1.89329 * log(wing_length)))
#' females: y = exp(-2.35497 + (1.87731 * log(wing_length))
#' males:  y = exp(-2.48655 + (1.91227 * log(wing_length)))
#' idiobiont: y = exp(-2.20067 + (1.81526 * log(wing_length))
#' koinobiont:  y = exp(-2.41816 + (1.87424 * log(wing_length)))

ichneumonidae_wwl <- function(wing_length, units_wing_length, 
                              sex_or_strategy = "unknown") {
  
  if (missing(wing_length) ) {
    stop("Argument 'wing_length' must be defined.")
  } 
  if (missing(units_wing_length)) {
    stop("Argument 'units_wing_length' must be defined.")
  }
  if (units_wing_length[1] == "in") {
    wing_length <- wing_length * 25.4
  } 
  if (units_wing_length[1] == "cm") {
    wing_length <- wing_length * 10
  }
  if (!units_wing_length[1] %in% c("cm", "mm", "inch") ) {
    stop("Units for wing length must be either mm, cm, or in.")
  }
  
  if (sex_or_strategy %in% c("unknown", "m", "f", "idi", "koi")) {
    wing_length <- log(wing_length)
    if (sex_or_strategy == "unknown") {y <- exp(-2.40998 + (1.89329 * wing_length))}
    if (sex_or_strategy == "m") {y <- exp(-2.48655 + (1.91227 * wing_length))}
    if (sex_or_strategy == "f") {y <- exp(-2.35497 + (1.87731 * wing_length))}
    if (sex_or_strategy == "idi") {y <- exp(-2.20067 + (1.81526 * wing_length))}
    if (sex_or_strategy == "koi") {y <- exp(-2.41816 + (1.87424 * wing_length))}
  } else {
    stop("Argument 'sex_or_strategy' is not valid, see function's help.")
  } 
  
  return(y)
}