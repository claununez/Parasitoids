#' Calculate weight from wing length in Ichneumonidae insects
#'
#' @description ichneumonidae_wwl helps to estimate the weight of the insects 
#' of Ichneumonidae family considering the length of the forewing.
#'
#' @param wing_length (numeric) Vector of wing length's measurements.
#' @param units_wing_length (character) Units of the wing length measurements. Options are 
#' millimeters = "mm", centimeters = "cm", or inches = "inch".
#' @param sex (character) Sex of the individuals. Options are unknown = "unknown", 
#' males = "m", or females = "f".
#' 
#'
#' @return
#' A vector of weight vales in milligrams (mg).
#'
#' @details 
#' Equations used are as follow
#' unknown sex:  y = exp(-2.39750 + (1.88688 * log(wing_length)))
#' females: y = exp(-2.34714 + (1.87114 * log(wing_length)))
#' males:  y = exp(-2.41326 + (1.85678 * log(wing_length)))

ichneumonidae_wwl <- function(wing_length, units_wing_length, sex = "unknown") {
  
  if (missing(wing_length) ) {
    stop("Argument wing_length must be defined.")
  } 
  if (missing(units_wing_length)) {
    stop("Argument units_wing_length must be defined.")
  }
  if (units_wing_length[1] == "inch") {
    wing_length <- wing_length[1] * 25.4
  } 
  if (units_wing_length[1] == "cm") {
    wing_length <- wing_length[1] * 10
  }
  if (!units_wing_length[1] %in% c("cm", "mm", "inch") ) {
    stop("Units for wing length must be either mm, cm, or inch.")
  }
  
  if (sex %in% c("unknown", "m", "f")) {
    wing_length <- log(wing_length)
    if (sex == "unknown") {y <- exp(-2.39750 + (1.88688 * wing_length))}
    if (sex == "m") {y <- exp(-2.41326 + (1.85678 * wing_length))}
    if (sex == "f") {y <- exp(-2.34714 + (1.87114 * wing_length))}
  } else {
    stop("Argument sex is not valid, see function's help.")
  } 
  
  return(y)
}