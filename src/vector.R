# Vector

# Statistics ----------------

#' Root Mean Square Error
#'
#' @param .sim simulation
#' @param .obs observation
#'
#' @return atomic numeric
#' @export
#'
#' @examples
#' 
rmse <- function(.sim, .obs) {
  sqrt(mean( (.sim - .obs)^2 ))
}