#' Title
#'
#' @param object
#' @param option
#'
#' @returns
#' @export
julia_output_testDS <- function(val1, val2){

  library(JuliaConnectoR)

  out <- juliaEval(paste0(val1, " + ", val2))

  return(out)
}
