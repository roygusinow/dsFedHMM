#' Title
#'
#' @param object
#' @param option
#'
#' @returns
#' @export
julia_interface_testDS <- function(object, option){

  library(JuliaConnectoR)

  print(juliaSetupOk())

  julia_out <- juliaEval('1+2')

  print("Working!")

  return(julia_out)
}
