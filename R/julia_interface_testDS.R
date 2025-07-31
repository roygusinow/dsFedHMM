#' Title
#'
#' @param object
#' @param option
#'
#' @returns
#' @export
julia_interface_testDS <- function(object, option){

  library(JuliaConnectoR)

  Fed_HMM <- juliaImport("Fed_HMM")
  out <- Fed_HMM$test_mod2()

  return(out)
}
