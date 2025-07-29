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

  Fed_HMM <- juliaImport("Fed_HMM")
  if (is.null(Fed_HMM)) {
    stop("Failed to import Fed_HMM module from Julia.")
  }

  julia_out <- juliaEval('1+2')

  if (julia_out != 3) {
    stop("Julia evaluation failed: expected 3, got ", julia_out)
  }


  return("Imported Fed_HMM and evaluated 1+2 successfully in Julia.")
}
