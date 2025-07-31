#' Title
#'
#' @param object
#' @param option
#'
#' @returns
#' @export
params_from_probs_testDS <- function(prob_vec){

  library(JuliaConnectoR)

  prob_vec <- as.numeric(unlist(strsplit(prob_vec, split=",")))

  Fed_HMM <- juliaImport("Fed_HMM")
  out <- Fed_HMM$params_from_probs(prob_vec)


  return(out)
}
