#' Title
#'
#' @param object
#' @param option
#'
#' @returns
#' @export
julia_interface_testDS <- function(object, option){

  library(JuliaConnectoR)
  LTA <- juliaImport("LTA")
  # out <- Fed_HMM$test_mod2()
  out <- LTA$gen_beta_initial(seed = as.integer(1),
                              n_states = as.integer(3), k = as.integer(5),
                              ref_state = as.integer(1))
  return(out)
}
