#' aggregate function
#'
#' @param current_parameters a vector of current parameters
#' @param aggr_grad a vector of aggregated gradients
#' @param data
#'
#' @returns a list containing:
#' @export
test_only_model_paramsDS <- function(
    data,
    current_parameters,
    aggr_grad
){
  # at the server now. We should interface to julia

  library(JuliaConnectoR)

  data <- eval(parse(text=data), envir = parent.frame())

  current_parameters <- as.numeric(unlist(strsplit(current_parameters, split=",")))
  aggr_grad <- as.numeric(unlist(strsplit(aggr_grad, split=",")))

  k_tup <- list(
    initial = c("x3", "x8"),
    trans   = c("x3", "x8"),
    em      = list("x1")
  )
  attr(k_tup, 'JLTYPE') <- "NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}"
  model_params <- list(
    covs = c("x3", "x8"),
    bins  = c("bin1", "bin2","bin3", "bin4"),
    conts = c("cont1", "cont2"),
    visits = c("v1","v2","v3","v4"),
    labels = c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months'),

    n_states = 3,
    k_tup = k_tup,
    sim_no = "fed_test",
    comments = "fed_test"
  )

  # connect to Julia and initialise the model
  Fed_HMM <- JuliaConnectoR::juliaImport("Fed_HMM")

  # LTS_output <- Fed_HMM$create_sim_mod_data(
  #
  #   data,
  #
  #   covs   = model_params$covs,
  #   bins   = model_params$bins,
  #   conts  = model_params$conts,
  #   visits = model_params$visits,
  #   labels = model_params$labels,
  #   n_states = model_params$n_states,
  #   k_tup = k_tup,
  #   sim_no = model_params$sim_no,
  #   comments = model_params$comments
  # )

  # return(typeof(data))

  LTS_output <- Fed_HMM$create_sim_mod_data_no_args(data)

  return("finshed lts_out")

  next_grad <- Fed_HMM$grad_one_step(
    current_parameters,
    aggr_grad,
    LTS_output
  )

  likelihood <- Fed_HMM$get_lkl_val(
    current_parameters,
    LTS_output
  )

  return(list(
    next_grad = next_grad,
    likelihood = likelihood
  ))
}
