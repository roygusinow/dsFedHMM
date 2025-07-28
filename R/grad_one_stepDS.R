#' aggregate function
#'
#' @param current_parameters a vector of current parameters
#' @param aggr_grad a vector of aggregated gradients
#' @param data
#' @param model_params a list of model parameters including:
#'
#' @returns a list containing:
#' @export
grad_one_stepDS <- function(
    data,
    current_parameters,
    aggr_grad,

    # unloaded model params
    covs,
    bins,
    conts,
    visits,
    labels,
    n_states,
    k_tup_initial,
    k_tup_trans,
    k_tup_em,
    sim_no = "fed_test",
    comments = ""
    ){
  # at the server now. We should interface to julia

  # k_tup <- list(
  #   initial = c('x3', 'x8'),
  #   trans   = c('x3', 'x8'),
  #   em      = list('x1')
  # )
  # model_params <- c(
  #   covs = c('x3', 'x8'),
  #   bins  = c('bin1', 'bin2','bin3', 'bin4'),
  #   conts = c('cont1', 'cont2'),
  #   visits = c('v1','v2','v3','v4'),
  #   labels = c('acute', '6-9months', '10-15months', '18-20months', '21-24months'), # spaces cause issues
  #
  #   n_states = 3,
  #   k_tup = k_tup,
  #   sim_no = 'fed_test',
  #   comments = ''
  # )

  data <- eval(parse(text=data), envir = parent.frame())

  labels = c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months') # temp fix to formatting

  k_tup <- list(
    initial = k_tup_initial,
    trans   = k_tup_trans,
    em      = k_tup_em
  )
  attr(k_tup, 'JLTYPE') <-
    'NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}'

  # connect to Julia and initialise the model
  Fed_HMM <- juliaImport("Fed_HMM")

  LTS_output <- Fed_HMM$create_sim_mod_data(

    data,

    # data params
    covs   = covs,
    bins   = bins,
    conts  = conts,
    visits = visits,
    labels = labels,

    # model params
    n_states = n_states,
    k_tup = k_tup,
    sim_no = "fed_test",
    comments = ""

  )

  output <- Fed_HMM$grad_one_step(
    current_parameters,
    aggr_grad,
    LTS_output
  )

  out_list <- juliaGet(output)

  return(list(
    next_grad = out_list[[1]],
    likelihood = out_list[[2]]
  ))
}
