#' aggregate function
#'
#' @param current_parameters a vector of current parameters
#' @param aggr_grad a vector of aggregated gradients
#' @param data
#'
#' @returns a list containing:
#' @export
grad_one_stepDS <- function(
    data,
    current_parameters,
    aggr_grad,

    # unloaded model params
    # model_params,
    covs,
    bins,
    conts,
    visits,
    labels,
    n_states,
    k_tup_initial,
    k_tup_trans,
    k_tup_em,
    sim_no,
    comments
    ){
  # at the server now. We should interface to julia

  data <- eval(parse(text=data), envir = parent.frame())

  current_parameters <- as.numeric(unlist(strsplit(current_parameters, split=",")))
  aggr_grad <- as.numeric(unlist(strsplit(aggr_grad, split=",")))

  # print("check")
  covs <- as.character(unlist(strsplit(covs, split=",")))
  bins <- as.character(unlist(strsplit(bins, split=",")))
  conts <- as.character(unlist(strsplit(conts, split=",")))
  visits <- as.character(unlist(strsplit(visits, split=",")))
  # labels <- as.character(unlist(strsplit(labels, split=",")))
  labels <- c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months') # temp fix to formatting
  n_states <- as.numeric(unlist(strsplit(n_states, split=",")))
  k_tup_initial <- as.character(unlist(strsplit(k_tup_initial, split=",")))
  k_tup_trans <- as.character(unlist(strsplit(k_tup_trans, split=",")))
  k_tup_em <- list(unlist(strsplit(k_tup_em, split=",")))
  sim_no <- as.character(unlist(strsplit(sim_no, split=",")))
  comments <- as.character(unlist(strsplit(comments, split=",")))


  k_tup <- list(
    initial = k_tup_initial,
    trans   = k_tup_trans,
    em      = k_tup_em
  )
  attr(k_tup, 'JLTYPE') <-
    "NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}"
  # attr(model_params$k_tup, 'JLTYPE') <-
  #   "NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}"
  #
  # print(k_tup)
  # print(model_params$k_tup)
  # print(identical(k_tup, model_params$k_tup))

  # connect to Julia and initialise the model
  Fed_HMM <- juliaImport("Fed_HMM")

  LTS_output <- Fed_HMM$create_sim_mod_data(

    data,

    # data params
    covs = covs,
    bins   = bins,
    conts  = conts,
    visits = visits,
    labels = labels,

    # # model params
    n_states = n_states,
    k_tup = k_tup,
    sim_no = sim_no,
    comments = comments

    # covs   = model_params$covs,
    # bins   = model_params$bins,
    # conts  = model_params$conts,
    # visits = model_params$visits,
    # labels = c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months'), # temp fix to formatting
    # n_states = model_params$n_states,
    # k_tup = model_params$k_tup
    # sim_no = model_params$sim_no,
    # comments = model_params$comments
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
