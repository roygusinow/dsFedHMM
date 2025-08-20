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
    covariate_tup_initial,
    covariate_tup_trans,
    covariate_tup_em,
    sim_no,
    comments
    ){
  # at the server now. We should interface to julia

  data <- eval(parse(text=data), envir = parent.frame())
  data <- sanitize_df_for_julia(data)

  current_parameters <- as.numeric(unlist(strsplit(current_parameters, split=",")))
  aggr_grad <- as.numeric(unlist(strsplit(aggr_grad, split=",")))

  # print("check")
  covs <- as.character(unlist(strsplit(covs, split=",")))
  bins <- as.character(unlist(strsplit(bins, split=",")))
  conts <- as.character(unlist(strsplit(conts, split=",")))
  visits <- as.character(unlist(strsplit(visits, split=",")))
  labels <- as.character(unlist(strsplit(labels, split=",")))
  # labels <- c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months') # temp fix to formatting
  # labels <- c("acute", "6-9_months", "10-15_months", "18-20_months", "21-24_months")
  n_states <- as.numeric(unlist(strsplit(n_states, split=",")))
  covariate_tup_initial <- as.list(strsplit(covariate_tup_initial, split=",")[[1]])
  covariate_tup_trans <- as.list(strsplit(covariate_tup_trans, split=",")[[1]])
  covariate_tup_em <- list()
  sim_no <- as.character(unlist(strsplit(sim_no, split=",")))
  comments <- as.character(unlist(strsplit(comments, split=",")))

  covariate_tup <- list(
    initial = covariate_tup_initial,
    trans   = covariate_tup_trans,
    em      = covariate_tup_em
  )

  # return(labels)
  attr(covariate_tup, "JLTYPE") <- "NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}"

  # connect to Julia and initialise the model
  LTA <- juliaImport("LTA")

  LTS_output <- LTA$create_sim_mod_data(

    data,

    # data params
    covs = covs,
    bins   = bins,
    conts  = conts,
    visits = visits,
    labels = labels,

    # # model params
    n_states = n_states,
    covariate_tup = covariate_tup,
    sim_no = sim_no,
    comments = comments
  )

  next_grad <- LTA$grad_one_step(
    current_parameters,
    aggr_grad,
    LTS_output
  )

  likelihood <- LTA$get_lkl_val(
    current_parameters,
    LTS_output
  )

  return(list(
    next_grad = next_grad,
    likelihood = likelihood
  ))
}
