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

  process_mod_param <- function(param, inside=FALSE) {
    if (inside) {
      out <- as.list(strsplit(param, split=",")[[1]])
    }else{
      out <- as.list(strsplit(param, split=","))[[1]]
    }

    if (length(out) == 0) {
      return(list())
    } else {
      return(out)
    }
  }

  data <- eval(parse(text=data), envir = parent.frame())
  data <- sanitize_df_for_julia(data)

  current_parameters <- as.numeric(unlist(strsplit(current_parameters, split=",")))
  aggr_grad <- as.numeric(unlist(strsplit(aggr_grad, split=",")))

  covs <- process_mod_param(covs)
  bins <- process_mod_param(bins)
  conts <- process_mod_param(conts)
  visits <- process_mod_param(visits)
  labels <- process_mod_param(labels)
  n_states <- as.numeric(unlist(strsplit(n_states, split=",")))
  covariate_tup_initial <- process_mod_param(covariate_tup_initial, inside = TRUE)
  covariate_tup_trans <- process_mod_param(covariate_tup_trans, inside = TRUE)
  covariate_tup_em <- process_mod_param(covariate_tup_em)
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
