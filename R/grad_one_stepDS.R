#' aggregate function
#'
#' @param current_parameters a vector of current parameters
#' @param aggr_grad a vector of aggregated gradients
#' @param data
#' @param model_params a list of model parameters including:
#'
#' @returns a list containing:
#' @export
grad_one_stepDS <- function(data, current_parameters, aggr_grad, model_params){
  # at the server now. We should interface to julia


  data <- eval(parse(text=data), envir = parent.frame())
  # print(data)

  # print("current_parameters")
  # print(current_parameters)
  #
  # print("model_params")
  # print(model_params)
  #
  # print("k_tup")
  # print(k_tup)

  model_params$labels = c('acute', '6-9 months', '10-15 months', '18-20 months', '21-24 months') # temp fix to formatting
  attr(model_params$k_tup, 'JLTYPE') <-
    'NamedTuple{(:initial,:trans,:em),Tuple{Vector{String},Vector{String},Vector{String}}}'
  # print("finsihed model_params")
  # print(model_params)

  # connect to Julia and initialise the model
  Fed_HMM <- juliaImport("Fed_HMM")

  LTS_output <- Fed_HMM$create_sim_mod_data(

    # df = df, # change from name to data in the server. Will change in DS implementation
    data,

    # data params
    covs   = model_params$covs,
    bins   = model_params$bins,
    conts  = model_params$conts,
    visits = model_params$visits,
    labels = model_params$labels,

    # model params
    n_states = model_params$n_states,
    k_tup = model_params$k_tup,
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
