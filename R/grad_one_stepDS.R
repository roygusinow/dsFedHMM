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

  # server-side utility (put in your package)
  sanitize_df_for_julia <- function(df) {
    # Make sure it's a base data.frame, not a tibble/data.table, etc.
    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Keep only essential top-level attributes (names/row.names/class)
    attributes(df) <- list(
      names = names(df),
      row.names = .set_row_names(NROW(df)),
      class = "data.frame"
    )

    # Scrub each column: drop all attributes; normalize common classes
    df[] <- lapply(df, function(col) {
      # drop all attributes on the column (this removes odd pointers)
      attributes(col) <- NULL

      # normalize classes that often carry metadata
      if (is.factor(col))         return(as.character(col))
      if (inherits(col, "Date"))  return(as.numeric(col))              # days
      if (inherits(col, "POSIXt"))return(as.numeric(as.POSIXct(col)))  # seconds
      if (is.list(col))           stop("List columns are not supported for Julia transfer.")
      if (typeof(col) == "externalptr")
        stop("Column has type externalptr after sanitize; cannot serialize.")
      col
    })
    df
  }

  data <- eval(parse(text=data), envir = parent.frame())
  data <- sanitize_df_for_julia(data)

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

  # return("Done")

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
