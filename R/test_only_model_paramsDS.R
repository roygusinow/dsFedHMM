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

  library(JuliaConnectoR)

  data <- eval(parse(text=data), envir = parent.frame())
  data <- sanitize_df_for_julia(data)

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
