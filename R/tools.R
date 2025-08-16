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
