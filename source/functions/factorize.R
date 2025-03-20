factorize <- function(
    dataframe,
    varnames,
    varlevels
) {
  for (i in seq_along(varnames)) {
    if (varnames[i] %in% colnames(dataframe)) {
      dataframe  <- dataframe  |>
        dplyr::mutate(
          !!varnames[i] := factor(get(varnames[i]), levels = varlevels[[i]])
        )
    }
  }
  return(dataframe)
}
