#' Apply variable names
#' @description Dictionary style application of variable names
#' using \code{haven::labelled}
#' @param df A data frame
#' @param column_names character vector of column names in \code{df}
#' @param var_labs  corresponding character vector of variable labels
#'
#' @return a data frame
#' @export
var_lab_dict = function(
    df,
    column_names,
    var_labs
){

  stopifnot(
    all(
      is.data.frame(df),
      is.character(column_names) & is.vector(column_names),
      is.character(var_labs) & is.vector(var_labs)
    )
  )

  if(
    !all(column_names %in% colnames(df))
  ){
    warning('Some values in column names not found in colnames(df)')
  }

  for(i in seq_along(column_names)){
    if(column_names[i] %in% colnames(df)){
      df[[column_names[i]]] = haven::labelled(
        x = df[[column_names[i]]],
        label = var_labs[i]
      )
    }
  }

  df

}
