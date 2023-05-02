#' Apply variable labels
#' @description Dictionary style application of variable labels. Sets the "label"
#' \code{attribute} of eacj \code{column_names} within \code{df} to the corresponding
#' values within \code{var_labs}.
#' @param df A data frame
#' @param column_names Character vector of column names in \code{df}
#' @param var_labs Corresponding character vector of variable labels
#' @param match_all Do all of \code{column_names} need to be in the columns of \code{df}?
#' @return \code{df} with modified attributes
#' @export
var_lab_dict = function(
    df,
    column_names,
    var_labs,
    match_all = TRUE
){

  stopifnot(
    all(
      is.data.frame(df),
      is.character(column_names) & is.vector(column_names),
      is.character(var_labs) & is.vector(var_labs),
      length(column_names) == length(var_labs)
    )
  )

  # Might be important sometimes that we assert that everything in the
  # dictionary matches a column name in df
  if(match_all){
    if(
      !all(column_names %in% colnames(df))
    ){
      stop('Some values in column_names not found in columns of df')
    }
  }

  for(i in seq_along(column_names)){
    if(column_names[i] %in% colnames(df)){
      attr(df[[column_names[i]]], 'label') = var_labs[i]
    }
  }

  df

}
