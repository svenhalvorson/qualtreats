#' Check if survey_id matches the pattern supplied by Qualtrics
#' @param survey_id string
#' @keywords internal

valid_survey_id = function(survey_id){
  # This is the pattern supplied by the Qualtrics API
  stringr::str_detect(survey_id, '^SV_[a-zA-Z0-9]{11,15}$')
}

#' Check if a qualtrics API key has been set.
#' @keywords internal
#' @param api_key Qualtrics API key
valid_api_key = function(api_key){
  api_key != ''
}

#' Check if a qualtrics base url is set
#' @keywords internal
#' @param base_url the prefix of your Qualtrics url between "https://" and ".qualtrics.com"
valid_base_url = function(base_url){
  stringr::str_detect(base_url, '.+\\.qualtrics\\.com/?$')
}

#' Check if a directory can be written to:
#' @keywords internal
#' @param out_dir a string to a directory
valid_out_dir = function(out_dir){
  if(length(out_dir) > 1){
    stop('out_dir must be a single path')
  }
  is.null(out_dir) || dir.exists(out_dir)
}


#' Pad to two digits
#' @param string a string
#' @export
#' @description Pad strings to two digits
#' @details convenience wrapper for \code{stringr::str_pad}
pad2 = function(string){
  stringr::str_pad(
    string = string,
    width = 2,
    side = 'left',
    pad = '0'
  )
}

#' Format Descriptions
#' @param string a string
#' @export
#' @description Remove some HTML nonsense from a string
format_description = function(string){

  string |>
    stringr::str_replace_all(
      pattern = '<[^>]*>',
      replacement = ' '
    ) |>
    stringr::str_replace_all(
      pattern = stringr::fixed('&nbsp;'),
      replacement = ' '
    ) |>
    stringr::str_replace_all(
      pattern = '(\\t|\\n)+',
      replacement = ' '
    ) |>
    stringr::str_replace_all(
      pattern = ' +',
      replacement = ' '
    ) |>
    trimws()

}
