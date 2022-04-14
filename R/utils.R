#' Check if survey_id matches the pattern supplied by Qualtrics
#' @param survey_id string
#' @keywords internal

valid_survey_id = function(survey_id){
  # This is the pattern supplied by the Qualtrics API
  stringr::str_detect(survey_id, '^SV_[a-zA-Z0-9]{11,15}$')
}

#' Check if a qualtrics API key has been set.
#' @keywords internal
valid_api_key = function(api_key){
  api_key != ''
}

#' Check if a qualtrics base url is set
valid_base_url = function(base_url){
  stringr::str_detect(base_url, '.+\\.qualtrics\\.com/?$')
}

#' Check if a directory can be written to:
#' @keywords internal
valid_out_dir = function(out_dir){
  if(length(out_dir) > 1){
    stop('out_dir must be a single path')
  }
  is.null(out_dir) || dir.exists(out_dir)
}


