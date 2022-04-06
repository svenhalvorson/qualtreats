#' Check if survey_id matches the pattern supplied by Qualtrics
#' @param survey_id string
#' @keywords internal

valid_survey_id = function(survey_id){
  # This is the pattern supplied by the Qualtrics API
  stringr::str_detect(survey_id, '^SV_[a-zA-Z0-9]{11,15}$')
}
