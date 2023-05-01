#' Get Information on a Qualtrics Survey
#' @description Retrieve a JSON object that has information on a given survey.
#' There are currently three different api calls that can be specified by
#' the \code{type} argument:
#' \itemize{
#'   \item \code{survey} gives the structure of the survey in a similar
#'   \emph{but not identical} format as downloading the metadata
#'   \item \code{definitions} contains information about the survey's body such as the flow, blocks, questions ect.
#'   \item \code{metadata} holds information about survey settings such as the name and status
#' }
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param type specify which API call to perform
#' @return a list containing the results of the API call
#' @export
#' @author Sven Halvorson (svenpubmail@gmail.com)
#' @seealso API call references:
#' \itemize{
#'   \item \href{https://api.qualtrics.com/73d7e07ec68b2-get-survey}{survey}
#'   \item \href{https://api.qualtrics.com/9d0928392673d-get-survey}{definitions}
#'   \item \href{https://api.qualtrics.com/a92bfbc7e58fe-get-survey-metadata}{metadata}
#' }
get_survey = function(
  survey_id,
  type = c('survey', 'definitions', 'metadata')
){

  # Argument checks:
  stopifnot(
    valid_survey_id(survey_id),
    valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
    valid_base_url(Sys.getenv('QUALTRICS_BASE_URL'))
  )
  type = rlang::arg_match(type)

  # So we can use some of their functions here to emulate
  # the python code I already wrote:
  call_url = 'https://{base_url}/API/v3/'
  call_suffix = switch(
    type,
    survey = 'surveys/{survey_id}',
    definitions = 'survey-definitions/{survey_id}',
    metadata = 'survey-definitions/{survey_id}/metadata'
  )

  call_url = call_url |>
    paste0(call_suffix) |>
    stringr::str_glue(
      base_url = Sys.getenv('QUALTRICS_BASE_URL'),
      survey_id = survey_id
    )

  # Headers:
  headers = c(
    "X-API-TOKEN" = Sys.getenv('QUALTRICS_API_KEY')
  )

  result = httr::VERB(
    verb = 'GET',
    url = call_url,
    httr::add_headers(headers)
  ) |>
    httr::content() |>
    purrr::pluck('result')

  invisible(result)

}
