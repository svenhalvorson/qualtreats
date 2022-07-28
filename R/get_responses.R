#' Get Qualtrics Responses
#' @param survey_id string
#' @param out_dir a string path to a directory for output
#' @param file_format one of \code{c('spss', 'csv', 'tsv')}
#' @param useLabels Should value labels be downloaded instead of recodes?
#' @return a \code{tibble} of the responses
#' @export
#' @author Sven Halvorson (svenpubmail@gmail.com)
# TODO: allow the api key as a parameter, allow additional arguments to the payloadS

get_responses = function(
  survey_id,
  file_format = c('spss', 'csv', 'tsv'),
  useLabels = FALSE,
  out_dir = NULL
){

  # Argument checks:
  stopifnot(
    valid_survey_id(survey_id),
    valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
    valid_base_url(Sys.getenv('QUALTRICS_BASE_URL')),
    valid_out_dir(out_dir)
  )

  file_format = rlang::arg_match(file_format)

  # Survey url:
  download_url = stringr::str_glue(
    'https://{base_url}/API/v3/surveys/{survey_id}/export-responses/',
    base_url = Sys.getenv("QUALTRICS_BASE_URL"),
    survey_id = survey_id
  )

  # Headers
  headers = headers = c(
    "X-API-TOKEN" = Sys.getenv('QUALTRICS_API_KEY'),
    "Content-Type" = "application/json",
    "Accept" = "*/*",
    "accept-encoding" = "gzip, deflate"
  )

  # Payload. (May consider expanding some of these options later)
  payload = list(
      'useLabels' = useLabels,
      'format'  = file_format,
      'includeLabelColumns' = FALSE,
      'timeZone' = 'America/Los_Angeles'
    ) %>%
    purrr::map(
      function(x){if(length(x) == 1) jsonlite::unbox(x) else x}
    ) %>%
    jsonlite::toJSON(auto_unbox = FALSE)

  # Next we make the request from qualtrics:
  result = httr::VERB(
    verb = 'POST',
    url = download_url,
    httr::add_headers(headers),
    body = payload
  ) %>%
    httr::content()

  request_id = result[['result']][['progressId']]

  # The qualtRics library has a check here that I'm not sure
  # what purpose it serves other than being used for a download
  # progress bar. It is necessary though
  check_url = paste0(
    download_url,
    request_id
  )

  # now we have to wait for this to complete. I previously omitted
  # this portion since I thought it was just for the visual but
  # if you try to procede before the server processes the request
  # it will not be able to download correctly
  cat(paste0('\nDownloading survey : ', survey_id, '\n\n'))
  progress <- 0
  while (progress < 100) {
    # Get percentage complete
    CU <- httr::VERB(
      'GET',
      url = check_url,
      httr::add_headers(headers),
      body = NULL
    ) %>%
      httr::content()

    # Set progress
    progress <- CU$result$percentComplete

  }

  # Now we make a new url for the file using this request ID
  file_url = paste0(
    download_url,
    CU$result$fileId,
    '/file'
  )

  # Download file:
  file_response = httr::GET(
    file_url,
    httr::add_headers(headers)
  )

  file_content =  httr::content(file_response, raw = TRUE)

  # Then we write the file_content to a temporary directory and extract it
  temp_dir = tempdir()
  temp_file = paste0(temp_dir, '/temp.zip')

  writeBin(
    object = file_content,
    con = temp_file
  )


  # Then we unzip it to our location
  responses_file = utils::unzip(
    zipfile = temp_file,
    exdir = ifelse(is.null(out_dir), temp_dir, out_dir)
  )


  # Now read it in:
  read_fun = switch(
    file_format,
    csv = function(x){readr::read_csv(x, show_col_types = FALSE)},
    tsv = function(x){utils::read.delim2(file = x, fileEncoding = 'UTF-16')},
    spss = haven::read_sav
  )

  responses = read_fun(responses_file)

  if(file_format != 'spss'){

    # apply value labels
    for(i in seq_along(responses)){
      expss::var_lab(responses[[i]]) = responses[1,i]
    }

    responses = dplyr::slice(responses, 3:dplyr::n())

  }

  invisible(responses)

}
