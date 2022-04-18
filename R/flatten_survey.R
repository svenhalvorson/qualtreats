#' Flatten a Qualtrics Survey
#' @description Decompose the blocks, questions, and choices of a
#' Qualtrics survey into rectangular data sets.
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param out_dir path to directory where files can be written
#' @param file_prefix string prefix to file names
#' @param file_format one of \code{c('tsv', 'csv')}
#' @return a named list of the four tables generated
#' @export
#' @author Sven Halvorson (svenpubmail@gmail.com)

flatten_survey = function(
  survey_id,
  out_dir = NULL,
  file_prefix = '',
  file_format = c('csv', 'tsv')
){

  stopifnot(
    all(
      valid_survey_id(survey_id),
      valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
      valid_base_url(Sys.getenv('QUALTRICS_BASE_URL')),
      is.null(out_dir) || dir.exists(out_dir),
      is.character(file_prefix) & length(file_prefix) == 1,
      is.null(out_dir) || (is.character(file_prefix) & length(file_prefix) == 1)
    )
  )

  type = rlang::arg_match(type)

  # Get the survey information to be used by all other functions:
  survey = get_survey(
    survey_id = survey_id,
    type = 'survey'
  )

  # Construct the file paths for the output
  tables = c(
    'blocks',
    'block_question',
    'questions',
    'choices'
  )

  results = list(
    flatten_blocks(survey),
    flatten_question_block(survey),
    flatten_questions(survey),
    flatten_choices(survey)
  )

  names(results) = tables

  # If we want to write:
  if(!is.null(out_dir)){

    # Write the results based off their chosen file format, directory, and prefix
    write_fun = ifelse(
      test = file_format == 'tsv',
      readr::write_tsv,
      readr::write_csv
    )

    if(!stringr::str_detect(out_dir, '/$')){
      out_dir = paste0(out_dir, '/')
    }

    file_names = paste0(
      file_prefix,
      ifelse(
        file_prefix == '',
        '',
        '_'
      ),
      tables,
      '.',
      file_format
    )


    file_paths = paste0(
      out_dir
    )

    purrr::walk2(
      .x = results,
      .y = file_paths,
      .f = write_fun
    )

  }

  invisible(results)

}


# Block flattener:
flatten_blocks = function(
  survey
){

  # Get all the survey blocks and their descriptions
  blocks = survey[['blocks']]

  # Make the table
  block_df = tibble::tibble(
    block_order = 1:length(blocks),
    block_id = names(blocks),
    block_description = purrr::map_chr(
      .x = blocks,
      .f = subset_safely,
      name = 'description'
    )
  )


  # Add a flag for loop and merge:
  if('loopAndMerge' %in% names(survey)){
    block_df[['loop_and_merge']] = as.numeric(
      block_df[['block_id']] %in% names(survey[['loopAndMerge']])
    )
  } else{
    block_df[['loop_and_merge']] = 0
  }

  block_df
}



# flatten_choices = function(
#   survey
# ){
#
#   # Use this to guard against cases where piece of the JSON object
#   # is missing:
#   replace_null = function(x){
#     ifelse(is.null(x), '', x)
#   }
#
#   # Preserve the order that the QIDs appear in for ease of viewing:
#   question_order = names(survey[['questions']])
#
#   # Extracting the choices is only valid for a few qustions types. We will treat
#   # the side by side (SBS) type differently than multiple choice, matrix, & forms
#   # since it's kind of like multiple of those put together. Partition the questions:
#
#   questions_sbs = purrr::keep(
#     .x = survey[['questions']],
#     .p = function(x){
#       x[['questionType']][['type']] == 'SBS'
#     }
#   )
#
#   questions = purrr::keep(
#     .x = survey[['questions']],
#     .p = function(x){
#       x[['questionType']][['type']] %in% c('MC', 'Matrix') |
#         x[['questionType']][['selector']] == 'FORM'}
#   )
#
#
#   # We'll use this function to extract the details of a choice:
#   extract_choice = function(
#     qid,
#     choice_list,
#     choice_name
#   ){
#
#     tibble::tibble(
#       'questionId' = qid,
#       'choice' = choice_name,
#       'choice_description' = replace_null(choice_list[['description']]),
#       'choiceText' = replace_null(choice_list[['choiceText']]),
#       'choice_recode' = replace_null(choice_list[['recode']]),
#       'textEntry' = 'textEntry' %in% names(choice_list),
#       'analyze' = replace_null(choice_list[['analyze']])
#     )
#   }
#
#   # Use this for the non-sbs questions:
#   get_choices = function(qid){
#
#     choices = questions[[qid]][['choices']]
#
#     # For each choice, we make a row of a data frame here:
#     purrr::pmap_dfr(
#       .l = list(
#         qid,
#         choices,
#         names(choices)
#       ),
#       .f = extract_choice
#     )
#   }
#
#
#   # Get non-sbs choices -----------------------------------------------------
#
#   # Now apply these functions to each of the
#   choice_df = purrr::map_dfr(
#     .x = names(questions),
#     .f = get_choices
#   )
#
#   # Now deal with the SBS questions. First need to extract all the columns used.
#   # This should actually mimic flatten_questions a bit since it's like
#   # we have multiple questions together
#
#   extract_column = function(qid, column, column_num){
#
#     question_type = column[['questionType']]
#
#     tibble::tibble(
#       'questionId' = qid,
#       'column_num' = column_num,
#       'column_text' = column[['questionText']],
#       'column_label' = replace_null(column[['questionLabel']]),
#       'column_type' = question_type[['type']],
#       'column_selector' = replace_null(question_type[['selector']]),
#       'column_subselector' = replace_null(question_type[['subSelector']])
#     )
#   }
#
#
#   # Get SBS choices ---------------------------------------------------------
#
#   # Frequently this won't be needed:
#   if(length(questions_sbs) > 0){
#
#     get_columns = function(qid){
#
#       columns = questions_sbs[[qid]][['columns']]
#       column_names = names(columns)
#
#       purrr::pmap_dfr(
#         .l = list(
#           qid,
#           columns,
#           names(columns)
#         ),
#         .f = extract_column
#       )
#
#
#     }
#
#     column_sbs_df = purrr::map_dfr(
#       .x = names(questions_sbs),
#       .f = get_columns
#     )
#
#     # Then we have to go through all these columns,
#     # and run a version of extract_choice on them.
#     get_column_choices = function(qid, column_num){
#
#       choices = questions_sbs[[qid]][['columns']][[column_num]][['choices']]
#
#       # For each choice, we make a row of a data frame here:
#       purrr::pmap_dfr(
#         .l = list(
#           qid,
#           choices,
#           names(choices)
#         ),
#         .f = extract_choice
#       ) %>%
#         dplyr::mutate(column_num = column_num)
#     }
#
#     column_sbs_df = column_sbs_df %>%
#       dplyr::left_join(
#         purrr::map2_dfr(
#           .x = column_sbs_df[['questionId']],
#           .y = column_sbs_df[['column_num']],
#           .f = get_column_choices
#         ),
#         by = c('questionId', 'column_num')
#       )
#
#     choice_df = dplyr::bind_rows(choice_df, column_sbs_df)
#
#     choice_df = tibble::tibble(
#       questionId = question_order
#     ) %>%
#       dplyr::left_join(choice_df)
#
#   } else{
#
#     # Ensure that these columns exist even if there are no SBS
#     # questions for uniformity of output:
#     for(column in c(
#       'column_num',
#       'column_text',
#       'column_label',
#       'column_type',
#       'column_selector',
#       'column_subselector'
#     )
#     ){
#       choice_df[column] = NA_character_
#     }
#
#   }
#
#
#   # Finish ------------------------------------------------------------------
#
#   # Write:
#   if(!is.null(out_path)){
#
#     write_fun = ifelse(
#       file_format == 'tsv',
#       readr::write_tsv,
#       readr::write_csv
#     )
#
#     write_fun(
#       x = choice_df,
#       file = out_path
#     )
#   }
#
#   choice_df
#
# }
#
#
# flatten_question_block = function(
#   survey
# ){
#
#   check_out_path(out_path, file_format)
#
#   # Here we'll just export a cross of the block ids and qids for joining purposes
#   blocks = survey[['blocks']]
#   block_ids = names(blocks)
#
#
#   get_qids = function(block, block_id){
#
#     # Keep only the questions and return empty tibble if none found
#     elements = block[['elements']]
#     elements = purrr::keep(
#       .x = elements,
#       .p = function(x){x[['type']] == 'Question'}
#     )
#
#     if(length(elements) == 0){
#       tribble(~block_id, ~questionId)
#     } else{
#
#       # Otherwise get each of the qids out:
#       tibble::tibble(
#         block_id = block_id,
#         questionId = purrr::map_chr(
#           .x = elements,
#           .f = function(x){x[['questionId']]}
#         )
#       )
#     }
#   }
#
#   block_question_df = purrr::map2_dfr(
#     .x = blocks,
#     .y = names(blocks),
#     .f = get_qids
#   )
#
#   if(!is.null(out_path)){
#
#     write_fun = ifelse(
#       file_format == 'tsv',
#       readr::write_tsv,
#       readr::write_csv
#     )
#
#     write_fun(
#       x = block_question_df,
#       file = out_path
#     )
#   }
#
#   block_question_df
#
# }

