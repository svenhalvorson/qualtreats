# Issues with the exportColumnMap
# 1. Radio buttons with text entry do not specify the
#   choice, only list one in the case of multiple choices
#   with text entry options
# 2. Signatures have uppercase suffixes that are a little differnet
#   in the map but lower case in reality
# 3. Loop numbers are in the wrong places for all file formats
# 4. Form fields have a suffix of _TEXT in the map but not in reality


library('tidyverse')

survey_id = 'SV_887tASY7oNJ0Uxo'

# get the various exported columns
get_exported_columns = function(
    file_format
){

  qualtreats::get_responses(survey_id, file_format = file_format) %>%
    select(matches('QID')) %>%
    colnames()

}

exported_columns = c('spss', 'csv', 'tsv') %>%
  map(get_exported_columns) %>%
  set_names(c('spss', 'csv', 'tsv')) %>%
  bind_cols()

test_csv = get_column_map(svy_id, file_format = 'csv')


get_column_map = function(
    survey_id,
    file_format = c('spss', 'csv', 'tsv')
){

  # argument checks:
  stopifnot(
    all(
      valid_survey_id(survey_id),
      valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
      valid_base_url(Sys.getenv('QUALTRICS_BASE_URL'))
    )
  )
  file_format = rlang::arg_match(file_format)

  svy_flat = qualtreats::flatten_survey(survey_id)
  svy = qualtreats::get_survey(survey_id)
  export_map = svy[['exportColumnMap']]

  column_map = tibble::tibble(

    exported_column = names(export_map),
    question_id = purrr::map_chr(export_map, subset_safely, 'question'),
    choice = purrr::map_chr(export_map, subset_safely, 'choice'),
    subq_number = purrr::map_chr(export_map, subset_safely, 'subQuestion'),
    column_number = purrr::map_chr(export_map, subset_safely, 'column'),
    text_entry = purrr::map_chr(export_map, subset_safely, 'textEntry')

  ) %>%
    dplyr::mutate(
      # Strip away the prefixes for these and make them numeric:
      dplyr::across(
        dplyr::all_of(c('choice', 'subq_number', 'column_number')),
        function(x){
          as.numeric(
            stringr::str_extract(
              string = x,
              pattern = '[0-9]+$'
            )
          )
        }
      ),
      text_entry = as.numeric(!is.na(text_entry))
    )

  # now assign the loop numbers. The export map gives it parentheses
  # at the end of the name but it's possible someone put that into their question
  # name so instead, find them:

  loop_questions = svy_flat[['blocks']] %>%
    dplyr::filter(loop_and_merge == 1) %>%
    dplyr::inner_join(svy_flat[['block_question']]) %>%
    purrr::pluck('question_id')

  column_map = column_map %>%
    dplyr::mutate(
      loop_number = ifelse(
        question_id %in% loop_questions,
        stringr::str_extract(
          string = exported_column,
          pattern = '\\([0-9]+\\)$'
        ),
        NA_character_
      ),
      loop_number = as.numeric(
        stringr::str_extract(
          string = loop_number,
          pattern = '[0-9]+'
        )
      )
    )

  # Adjust the exported columns to match whatever format
  # is supplied. Looks like it gives you the csv exported names
  if(file_format == 'spss'){

    column_map = column_map %>%
      dplyr::mutate(
        exported_column = ifelse(
          is.na(column_number),
          exported_column,
          str_replace(exported_column, '#', '_')
        )
      ) %>%
      dplyr::mutate(
        exported_column = ifelse(
          is.na(loop_number),
          exported_column,
          paste0('A', exported_column)
        )
      )

  } else if(file_format == 'tsv'){

    column_map = column_map %>%
      dplyr::mutate(
        exported_column = ifelse(
          is.na(column_number),
          exported_column,
          str_replace(exported_column, '#', '.')
        )
      ) %>%
      dplyr::mutate(
        exported_column = ifelse(
          is.na(loop_number),
          exported_column,
          paste0('X', exported_column)
        )
      )

  } else{

    column_map = column_map %>%
      dplyr::mutate(
        exported_column = ifelse(
          is.na(loop_number),
          exported_column,
          paste0(loo_number, '_', exported_column)
        )
      )

  }

  # Then remove the parenthetical version at the end:



  # Attach the choice_recodes:
  column_map = column_map %>%
    dplyr::left_join(
      svy_flat[['choices']] %>%
        dplyr::select(question_id, choice, choice_recode)
    )


  # Now generate the suffix for the column name:
  column_map = column_map %>%
    dplyr::mutate(
      CL = ifelse(
        is.na(column_number),
        '',
        paste0('_CL', pad2(column_number))
      ),
      SQ = ifelse(
        is.na(subq_number),
        '',
        paste0('_SQ', pad2(subq_number))
      ),
      CH = ifelse(
        is.na(choice_recode),
        '',
        paste0('_CH', pad2(choice_recode))
      ),
      LP = ifelse(
        is.na(loop_number),
        '',
        paste0('_LP', pad2(loop_number))
      ),
      suffix = paste0(CL, SQ, CH, LP)
    )


  # Attach the prefixes by block name:
  block_prefixes = svy_flat[['blocks']] %>%
    dplyr::left_join(svy_flat[['block_question']]) %>%
    dplyr::distinct(block_id, block_description, question_id) %>%
    dplyr::group_by(block_id) %>%
    dplyr::mutate(
      question_number = pad2(dplyr::row_number()),
      block_prefix = paste0(block_description, '_', question_number)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(question_id, block_prefix)

  # Join these on, make the default name, subset columns, and return:
  column_map %>%
    dplyr::left_join(block_prefixes) %>%
    dplyr::mutate(
      default_column = paste0(block_prefix, suffix)
    ) %>%
    dplyr::select(
      exported_column,
      default_column,
      suffix,
      question_id,
      column_number,
      subq_number,
      choice,
      choice_recode,
      loop_number
    )


}





