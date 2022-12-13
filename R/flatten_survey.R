#' Flatten a Qualtrics Survey
#' @description Decompose the blocks, questions, and choices of a
#' Qualtrics survey into rectangular data sets.
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param out_dir path to directory where files can be written
#' @param file_prefix string prefix to file names to be written
#' @param file_format one of \code{c('csv', 'tsv')}
#' @param drop_trash Should questions in the trash bin be discarded?
#' @return a named list of the three tables generated
#' @export "flatten_survey"
#' @author Sven Halvorson (svenpubmail@gmail.com)

flatten_survey = function(
  survey_id,
  out_dir = NULL,
  file_prefix = '',
  file_format = c('csv', 'tsv'),
  drop_trash = TRUE
){

  stopifnot(
    all(
      valid_survey_id(survey_id),
      valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
      valid_base_url(Sys.getenv('QUALTRICS_BASE_URL')),
      is.null(out_dir) || dir.exists(out_dir),
      is.character(file_prefix) & length(file_prefix) == 1
    )
  )

  file_format = rlang::arg_match(file_format)

  # Get the survey information to be used by all other functions:
  survey = get_survey(
    survey_id = survey_id,
    type = 'definitions'
  )

  # Construct the file paths for the output
  tables = c(
    'blocks',
    'questions',
    'choices'
  )

  results = list(
    flatten_blocks(survey),
    flatten_questions(survey),
    flatten_choices(survey)
  )
  names(results) = tables

  # Drop trash:
  if(drop_trash){

    results[['blocks']] = dplyr::filter(
      results[['blocks']],
      block_description != 'Trash / Unused Questions'
    )

    results[['questions']] = dplyr::filter(
      results[['questions']],
      block_id %in% results[['blocks']][['block_id']]
    )

    results[['choices']] = dplyr::filter(
      results[['choices']],
      question_id %in% results[['questions']][['question_id']]
    )

  }

  # All the purr map functions keep names (which I hate) so let's kill them off:
  results = purrr::map(
    .x = results,
    .f = function(x){
      dplyr::mutate(
        .data = x,
        dplyr::across(
          .fns = unname
        )
      )
    }
  )


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

    file_paths = paste0(out_dir, file_names)

    purrr::walk2(
      .x = results,
      .y = file_paths,
      .f = write_fun
    )

  }

  invisible(results)

}


flatten_blocks = function(
  survey
){

  blocks = survey[['Blocks']]
  block_order = tibble::tibble(
    block_id = purrr::map_chr(
      .x = survey[['SurveyFlow']][['Flow']],
      .f = purrr::pluck,
      'ID',
      .default = NA_character_
    )
  ) %>%
    dplyr::distinct()


  block_df = tibble::tibble(
    block_id = purrr::map_chr(
      .x = blocks,
      .f = purrr::pluck,
      'ID',
      .default = NA_character_
    ),
    block_description = purrr::map_chr(
      .x = blocks,
      .f = purrr::pluck,
      'Description',
      .default = NA_character_
    ),
    loop_and_merge = purrr::map_chr(
      .x = blocks,
      .f = purrr::pluck,
      'Options',
      'Looping',
      .default = NA_character_
    )
  ) %>%
    dplyr::mutate(
      loop_and_merge = as.integer(!is.na(loop_and_merge)),
      dplyr::across(.fns = unname)
    )

  block_df = block_order %>%
    dplyr::full_join(block_df, by = 'block_id') %>%
    dplyr::transmute(
      block_order = dplyr::row_number(),
      block_id,
      block_description,
      loop_and_merge
    )

  block_df
}


# This one makes the links between questions and blocks:
flatten_question_block = function(
  survey
){

  blocks = survey[['Blocks']]
  block_ids = names(blocks)


  get_qids = function(block, block_id){

    # Keep only the questions and return empty tibble if none found
    elements = block[['BlockElements']]
    elements = purrr::keep(
      .x = elements,
      .p = function(x){x[['Type']] == 'Question'}
    )

    if(length(elements) == 0){
      # return a tibble without rows
      tibble::tibble(
        block_id = character(0),
        question_id = character(0)
      )
    } else{
      # Otherwise get each of the qids out:
      tibble::tibble(
        block_id = block_id,
        question_id = purrr::map_chr(
          .x = elements,
          .f = purrr::pluck,
          'QuestionID',
          .default = NA_character_
        )
      )
    }
  }

  question_block_df = purrr::map2_dfr(
    .x = blocks,
    .y = names(blocks),
    .f = get_qids
  )

  # Exports it in order created, not viewed, so we have to do that ourselves
  block_order = tibble::tibble(
    block_id = purrr::map_chr(
      .x = survey[['SurveyFlow']][['Flow']],
      .f = purrr::pluck,
      'ID',
      .default = NA_character_
    )
  ) %>%
    dplyr::distinct()

  question_block_df = dplyr::full_join(
    x = block_order,
    y = question_block_df,
    by = 'block_id'
  )

  question_block_df

}


flatten_questions = function(
  survey
){

  questions = survey[['Questions']]
  question_ids = names(questions)

  # get the attributes of questions:
  get_question_deets = function(question_id){

    question = questions[[question_id]]

    tibble::tibble(
      'question_id' = purrr::pluck(question, 'QuestionID', .default = NA_character_),
      'question_text' = purrr::pluck(question, 'QuestionText', .default = NA_character_),
      'question_description' = purrr::pluck(question, 'QuestionDescription', .default = NA_character_),
      'question_export_tag' = purrr::pluck(question, 'DataExportTag', .default = NA_character_),
      'question_type' = purrr::pluck(question, 'QuestionType', .default = NA_character_),
      'question_selector' = purrr::pluck(question, 'Selector', .default = NA_character_),
      'question_subselector' = purrr::pluck(question, 'SubSelector', .default = NA_character_)
    )
  }

  question_df = purrr::map_dfr(
    .x = question_ids,
    .f = get_question_deets
  )


  # Then we also want to get the subquestions from matrix style entry:
  matrix_questions = purrr::keep(
    .x = questions,
    .p = function(x){
      x[['QuestionType']] %in% c('Matrix', 'SBS', 'DD')
    }
  )

  # TODO: decide if we care that this doesn't work:
  if(
    length(purrr::keep(matrix_questions, function(x){x[['Selector']] == 'Profile'})) > 0
  ){
    warning('Choices not implemented correctly for profile questions')
  }

  if(length(matrix_questions) > 0){
    get_subquestions = function(question_id){

      question = questions[[question_id]]
      subquestions = question[['Choices']]
      subquestion_df = tibble::tibble(
        'question_id' = question_id,
        'subq_number' = as.integer(names(subquestions)),
        'subq_description' = purrr::map_chr(
          .x = subquestions,
          .f = purrr::pluck,
          'Display',
          .default = NA_character_
        ),
        'subq_text_entry' = purrr::map_int(
          .x = subquestions,
          .f = function(x){'TextEntry' %in% names(x)}
        )
      )

      # order them as they appear:
      subquestion_df = tibble::tibble(
        subq_order = as.integer(1:length(question[['ChoiceOrder']])),
        subq_number = purrr::map_int(question[['ChoiceOrder']], as.integer)
      ) %>%
        dplyr::left_join(
          y = subquestion_df,
          by = 'subq_number'
        )

      # Export tags may be missing, FALSE, or a list:
      subq_export_tags = question[['ChoiceDataExportTags']]
      if(is.list(subq_export_tags)){

        subquestion_df = subquestion_df %>%
          dplyr::left_join(
            y = tibble::tibble(
              'subq_number' = as.integer(names(subq_export_tags)),
              'subq_export_tag' = purrr::flatten_chr(subq_export_tags)
            ),
            by = 'subq_number'
          )
      }

      subquestion_df

    }

    subquestion_df = purrr::map_dfr(
      .x = names(matrix_questions),
      .f = get_subquestions
    )
  } else{
    subquestion_df = tibble::tibble(question_id = character(0))
  }

  # Now the dreaded sbs question
  sbs_questions = purrr::keep(
    .x = matrix_questions,
    .p = function(x){
      x[['QuestionType']] == 'SBS'
    }
  )

  if(length(sbs_questions) > 0){
    get_sbs_columns = function(question_id){

      sbs_question = sbs_questions[[question_id]]
      sbs_columns = sbs_question[['AdditionalQuestions']]
      tibble::tibble(
        question_id = question_id,
        column_number = as.integer(names(sbs_columns)),
        column_export_tag = purrr::map_chr(sbs_columns, purrr::pluck, 'AnswerDataExportTag', .default = NA_character_),
        column_description = purrr::map_chr(sbs_columns, purrr::pluck, 'QuestionText', .default = NA_character_),
        column_type = purrr::map_chr(sbs_columns, purrr::pluck, 'QuestionType', .default = NA_character_),
        column_selector = purrr::map_chr(sbs_columns, purrr::pluck, 'Selector', .default = NA_character_),
        column_subselector = purrr::map_chr(sbs_columns, purrr::pluck, 'SubSelector', .default = NA_character_)
      )

    }

    column_df = purrr::map_dfr(
      .x = names(sbs_questions),
      .f = get_sbs_columns
    )

    subquestion_df = dplyr::left_join(
      subquestion_df,
      column_df,
      by = 'question_id'
    )

  }

  # Sometimes there will be no matrix questions so:
  if(nrow(subquestion_df) > 0){
    question_df = question_df %>%
      dplyr::left_join(
        subquestion_df ,
        by = 'question_id'
      )
  }

  # UPDATE 2022-10-05: moving the block ID to be within the questions
  # data frame instead of its own data frame. This also sorts the questions:
  question_block = flatten_question_block(survey)
  question_df = dplyr::left_join(
    dplyr::distinct(question_block, question_id, block_id),
    question_df,
    by = 'question_id'
  )

  # Set the column order of the output:
  question_df = tibble::tibble(
    question_id = character(0),
    block_id = character(0),
    question_export_tag = character(0),
    question_description = character(0),
    question_text = character(0),
    question_type = character(0),
    question_selector = character(0),
    question_subselector = character(0),
    subq_order = integer(0),
    subq_number = integer(0),
    subq_export_tag = character(0),
    subq_description = character(0),
    subq_text_entry = integer(0),
    column_number = integer(0),
    column_export_tag = character(0),
    column_description = character(0),
    column_type = character(0),
    column_selector = character(0),
    column_subselector = character(0)
  ) %>%
  dplyr::bind_rows(question_df)

  question_df

}

flatten_choices = function(
  survey
){

  questions = purrr::keep(
    .x = survey[['Questions']],
    .p = function(x){
      x[['QuestionType']] %in% c('MC', 'Matrix', 'DD', 'SBS') |
        x[['Selector']] == 'FORM'
    }
  )

  # So what I had written previously was a bit convoluted. Feel like I
  # just need to write one function that takes in a question, and
  # givs back the associated data frame for that. Should have these columns:
  # tibble::tribble(
  #   ~question_id,
  #   ~column_number,
  #   ~choice_order,
  #   ~choice,
  #   ~choice_recode,
  #   ~choice_description,
  #   ~choice_text_entry,
  #   ~choice_analyze
  # )

  get_choice_df = function(question){

    qtype = question[['QuestionType']]

    # for whatever reason the rows are called choices in matrix
    corder = dplyr::case_when(
      qtype %in% c('Matrix', 'SBS') ~ 'AnswerOrder',
      qtype == 'DD' ~ 'Answers',
      TRUE ~ 'ChoiceOrder'
    )

    if(qtype == 'SBS'){

      # TODO need to make this work for the profile questions too

      if(question$QuestionID == 'QID43'){browser()}
      get_column_choices = function(question_id, column_number, column_question){

        choices = column_question[['Answers']]

        if('RecodeValues' %in% names(column_question) && length(column_question[['RecodeValues']]) > 0 ){
          recodes = as.integer(column_question[['RecodeValues']])
        } else{
          recodes = NA_integer_
        }

        column_choice_df = tibble::tibble(
          question_id = question[['QuestionID']],
          column_number = column_number,
          choice = as.integer(names(choices)),
          choice_order = as.integer(1:length(choices)), # always in order for SBS it seems?
          choice_recode = recodes,
          choice_description = purrr::map_chr(choices, purrr::pluck, 'Display', .default = NA_character_)
        )


      }

      choice_df = purrr::pmap_dfr(
        .l = list(
          question_id = question[['QuestionID']],
          column_number = as.integer(names(question[['AdditionalQuestions']])),
          column_question = question[['AdditionalQuestions']]
        ),
        .f = get_column_choices
      )



    } else{

      choices = question[[ifelse(qtype %in% c('Matrix', 'DD'), 'Answers', 'Choices')]]

      choice_order = tibble::tibble(
        choice_order = as.integer(1:length(question[[corder]])),
        choice = as.integer(question[[corder]])
      )

      if(qtype == 'DD'){
        choice_order[['choice']] = choice_order[['choice_order']]
      }

      if('RecodeValues' %in% names(question)){
        recodes = as.integer(question[['RecodeValues']])
      } else{
        recodes = NA_integer_
      }

      choice_df = tibble::tibble(
        question_id = question[['QuestionID']],
        choice = as.integer(names(choices)),
        choice_recode = recodes,
        choice_description = purrr::map_chr(choices, purrr::pluck, 'Display', .default = NA_character_),
        choice_text_entry = purrr::map_int(choices, function(x){'TextEntry' %in% names(x)})
      )

      # Choice analyze kept in another piece:
      if('AnalyzeChoices' %in% names(question)){
        analyze_choices = question[['AnalyzeChoices']] %>%
          purrr::keep(.p = function(x){x == 'No'}) %>%
          names() %>%
          as.integer()

        choice_df[['choice_analyze']] = as.integer(!choice_df$choice %in% analyze_choices)

      } else{
        choice_df[['choice_analyze']] = 1L
      }

      choice_df = dplyr::left_join(choice_order, choice_df, by = 'choice')

    }

    return(choice_df)

  }

  choice_df = purrr::map_dfr(
    .x = questions,
    .f = get_choice_df
  )

  # Reorder them based off the survey. It's a little redundant to call this
  # so many times but it doesn't really slow the whole thing down much:
  choice_df = flatten_question_block(survey) %>%
    dplyr::distinct(question_id) %>%
    dplyr::inner_join(choice_df, by = 'question_id')

  # make sure we always have these columns:

  tibble::tibble(
    question_id = character(0),
    column_number = integer(0),
    choice_order = integer(0),
    choice = integer(0),
    choice_recode = integer(0),
    choice_description = character(0),
    choice_text_entry = integer(0),
    choice_analyze = integer(0)
  ) %>%
    dplyr::bind_rows(choice_df)

}


