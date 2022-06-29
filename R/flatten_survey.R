#' Flatten a Qualtrics Survey
#' @description Decompose the blocks, questions, and choices of a
#' Qualtrics survey into rectangular data sets.
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param out_dir path to directory where files can be written
#' @param file_prefix string prefix to file names
#' @param file_format one of \code{c('tsv', 'csv')}
#' @return a named list of the four tables generated
#' @export "flatten_survey"
#' @author Sven Halvorson (svenpubmail@gmail.com)
#' TODO : make a version of subq_description that has no HTML


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
      is.character(file_prefix) & length(file_prefix) == 1
    )
  )

  file_format = rlang::arg_match(file_format)

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

  blocks = survey[['blocks']]

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


# This one makes the links between questions and blocks:
flatten_question_block = function(
  survey
){

  blocks = survey[['blocks']]
  block_ids = names(blocks)


  get_qids = function(block, block_id){

    # Keep only the questions and return empty tibble if none found
    elements = block[['elements']]
    elements = purrr::keep(
      .x = elements,
      .p = function(x){x[['type']] == 'Question'}
    )

    if(length(elements) == 0){
      # return a tibble without rows
      tibble::tribble(~block_id, ~question_id)
    } else{

      # Otherwise get each of the qids out:
      tibble::tibble(
        block_id = block_id,
        question_id = purrr::map_chr(
          .x = elements,
          .f = function(x){x[['questionId']]}
        )
      )
    }
  }

  purrr::map2_dfr(
    .x = blocks,
    .y = names(blocks),
    .f = get_qids
  )

}


flatten_questions = function(
  survey
){

  questions = survey[['questions']]
  question_ids = names(questions)


  # get the attributes of questions:
  get_question_deets = function(question_id){


    question = questions[[question_id]]

    tibble::tibble(
      'question_id' = question_id,
      'question_name' = subset_safely(question, 'questionName'),
      'question_text' = subset_safely(question, 'questionText'),
      'question_label' = subset_safely(question, 'questionLabel'),
      'question_type' = subset_safely(question[['questionType']], 'type'),
      'selector' = subset_safely(question[['questionType']], 'selector'),
      'sub_selector' = subset_safely(question[['questionType']], 'subSelector')
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
      x[['questionType']][['type']] %in% c('Matrix', 'SBS', 'DD')
    }
  )

  get_subquestions = function(question_id){

    question = questions[[question_id]]
    sub_questions = question[['subQuestions']]

    get_subquestion_deets = function(sub_question_num){

      sub_question = sub_questions[[sub_question_num]]

      tibble::tibble(
        'question_id' = question_id,
        'subq_num'  = sub_question_num,
        'subq_recode' = sub_question[['recode']],
        'subq_description' = sub_question[['description']],
        'subq_choice_text' = sub_question[['choiceText']],
        'subq_text_entry' = as.numeric('textEntry' %in% names(sub_question))
      )

    }

    subquestion_df = purrr::map_dfr(
      .x = names(sub_questions),
      .f = get_subquestion_deets
    )
  }

  subquestion_df = purrr::map_dfr(
    .x = names(matrix_questions),
    .f = get_subquestions
  )

  # Set the column order of the output:
  col_order = c(
    'question_id',
    'question_name',
    'question_text',
    'question_text_clean',
    'question_label',
    'question_type',
    'selector',
    'sub_selector',
    'subq_num',
    'subq_recode',
    'subq_description',
    'subq_choice_text',
    'subq_text_entry'
  )

  # Sometimes there will be no matrix questions so:
  if(nrow(subquestion_df) > 0){
    question_df = question_df %>%
      dplyr::left_join(
        subquestion_df ,
        by = 'question_id'
      )
  }

  question_df = question_df %>%
    # Strip away HTML nonsense
    dplyr::mutate(
      question_text_clean = stringr::str_remove_all(
        string = question_text,
        pattern = '<[^>]*>'
      ),
      question_text_clean = stringr::str_replace_all(
        string = question_text_clean,
        pattern = '\\n|\\t',
        replacement = ' '
      )
    ) %>%
    dplyr::select(
      dplyr::any_of(col_order)
    )

  question_df

}

flatten_choices = function(
  survey
){

  # Preserve the order that the QIDs appear in for ease of viewing:
  question_order = names(survey[['questions']])

  # Extracting the choices is only valid for a few qustions types. We will treat
  # the side by side (SBS) type differently than multiple choice, matrix, & forms
  # since it's kind of like multiple of those put together. Partition the questions:

  questions_sbs = purrr::keep(
    .x = survey[['questions']],
    .p = function(x){
      x[['questionType']][['type']] == 'SBS'
    }
  )

  questions = purrr::keep(
    .x = survey[['questions']],
    .p = function(x){
      x[['questionType']][['type']] %in% c('MC', 'Matrix', 'DD') |
        x[['questionType']][['selector']] == 'FORM'}
  )


  # We'll use this function to extract the details of a choice:
  extract_choice = function(
    qid,
    choice_list,
    choice_name
  ){

    #TODO: decide if we care the form field entries have text_entry = FALSE

    tibble::tibble(
      'question_id' = qid,
      'choice' = choice_name,
      'choice_description' = subset_safely(choice_list, 'description'),
      'choice_text' = subset_safely(choice_list, 'choiceText'),
      'choice_recode' = subset_safely(choice_list, 'recode'),
      'text_entry' = 'textEntry' %in% names(choice_list),
      'analyze' = subset_safely(choice_list, 'analyze')
    )
  }

  # Use this for the non-sbs questions:
  get_choices = function(qid){

    choices = questions[[qid]][['choices']]

    # For each choice, we make a row of a data frame here:
    purrr::pmap_dfr(
      .l = list(
        qid,
        choices,
        names(choices)
      ),
      .f = extract_choice
    )
  }

  # Now apply these functions to each of the
  choice_df = purrr::map_dfr(
    .x = names(questions),
    .f = get_choices
  )


  # Now deal with the SBS questions. First need to extract all the columns used.
  # This should actually mimic flatten_questions a bit since it's like
  # we have multiple questions together

  extract_column = function(qid, column, column_num){

    question_type = column[['questionType']]

    tibble::tibble(
      'question_id' = qid,
      'column_num' = column_num,
      'column_text' = column[['questionText']],
      'column_label' = subset_safely(column, 'questionLabel'),
      'column_type' = question_type[['type']],
      'column_selector' = subset_safely(question_type, 'selector'),
      'column_subselector' = subset_safely(question_type, 'subSelector')
    )
  }

  # Frequently this won't be needed:
  if(length(questions_sbs) > 0){

    get_columns = function(qid){

      columns = questions_sbs[[qid]][['columns']]
      column_names = names(columns)

      purrr::pmap_dfr(
        .l = list(
          qid,
          columns,
          names(columns)
        ),
        .f = extract_column
      )


    }

    column_sbs_df = purrr::map_dfr(
      .x = names(questions_sbs),
      .f = get_columns
    )

    # Then we have to go through all these columns,
    # and run a version of extract_choice on them.
    get_column_choices = function(qid, column_num){

      choices = questions_sbs[[qid]][['columns']][[column_num]][['choices']]

      # For each choice, we make a row of a data frame here:
      purrr::pmap_dfr(
        .l = list(
          qid,
          choices,
          names(choices)
        ),
        .f = extract_choice
      ) %>%
        dplyr::mutate(column_num = column_num)
    }

    column_sbs_df = column_sbs_df %>%
      dplyr::left_join(
        purrr::map2_dfr(
          .x = column_sbs_df[['question_id']],
          .y = column_sbs_df[['column_num']],
          .f = get_column_choices
        ),
        by = c('question_id', 'column_num')
      )

    choice_df = dplyr::bind_rows(choice_df, column_sbs_df)

    choice_df = tibble::tibble(
      question_id = question_order
    ) %>%
      dplyr::inner_join(
        choice_df,
        by = 'question_id'
      )

  } else{

    # Ensure that these columns exist even if there are no SBS
    # questions for uniformity of output:
    for(column in c(
      'column_num',
      'column_text',
      'column_label',
      'column_type',
      'column_selector',
      'column_subselector'
    )
    ){
      choice_df[column] = NA_character_
    }

  }

  choice_df

}



