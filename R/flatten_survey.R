#' Flatten a Qualtrics Survey
#' @description Decompose the blocks, questions, and choices of a
#' Qualtrics survey into rectangular data sets.
#' @param survey_id string of the survey id, begins with 'SV_'
#' @param file_format export format for column names. One of \code{c('spss', 'csv', 'tsv')}
#' @param drop_trash should questions in the trash bin be discarded?
#' @return a named list of the four tables generated
#' @export "flatten_survey"
#' @author Sven Halvorson (svenpubmail@gmail.com)

flatten_survey = function(
  survey_id,
  file_format = c('spss', 'csv', 'tsv'),
  drop_trash = TRUE
){

  stopifnot(
    all(
      valid_survey_id(survey_id),
      valid_api_key(Sys.getenv('QUALTRICS_API_KEY')),
      valid_base_url(Sys.getenv('QUALTRICS_BASE_URL'))
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

  survey_flat = list(
    flatten_blocks(survey),
    flatten_questions(survey),
    flatten_choices(survey)
  )
  names(survey_flat) = tables

  # Drop trash:
  if(drop_trash){

    survey_flat[['blocks']] = dplyr::filter(
      survey_flat[['blocks']],
      block_description != 'Trash / Unused Questions'
    )

    survey_flat[['questions']] = dplyr::filter(
      survey_flat[['questions']],
      block_id %in% survey_flat[['blocks']][['block_id']]
    )

    survey_flat[['choices']] = dplyr::filter(
      survey_flat[['choices']],
      question_id %in% survey_flat[['questions']][['question_id']]
    )

  }

  if(any(is.na(survey_flat[['questions']][['question_style']]))
  ){
    warning('Some questions have no associated question_style DEV FIX THIS!')
  }

  # We're moving get_column_map here so that the output is bundled:
  survey_flat$columns = get_column_map(survey_flat, survey_id, file_format)

  # All the purr map functions keep names (which I hate) so let's kill them off:
  survey_flat = purrr::map(
    .x = survey_flat,
    .f = function(x){
      dplyr::mutate(
        .data = x,
        dplyr::across(
          .cols = tidyselect::everything(),
          .fns = unname
        )
      )
    }
  )

  # Create question_name in questions:
  survey_flat$questions = dplyr::left_join(
    x = survey_flat$questions,
    y = dplyr::distinct(survey_flat$blocks, block_id, block_description),
    by = c('block_id'),
    relationship = 'many-to-one',
    unmatched = 'error'
  ) |>
    dplyr::mutate(
      question_name = paste0(
        block_description,
        '_',
        pad2(question_number)
      )
    ) |>
    dplyr::select(-question_number, -block_description) |>
    dplyr::relocate(question_name, .after = block_id)

  # Validate some of the ouput:
  stopifnot(
    all(
      sort(unique(survey_flat$questions$question_name)) == sort(unique(survey_flat$questions$question_name)),
      nrow(survey_flat$columns) == nrow(
        dplyr::left_join(
          survey_flat$columns,
          survey_flat$questions,
          by = c('question_name', 'question_id', 'subq_number', 'sbs_number'),
          relationship = 'many-to-one'
        )
      )
    )
  )

  invisible(survey_flat)

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
  ) |>
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
  ) |>
    dplyr::mutate(
      loop_and_merge = as.integer(!is.na(loop_and_merge)),
      dplyr::across(
        .cols = tidyselect::everything(),
        .fns = unname
      )
    )

  block_df = block_order |>
    dplyr::full_join(
      block_df,
      by = 'block_id'
    ) |>
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
    elements = purrr::keep(
      .x = block[['BlockElements']],
      .p = function(x){x[['Type']] == 'Question'}
    )

    if(length(elements) == 0){

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
  ) |>
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
      x[['QuestionType']] %in% c('Matrix', 'SBS', 'DD') | x[['Selector']] %in% c('FORM')
    }
  )

  if(length(matrix_questions) > 0){

    get_subquestions = function(question_id){

      question = questions[[question_id]]
      subquestions = question[['Choices']]

      # Safegaurd against zero length empty matrix:
      if(
        any(
          length(subquestions) == 0,
          # Treating these like sbs that have no subquestions:
          question[['Selector']] == 'Profile'
        )
      ){
        return(tibble::tibble(question_id = character(0)))
      }

      subquestion_df = tibble::tibble(
        'question_id' = question_id,
        'is_matrix' = 1L,
        'subq_number' = as.integer(names(subquestions)),
        'subq_text' = purrr::map_chr(
          .x = subquestions,
          .f = purrr::pluck,
          'Display',
          .default = NA_character_
        )
      )

      # order them as they appear:
      subquestion_df = tibble::tibble(
        subq_order = as.integer(1:length(question[['ChoiceOrder']])),
        subq_number = purrr::map_int(question[['ChoiceOrder']], as.integer)
      ) |>
        dplyr::left_join(
          y = subquestion_df,
          by = 'subq_number'
        )

      # Export tags may be missing, FALSE, or a list:
      subq_export_tags = question[['ChoiceDataExportTags']]
      if(is.list(subq_export_tags)){

        subquestion_df = subquestion_df |>
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

      sbs_columns = tibble::tibble(
        question_id = question_id,
        is_sbs = 1L,
        sbs_number = as.integer(names(sbs_columns)),
        sbs_export_tag = purrr::map_chr(sbs_columns, purrr::pluck, 'AnswerDataExportTag', .default = NA_character_),
        sbs_text = purrr::map_chr(sbs_columns, purrr::pluck, 'QuestionText', .default = NA_character_),
        sbs_type = purrr::map_chr(sbs_columns, purrr::pluck, 'QuestionType', .default = NA_character_),
        sbs_selector = purrr::map_chr(sbs_columns, purrr::pluck, 'Selector', .default = NA_character_),
        sbs_subselector = purrr::map_chr(sbs_columns, purrr::pluck, 'SubSelector', .default = NA_character_)
      )

      if(!is.null(sbs_question$Configuration$ColumnOrder)){

        sbs_columns = tibble::tibble(
          sbs_number = as.integer(sbs_question$Configuration$ColumnOrder)
        ) |>
          dplyr::filter(sbs_number > 0) |>
          dplyr::left_join(
            y = sbs_columns,
            by = 'sbs_number',
            relationship = 'one-to-one'
          )

      }

      dplyr::mutate(sbs_columns, sbs_order = dplyr::row_number())

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

  # Dumb ole profile questions:
  profile_questions = purrr::keep(
    .x = questions,
    .p = function(x){
      x[['Selector']] == 'Profile'
    }
  )

  if(length(profile_questions) > 0){
    get_profile_columns = function(question_id){

      profile_question = profile_questions[[question_id]]
      profile_columns = profile_question[['Choices']]

      profile_order = tibble::tibble(
        sbs_number = as.integer(profile_question[['AnswerOrder']])
      )

      profile_df = tibble::tibble(
        question_id = question_id,
        is_sbs = 1L,
        sbs_number = as.integer(names(profile_columns)),
        sbs_text = purrr::map_chr(profile_question[['Choices']], purrr::pluck, 'Display', .default = NA_character_)
      )

      if(is.list(profile_question[['ChoiceDataExportTags']])){
        profile_df[['sbs_export_tag']] = unlist(profile_question[['ChoiceDataExportTags']])
      }

      # Do this to order them correctly:
      dplyr::left_join(
        x = tibble::tibble(
          sbs_number = as.integer(profile_question[['ChoiceOrder']])
        ),
        y = profile_df,
        by = c('sbs_number')
      )

    }

    profile_df = purrr::map_dfr(
      .x = names(profile_questions),
      .f = get_profile_columns
    )

    # Profile questions may or may not occur at the same time as matrix/sbs
    # so do this:
    if(nrow(subquestion_df) > 0){
      subquestion_df = dplyr::bind_rows(subquestion_df, profile_df)
    } else{
      subquestion_df = profile_df
    }

  }

  # Sometimes there will be no matrix questions so:
  if(nrow(subquestion_df) > 0){
    question_df = question_df |>
      dplyr::left_join(
        subquestion_df ,
        by = 'question_id'
      )
  }

  # UPDATE 2022-10-05: moving the block ID to be within the questions
  # data frame instead of its own data frame. This also helps us get question_number
  question_block = flatten_question_block(survey)
  question_df = dplyr::left_join(
    dplyr::distinct(question_block, question_id, block_id),
    question_df,
    by = 'question_id'
  ) |>
    dplyr::group_by(block_id, question_id) |>
    dplyr::mutate(
      question_number = dplyr::row_number() == 1
    ) |>
    dplyr::group_by(block_id) |>
    dplyr::mutate(
      question_number = cumsum(question_number)
    ) |>
    dplyr::ungroup()

  # Set the column order of the output:
  question_df = tibble::tibble(
    question_id = character(0),
    block_id = character(0),
    question_number = integer(0),
    question_style = character(0),
    question_export_tag = character(0),
    question_description = character(0),
    question_text = character(0),
    question_type = character(0),
    question_selector = character(0),
    question_subselector = character(0),
    is_matrix = integer(0),
    subq_order = integer(0),
    subq_number = integer(0),
    subq_export_tag = character(0),
    subq_text = character(0),
    is_sbs = integer(0),
    sbs_order = integer(0),
    sbs_number = integer(0),
    sbs_export_tag = character(0),
    sbs_text = character(0),
    sbs_type = character(0),
    sbs_selector = character(0),
    sbs_subselector = character(0)
  ) |>
  dplyr::bind_rows(question_df) |>
  dplyr::mutate(
    question_style = dplyr::case_when(
      question_type == 'Captcha' ~ 'captcha',
      question_selector %in% c('TB', 'FLB', 'GRB') ~ 'descriptive',
      question_selector == 'Signature' ~ 'signature',
      question_type == 'TE' | question_selector == 'TE' | sbs_selector == 'TE' ~ 'text',
      # radio
      question_selector %in% c('SAHR', 'SAVR', 'SACOL', 'DL', 'SB', 'Likert') ~ 'radio',
      question_subselector %in% c('SingleAnswer', 'DL') ~ 'radio',
      sbs_subselector %in% c('SingleAnswer', 'DL') ~ 'radio',
      # checkbox
      question_selector %in% c('MAHR', 'MAVR', 'MACOL', 'MSB') ~ 'checkbox',
      question_subselector == 'MultipleAnswer' ~ 'checkbox',
      sbs_subselector == 'MultipleAnswer' ~ 'checkbox',
      TRUE ~ NA_character_
    ),
    is_matrix = tidyr::replace_na(is_matrix, 0L),
    is_sbs = tidyr::replace_na(is_sbs, 0L)
  )

  question_df

}

flatten_choices = function(
  survey
){

  questions = purrr::keep(
    .x = survey[['Questions']],
    .p = function(x){
      x[['QuestionType']] %in% c('MC', 'Matrix', 'DD', 'SBS')
    }
  )

  get_choice_df = function(question){

    qtype = question[['QuestionType']]

    # for whatever reason the rows are called choices in matrix
    corder = dplyr::case_when(
      qtype %in% c('Matrix', 'SBS') ~ 'AnswerOrder',
      qtype %in% 'DD' ~ 'Answers',
      TRUE ~ 'ChoiceOrder'
    )

    if(qtype == 'SBS'){

      # TODO need to make this work for the profile questions too

      get_column_choices = function(question_id, sbs_number, column_question){

        choices = column_question[['Answers']]

        if(length(choices) == 0){
          return(tibble::tibble(question_id = character(0)))
        }

        if('RecodeValues' %in% names(column_question) && length(column_question[['RecodeValues']]) > 0 ){
          recodes = as.integer(column_question[['RecodeValues']])
        } else{
          recodes = NA_integer_
        }

        column_choice_df = tibble::tibble(
          question_id = question[['QuestionID']],
          sbs_number = sbs_number,
          choice = as.integer(names(choices)),
          choice_order = as.integer(1:length(choices)), # always in order for SBS it seems?
          choice_recode = recodes,
          checkbox_number = dplyr::case_when(
            column_question[['SubSelector']] == 'MultipleAnswer' ~ dplyr::coalesce(choice_recode, choice),
            .default = NA_integer_
          ),
          choice_description = purrr::map_chr(choices, purrr::pluck, 'Display', .default = NA_character_)
        )

        column_choice_df

      }

      choice_df = purrr::pmap_dfr(
        .l = list(
          question_id = question[['QuestionID']],
          sbs_number = as.integer(names(question[['AdditionalQuestions']])),
          column_question = question[['AdditionalQuestions']]
        ),
        .f = get_column_choices
      )

    # stupid profile questions:
    } else if(question[['Selector']] == 'Profile'){

      get_profile_choices = function(sbs_number){

        column_choices = question[['Answers']][[sbs_number]]

        profile_column_df = tibble::tibble(
          question_id = question[['QuestionID']],
          sbs_number = as.integer(sbs_number),
          choice_order = as.integer(names(column_choices)),
          choice = as.integer(names(column_choices)),
          choice_recode = NA_integer_,
          choice_description = purrr::map_chr(.x = column_choices, .f = purrr::pluck, 'Display', .default = NA_character_),
          # The columns don't have text entry
          has_text_entry = 0L,
          choice_analyze = 1L
        )

        # Couldn't think of an an elegant way to do this within the tibble statement
        if('RecodeValues' %in% names(question)){
          profile_column_df[['choice_recode']] = as.integer(unlist(question[['RecodeValues']]))
        }

        profile_column_df

      }

      choice_df = purrr::map_dfr(
        .x = names(question[['Answers']]),
        .f = get_profile_choices
      ) |>
        dplyr::filter(choice_description != '&nbsp;')

    } else{

      choices = question[[ifelse(qtype %in% c('Matrix', 'DD'), 'Answers', 'Choices')]]

      if(length(choices) == 0){
        return(tibble::tibble(question_id = character(0)))
      }

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
        has_text_entry = purrr::map_int(choices, function(x){'TextEntry' %in% names(x)})
      )

      # Choice analyze kept in another piece:
      if('AnalyzeChoices' %in% names(question)){
        analyze_choices = question[['AnalyzeChoices']] |>
          purrr::keep(.p = function(x){x == 'No'}) |>
          names() |>
          as.integer()

        choice_df[['choice_analyze']] = as.integer(!choice_df$choice %in% analyze_choices)

      } else{
        choice_df[['choice_analyze']] = 1L
      }

      choice_df = dplyr::left_join(choice_order, choice_df, by = 'choice')

      # Add another column for the choices if it's a checkbox:
      is_checkbox = any(
        question$Selector %in% c('MAHR', 'MAVR', 'MACOL', 'MSB'),
        question$SubSelector == 'MultipleAnswer'
      )

      if(is_checkbox){
        choice_df = dplyr::mutate(
          choice_df,
          checkbox_number = dplyr::coalesce(choice_recode, choice),
          choice_value = 1L
        )
      }

    }

    return(choice_df)

  }

  choice_df = purrr::map_dfr(
    .x = questions,
    .f = get_choice_df
  )

  # Reorder them based off the survey. It's a little redundant to call this
  # so many times but it doesn't really slow the whole thing down much:
  choice_df = flatten_question_block(survey) |>
    dplyr::distinct(question_id) |>
    dplyr::inner_join(choice_df, by = 'question_id')

  # make sure we always have these columns:
  tibble::tibble(
    question_id = character(0),
    sbs_number = integer(0),
    checkbox_number = integer(0),
    choice = integer(0),
    choice_order = integer(0),
    choice_recode = integer(0),
    choice_analyze = integer(0),
    has_text_entry = integer(0),
    choice_value = integer(0),
    choice_description = character(0)
  ) |>
    dplyr::bind_rows(choice_df) |>
    dplyr::mutate(
      # Now create a derived column of what the user user probably wants:
      choice_value = dplyr::coalesce(choice_value, choice_recode, choice)
    )

}

get_column_map = function(
    survey_flat,
    survey_id,
    file_format = c('spss', 'csv', 'tsv')
){

  file_format = rlang::arg_match(file_format)

  # Get exported columns ----------------------------------------------------

  # Get the exported column names and associated columns:
  exported_columns = suppressMessages(
    qualtables::get_responses(
      survey_id = survey_id,
      file_format = file_format,
      trim_rows = FALSE,
      limit = 0
    )
  ) |>
    dplyr::select(-(StartDate:UserLanguage))

  # Have to re-do it if file_format == 'spss' since it doesn't give metadata:
  if(file_format == 'spss'){

    exported_metadata = suppressMessages(
      qualtables::get_responses(
        survey_id = survey_id,
        file_format = 'csv',
        trim_rows = FALSE,
        limit = 0
      )
    ) |>
      dplyr::select(-(StartDate:UserLanguage)) |>
      dplyr::slice(2) |>
      as.character()

    variable_label_exported = purrr::map_chr(
      .x = exported_columns,
      .f = attr,
      'label'
    )

  }else {
    exported_metadata = as.character(dplyr::slice(exported_columns, 2))
    variable_label_exported = as.character(dplyr::slice(exported_columns, 1))
  }

  # Start making the table of exported columns and their features.
  # Want these elements:
  # question_id, qtype, matrix, sbs, export_tags
  get_exported_choice = function(x){
    as.integer(purrr::pluck(jsonlite::fromJSON(x), 'choiceId', .default = NA_character_))
  } # This only seems to get the checkbox questions

  get_variable_label = function(x){
    purrr::pluck(jsonlite::fromJSON(x), 'ImportId', .default = NA_character_)
  }

  column_map = tibble::tibble(
    column_exported = colnames(exported_columns),
    variable_label_exported = variable_label_exported,
    import_id = purrr::map_chr(
      .x = exported_metadata,
      .f = get_variable_label
    ),
    question_id = stringr::str_extract(
      string = import_id,
      pattern = 'QID[0-9]+'
    ),
    choice_recode = purrr::map_int(
      .x = exported_metadata,
      .f = get_exported_choice
    ),
    embedded_data = as.integer(is.na(question_id))
  )

  # Set aside for error check:
  column_map_nrow = nrow(column_map)


  #  Loop number ------------------------------------------------------------

  # The first thing I want to do is mark the loops. This is because
  # it's used as a prefix to the export tag
  loop_question_ids = survey_flat |>
    purrr::pluck('blocks') |>
    dplyr::filter(loop_and_merge == 1L) |>
    dplyr::inner_join(
      purrr::pluck(survey_flat, 'questions'),
      by = 'block_id'
    ) |>
    dplyr::pull(question_id)

  column_map = column_map |>
    dplyr::mutate(
      loop_number = dplyr::case_when(
        question_id %in% loop_question_ids ~ stringr::str_extract(
          string = import_id,
          pattern = '^[0-9]+_'
        )
      ),
      loop_number = as.integer(
        stringr::str_extract(
          string = loop_number,
          pattern = '[0-9]+'
        )
      ),
      # suffix will be the piece we keep grinding down and extracting from:
      suffix = dplyr::case_when(
        question_id %in% loop_question_ids ~ stringr::str_remove(
          string = import_id,
          pattern = '^[0-9]+_'
        ),
        TRUE ~ import_id
      ),
      suffix = stringr::str_remove(
        string = suffix,
        pattern = 'QID[0-9]+_?'
      )
    )


  # Column number ----------------------------------------------------------

  # We might be able to get away with recognizing pound signs
  # but let's try to not infer which questions are sbs:
  sbs_question_ids = survey_flat |>
    purrr::pluck('questions') |>
    dplyr::filter(is_sbs == 1L) |>
    dplyr::pull(question_id)

  # Add in the profiles since they act like a sbs
  profile_question_ids = survey_flat |>
    purrr::pluck('questions') |>
    dplyr::filter(question_selector == 'Profile') |>
    dplyr::pull(question_id)

  column_map = column_map |>
    dplyr::mutate(
      sbs_number = dplyr::case_when(
        question_id %in% profile_question_ids ~ stringr::str_extract(
          string = suffix,
          pattern = '[0-9]+'
        ),
        question_id %in% sbs_question_ids ~ stringr::str_extract(
          string = suffix,
          pattern = '^#[0-9]+_'
        )
      ),
      sbs_number = as.integer(
        stringr::str_extract(
          string = sbs_number,
          pattern = '[0-9]+'
        )
      ),
      suffix = dplyr::case_when(
        question_id %in% sbs_question_ids ~ stringr::str_remove(suffix, '^#[0-9]+_'),
        TRUE ~ suffix
      )
    )

  # little check in case we don't understand this export schema as well as we think:
  if(
    any(
      column_map |>
      dplyr::filter(embedded_data == 0) |>
      dplyr::pull(suffix) |>
      stringr::str_detect(
        pattern = '#'
      )
    )
  ){
    stop('residual pound signs found in suffix')
  }


  # SubQuestions ------------------------------------------------------------

  matrix_question_ids = survey_flat |>
    purrr::pluck('questions') |>
    dplyr::filter(is_matrix == 1L) |>
    dplyr::pull(question_id)

  column_map = column_map |>
    dplyr::mutate(
      subq_number = dplyr::case_when(
        question_id %in% matrix_question_ids ~ stringr::str_extract(
          string = suffix,
          pattern = '^[0-9]+_?'
        )
      ),
      subq_number = as.integer(
        stringr::str_extract(
          string = subq_number,
          pattern ='[0-9]+'
        )
      ),
      suffix = dplyr::case_when(
        question_id %in% matrix_question_ids ~ stringr::str_remove(suffix, '^[0-9]+_?'),
        TRUE ~ suffix
      )
    )


  # Choices -----------------------------------------------------------------

  # Think I'm content to just assume any leftover numbers in the suffix are choices:

  column_map = column_map |>
    dplyr::mutate(
      choice = stringr::str_extract(
        string = suffix,
        pattern = '^[0-9]+_?'
      ),
      choice = as.integer(
        stringr::str_extract(
          string = choice,
          pattern ='[0-9]+'
        )
      ),
      suffix = stringr::str_remove(
        string = suffix,
        pattern = '^[0-9]+_?'
      )
    )

  # Now we need to join BOTH the choices and the choice_recodes because
  # for whatever reason Qualtrics exports both in different circumstances:
  column_map = column_map |>
    dplyr::left_join(
      dplyr::transmute(
        survey_flat[['choices']],
        question_id,
        sbs_number,
        choice,
        choice_recode2 = choice_recode
      ),
      by = c('question_id', 'sbs_number', 'choice')
    )

  column_map = column_map |>
    dplyr::left_join(
      dplyr::transmute(
        survey_flat[['choices']],
        question_id,
        sbs_number,
        choice2 = choice,
        choice_recode
      ) |>
        dplyr::filter(!is.na(choice_recode)),
      by = c('question_id', 'sbs_number', 'choice_recode')
    ) |>
    dplyr::mutate(
      choice_recode = dplyr::coalesce(choice_recode, choice_recode2),
      choice = dplyr::coalesce(choice, choice2),
      choice_value = dplyr::coalesce(choice_recode, choice)
    )

  # Want to differentiate cases where it's a checkbox:
  checkbox_qids = survey_flat |>
    purrr::pluck('questions') |>
    dplyr::filter(
      question_selector %in% c('MAHR', 'MAVR', 'MACOL', 'MSB') |
        question_subselector == 'MultipleAnswer' |
        sbs_subselector == 'MultipleAnswer'
    ) |>
    purrr::pluck('question_id')

  column_map = column_map |>
    dplyr::mutate(
      checkbox_number = dplyr::case_when(
        question_id %in% checkbox_qids ~ dplyr::coalesce(choice_recode, choice),
        TRUE ~ NA_integer_
      ),
      choice_value = dplyr::case_when(
        question_id %in% checkbox_qids ~ 1L,
        TRUE ~ choice_value
      )
    )

  # Shouldn't be any numbers remaining as far as I know:
  if(
    any(
      column_map |>
      dplyr::filter(embedded_data == 0) |>
      dplyr::pull(suffix) |>
      stringr::str_detect(
        pattern = '[0-9]'
      )
    )
  ){
    stop('residual numbers found in suffix')
  }


  # Text entry --------------------------------------------------------------

  text_question_ids = survey_flat |>
    purrr::pluck('questions') |>
    dplyr::filter(question_style == 'text') |>
    dplyr::pull(question_id)

  column_map = column_map |>
    dplyr::mutate(
      is_text_entry = as.integer(
        stringr::str_detect(
          string = suffix,
          pattern = 'TEXT'
        ) | question_id %in% text_question_ids
      ),
      suffix = stringr::str_remove(
        string = suffix,
        pattern = 'TEXT'
      )
    )

  # Export tags -------------------------------------------------------------

  column_map = column_map |>
    dplyr::left_join(
      y = dplyr::select(
        survey_flat[['questions']],
        question_id,
        question_export_tag,
        sbs_export_tag,
        subq_export_tag,
        sbs_order,
        sbs_number,
        subq_order,
        subq_number
      ),
      by = c('question_id', 'sbs_number', 'subq_number')
    )

  # Nice suffix -------------------------------------------------------------

  # now we use all that data we extracted to make the coded suffix:
  column_map = column_map |>
    dplyr::mutate(
      LP = dplyr::case_when(
        !is.na(loop_number) ~ paste0('_LP', pad2(loop_number)),
        TRUE ~ ''
      ),
      SBS = dplyr::case_when(
        !is.na(sbs_order) ~ paste0('_SBS', pad2(sbs_order)),
        TRUE ~ ''
      ),
      SQ = dplyr::case_when(
        !is.na(subq_order) ~ paste0('_SQ', pad2(subq_order)),
        TRUE ~ ''
      ),
      CB = dplyr::case_when(
        !is.na(checkbox_number) ~ paste0('_CB', pad2(checkbox_number)),
        TRUE ~ ''
      ),
      CH = dplyr::case_when(
        is.na(checkbox_number) & !is.na(dplyr::coalesce(choice_recode, choice)) ~ paste0(
          '_CH', pad2(dplyr::coalesce(choice_recode, choice))
        ),
        TRUE ~ ''
      ),
      TEXT = dplyr::case_when(
        is_text_entry == 1 & !(question_id %in% text_question_ids) ~ '_TEXT',
        TRUE ~ ''
      ),
      suffix = dplyr::case_when(
        suffix != '' ~ paste0('_', suffix),
        TRUE ~ ''
      ),
      suffix = paste0(LP, SBS, SQ, CB, CH, TEXT, suffix)
    )


  # Question names -------------------------------------------------------------


  # Now we're on to styling up our custom names:
  question_names = survey_flat[['blocks']] |>
    dplyr::select(block_description, block_id) |>
    dplyr::left_join(
      y = dplyr::select(survey_flat[['questions']], question_id, block_id, question_type),
      by = 'block_id',
      relationship = 'one-to-many'
    ) |>
    # Descriptive boxes aren't exported:
    dplyr::filter(question_type != 'DB') |>
    # repeats on sbs_number and subq_number mean we wanna grind this down:
    dplyr::distinct(block_description, block_id, question_id) |>
    dplyr::group_by(block_id) |>
    dplyr::mutate(
      question_number = pad2(dplyr::row_number())
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      block_id,
      question_number,
      question_name = paste0(
        block_description,
        '_',
        question_number
      ),
      question_id
    )

  column_map = dplyr::left_join(
    x = question_names,
    y = column_map,
    by = 'question_id'
  )


  # Prettier variable labels ------------------------------------------------

  # TODO: signature suffixes are gone
  # I'm finding that the variable labels exported both have unnecessary junk
  # in them but also have truncated text. Let's see if we can pretty them up
  # a bit:

  var_labs_harmonized = survey_flat[['questions']] |>
    dplyr::transmute(
      question_id,
      sbs_number,
      subq_number,
      question_description = format_description(question_text),
      sbs_text = format_description(sbs_text),
      subq_text = format_description(subq_text)
    )

  # Add to the columns exported:
  var_labs_harmonized = column_map |>
    dplyr::transmute(
      column_exported,
      question_id,
      sbs_number,
      subq_number,
      loop_number,
      choice_join = dplyr::coalesce(checkbox_number, choice_recode, choice),
      is_text_entry
    ) |>
    dplyr::left_join(
      y = var_labs_harmonized,
      by = c('question_id', 'sbs_number', 'subq_number')
    ) |>
    # Let's set it so that the question description only shows for the
    # first entry within a question
    dplyr::group_by(question_id, loop_number) |>
    dplyr::mutate(
      question_description = dplyr::case_when(
        dplyr::row_number() == 1 ~ question_description,
        TRUE ~ ''
      )
    )

  # add choice descriptions and make the variable label:
  var_labs_harmonized = survey_flat[['choices']] |>
    dplyr::transmute(
      question_id,
      sbs_number,
      choice_description = format_description(choice_description),
      choice_join = dplyr::coalesce(checkbox_number, choice_recode, choice)
    ) |>
    dplyr::right_join(
      y = var_labs_harmonized,
      by = c('question_id', 'sbs_number', 'choice_join')
    ) |>
    dplyr::mutate(
      text_suffix = ifelse(
        is_text_entry == 1,
        ' - Text',
        ''
      ),
      dplyr::across(
        .cols = c('sbs_text', 'subq_text', 'choice_description'),
        .fns = function(x){
          dplyr::case_when(
            is.na(x) ~ '',
            TRUE ~ paste0(' - ', x)
          )
        }
      ),
      variable_label = paste0(
        question_description,
        sbs_text,
        subq_text,
        choice_description,
        text_suffix
      ),
      # If there is no question description (not first), we want to remove the leading dash
      variable_label = stringr::str_remove(
        string = variable_label,
        pattern = '^ - '
      )
    )

  column_map = column_map |>
    dplyr::left_join(
      y = dplyr::select(
        var_labs_harmonized,
        column_exported,
        variable_label
      ),
      by = 'column_exported'
    )

  # Clean up ----------------------------------------------------------------

  # now we just attach the question name and suffix
  column_map = column_map |>
    dplyr::transmute(
      column_exported,
      column_harmonized = paste0(question_name, suffix),
      question_name,
      question_number = as.integer(question_number),
      suffix,
      variable_label,
      variable_label_exported,
      question_export_tag,
      sbs_export_tag,
      subq_export_tag,
      import_id,
      embedded_data,
      block_id,
      question_id,
      loop_number,
      subq_number,
      subq_order,
      sbs_number,
      sbs_order,
      choice_value,
      checkbox_number,
      is_text_entry
    )

  # Don't think we want to have any values for the embedded data value except
  # column_exported, import_id, and embedded_data:
  column_map = column_map |>
    dplyr::mutate(
      dplyr::across(
        .cols = -c('column_exported', 'import_id', 'embedded_data'),
        .fns = function(x){
          replace(
            x = x,
            list = embedded_data == 1L,
            NA
          )
        }
      ),
      column_harmonized = dplyr::case_when(
        embedded_data == 1L ~ column_exported,
        TRUE ~ column_harmonized
      )
    )


  # Check -------------------------------------------------------------------

  if(
    any(
      nrow(column_map) != column_map_nrow,
      length(column_map$column_harmonized) != length(unique(column_map$column_harmonized))
    )
  ){
    stop('problem in creating column map!')
  }

  column_map

}

