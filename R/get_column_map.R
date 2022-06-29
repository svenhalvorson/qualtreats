
# Goal of this function is to create a table from the survey
# definitions of these columns:
# 1. The export column names defined by qualtrics (or user)
# 2. The associated QIDs
# 3. A transformed version of the default exported column names.
#     this should replace the export tags with
#     the QIDs in #2 and then apply some clarifying symbols:
#     i) CH for choice
#     ii) RW for row (in the case of a matrix or side by side)
#     iii) CL for column
#     iv) LP for loop number (loop and merge)
# 4. A version with the block name
# 5. A blank column for users to import

# Required inputs would be:
# A) survey defs
# B) blocks table
# C) question_block table

get_column_map = function(
  survey_id
){


  # First we download the survey and flatten it
  survey_id = 'SV_887tASY7oNJ0Uxo'

  survey_defs = get_survey(survey_id, type = 'survey')

  flattened_survey = flatten_survey(
    survey_id = survey_id
  )

  # And the responses in multiple formats:
  responses_csv = get_responses(
    survey_id,
    out_dir = here::here('data'),
    file_format = 'csv'
  ) %>%
    select(-(StartDate:UserLanguage))

  responses_tsv = get_responses(
    survey_id,
    out_dir = here::here('data'),
    file_format = 'tsv'
  ) %>%
    select(-(StartDate:UserLanguage))

  responses_spss = get_responses(
    survey_id,
    out_dir = here::here('data'),
    file_format = 'spss'
  ) %>%
    select(-(StartDate:UserLanguage))

  # Now start to assemble the tibble:
  column_map = survey_defs[['exportColumnMap']]

  column_map = tibble(
    exported_name = names(column_map),
    question_id = map_chr(
      .x = column_map,
      .f = function(x){pluck(x, 'question')}
    ),
    choice = map_chr(
      .x = column_map,
      .f = function(x){
        ifelse(
          'choice' %in% names(x),
          x[['choice']],
          NA_character_
        )
      }
    ),
    subq_num = map_chr(
      .x = column_map,
      .f = subset_safely,
      name = 'subQuestion'
    ),
    text_entry = as.numeric(
      str_detect(exported_name, '_TEXT$')
    )
  ) %>%
    mutate(
      across(
        all_of(c('choice', 'subq_num')),
        str_extract,
        pattern = '[0-9]+$'
      )
    )

  # For whatever reason, the API lists the textboxes when
  # they are not actually exported:
  text_boxes = flattened_survey[['questions']] %>%
    filter(question_type == 'DB')

  column_map = dplyr::filter(
    column_map,
    !question_id %in% text_boxes[['question_id']]
  )

  exported_names = tibble(
    api = column_map[['exported_name']],
    csv = colnames(responses_csv),
    tsv = colnames(responses_tsv),
    spss = colnames(responses_spss)
  )

  # It's temping to flag text entry and loop out of this by pattern alone
  # but we have more definitive ways of getting this info from the survey
  # definitions:
  loop_qids = flattened_survey[['blocks']] %>%
    filter(loop_and_merge == 1) %>%
    inner_join(flattened_survey[['block_question']]) %>%
    pull(question_id)

  text_qids = flattened_survey[['questions']] %>%
    inner_join(flattened_survey[['choices']]) %>%
    filter(text_entry == 1) %>%
    select(question_id, que)

  column_map = column_map %>%
    mutate(
      loop_number = ifelse(
        questionId %in% loop_qids,
        str_extract(
          string = exported_name,
          pattern = '\\([0-9]+\\)$'
        ),
        NA_character_
      ),
      loop_number = as.numeric(
        str_extract(
          string = loop_number,
          pattern = '[0-9]+'
        )
      )
    )
}






#
# # And the text entry:
#
#
#
# # Now, what we want to do is make some classification as to
#
#
# questions = flattened_survey[['questions']]
# blocks = flattened_survey[['blocks']]
# block_question = flattened_survey[['block_question']]
#
#
#
# # Now we want to go about building this tibble, which we'll (creatively) call column_df
# # Start by just getting the exports and qids
#
#
# # So far I have only found one instance
# column_df = tibble::tibble(
#   exported_colname = responses %>%
#     dplyr::select(-(StartDate:UserLanguage)) %>%
#     colnames(),
#   exported_json = responses %>%
#     dplyr::select(-(StartDate:UserLanguage)) %>%
#     dplyr::slice(2) %>%
#     unlist()
# ) %>%
#   mutate(
#     questionId = str_extract(exported_json, 'QID[0-9]+')
#   )
#
# # Let's just do some experiments to see how annoying this is to actually do
#
# single_mcs = questions %>%
#   filter(
#     questionType == 'MC'
#   )
#
# column_df = column_df %>%
#   mutate(
#     RW = NA,
#     CL = str_extract(exported_json, '#[0-9]_+'),
#     CL = str_remove_all(CL, '(#|_)'),
#     CH = NA,
#     LP = str_extract(exported_colname, '^[0-9]+'),
#     CH = case_when(
#       !is.na(CH) ~ CH,
#       questionId %in% single_mcs$questionId ~ str_extract()
#     )
#   )
#












