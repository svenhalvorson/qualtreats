flatten_cpo = function(survey_flat, column_map){
  
  output = list()
  
  output$questions = survey_flat |> 
    purrr::pluck('questions') |> 
    dplyr::left_join(
      purrr::pluck(survey_flat, 'blocks'),
      by = 'block_id'
    ) |> 
    dplyr::transmute(
      measure_id = block_description,
      question_number,
      question_style,
      question_text = question_description,
      is_matrix,
      subq_number,
      subq_text,
      sbs_number
    )
  
  
  
  
}