
# qualtreats

Process, organize, and document data from Qualtrics

Install like this:

``` r

devtools::install_github('svenhalvorson/qualtreats')
```

These two environment variables (should be accessible by `Sys.getenv`)
need to be set in the same way that the [qualtRics
library](https://github.com/ropensci/qualtRics) uses them:

1.  `QUALTRICS_API_KEY` = ‘your_api_key’
2.  `QUALTRICS_BASE_URL` = ‘….qualtrics.com’

The function `flatten_survey` creates a semi-curated version of the
survey definitions from [this API
call](https://api.qualtrics.com/9d0928392673d-get-survey). Set a survey
ID and flatten the survey into three tables:

``` r

survey_id = 'SV_bg4hf9VdW9CwmiO'

survey_flat = qualtreats::flatten_survey(survey_id)

survey_flat[['questions']] |> 
  dplyr::select(question_id, question_description, question_type, question_selector) |>
  head()
#> # A tibble: 6 × 4
#>   question_id question_description                               quest…¹ quest…²
#>   <chr>       <chr>                                              <chr>   <chr>  
#> 1 QID8        "Thanks for using a Qualtrics Survey Template! To… DB      TB     
#> 2 QID1        "What is your gender?"                             MC      SAVR   
#> 3 QID2        "What is your age?"                                MC      SAVR   
#> 4 QID3        "Are you of Hispanic, Latino, or Spanish origin? " MC      SAVR   
#> 5 QID4        "How would you describe yourself? Please select a… MC      MAVR   
#> 6 QID5        "What is the highest degree or level of school yo… MC      SAVR   
#> # … with abbreviated variable names ¹​question_type, ²​question_selector
```

``` r

head(survey_flat[['choices']])
#> # A tibble: 6 × 7
#>   question_id choice_order choice choice_recode choice_descrip…¹ choic…² colum…³
#>   <chr>              <int>  <int>         <int> <chr>              <int>   <int>
#> 1 QID1                   1      1            NA Male                   0      NA
#> 2 QID1                   2      2            NA Female                 0      NA
#> 3 QID1                   3      3            NA Other                  0      NA
#> 4 QID2                   1      1            NA Under 18               0      NA
#> 5 QID2                   2      2            NA 18 - 24                0      NA
#> 6 QID2                   3      3            NA 25 - 34                0      NA
#> # … with abbreviated variable names ¹​choice_description, ²​choice_text_entry,
#> #   ³​column_number
```

The next function, `simplify_qtypes`, makes a list of all the questions
and assigns some flags to them. I characterized the question attributes
in the column `question_style`. This can accept either the survey_id or
a flattened survey (faster).

``` r

survey_qtypes = qualtreats::simplify_qtypes(survey_flat = survey_flat)
head(survey_qtypes)
#> # A tibble: 6 × 6
#>   question_id column_number question_style question_matrix question_sbs questi…¹
#>   <chr>               <int> <chr>                    <int>        <int>    <int>
#> 1 QID8                   NA descriptive                  0            0        0
#> 2 QID1                   NA radio                        0            0        0
#> 3 QID2                   NA radio                        0            0        0
#> 4 QID3                   NA radio                        0            0        0
#> 5 QID4                   NA checkbox                     0            0        0
#> 6 QID5                   NA radio                        0            0        0
#> # … with abbreviated variable name ¹​question_loop
```

When working with survey responses, I found it useful to know the
associations between what Qualtrics exports and the question attributes.
Here we call `get_column_map`:

``` r

column_map = qualtreats::get_column_map(survey_id)
#> 
#> Downloading survey : SV_bg4hf9VdW9CwmiO
#> 
#> 
#> Downloading survey : SV_bg4hf9VdW9CwmiO

head(column_map)
#> # A tibble: 6 × 17
#>   column_expor…¹ colum…² varia…³ varia…⁴ quest…⁵ colum…⁶ subq_…⁷ quest…⁸ impor…⁹
#>   <chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#> 1 Gender         Demogr… What i… What i… Gender  <NA>    <NA>    QID1    QID1   
#> 2 Age            Demogr… What i… What i… Age     <NA>    <NA>    QID2    QID2   
#> 3 Ethnicity1     Demogr… Are yo… Are yo… Ethnic… <NA>    <NA>    QID3    QID3   
#> 4 Ethnicity2_1   Demogr… How wo… How wo… Ethnic… <NA>    <NA>    QID4    QID4   
#> 5 Ethnicity2_2   Demogr… How wo… How wo… Ethnic… <NA>    <NA>    QID4    QID4   
#> 6 Ethnicity2_3   Demogr… How wo… How wo… Ethnic… <NA>    <NA>    QID4    QID4   
#> # … with 8 more variables: question_name <chr>, suffix <chr>,
#> #   loop_number <int>, column_number <int>, subq_number <int>, choice <int>,
#> #   choice_recode <int>, text_entry <int>, and abbreviated variable names
#> #   ¹​column_exported, ²​column_harmonized, ³​variable_label_exported,
#> #   ⁴​variable_label, ⁵​question_export_tag, ⁶​column_export_tag,
#> #   ⁷​subq_export_tag, ⁸​question_id, ⁹​import_id
```

The first column of this data frame is the columns of the data set that
qualtrics gives when responses are exported. Some of the columns are
attributes of that column such as the associated `choice`,
`subq_number`, and `column_number`. Other columns are ones I created
(ex: `column_harmonized` & `question_name`) which may help with
renaming:

``` r

responses = qualtreats::get_responses(survey_id)
#> 
#> Downloading survey : SV_bg4hf9VdW9CwmiO

responses = qualtreats::rename_dict(
  df = responses,
  old_names = column_map[['column_exported']],
  new_names = column_map[['column_harmonized']]
)

responses = qualtreats::var_lab_dict(
  df = responses,
  column_names = column_map[['column_harmonized']],
  var_labs = column_map[['variable_label']]
)
```
