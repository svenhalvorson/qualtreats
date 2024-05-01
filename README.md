
# qualtables
I am in the process of hard forking this repository. The core idea of this is very good but there are some conceptual flaws in the current version. If you wish to collaborate, please contact me at <svenpubmail@gmail.com>

*Huge thank you to authors of
[qualtRics](https://github.com/ropensci/qualtRics). Appreciate
learn/steal-ing your `httr` code*

`qualtables` is a library that can retrieve data about Qualtrics surveys
extracted from the API. You can generate tables that describe the
questions, blocks, choices, and exported columns of a survey. This is
particularly useful for checking that the survey is configured correctly
or comparing surveys.

Install it like this:

``` r
devtools::install_github('svenhalvorson/qualtables')
```

These two environment variables (should be accessible by `Sys.getenv`)
need to be set in the same way that the
[qualtRics](https://github.com/ropensci/qualtRics) library uses them:

1.  `QUALTRICS_API_KEY`: ‘your_api_key’
2.  `QUALTRICS_BASE_URL`: ‘oregon.ca1.qualtrics.com’ (depends on
    organization)

### Flatten a survey

The function `flatten_survey` creates a semi-curated version of the
survey definitions from [this API
call](https://api.qualtrics.com/9d0928392673d-get-survey). Use a survey
ID (can be found in the URL) to flatten the survey into three tables:

``` r
survey_id = 'SV_bg4hf9VdW9CwmiO'

survey_flat = qualtables::flatten_survey(survey_id)
```

Here are *a few* of the columns in the questions table:

``` r
survey_flat[['questions']] |> 
  dplyr::select(question_id, question_description, question_type, question_selector) |>
  head()
#> # A tibble: 6 × 4
#>   question_id question_description               question_type question_selector
#>   <chr>       <chr>                              <chr>         <chr>            
#> 1 QID8        "Thanks for using a Qualtrics Sur… DB            TB               
#> 2 QID1        "What is your gender?"             MC            SAVR             
#> 3 QID2        "What is your age?"                MC            SAVR             
#> 4 QID3        "Are you of Hispanic, Latino, or … MC            SAVR             
#> 5 QID4        "How would you describe yourself?… MC            MAVR             
#> 6 QID5        "What is the highest degree or le… MC            SAVR
```

And the choices table:

``` r
head(survey_flat[['choices']])
#> # A tibble: 6 × 8
#>   question_id column_number choice_order choice choice_recode choice_description
#>   <chr>               <int>        <int>  <int>         <int> <chr>             
#> 1 QID1                   NA            1      1            NA Male              
#> 2 QID1                   NA            2      2            NA Female            
#> 3 QID1                   NA            3      3            NA Other             
#> 4 QID2                   NA            1      1            NA Under 18          
#> 5 QID2                   NA            2      2            NA 18 - 24           
#> 6 QID2                   NA            3      3            NA 25 - 34           
#> # ℹ 2 more variables: choice_text_entry <int>, choice_analyze <int>
```

There is also a blocks table which is typically less interesting (but
still useful).

### Simplify question types

The next function, `simplify_qtypes`, makes a list of all the questions
and assigns some flags to them. It will tell you if a question is a
matrix, side-by-side (sbs), or is part of a loop-and-merge. It
categorizes the questions by `question_style`. This can accept either
the `survey_id` or a flattened survey (faster).

``` r
survey_qtypes = qualtables::simplify_qtypes(survey_flat = survey_flat)
head(survey_qtypes)
#> # A tibble: 6 × 6
#>   question_id column_number question_style question_matrix question_sbs
#>   <chr>               <int> <chr>                    <int>        <int>
#> 1 QID8                   NA descriptive                  0            0
#> 2 QID1                   NA radio                        0            0
#> 3 QID2                   NA radio                        0            0
#> 4 QID3                   NA radio                        0            0
#> 5 QID4                   NA checkbox                     0            0
#> 6 QID5                   NA radio                        0            0
#> # ℹ 1 more variable: question_loop <int>
```

### Get a column map

When working with survey responses, I found it useful to know the
associations between what Qualtrics exports and the question attributes.
Here we call `get_column_map`:

``` r
column_map = qualtables::get_column_map(survey_id)

column_map |>
  dplyr::select(column_exported, column_harmonized, choice, question_id) |>
  head()
#> # A tibble: 6 × 4
#>   column_exported column_harmonized    choice question_id
#>   <chr>           <chr>                 <int> <chr>      
#> 1 Gender          Demographics_01          NA QID1       
#> 2 Age             Demographics_02          NA QID2       
#> 3 Ethnicity1      Demographics_03          NA QID3       
#> 4 Ethnicity2_1    Demographics_04_CH01      1 QID4       
#> 5 Ethnicity2_2    Demographics_04_CH02      2 QID4       
#> 6 Ethnicity2_3    Demographics_04_CH03      3 QID4
```

The first column of this data frame is the columns of the data set that
qualtrics gives when responses are exported. Some of the variables are
attributes of that column such as the associated `choice`,
`subq_number`, and `column_number`. Other variables, like
`column_harmonized` & `question_name`, are ones qualtables generates. I
tried to make some question names and suffixes that could be used if you
do not want to define them yourself. You an use the “dictionary style”
renaming and label functions:

``` r
responses = qualtables::get_responses(survey_id)

responses = qualtables::rename_dict(
  df = responses,
  old_names = column_map[['column_exported']],
  new_names = column_map[['column_harmonized']]
)

responses = qualtables::var_lab_dict(
  df = responses,
  column_names = column_map[['column_harmonized']],
  var_labs = column_map[['variable_label']]
)
```
