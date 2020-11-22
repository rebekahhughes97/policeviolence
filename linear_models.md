Linear models
================

## Model: Outcome = Charges

``` r
mpv_df_charges =
  read_csv("./data/mpv_final.csv") %>% 
  mutate(
    charges = case_when(
      startsWith(criminal_charges, "Charged") ~ 1,
      startsWith(criminal_charges, "No") ~ 0),
    race_rec = case_when(
      race == "Unknown race" ~ "Unknown",
      race == "unknown race" ~ "Unknown",
      race == "Native American" ~ "Other",
      race == "Pacific Islander" ~ "Other",
      race == "Black" ~ "Black",
      race == "Hispanic" ~ "Hispanic",
      race == "White" ~ "White",
      race == "Asian" ~ "Asian"),
    race_rec = fct_infreq(race_rec),
    age_rec = 
      cut(age, breaks = c(0,20,40,60,100), right = FALSE),
    age_rec = fct_infreq(age_rec),
    MI = case_when(
      symptoms_of_mental_illness == "Yes" ~ "Yes",
      symptoms_of_mental_illness == "No" ~ "No",
      symptoms_of_mental_illness == "Unknown" ~ "Unknown",
      symptoms_of_mental_illness == "Drug or alcohol use" ~ "No",
    )) %>% 
  filter(MI != "Unknown") %>% 
  select(MI, age_rec, race_rec, charges)
```

    ## Parsed with column specification:
    ## cols(
    ##   age = col_double(),
    ##   gender = col_character(),
    ##   race = col_character(),
    ##   police_dept = col_character(),
    ##   disposition = col_character(),
    ##   year = col_double(),
    ##   month = col_double(),
    ##   day = col_double(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   county = col_character(),
    ##   cause_of_death = col_character(),
    ##   criminal_charges = col_character(),
    ##   symptoms_of_mental_illness = col_character(),
    ##   lat = col_double(),
    ##   lng = col_double()
    ## )

``` r
xtabs(~charges + MI, data = mpv_df_charges)
```

    ##        MI
    ## charges   No  Yes
    ##       0 5656 1598
    ##       1  119   12

Modeling charges laid as outcome:

``` r
fit = glm(charges ~ race_rec, data = mpv_df_charges, family = "binomial")

broom::tidy(fit) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term           | estimate | p.value |
| :------------- | -------: | ------: |
| (Intercept)    |  \-4.204 |   0.000 |
| Race: Black    |    0.846 |   0.000 |
| Race: Hispanic |  \-0.165 |   0.572 |
| Race: Unknown  | \-15.362 |   0.968 |
| Race: Other    |    0.347 |   0.564 |
| Race: Asian    |    0.262 |   0.719 |

``` r
fit = glm(charges ~ age_rec, data = mpv_df_charges, family = "binomial")

broom::tidy(fit) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "age_rec", "Age: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term           | estimate | p.value |
| :------------- | -------: | ------: |
| (Intercept)    |  \-3.926 |   0.000 |
| Age: \[40,60)  |  \-0.262 |   0.214 |
| Age: \[60,100) |  \-0.291 |   0.494 |
| Age: \[0,20)   |    0.468 |   0.136 |

## Hypothesis tests

Testing influence of mental illness symptoms.

``` r
fit_null = glm(charges ~ race_rec, data = mpv_df_charges, family = "binomial")
fit_alt = glm(charges ~ race_rec + MI, data = mpv_df_charges, family = "binomial")

anova(fit_null, fit_alt) %>% 
  broom::tidy()
```

    ## Warning in tidy.anova(.): The following column names in ANOVA output were not
    ## recognized or transformed: Resid..Df, Resid..Dev, Deviance

    ## # A tibble: 2 x 4
    ##   Resid..Df Resid..Dev    df Deviance
    ##       <dbl>      <dbl> <dbl>    <dbl>
    ## 1      7377      1262.    NA     NA  
    ## 2      7376      1250.     1     12.0
