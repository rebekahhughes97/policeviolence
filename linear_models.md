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
      symptoms_of_mental_illness == "Drug or alcohol use" ~ "No"),
    gunshot = case_when(
      cause_of_death == "Asphyxiated" ~ "No",
      cause_of_death == "Pepper Spray, Physical Restraint" ~ "No",
      cause_of_death == "Beaten Chemical agent/Pepper spray" ~ "No",
      cause_of_death == "Gunshot" ~ "Yes",
      cause_of_death == "Gunshot, Bean Bag Gun" ~ "Yes",
      cause_of_death == "Gunshot, Beanbag Gun" ~ "Yes",
      cause_of_death == "Gunshot, Pepper Spray" ~ "Yes",
      cause_of_death == "Gunshot, Police Dog" ~ "Yes",
      cause_of_death == "Gunshot, Stabbed" ~ "Yes",
      cause_of_death == "Gunshot, Taser" ~ "Yes",
      cause_of_death == "Gunshot, Unspecified Less Lethal Weapon" ~ "Yes",
      cause_of_death == "Gunshot, Vehicle" ~ "Yes",
      cause_of_death == "Other" ~ "No",
      cause_of_death == "Pepper Spray" ~ "No",
    )) %>% 
  filter(MI != "Unknown", race_rec != "Unknown", gender != "Transgender", gender != "Unknown") %>% 
  select(MI, age_rec, race_rec, charges, gender, gunshot)
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
    ##       0 5014 1439
    ##       1  119   12

``` r
xtabs(~charges + gender, data = mpv_df_charges)
```

    ##        gender
    ## charges Female Male
    ##       0    303 6150
    ##       1     25  106

``` r
xtabs(~gunshot + MI, data = mpv_df_charges)
```

    ##        MI
    ## gunshot   No  Yes
    ##     No    13    2
    ##     Yes 4924 1389

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
| (Intercept)    |  \-4.202 |   0.000 |
| Race: Black    |    0.846 |   0.000 |
| Race: Hispanic |  \-0.164 |   0.574 |
| Race: Other    |    0.352 |   0.558 |
| Race: Asian    |    0.261 |   0.720 |

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
| (Intercept)    |  \-3.854 |   0.000 |
| Age: \[40,60)  |  \-0.200 |   0.343 |
| Age: \[60,100) |  \-0.159 |   0.709 |
| Age: \[0,20)   |    0.434 |   0.167 |

``` r
fit = glm(charges ~ gender, data = mpv_df_charges, family = "binomial")

broom::tidy(fit) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "gender", "Gender: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term         | estimate | p.value |
| :----------- | -------: | ------: |
| (Intercept)  |  \-2.495 |       0 |
| Gender: Male |  \-1.566 |       0 |

``` r
fit = glm(charges ~ gunshot, data = mpv_df_charges, family = "binomial")

broom::tidy(fit) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "gunshot", "Gunshot: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term         | estimate | p.value |
| :----------- | -------: | ------: |
| (Intercept)  |  \-1.872 |   0.014 |
| Gunshot: Yes |  \-2.124 |   0.006 |

## Model 2: Outcome = MI

``` r
mpv_df_mi =
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
      symptoms_of_mental_illness == "Yes" ~ 1,
      symptoms_of_mental_illness == "No" ~ 0,
      symptoms_of_mental_illness == "Drug or alcohol use" ~ 0),
    gunshot = case_when(
      cause_of_death == "Asphyxiated" ~ "No",
      cause_of_death == "Pepper Spray, Physical Restraint" ~ "No",
      cause_of_death == "Beaten Chemical agent/Pepper spray" ~ "No",
      cause_of_death == "Gunshot" ~ "Yes",
      cause_of_death == "Gunshot, Bean Bag Gun" ~ "Yes",
      cause_of_death == "Gunshot, Beanbag Gun" ~ "Yes",
      cause_of_death == "Gunshot, Pepper Spray" ~ "Yes",
      cause_of_death == "Gunshot, Police Dog" ~ "Yes",
      cause_of_death == "Gunshot, Stabbed" ~ "Yes",
      cause_of_death == "Gunshot, Taser" ~ "Yes",
      cause_of_death == "Gunshot, Unspecified Less Lethal Weapon" ~ "Yes",
      cause_of_death == "Gunshot, Vehicle" ~ "Yes",
      cause_of_death == "Other" ~ "No",
      cause_of_death == "Pepper Spray" ~ "No",
    )) %>% 
  filter(MI != "Unknown", race_rec != "Unknown", gender != "Transgender", gender != "Unknown") %>% 
  select(MI, age_rec, race_rec, charges, gender, gunshot)
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
xtabs(~MI + gender, data = mpv_df_charges)
```

    ##      gender
    ## MI    Female Male
    ##   No     223 4910
    ##   Yes    105 1346

Modeling charges laid as outcome:

``` r
fit_mi_race = glm(MI ~ race_rec, data = mpv_df_mi, family = "binomial")

broom::tidy(fit_mi_race) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term           | estimate | p.value |
| :------------- | -------: | ------: |
| (Intercept)    |  \-0.898 |   0.000 |
| Race: Black    |  \-0.921 |   0.000 |
| Race: Hispanic |  \-0.749 |   0.000 |
| Race: Other    |  \-0.479 |   0.023 |
| Race: Asian    |    0.073 |   0.733 |

``` r
fit_mi_gender = glm(MI ~ gender, data = mpv_df_mi, family = "binomial")

broom::tidy(fit_mi_gender) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "gender", "Gender: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term         | estimate | p.value |
| :----------- | -------: | ------: |
| (Intercept)  |  \-0.753 |       0 |
| Gender: Male |  \-0.541 |       0 |

``` r
fit_mi_age = glm(MI ~ age_rec, data = mpv_df_mi, family = "binomial")

broom::tidy(fit_mi_age) %>% 
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "age_rec", "Age: ")
  ) %>% 
  knitr::kable(digits = 3)
```

| term           | estimate | p.value |
| :------------- | -------: | ------: |
| (Intercept)    |  \-1.386 |    0.00 |
| Age: \[40,60)  |    0.312 |    0.00 |
| Age: \[60,100) |    0.814 |    0.00 |
| Age: \[0,20)   |  \-0.388 |    0.01 |

## Hypothesis tests

Testing influence of mental illness symptoms. THIS DOESN’T WORK -
unclear why, will revisit.

``` r
fit_null = glm(MI ~ race_rec, data = mpv_df_mi, family = "binomial")
fit_alt = glm(MI ~ race_rec + gender, data = mpv_df_mi, family = "binomial")

anova(fit_null, fit_alt) %>% 
  broom::tidy()
```

    ## Warning in tidy.anova(.): The following column names in ANOVA output were not
    ## recognized or transformed: Resid..Df, Resid..Dev, Deviance

    ## # A tibble: 2 x 4
    ##   Resid..Df Resid..Dev    df Deviance
    ##       <dbl>      <dbl> <dbl>    <dbl>
    ## 1      6579      6751.    NA     NA  
    ## 2      6578      6740.     1     10.7
