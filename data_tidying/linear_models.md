Linear models
================

## Importing Data

``` r
mpv_df_charges =
  read_csv("./data/mpv_final.csv", na = c("", "NA", "Unknown")) %>% 
  mutate(
    charges = case_when(
      startsWith(criminal_charges, "Charged") ~ 1,
      startsWith(criminal_charges, "No") ~ 0),
    resolved = 
           ifelse(startsWith(disposition, "Pending"), 
                  {resolved = 0}, 
                  {resolved = 1}),
    race_rec = case_when(
      race == "Native American" ~ "Other",
      race == "Pacific Islander" ~ "Other",
      race == "Black" ~ "Black",
      race == "Hispanic" ~ "Other",
      race == "White" ~ "White",
      race == "Asian" ~ "Other"),
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
  filter(race_rec != "Other", gender != "Transgender") %>% 
  select(MI, age_rec, race_rec, charges, gender, gunshot, year, resolved)
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

## Model 1: Charges

Modeling charges laid as outcome and race as the exposure:

``` r
xtabs(~charges + race_rec, data = mpv_df_charges) %>% 
  knitr::kable()
```

|   | White | Black | Other |
| :- | ----: | ----: | ----: |
| 0 |  3689 |  2074 |     0 |
| 1 |    57 |    68 |     0 |

``` r
glm(charges ~ race_rec, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")) %>% 
    knitr::kable(digits = 3)
```

| term        |    OR | CI\_lower | CI\_upper |
| :---------- | ----: | --------: | --------: |
| (Intercept) | 0.015 |     0.012 |      0.02 |
| Race: Black | 2.122 |     1.486 |      3.03 |

Crude analyses show that, in instances where the victim of police
violence is Black, the odds of charges being laid is increased by 112%
(95% CI: 1.49, 3.03).

The following code chunk explores potential confounders.

``` r
#Year - Not significant, not further explored
glm(charges ~ year, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)  47.0      84.0        0.560   0.575
    ## 2 year         -0.0252    0.0416    -0.606   0.545

``` r
#Gender - Significant, further explored
glm(charges ~ gender, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    -2.56     0.216    -11.8  3.09e-32
    ## 2 genderMale     -1.42     0.238     -5.97 2.38e- 9

``` r
glm(charges ~ race_rec + gender, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy()
```

    ## # A tibble: 3 x 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     -2.81      0.228    -12.3  7.20e-35
    ## 2 race_recBlack    0.860     0.185      4.65 3.25e- 6
    ## 3 genderMale      -1.58      0.243     -6.49 8.35e-11

``` r
#Age - Not significant, not further explored
glm(charges ~ age_rec, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy()
```

    ## # A tibble: 4 x 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)       -3.80      0.118   -32.1   4.85e-226
    ## 2 age_rec[40,60)    -0.163     0.210    -0.777 4.37e-  1
    ## 3 age_rec[60,100)   -0.150     0.399    -0.376 7.07e-  1
    ## 4 age_rec[0,20)      0.513     0.329     1.56  1.19e-  1

Adjusted analysis included gender as a confounder.

``` r
glm(charges ~ race_rec + gender, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")) %>% 
    knitr::kable(digits = 3)
```

| term        |    OR | CI\_lower | CI\_upper |
| :---------- | ----: | --------: | --------: |
| (Intercept) | 0.060 |     0.038 |     0.094 |
| Race: Black | 2.362 |     1.645 |     3.393 |
| genderMale  | 0.207 |     0.129 |     0.333 |

Adjusted analyses show that, in instances where the victim of police
violence is Black, the odds of charges being laid is increased by 136%
(95% CI: 1.65, 3.39).

#### Sensitivity Analysis: Model 1

We then opted to conducted a sensitivity analysis, exploring whether a
pending investigation was the cause for what seems like a spuriously
inflated OR. Therefore, we restricted the sample to only resolved cases.

``` r
mpv_solved = 
  mpv_df_charges %>% 
  filter(resolved == "1")

glm(charges ~ race_rec + gender, data = mpv_solved, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")) %>% 
    knitr::kable(digits = 3)
```

| term        |    OR | CI\_lower | CI\_upper |
| :---------- | ----: | --------: | --------: |
| (Intercept) | 0.164 |     0.103 |     0.263 |
| Race: Black | 2.282 |     1.571 |     3.315 |
| genderMale  | 0.225 |     0.136 |     0.373 |

Adjusted analyses show that, in instances where the victim of police
violence is Black, the odds of charges being laid is increased by 128%
(95% CI: 1.57, 3.32), adjusting for gender. Restricting to solved cases
did not change the association.

## Model 2: Symptoms of Mental Illness

Modeling symptoms of mental illness as outcome.

``` r
xtabs(~MI + gender, data = mpv_df_charges)
```

    ##    gender
    ## MI  Female Male
    ##   0    183 3692
    ##   1     90 1094

``` r
glm(MI ~ gender, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "gender", "Gender: ")) %>% 
    knitr::kable(digits = 3)
```

| term         |    OR | CI\_lower | CI\_upper |
| :----------- | ----: | --------: | --------: |
| (Intercept)  | 0.492 |     0.382 |     0.633 |
| Gender: Male | 0.603 |     0.464 |     0.782 |

Gender is significantly associated with MI; males are 40% less likely to
exhibit symptoms of mental illness among those killed by police (OR:
0.60; 95% CI: 0.46, 0.78).

``` r
glm(MI ~ race_rec, data = mpv_df_charges, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")) %>% 
    knitr::kable(digits = 3)
```

| term        |    OR | CI\_lower | CI\_upper |
| :---------- | ----: | --------: | --------: |
| (Intercept) | 0.407 |     0.377 |     0.440 |
| Race: Black | 0.398 |     0.342 |     0.463 |

Race is also significantly associated with symptoms of mental illness,
in that Black individuals have 0.40 times the odds of reporting symptoms
of mental illness (95% CI: 0.34, 0.46).

#### Sensitivity Analysis: Model 2

``` r
glm(MI ~ gender + race_rec, data = mpv_solved, family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  mutate(
    term = str_replace(term, "race_rec", "Race: ")) %>% 
    knitr::kable(digits = 3)
```

| term        |    OR | CI\_lower | CI\_upper |
| :---------- | ----: | --------: | --------: |
| (Intercept) | 0.467 |     0.308 |     0.707 |
| genderMale  | 0.934 |     0.607 |     1.437 |
| Race: Black | 0.370 |     0.287 |     0.478 |

After restricting to solved cases, gender was no longer significantly
associated with symptoms of mental illness.
