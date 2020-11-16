Data Wrangling for All Datasets Used
================

## MPV Dataset

``` r
mpv_df =
  read_excel("./data/MPVDatasetDownload.xlsx",
             sheet = "2013-2020 Police Killings") %>%
  janitor::clean_names() %>% 
  separate(date_of_incident_month_day_year, c("year", "month", "day")) %>% 
  mutate(
    age = as.numeric(victims_age),
    gender  = victims_gender,
    race = victims_race,
    police_dept = agency_responsible_for_death,
    description = a_brief_description_of_the_circumstances_surrounding_the_death,
    disposition = official_disposition_of_death_justified_or_other,
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)) %>%
  select(
    age, gender, race, police_dept, description, disposition, year,
    month, day, city, state, county, cause_of_death, criminal_charges,
    symptoms_of_mental_illness)

write_csv(mpv_df, "./data/mpv_tidy.csv")
```

In the above code chunk, the MPV dataset has been manipulated and tidied
by reading in the excel sheet and cleaning the variable names,
separating out the date variable, changing the names of some of the
variables and changing some variables to be numeric, and selecting only
the variables of importance to our analysis for inclusion in the tidied
dataset.

## USA October 2020 Dataset

``` r
usa_df =
  read_excel("./data/usa_2020_oct.xlsx") %>%
  janitor::clean_names() %>% 
  separate(event_date, c("year", "month", "day"))
```

## ACS Datasets

#### Age and Sex

``` r
age_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(-c(4, 41:80, 117:156, 193:232, 269:308, 345:384, 421:458, starts_with("margin_of_error"))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% view()
  pivot_longer(
     estimate_total_total_population_age_under_5_years:estimate_total_total_population_age_85_years_and_over,
     names_to = "age",
     values_to = "population_totals",
     names_prefix = "estimate_total_total_population_age"
    ) %>% view()
# error: says my variable isn't found 
   pivot_longer(
     cols = starts_with("estimate_percent_total"),
     names_to = "age_2",
     values_to = "population_percent", 
     names_prefix = "estimate_percent_total_population_age"
     ) %>% 
   pivot_longer(
     cols = starts_with("estimate_male_total"),
     names_to = "age_4",
     values_to = "male_totals", 
     names_prefix = "estimate_male_total_population_age"
     ) %>% 
   pivot_longer(
     cols = starts_with("estimate_percent_male_total"),
     names_to = "age_6",
     values_to = "male_percent", 
     names_prefix = "estimate_percent_male_total_population_age"
     ) %>% 
   pivot_longer(
     cols = starts_with("estimate_female_total"),
     names_to = "age_8",
     values_to = "female_totals", 
     names_prefix = "estimate_female_total_population_age"
     ) %>% 
   pivot_longer(
     cols = starts_with("estimate_percent_female_total"),
     names_to = "age_10",
     values_to = "female_percent", 
     names_prefix = "estimate_percent_female_total_population_age"
     ) %>% 
view()
```

#### Age and Sex\_MM edits

``` r
age_df =
  read_csv("./data/acs_age_county.csv", skip = 1, na = "null") %>% 
  janitor::clean_names() %>% 
  select(-c(starts_with("margin_of_error"))) %>% 
  select(geographic_area_name,
         estimate_total_total_population_age_under_5_years,
         estimate_total_total_population_age_5_to_9_years,
         estimate_total_total_population_age_10_to_14_years,
         estimate_total_total_population_age_15_to_19_years,
         estimate_total_total_population_age_20_to_24_years,
         estimate_total_total_population_age_25_to_29_years,
         estimate_total_total_population_age_30_to_34_years,
         estimate_total_total_population_age_35_to_39_years,
         estimate_total_total_population_age_40_to_44_years,
         estimate_total_total_population_age_45_to_49_years,
         estimate_total_total_population_age_50_to_54_years,
         estimate_total_total_population_age_55_to_59_years,
         estimate_total_total_population_age_60_to_64_years,
         estimate_total_total_population_age_65_to_69_years,
         estimate_total_total_population_age_70_to_74_years,
         estimate_total_total_population_age_75_to_79_years,
         estimate_total_total_population_age_80_to_84_years,
         estimate_total_total_population_age_85_years_and_over,
         estimate_percent_total_population_age_under_5_years,
         estimate_percent_total_population_age_5_to_9_years,
         estimate_percent_total_population_age_10_to_14_years,
         estimate_percent_total_population_age_15_to_19_years,
         estimate_percent_total_population_age_20_to_24_years,
         estimate_percent_total_population_age_25_to_29_years,
         estimate_percent_total_population_age_30_to_34_years,
         estimate_percent_total_population_age_35_to_39_years,
         estimate_percent_total_population_age_40_to_44_years,
         estimate_percent_total_population_age_45_to_49_years,
         estimate_percent_total_population_age_50_to_54_years,
         estimate_percent_total_population_age_55_to_59_years,
         estimate_percent_total_population_age_60_to_64_years,
         estimate_percent_total_population_age_65_to_69_years,
         estimate_percent_total_population_age_70_to_74_years,
         estimate_percent_total_population_age_75_to_79_years,
         estimate_percent_total_population_age_80_to_84_years,
         estimate_percent_total_population_age_85_years_and_over) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  rename(age_under_5 = estimate_total_total_population_age_under_5_years,
         age_5_9 = estimate_total_total_population_age_5_to_9_years,
         age_10_14 = estimate_total_total_population_age_10_to_14_years,
         age_15_19 = estimate_total_total_population_age_15_to_19_years,
         age_20_24 = estimate_total_total_population_age_20_to_24_years,
         age_25_29 = estimate_total_total_population_age_25_to_29_years,
         age_30_34 = estimate_total_total_population_age_30_to_34_years,
         age_35_39 = estimate_total_total_population_age_35_to_39_years,
         age_40_44 = estimate_total_total_population_age_40_to_44_years,
         age_45_49 = estimate_total_total_population_age_45_to_49_years,
         age_50_54 = estimate_total_total_population_age_50_to_54_years,
         age_55_59 = estimate_total_total_population_age_55_to_59_years,
         age_60_64 = estimate_total_total_population_age_60_to_64_years,
         age_65_69 = estimate_total_total_population_age_65_to_69_years,
         age_70_74 = estimate_total_total_population_age_70_to_74_years,
         age_75_79 = estimate_total_total_population_age_75_to_79_years,
         age_80_84 = estimate_total_total_population_age_80_to_84_years,
         age_85_over = estimate_total_total_population_age_85_years_and_over,
         age_pct_under_5 = estimate_percent_total_population_age_under_5_years,
         age_pct_5_9 = estimate_percent_total_population_age_5_to_9_years,
         age_pct_10_14 = estimate_percent_total_population_age_10_to_14_years,
         age_pct_15_19 = estimate_percent_total_population_age_15_to_19_years,
         age_pct_20_24 = estimate_percent_total_population_age_20_to_24_years,
         age_pct_25_29 = estimate_percent_total_population_age_25_to_29_years,
         age_pct_30_34 = estimate_percent_total_population_age_30_to_34_years,
         age_pct_35_39 = estimate_percent_total_population_age_35_to_39_years,
         age_pct_40_44 = estimate_percent_total_population_age_40_to_44_years,
         age_pct_45_49 = estimate_percent_total_population_age_45_to_49_years,
         age_pct_50_54 = estimate_percent_total_population_age_50_to_54_years,
         age_pct_55_59 = estimate_percent_total_population_age_55_to_59_years,
         age_pct_60_64 = estimate_percent_total_population_age_60_to_64_years,
         age_pct_65_69 = estimate_percent_total_population_age_65_to_69_years,
         age_pct_70_74 = estimate_percent_total_population_age_70_to_74_years,
         age_pct_75_79 = estimate_percent_total_population_age_75_to_79_years,
         age_pct_80_84 = estimate_percent_total_population_age_80_to_84_years,
         age_pct_85_over = estimate_percent_total_population_age_85_years_and_over
  )
```

## ACS Income

``` r
income_df =
  read_csv("./data/acs_income_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  select(
      -c(id, 
         starts_with("margin_of_error"), 
         starts_with("estimate_families"), 
         starts_with("estimate_married"), 
         starts_with("estimate_nonfamily"), 
         starts_with("estimate_households_percent"))
      ) %>%
  mutate(
    disparity_value = estimate_households_mean_income_dollars - estimate_households_median_income_dollars) 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Estimate!!Households!!Total` = col_double(),
    ##   `Margin of Error!!Households!!Total` = col_double(),
    ##   `Estimate!!Households!!Total!!Less than $10,000` = col_double(),
    ##   `Margin of Error!!Households!!Total!!Less than $10,000` = col_double(),
    ##   `Estimate!!Households!!Total!!$10,000 to $14,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$10,000 to $14,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$15,000 to $24,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$15,000 to $24,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$25,000 to $34,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$25,000 to $34,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$35,000 to $49,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$35,000 to $49,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$50,000 to $74,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$50,000 to $74,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$75,000 to $99,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$75,000 to $99,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$100,000 to $149,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$100,000 to $149,999` = col_double(),
    ##   `Estimate!!Households!!Total!!$150,000 to $199,999` = col_double(),
    ##   `Margin of Error!!Households!!Total!!$150,000 to $199,999` = col_double()
    ##   # ... with 39 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
# mutate(
#    income_class =
#     case_when(
#  estimate_households_mean_income_dollars < 48500 ~ "Low",
#      estimate_households_mean_income_dollars %in% 48500:145500 ~ "Middle",
#      estimate_households_mean_income_dollars > 145500 ~ "High"
#    )) 

# mutate(
#    income_class =
#     case_when(
#  estimate_households_median_income_dollars < 48500 ~ "Low",
#      estimate_households_median_income_dollars %in% 48500:145500 ~ "Middle",
#      estimate_households_median_income_dollars > 145500 ~ "High"
#    )) 

top_disparity = 
  income_df %>% 
  select(county, state, disparity_value) %>% 
  arrange(desc(disparity_value)) %>% 
  knitr::kable()

disparity_plot =
  income_df %>% 
    ggplot(aes(x = disparity_value)) + 
    geom_histogram()
```

#### ACS Race

``` r
race_df =
  read_csv("./data/acs_race_county.csv", skip = 1, na = "null") %>% 
  janitor::clean_names() %>% 
  separate(geographic_area_name, 
           c("county", "state"), sep = ",") %>% 
  select(-c(starts_with("margin_of_error"),
            estimate_total_two_or_more_races,
            estimate_total_two_or_more_races_two_races_including_some_other_race,
            estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races)) %>% 
  rename(
    total_pop = estimate_total,
    white = estimate_total_white_alone,
    black = estimate_total_black_or_african_american_alone,
    am_in_alask = estimate_total_american_indian_and_alaska_native_alone,
    asian = estimate_total_asian_alone,
    hawaii_pi = estimate_total_native_hawaiian_and_other_pacific_islander_alone,
    other = estimate_total_some_other_race_alone
  ) %>% 
  mutate(
    prop_white = white / total_pop * 100,
    prop_black = black / total_pop * 100,
    prop_am_in_alask = am_in_alask / total_pop * 100,
    prop_asian = asian / total_pop * 100,
    prop_hawaii_pi = hawaii_pi / total_pop * 100,
    prop_other = other / total_pop * 100) %>% 
  select(
    id, total_pop, white, prop_white, black, prop_black, asian, prop_asian, am_in_alask, prop_am_in_alask, hawaii_pi,
    prop_hawaii_pi, other, prop_other
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total:` = col_character()
    ## )

    ## See spec(...) for full column specifications.
