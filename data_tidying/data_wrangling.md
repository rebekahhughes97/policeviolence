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
    age, gender, race, police_dept, disposition, year,
    month, day, city, state, county, cause_of_death, criminal_charges,
    symptoms_of_mental_illness) %>% 
  view()
```

In the above code chunk, the MPV dataset has been manipulated and tidied
by reading in the excel sheet and cleaning the variable names,
separating out the date variable, changing the names of some of the
variables and changing some variables to be numeric, and selecting only
the variables of importance to our analysis for inclusion in the tidied
dataset.

## Lat/Long Dataset

``` r
lat_long =
  read_csv("./data/uscities.csv") %>% 
  mutate(state = state_id) %>% 
  select(city, lat, lng, state)
```

    ## Parsed with column specification:
    ## cols(
    ##   city = col_character(),
    ##   city_ascii = col_character(),
    ##   state_id = col_character(),
    ##   state_name = col_character(),
    ##   county_fips = col_character(),
    ##   county_name = col_character(),
    ##   lat = col_double(),
    ##   lng = col_double(),
    ##   population = col_double(),
    ##   density = col_double(),
    ##   source = col_character(),
    ##   military = col_logical(),
    ##   incorporated = col_logical(),
    ##   timezone = col_character(),
    ##   ranking = col_double(),
    ##   zips = col_character(),
    ##   id = col_double()
    ## )

``` r
mpv_final = left_join(mpv_df, lat_long, by = c("city" = "city", "state" = "state"))

write_csv(mpv_final, "./data/mpv_final.csv")
```

## USA October 2020 Dataset

``` r
usa_df =
  read_excel("./data/usa_2020_oct.xlsx") %>%
  janitor::clean_names() %>% 
  separate(event_date, c("year", "month", "day")) %>%
  mutate(
    state = admin1,
    county = admin2,
    city = location,
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    fatalities = as.numeric(fatalities)) %>%
  select(
    year, month, day, event_type, sub_event_type, state, county, city, latitude, longitude, fatalities)

write_csv(usa_df, "./data/usa_tidy.csv")
```

## ACS Datasets

#### Age and Sex

Using the ACS age/sex dataset to create an age only dataset:

``` r
# getting the total ages by county and tidying

age_total_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_total_total"))) %>% 
  select(-c("estimate_total_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>%
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "total",
    names_prefix = "estimate_total_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age)
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
# getting the percentage of ages by county and tidying

age_percent_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_percent_total"))) %>% 
  select(-c("estimate_total_total_population", "estimate_percent_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>%
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "percent",
    names_prefix = "estimate_percent_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age)
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
# joining the total and percentages together into one dataset

age_df = 
  left_join(age_total_df, age_percent_df, by = c("id", "county", "state", "age")) %>% view()
  
# export tidy age dataset

write_csv(age_df, "./data/age_tidy.csv")
```

Using the ACS age/sex dataset to create an age by sex dataset:

``` r
# read in total for males by age

male_total_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_male_total"))) %>% 
  select(-c("estimate_total_total_population", "estimate_male_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "total",
    names_prefix = "estimate_male_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age),
    sex = "male"
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
# read in total for females by age

female_total_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_female_total"))) %>% 
  select(-c("estimate_total_total_population", "estimate_female_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>%
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "total",
    names_prefix = "estimate_female_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age),
    sex = "female"
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
# bind male/female total dfs together

sex_total_df =
  bind_rows(male_total_df, female_total_df) %>% 
  relocate(id, county, state, age, sex) %>% 
  view()

# read in percent for males by age

male_percent_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_percent_male"))) %>% 
  select(-c("estimate_total_total_population", "estimate_percent_male_total_population", contains(c("selected_age_categories", "summary_indicators", "allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "percent",
    names_prefix = "estimate_percent_male_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age),
    sex = "male"
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
# read in percent for females by age

female_percent_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with("estimate_percent_female"))) %>% 
  select(-c("estimate_total_total_population", "estimate_percent_female_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>%
  pivot_longer(
    4:21,
    names_to = "age",
    values_to = "percent",
    names_prefix = "estimate_percent_female_total_population_age_"
  ) %>%
  mutate(
    age = str_replace_all(age, "_", " "),
    age = str_replace(age, "years", ""),
    age = str_replace_all(age, "under 5", "0-5"),
    age = str_replace_all(age, "85  and over", "85+"),
    age = as.factor(age),
    sex = "female"
  ) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
# bind male/female percent dfs together

sex_percent_df =
  bind_rows(male_percent_df, female_percent_df) %>% 
  relocate(id, county, state, age, sex) %>% 
  view()

# join percent and total dfs together

sex_df = 
  left_join(sex_total_df, sex_percent_df, by = c("id", "county", "state", "age", "sex")) %>% view()
  
# export sex by age dataset
write_csv(sex_df, "./data/age_sex_tidy.csv")
```

Age by Sex dataset (not tidy)

``` r
male_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with(c("estimate_male_total", "estimate_percent_male")))) %>% 
  select(-c("estimate_total_total_population", "estimate_male_total_population", "estimate_percent_male_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  rename_at(
    vars(starts_with("estimate_male_total_population")), funs(str_replace(., "estimate_male_total_population_age", "male")),
    ) %>% 
  rename_at(
        vars(starts_with("estimate_percent_male_total_population")), funs(str_replace(., "estimate_percent_male_total_population_age", "male_percent"))
  ) %>% 
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
female_df =
  read_csv("./data/acs_age_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(c(1:3, starts_with(c("estimate_female_total", "estimate_percent_female")))) %>% 
  select(-c("estimate_total_total_population", "estimate_female_total_population", "estimate_percent_female_total_population", contains(c("selected_age_categories", "summary_indicators", "percent_allocated")))) %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>%
  rename_at(
    vars(starts_with("estimate_female_total_population")), funs(str_replace(., "estimate_female_total_population_age", "female")),
    ) %>% 
  rename_at(
        vars(starts_with("estimate_percent_female_total_population")), funs(str_replace(., "estimate_percent_female_total_population_age", "female_percent"))
  ) %>% 
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total!!Total population` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!Under 5 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!15 to 19 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!20 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!25 to 29 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!30 to 34 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!45 to 49 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!AGE!!50 to 54 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!5 to 14 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!Under 18 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 44 years` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SELECTED AGE CATEGORIES!!75 years and over` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error!!Total!!Total population!!SUMMARY INDICATORS!!Age dependency ratio` = col_character()
    ##   # ... with 161 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
sex_df2 =
  left_join(male_df, female_df, by = c("id", "county", "state")) %>%
  mutate(
    county = str_replace(county, " County", ""),
    county = str_replace(county, " Parish", ""),
    county = str_replace(county, " Borough", ""),
    county = str_trim(county, side = "right"),
    state = str_trim(state, side = "left")) %>% view()

write_csv(sex_df2, "./data/sex2.csv")
```

Age dataset (not tidy)

``` r
age_df2 =
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
  ) %>%
  mutate(
    county = str_replace(county, " County", ""),
    county = str_replace(county, " Parish", ""),
    county = str_replace(county, " Borough", ""),
    county = str_trim(county, side = "right"),
    state = str_trim(state, side = "left"))

write_csv(age_df2, "./data/age2.csv")
```

#### ACS Income

Tidy income dataset:

``` r
income_df =
  read_csv("./data/acs_income_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  select(
      -c( 
         starts_with("margin_of_error"), 
         starts_with("estimate_families"), 
         starts_with("estimate_married"), 
         starts_with("estimate_nonfamily"), 
         starts_with("estimate_households_percent"))
      ) %>%
  mutate(
    disparity_value = estimate_households_mean_income_dollars - estimate_households_median_income_dollars,
    median_income = estimate_households_mean_income_dollars,
    mean_income = estimate_households_median_income_dollars
    ) %>% 
  pivot_longer(
    5:14,
    names_to = "total_income",
    names_prefix = "estimate_households_total_",
    values_to = "estimate_households"
  ) %>% 
  select(-c(estimate_households_total, estimate_households_mean_income_dollars, estimate_households_median_income_dollars)) %>%
  relocate(c(id, county, state, total_income, estimate_households, mean_income, median_income)) %>% 
  mutate(
    total_income = str_replace_all(total_income, "_", " "),
    total_income = str_replace_all(total_income, "less than 10 000", "0-10,000"),
    total_income = str_replace_all(total_income, "200 000 or more", "200,000+"),
    total_income = str_replace_all(total_income, " to ", "-"),
    total_income = str_replace_all(total_income, " ", ","),
    total_income = as.factor(total_income)
  ) %>% 
  mutate(
   income_class =
    case_when(
     median_income < 55000 ~ "Very Low",
     median_income %in% 55000:59999 ~ "Low",
     median_income %in% 60000:64999 ~ "Middle",
     median_income %in% 65000:74999 ~ "High",
     median_income > 75000 ~ "Very High"
   ),
   income_class = fct_relevel(income_class, "Very Low", "Low", "Middle", "High", "Very High")
  ) %>% 
  view()
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
# export tidy income dataset
write_csv(income_df, "./data/income_tidy.csv")
```

Income dataset (not tidy)

``` r
income_df =
  read_csv("./data/acs_income_county.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  separate(geographic_area_name, c("county", "state"), sep = ",") %>% 
  select(
      -c( 
         starts_with("margin_of_error"), 
         starts_with("estimate_families"), 
         starts_with("estimate_married"), 
         starts_with("estimate_nonfamily"), 
         starts_with("estimate_households_percent"))
      ) %>%
  mutate(
    disparity_value = estimate_households_mean_income_dollars - estimate_households_median_income_dollars) %>% 
mutate(
   income_class =
    case_when(
     estimate_households_median_income_dollars < 55000 ~ "Very Low",
     estimate_households_median_income_dollars %in% 55000:59999 ~ "Low",
     estimate_households_median_income_dollars %in% 60000:64999 ~ "Middle",
     estimate_households_median_income_dollars %in% 65000:74999 ~ "High",
     estimate_households_median_income_dollars > 75000 ~ "Very High"
   ),
   income_class = fct_relevel(income_class, "Very Low", "Low", "Middle", "High", "Very High")
  ) %>%
  mutate(
    county = str_replace(county, " County", ""),
    county = str_replace(county, " Parish", ""),
    county = str_replace(county, " Borough", ""),
    county = str_trim(county, side = "right"),
    state = str_trim(state, side = "left"))
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
lat_long2 =
  read_csv("./data/uscities.csv") %>% 
  mutate(state = state_id) %>% 
  select(city, lat, lng, state, state_name)
```

    ## Parsed with column specification:
    ## cols(
    ##   city = col_character(),
    ##   city_ascii = col_character(),
    ##   state_id = col_character(),
    ##   state_name = col_character(),
    ##   county_fips = col_character(),
    ##   county_name = col_character(),
    ##   lat = col_double(),
    ##   lng = col_double(),
    ##   population = col_double(),
    ##   density = col_double(),
    ##   source = col_character(),
    ##   military = col_logical(),
    ##   incorporated = col_logical(),
    ##   timezone = col_character(),
    ##   ranking = col_double(),
    ##   zips = col_character(),
    ##   id = col_double()
    ## )

``` r
mpv_final2 = left_join(mpv_df, lat_long2, by = c("city" = "city", "state" = "state"))

#combines MPV data and non-tidy income data
build_df = 
  left_join(mpv_final2, income_df, 
            by = c("county" = "county", "state_name" = "state"))

#MPV data and non-tidy income data csv
write_csv(build_df, "./data/build_df.csv")

#combine non-tidy income data and protest data
protest_income_df = 
  left_join(usa_df, income_df, 
            by = c("county" = "county", "state" = "state"))

write_csv(protest_income_df, "./data/protest_income_df.csv")
```

#### ACS Race

Tidy race dataset

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
  pivot_longer(
    5:10,
    names_to = "race",
    values_to = "total"
  ) %>% 
  mutate(
    prop = total / total_pop * 100
    ) %>% 
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total:` = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
# export tidy race dataset
write_csv(race_df, "./data/race_tidy.csv")
```

Race dataset (not tidy)

``` r
race_df2 =
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
    id, county, state, total_pop, white, prop_white, black, prop_black, asian, prop_asian, am_in_alask, prop_am_in_alask, hawaii_pi,
    prop_hawaii_pi, other, prop_other
  ) %>%
  mutate(
    county = str_replace(county, " County", ""),
    county = str_replace(county, " Parish", ""),
    county = str_replace(county, " Borough", ""),
    county = str_trim(county, side = "right"),
    state = str_trim(state, side = "left"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   `Margin of Error!!Total:` = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
write_csv(race_df2, "./data/race2.csv")
```

### Joining non-tidy ACS data with the protest data:

``` r
race_df =
  read_csv("./data/race2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   total_pop = col_double(),
    ##   white = col_double(),
    ##   prop_white = col_double(),
    ##   black = col_double(),
    ##   prop_black = col_double(),
    ##   asian = col_double(),
    ##   prop_asian = col_double(),
    ##   am_in_alask = col_double(),
    ##   prop_am_in_alask = col_double(),
    ##   hawaii_pi = col_double(),
    ##   prop_hawaii_pi = col_double(),
    ##   other = col_double(),
    ##   prop_other = col_double()
    ## )

``` r
age_df =
  read_csv("./data/age2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
sex_df =
  read_csv("./data/sex2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
income_df =
  read_csv("./data/income2.csv") %>% 
  select(-estimate_households_total) %>% 
  rename_at(
    vars(starts_with("estimate_households_total")), funs(str_replace(., "estimate_households_total_", "")),
    ) %>% 
  rename_at(
        vars(starts_with("estimate_households")), funs(str_replace(., "estimate_households_", ""))
  ) %>% 
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   estimate_households_total = col_double(),
    ##   estimate_households_total_less_than_10_000 = col_double(),
    ##   estimate_households_total_10_000_to_14_999 = col_double(),
    ##   estimate_households_total_15_000_to_24_999 = col_double(),
    ##   estimate_households_total_25_000_to_34_999 = col_double(),
    ##   estimate_households_total_35_000_to_49_999 = col_double(),
    ##   estimate_households_total_50_000_to_74_999 = col_double(),
    ##   estimate_households_total_75_000_to_99_999 = col_double(),
    ##   estimate_households_total_100_000_to_149_999 = col_double(),
    ##   estimate_households_total_150_000_to_199_999 = col_double(),
    ##   estimate_households_total_200_000_or_more = col_double(),
    ##   estimate_households_median_income_dollars = col_double(),
    ##   estimate_households_mean_income_dollars = col_double(),
    ##   disparity_value = col_double(),
    ##   income_class = col_character()
    ## )

``` r
usa_protest_df =
  read_csv("./data/usa_tidy.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   month = col_double(),
    ##   day = col_double(),
    ##   event_type = col_character(),
    ##   sub_event_type = col_character(),
    ##   state = col_character(),
    ##   county = col_character(),
    ##   city = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   fatalities = col_double()
    ## )

``` r
# joining protest data with acs data (nontidy)
protest_demo_df = 
  left_join(usa_protest_df, income_df,
            by = c("county" = "county", "state" = "state")) %>% 
  left_join(sex_df,
            by = c("county" = "county", "state" = "state", "id" = "id")) %>% 
  left_join(race_df,
            by = c("county" = "county", "state" = "state", "id" = "id")) %>% 
  view()

# export as csv
write_csv(protest_demo_df, "./data/protest_demo_df.csv")
```

### Joining MPV data and non-tidy ACS data

``` r
race_df =
  read_csv("./data/race2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   total_pop = col_double(),
    ##   white = col_double(),
    ##   prop_white = col_double(),
    ##   black = col_double(),
    ##   prop_black = col_double(),
    ##   asian = col_double(),
    ##   prop_asian = col_double(),
    ##   am_in_alask = col_double(),
    ##   prop_am_in_alask = col_double(),
    ##   hawaii_pi = col_double(),
    ##   prop_hawaii_pi = col_double(),
    ##   other = col_double(),
    ##   prop_other = col_double()
    ## )

``` r
age_df =
  read_csv("./data/age2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
sex_df =
  read_csv("./data/sex2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   id = col_character(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
mpv_df3 = 
  read_csv("./data/mpv_final.csv")
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
lat_long3 =
  read_csv("./data/uscities.csv") %>% 
  mutate(state = state_id) %>% 
  select(city, lat, lng, state, state_name)
```

    ## Parsed with column specification:
    ## cols(
    ##   city = col_character(),
    ##   city_ascii = col_character(),
    ##   state_id = col_character(),
    ##   state_name = col_character(),
    ##   county_fips = col_character(),
    ##   county_name = col_character(),
    ##   lat = col_double(),
    ##   lng = col_double(),
    ##   population = col_double(),
    ##   density = col_double(),
    ##   source = col_character(),
    ##   military = col_logical(),
    ##   incorporated = col_logical(),
    ##   timezone = col_character(),
    ##   ranking = col_double(),
    ##   zips = col_character(),
    ##   id = col_double()
    ## )

``` r
mpv_final3 = left_join(mpv_df3, lat_long3, by = c("city" = "city", "state" = "state"))

#combines MPV data and non-tidy acs data
mpv_demo_df = 
  left_join(mpv_final3, income_df,
            by = c("county" = "county", "state_name" = "state")) %>% 
  left_join(sex_df,
            by = c("county" = "county", "state_name" = "state", "id" = "id")) %>% 
  left_join(race_df,
            by = c("county" = "county", "state_name" = "state", "id" = "id")) %>% 
  view()

#MPV data and non-tidy acs data csv
write_csv(mpv_demo_df, "./data/mpv_demo_df.csv")
```
