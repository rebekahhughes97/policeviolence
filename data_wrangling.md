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
