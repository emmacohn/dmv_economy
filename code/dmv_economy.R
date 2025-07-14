library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)
library(here)
library(ipumsr)
library(epidatatools)

# Download ACS data

acs_2023_5yr_vars <-c('MET2013', 'PERWT', 'SEX', 'AGE', 'RACE', 'HISPAN', 'CITIZEN', 
                      'EMPSTAT', 'LABFORCE', 'OCC', 'IND', 'POVERTY', 'INCWAGE', 'PWSTATE2')
  
#acs_samps <- ipumsr::get_sample_info('usa')
acs_samps <-ipums_data_collections()
acs_samp <- get_sample_info('usa')

acs2023_5yr <- define_extract_micro(
  collection = "usa",
  description = '2023 ACS 5yr for DMV economy report',
  samples = c('us2023c'),
  variables = acs_2023_5yr_vars) |>
  submit_extract() |> 
  wait_for_extract()

dl_acs2023_5yr <- download_extract(extract = acs2023_5yr,
                               download_dir = here("/projects/ecohn/dmv_economy/data"),
                               overwrite = TRUE)

# CLean ACS data for employed DMV workers

acs2023_5yr_final <- read_ipums_micro(dl_acs2023_5yr) |>
  filter(PWSTATE2 == 11, EMPSTAT == 1) |>
  janitor::clean_names() |>
  mutate(
    pop=1,
    wbhao = case_when(
      hispan %in% c(1,2,3,4) ~ 3,
      race == 1 ~ 1,
      race == 2 ~ 2,
      race %in% c(4,5,6) ~ 4,
      race %in% c(3,6,7,8,9) ~ 5
    ),
    wbhao = labelled(wbhao,c(
      "White" = 1,
      "Black" = 2,
      "Hispanic" = 3,
      "AAPI" = 4,
      "Other" = 5)),
    us_citizen = case_when(
      citizen %in% c(0,1,2) ~ 1,
      citizen ==3 ~0
    ),
    us_citizen = labelled(us_citizen,c(
      "US citizen" = 1,
      "Non US citizen" = 0))
  )

# Define tipped workers

tipped_workers <- acs2023_5yr_final |>
  mutate(
    tipc = case_when(
      occ %in% c(4120, 4130) & ind %in% c(8580, 8590, 8660, 8670, 8680, 8690, 8970, 8980, 8990, 9090) ~ 1,
      occ %in% c(4040, 4110, 4400, 4500, 4510, 4521, 4522, 4525) ~ 1,
      TRUE ~ 0
    )
  )

# Set up Excel
tipped_acs <- wb_workbook()

# Number of workers

total_tipped <- tipped_workers |>
  count(tipc == 1, wt = perwt)

tipped_acs$add_worksheet(sheet = "total_workers") $
  add_data(x = total_tipped)

#Demographics by race and gender 
tipped_race <- tipped_workers |>
  filter(tipc == 1) |>
  mutate(race = to_factor(wbhao)) |> 
  summarize(total_emp = sum(empstat * perwt, na.rm=TRUE),
            n=n(),
            .by=race) |>
  mutate(share = total_emp/sum(total_emp))

tipped_gender <- tipped_workers |>
  filter(tipc == 1) |>
  mutate(gender = to_factor(sex)) |> 
  summarize(total_emp = sum(empstat * perwt, na.rm=TRUE),
            n=n(),
            .by=gender) |>
  mutate(share = total_emp/sum(total_emp))

race_gender <- tipped_workers |>
  filter(tipc == 1) |>
  mutate(gender = to_factor(sex),
         race = to_factor(wbhao)) |> 
  summarize(total_emp = sum(empstat * perwt, na.rm=TRUE),
            n=n(),
            .by= c(race, gender)) |>
  mutate(share = total_emp/sum(total_emp))

tipped_acs$add_worksheet(sheet = "by_demographics") $
  add_data(x = race_gender)

# Poverty rates
tipped_pov <- tipped_workers |>
  filter(poverty != 0) |>  # exclude N/A poverty values
  mutate(below_poverty = if_else(poverty <= 100, 1, 0),
         poverty_200 = if_else(poverty <= 200, 1, 0)) |>
  summarize(total_emp = sum(empstat * perwt, na.rm=TRUE),
            total_pov = sum(below_poverty * perwt, na.rm=TRUE),
            total_pov_200 = sum(poverty_200 * perwt, na.rm = TRUE),
            n=n(),
            .by = tipc) |>
  mutate(pov_share = total_pov/total_emp,
         pov_200_share = total_pov_200/total_emp)

tipped_acs$add_worksheet(sheet = "poverty_rate") $
  add_data(x = tipped_pov)

# Median earnings
tipped_inc <- tipped_workers |>
 # filter(tipc == 1) |>
  summarise(
      earnings_median = averaged_median(
        x = incwage, 
        w = perwt,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by = tipc)

tipped_acs$add_worksheet(sheet = "median_earnings") $
  add_data(x = tipped_inc)

# Age group (teenagers vs not teenagers)
tipped_teen <- tipped_workers |>
  filter(tipc == 1) |>
  mutate(teenager = if_else(age <= 19, 1, 0)) |>
  summarize(total_emp = sum(empstat * perwt, na.rm=TRUE),
            total_teen = sum(teenager * perwt, na.rm=TRUE),
            n=n()) |>
  mutate(teen_share = total_teen/total_emp)

tipped_acs$add_worksheet(sheet = "teen_share") $
  add_data(x = tipped_teen)

# Save Excel
wb_save(tipped_acs, "./output/tipped_workers_acs.xlsx")
