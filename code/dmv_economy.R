library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)


acs_2023_5yr_vars <-c('MET2013', 'PERWT', 'SEX', 'AGE', 'RACE', 'HISPAN', 'CITIZEN', 
                      'EMPSTAT', 'LABFORCE', 'OCC', 'IND', 'POVERTY', 'INCWAGE')
  
#acs_samps <- ipumsr::get_sample_info('usa')
acs_samps <-ipums_data_collections()
acs_samp <- get_sample_info('usa')

acs2023_5yr <- define_extract_micro(
  collection = "usa",
  description = '2023 ACS 5yr for DMV economy report',
  samples = c('us2023c'),
  variables = acs_2023_5yr_vars) %>% 
  submit_extract() %>% 
  wait_for_extract()

dl_acs2023_5yr <- download_extract(extract = acs2023_5yr,
                               download_dir = "./data",
                               overwrite = TRUE)

acs2023_5yr_final <- read_ipums_micro(dl_acs2023_5yr) %>%
  filter(AGE>=16,CLASSWKRD %in% c(22,23,25,27,28)) %>%
  mutate(
    pop=1,
    wbhao = case_when(
      HISPAN %in% c(1,2,3,4) ~ 3,
      RACE == 1 ~ 1,
      RACE == 2 ~ 2,
      RACE %in% c(4,5,6) ~ 4,
      RACE %in% c(3,6,7,8,9) ~ 5
    ),
    wbhao = labelled(wbhao,c(
      "White" = 1,
      "Black" = 2,
      "Hispanic" = 3,
      "AAPI" = 4,
      "Other" = 5)),
    us_citizen = case_when(
      CITIZEN %in% c(0,1,2) ~ 1,
      CITIZEN ==3 ~0
    ),
    us_citizen = labelled(us_citizen,c(
      "US citizen" = 1,
      "Non US citizen" = 0)),
    construct = case_when(
      IND==770 ~1,
      TRUE ~0)
  )

tipped_workers <- acs %>%
  mutate(
    tipc = case_when(
      emp == 1 ~ 0L,  # Initialize tipc = 0 if emp == 1
      occ18 %in% c(4120, 4130) & ind17 %in% c(8580, 8590, 8660, 8670, 8680, 8690, 8970, 8980, 8990, 9090) ~ 1L,
      occ18 %in% c(4040, 4060, 4110, 4400, 4500, 4510, 4520) ~ 1L,  # pre occ18
      occ18 %in% c(4040, 4110, 4400, 4500, 4510, 4521, 4522, 4525) ~ 1L,  # with occ18
      TRUE ~ NA_integer_  # Default to missing (equivalent to . in Stata)
    )
  )