library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)
library(here)
library(epidatatools)

# Import CPS org data
cps_org <- load_org(2020:2024, "year", "age", "statefips", "wageotc", "union", "orgwgt", "cow1", "occ18", "ind17", "wbhao", "emp",) %>%
  # Age and labor force restrictions.
  filter(statefips == 11, emp == 1)

#define tipped workers
tipped_workers <- cps_org|>
  mutate(
    tipc = case_when(
      occ18 %in% c(4120, 4130) & ind17 %in% c(8580, 8590, 8660, 8670, 8680, 8690, 8970, 8980, 8990, 9090) ~ 1,
      occ18 %in% c(4040, 4110, 4400, 4500, 4510, 4521, 4522, 4525) ~ 1,
      TRUE ~ 0
    )
  )

# Set up workbook
tipped_cps <- wb_workbook()

#Employment count
total_tipped <- tipped_workers |>
  filter(tipc == 1) |>
  summarize(total_emp = sum(tipc * orgwgt/60, na.rm=TRUE),
            n=n())

tipped_cps$add_worksheet(sheet = "total_workers") $
  add_data(x = total_tipped)

# Calculate median wages
tipped_wage <- tipped_workers |>
  summarise(
      median_wage = averaged_median(
        x = wageotc, 
        w = orgwgt/60,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by = tipc)

tipped_cps$add_worksheet(sheet = "median_wage") $
  add_data(x = tipped_wage)

# Save Excel
wb_save(tipped_cps, "./output/tipped_workers_cps.xlsx")
