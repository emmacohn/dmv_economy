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
cps_org <- load_org(2019:2024, "year", "age", "statefips", "wage", "union", "orgwgt", "a_earnhour", "cow1") %>%
  # Age and labor force restrictions (exclude self-employed and self-incorporated), non-imputed wages.
  filter(age >= 16, cow1 <= 5, a_earnhour != 1, !is.na(wage))
