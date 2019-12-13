#import library 
library(tidyverse)
library(readxl)

#read datasets
districts <- read_csv("data/districts.csv")

dis_to_county <- read_xls("data/data_district_to_county_crosswalk.xls") %>% 
  rename(system = `District Number`,
         county = `County Name`,
         county_number = `County Number`)

tvaas <- read_csv("data/tvaas.csv") %>%
  rename(system = `District Number`,
         `tvaas_composite` = `Composite`,
         `tvaas_literacy` = `Literacy`,
         `tvaas_numeracy` = `Numeracy`) %>%
  select(-`District Name`)

income <- read_csv("data/income.csv") %>% 
  rename(county_number  = county,
         median_income = Median_Income) %>%
  select(-NAME)

#merge the datasets
edu_income <- districts %>%
  inner_join(tvaas, by = "system") %>%
  inner_join(dis_to_county, by = "system") %>%
  inner_join(income, by = "county_number") %>%
  mutate(bhn = black + hispanic + native) %>% 
  select(-black,-hispanic,-native)
  