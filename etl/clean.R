# For data cleaning

install.packages("janitor",repos = "http://cran.us.r-project.org")
install.packages("lubridate",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")

library(janitor)
# For working with datetime
library(lubridate)
library(tidyverse)

## Functions 

# Function for formatted table output



umd_police_arrest_data <- read_csv(paste0("../data/raw/arrests_data_", today(),".csv"))
umd_police_incident_data <- read_csv(paste0("../data/raw/incidents_data_", today(),".csv"))

# Clean UMD
umd_police_arrest_data_clean = umd_police_arrest_data %>% 
  mutate(date = substr(arrested_date_time_charge,1,8)) %>% 
  rowwise() %>% 
  mutate(time = strsplit(arrested_date_time_charge," ")[[1]][2]) %>% 
  mutate(date = mdy(date), year = year(date), month = month(date)) %>% 
  select(-c(1)) %>% 
  mutate(time_list = (str_split(time,":"))) %>% 
  mutate(time_hour = time_list[[1]]) %>% 
  filter(year >= 2014) %>% 
  select(-time_list)

umd_police_incident_data_clean = umd_police_incident_data %>% 
  mutate(date = substr(occured_date_time_location, 1,8)) %>% 
  rowwise() %>% 
  mutate(time = strsplit(occured_date_time_location, " ")[[1]][2]) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(year = year(date)) %>%
  mutate(time_list = (str_split(time,":"))) %>% 
  mutate(time_hour = time_list[[1]]) %>% 
  rename(type = "_type") %>% 
  filter(year >= 2014)

## get incident_type for arrest data

umpd_incident_type = umd_police_incident_data_clean %>% 
  select(umpd_case_number, type) %>% 
  distinct(umpd_case_number, type)


umd_police_arrest_data_clean_with_type = left_join(umd_police_arrest_data_clean, umpd_incident_type, by = "umpd_case_number")


#### data for arrest totals
cases_by_year = umd_police_arrest_data_clean_with_type %>% 
  distinct(umpd_case_number, .keep_all = TRUE) %>% 
  group_by(year, type) %>% 
  summarise(number = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  slice_max(order_by = number, n = 3) %>% 
  mutate(top_3 = "yes") %>% 
  select(-number)

cases_by_year_og = umd_police_arrest_data_clean_with_type %>% 
  distinct(umpd_case_number, .keep_all = TRUE) %>% 
  group_by(year, type) %>% 
  summarise(number = n()) 

arrest_combined = left_join(cases_by_year_og, cases_by_year, by = c("year", "type")) %>% 
  mutate(top_3 = ifelse(is.na(top_3), "no", top_3)) %>% 
  mutate(final_type = ifelse(top_3 == "yes", type, "Types Other Than Top 3"))

write_rds(arrest_combined, "../police-logs-app/data/arrest_combined.rds")
write_rds(umd_police_arrest_data_clean_with_type, "../police-logs-app/data/umd_arrest.rds")
write_rds(umd_police_incident_data_clean, "../police-logs-app/data/umd_incident.rds")

write_csv(arrest_combined, paste0("../data/processed/arrest_combined_" , today() , ".csv"))
write_csv(umd_police_arrest_data_clean_with_type, paste0("../data/processed/umd_arrest_" , today() , ".csv"))
write_csv(umd_police_incident_data_clean %>% 
            select(-time_list), paste0("../data/processed/umd_incident_" , today() , ".csv"))















