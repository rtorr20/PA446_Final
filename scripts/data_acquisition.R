library(tidyverse)
library(sf)
library(jsonlite)
library(janitor)
library(httr)
library(lubridate)
library(dplyr)


app_token <- CHICAGO_API_TOKEN=GAWxOAqMnBdo8b1KBho8Zq3AG



# 1. Crime data (CPD) 

#API URL for Chicago crime data.
# 6zsd-86xi = "Crimes - 2001 to Present" dataset.
# $where=year=2022 - only keep incidents from 2022
# $limit=500000 - raise the row limit because it only give me 1,000

crime_url <- 
  "https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=year=2022&$limit=500000"

# convert to a data frame, clean column names,
# make sure community_area is numeric, and drop rows with missing community_area

crime_2022 <- fromJSON(crime_url, flatten = TRUE) %>%
  clean_names() %>%
  mutate(community_area = as.numeric(community_area)) %>%
  filter(!is.na(community_area))

# quickly look at the structure
# glimpse(crime_2022)
# head(crime_2022)



# 2. 311 service requests (all types) - 2022

sr_res <- httr::GET(
  url = "https://data.cityofchicago.org/resource/vcbm-nzcv.json",
  query = list(
    "$where"      = "created_date between '2022-01-01T00:00:00' and '2022-12-31T23:59:59'",
    "$limit"      = "500000",      # NOTE: as a STRING, not a number
    "$$app_token" = app_token
  )
)


# Parse JSON → tibble → clean names
sr_2022_raw <- jsonlite::fromJSON(
  httr::content(sr_res, "text"),
  flatten = TRUE,
  simplifyVector = TRUE) %>%
  as_tibble() %>%
  clean_names()


# created a data frame with a "created_date" column
sr_2022 <- sr_2022_raw %>%
  mutate(
    created_date   = ymd_hms(created_date),
    community_area = as.numeric(community_area)) %>%
  filter(year(created_date) == 2022)


sr_2023 <- sr_2022

# look at the structure
# names(sr_2023)
# head(sr_2023$created_date)



# 3. 311 request types


# trying to see what kinds of 311 requests exist
# and decide which ones are "police-related" for the project.
sr_types <- sr_2023 %>% 
  count(sr_type, sort = TRUE)

# Look at the first 50 types
# head(sr_types, 50)
# View(sr_types)



# 4. police-related 311 types

# These are the 311 request types where police are clearly or likely involved:

# - Enforcement (vehicles, stickers, permits, businesses)
# - Public safety concerns (animals, liquor, abandoned buildings)
police_related_types <- c(
  # vehicle / traffic / enforcement
  "Abandoned Vehicle Complaint",
  "City Vehicle Sticker Violation",
  "Vehicle Parked in Bike Lane Complaint",
  
  # business / liquor / public vehicle complaints
  "Liquor Establishment Complaint",
  "Public Vehicle/Valet Complaint",
  "Business Complaints",
  "Shared Housing/Vacation Rental Complaint",
  "Cab Feedback",  # many involve safety/service complaints
  
  # code / property enforcement with safety implications
  "No Building Permit and Construction Violation",
  "Vacant/Abandoned Building Complaint",
  "Sanitation Code Violation",
  "Consumer Fraud Complaint",
  "Consumer Retail Business Complaint",
  
  # animals & wildlife – often require CPD + Animal Control
  "Stray Animal Complaint",
  "Vicious Animal Complaint",
  "Nuisance Animal Complaint",
  "Report an Injured Animal",
  "Animal In Trap Complaint",
  "Coyote Interaction Complaint",
  "Pet Wellness Check Request"
)



# 5. Filtered 311 (from 2022) + police-related requests

# Converted "created_date" to a proper datetime and kept only 2022 requests
sr_2023_clean <- sr_2023 %>%       
  filter(!is.na(community_area))    # include community area

# Keep only the police-related 311 request types (from list above)
sr_police_2023 <- sr_2023_clean %>%
  filter(sr_type %in% police_related_types)

# number of police-related 311 requests by community area
sr_police_ca <- sr_police_2023 %>%
  count(community_area, name = "police_311_count")


# 6. Aggregated crime incidents by community area 

crime_ca <- crime_2022 %>%
  count(community_area, name = "crime_count")



# 7. Combined 311 + crime into one dataset

# full_join to keep community areas that appear in either dataset.
# Replaced NAs with 0, so "no records" = 0 counts.
ca_crime_311_2022 <- full_join(crime_ca, sr_police_ca, by = "community_area") %>%
  replace_na(list(crime_count = 0, police_311_count = 0))

# Save the final dataset 
write_csv(ca_crime_311_2022, "data/ca_crime_311_2022.csv")




