library(tidyverse)   
library(tidymodels)  
library(rpart)       
library(rpart.plot)  
library(jsonlite)


urledu = "https://data.cityofchicago.org/resource/9xs2-f89t.json"
urlincome = "https://data.cityofchicago.org/resource/t68z-cikk.json" 

rawedu = fromJSON(urledu)
rawincome = fromJSON(urlincome)

colnames(rawedu)[colnames(rawedu) == "community_area_name"] = "community_area"

eduAincome = rawedu%>%
  left_join(
    rawincome,
    by = "community_area"
  )

rawincome = rawincome %>%
  mutate(
    `_50_000_to_74_999`   = as.numeric(`_50_000_to_74_999`),
    `_75_000_to_125_000`  = as.numeric(`_75_000_to_125_000`),
    `_125_000`            = as.numeric(`_125_000`),
    under_25_000          = as.numeric(under_25_000),
    `_25_000_to_49_999`   = as.numeric(`_25_000_to_49_999`),
    male_65               = as.numeric(male_65),
    female_65             = as.numeric(female_65),
    total_population      = as.numeric(total_population)
  )

rawincome = rawincome %>%
  mutate( 
    middlerate = 
      (`_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`) /
      (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`),
    
    povertyrate = 
      under_25_000 /
      (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`),
    oldrate = 
      ((male_65 + female_65)/total_population)
  )

eduAincome = eduAincome %>%
  mutate(
    `_50_000_to_74_999`   = as.numeric(`_50_000_to_74_999`),
    `_75_000_to_125_000`  = as.numeric(`_75_000_to_125_000`),
    `_125_000`            = as.numeric(`_125_000`),
    under_25_000          = as.numeric(under_25_000),
    `_25_000_to_49_999`   = as.numeric(`_25_000_to_49_999`),
    male_65               = as.numeric(male_65),
    female_65             = as.numeric(female_65),
    total_population      = as.numeric(total_population)
  )


eduAincome = eduAincome %>%
  mutate( 
    middlerate = 
      (`_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`) /
        (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`),
    
    povertyrate = 
      under_25_000 /
        (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`),
    oldrate = 
      ((male_65 + female_65)/total_population),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )



finaldata = eduAincome%>%
  select(
    name_of_school,
    school_id,
    community_area,
    college_enrollment_rate,
    cps_performance_policy_level,
    safety_score,
    environment_score,
    instruction_score,
    povertyrate,
    middlerate,
    oldrate,
    latitude,
    longitude
  )%>%
  drop_na()

rawincome%>%
  drop_na()

savepath = "D:/MSCA/PA446/final/Chicago_Education_Analysis_446Final/data/finaldata"
write_csv(finaldata,savepath)
savepath1 = "D:/MSCA/PA446/final/Chicago_Education_Analysis_446Final/data/incomedata"
write_csv(rawincome,savepath1)


colnames(finaldata)
summary(finaldata)

# I utilized the R programming language to programmatically access the City of Chicago's Open Data Portal API. Specifically, I fetched two primary datasets: the "Chicago Public Schools School Profile Information" and "Census Data - Selected Socioeconomic Indicators." By using API calls rather than manual downloads, I ensured the process is reproducible and allows for real-time data updates.

# Using the tidyverse and janitor packages, I performed rigorous data wrangling to prepare the raw datasets for analysis. Key steps included standardizing column names, removing useless columns, and converting data types (e.g., parsing numbers as text into numeric values). I also handled missingness by dropping incomplete records for the machine learning training set and creating a specific pipeline to merge school-level data with community-level socioeconomic metrics using standardized community area names.