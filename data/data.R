library(tidyverse)   
library(tidymodels)  
library(rpart)       
library(rpart.plot)  
library(jsonlite)


urledu = "https://data.cityofchicago.org/resource/9xs2-f89t.json"
urlincome = "https://data.cityofchicago.org/resource/t68z-cikk.json" 

rawedu = fromJSON(urledu)
rawcrim = fromJSON(urlcrim)
rawincome = fromJSON(urlincome)

colnames(rawedu)[colnames(rawedu) == "community_area_name"] = "community_area"

eduAincome = rawedu%>%
  left_join(
    rawincome,
    by = "community_area"
  )

eduAincome <- eduAincome %>%
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


eduAincome <- eduAincome %>%
  mutate( 
    middlerate = percent(
      (`_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`) /
        (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`)
    ),
    
    povertyrate = percent(
      under_25_000 /
        (under_25_000 + `_25_000_to_49_999` + `_50_000_to_74_999` + `_75_000_to_125_000` + `_125_000`)
    ),
    oldrate = percent(
      ((male_65 + female_65)/total_population)
    )
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
    oldrate
  )