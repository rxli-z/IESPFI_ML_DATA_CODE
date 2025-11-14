# Data Cleaning

# Load libraries
library(tidyverse)
library(tidymodels)
library(janitor)
library(xgboost)
library(vip)
library(yardstick)
library(rpart)
library(rpart.plot)
library(polycor)
library(ggplot2)
library(reshape2)
library(EGAnet)
library(factoextra)
library(NbClust)
library(cluster)
library(nnet)
library(glmnet)
library(caret)
library(readr)
library(stringr)
library(labelled)
library(hardhat)
library(themis)


# Load the dataset
pfi_data <- read_csv("pfi_data.csv")

# Inspect the structure of the dataset
glimpse(pfi_data[, c(1:10, (ncol(pfi_data)-9):ncol(pfi_data))])

# Select only relevant variables
vars <- c("AGE2018", "ALLGRADEX", "S19TYPE", "RELATION", 
          "RACEETH", "CSEX", "HHTOTALXX", "TTLHHINC", "PARGRADEX", "ALLGRADEX", #demographic
          "HDAUTISMX", "CENGLPRG", "DSBLTY", "HDIEPX", #student learning needs
          "SESCHWRK", "SEGWORK", "SEGRADEQ", #student academics
          "SEBEHAVX", "SEGBEHAV", "SEABSNT", "SESUSOUT", 
          "SESUSPIN", "SEEXPEL", #student behavior
          "FSNOTESX", "FSMEMO", "FSPHONCHX", "FSSPPERF", 
          "FSSPHW", "FSSPCOUR", "FSSPROLE", #fpp communication
          "FCSCHOOL", "FCTEACHR", "FCSTDS", "FCORDER", 
          "FCSUPPRT", #fpp satisfaction
          "FSATCNFN", "FSCOUNSLR", 
          "FHCHECKX", "FHHELP", #fpp educational involvement
          "FSSPORTX", "FSVOL", "FSMTNG", "FSPTMTNG", 
          "FSFUNDRS", "FSCOMMTE", "FSFREQ" #fpp school involvement
)

# Subset the data to include only relevant variables
pfi_data_clean <- pfi_data %>% 
  select(all_of(vars)) %>%
  mutate(CENGLPRG = recode(CENGLPRG, `-1` = 2)) %>% #recode branch items so skip becomes no
  mutate(HDIEPX = recode(HDIEPX, `-1` = 2)) %>% #recode branch items so skip becomes no
  filter(if_all(everything(), ~ . != -1)) %>% #remove rows where any selected variable is -1 or Valid Skips
  mutate(CENGLPRG = factor(recode(CENGLPRG, '1' = "ELL", '2' = "Non-ELL"))) %>%
  mutate(HDIEPX = factor(recode(HDIEPX, '1' = "IEP", '2' = "Non-IEP")))

# Re-code variables
recode_ord <- c("SEGRADEQ", "FSSPPERF", "FSSPHW", "FSSPCOUR", 
                "FSSPROLE", "FCSCHOOL", "FCTEACHR", "FCSTDS", "FCORDER", 
                "FCSUPPRT") #ordinal variables

recode_dic <- c("SESUSOUT", "SESUSPIN", "SEEXPEL", 
                "FSNOTESX", "FSMEMO", "FSPHONCHX", "FSATCNFN", 
                "FSCOUNSLR", "FSSPORTX", "FSVOL", "FSMTNG", 
                "FSPTMTNG", "FSFUNDRS", "FSCOMMTE") #dichotomous

pfi_data_clean <- pfi_data_clean %>%
  mutate(across(all_of(recode_ord), ~ recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1))) %>% #higher = better
  mutate(across(all_of(recode_dic), ~ recode(., `2` = 0))) %>% #no's becomes 0s
  mutate(SEGRADEQ = recode(SEGRADEQ, `5` = 0)) %>% #failing becomes 0s
  mutate(FHHELP = recode(FHHELP, `5` = 0)) #never becomes 0s

#Check Data
glimpse(pfi_data_clean)

pfi_data_clean <- pfi_data_clean[, c(2, 5:45)]

write.table(pfi_data_clean, file = "pfi_data_01.csv")