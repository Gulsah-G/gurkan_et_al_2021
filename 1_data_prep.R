
# PIAAC Data prep.----
# Empty environment.
rm(list=ls())
# Load libraries.
library(dplyr)
library(readr)


# Data prep.
## Loop for Q1 and Q4 dataset separately.
for (i in c("Q1","Q4")) {
  # Load data.
  df <- read_csv(file = paste0("Data\\original\\Data_", i, ".csv"))
  
  # Data prep/cleaning.
  df1 <- df %>% 
    #country id as character
    mutate(CNTRYID = as.character(CNTRYID)) %>% 
    # remove Canada and Slovak Republic.
    filter(!CNTRYID %in% c("Canada","Slovak Republic")) %>% 
    # relevel the factors.
    mutate(AGEG10LFS = relevel(as.factor(AGEG10LFS), ref = 3)) %>% 
    mutate(PARED = relevel(as.factor(PARED), ref = 3)) %>% 
    mutate(BOOKS = relevel(as.factor(BOOKS), ref = 3)) %>% 
    mutate(EDCAT4 = relevel(as.factor(EDCAT4), ref = 4)) %>% 
    # filter if predictors are NA.
    filter(!is.na(PARED), !is.na(BOOKS), !is.na(EDCAT4)) %>% 
    # Create dummies.
    mutate(AGEG10LFS2 = ifelse(AGEG10LFS %in% c(2), 1, 0)) %>%
    mutate(AGEG10LFS3 = ifelse(AGEG10LFS %in% c(3), 1, 0)) %>%
    mutate(AGEG10LFS4 = ifelse(AGEG10LFS %in% c(4), 1, 0)) %>%
    mutate(PARED1 = ifelse(PARED %in% c(1), 1, 0)) %>%
    mutate(PARED2 = ifelse(PARED %in% c(2), 1, 0)) %>%
    mutate(PARED3 = ifelse(PARED %in% c(3), 1, 0)) %>%
    mutate(BOOKS1 = ifelse(BOOKS %in% c(1), 1, 0)) %>%
    mutate(BOOKS2 = ifelse(BOOKS %in% c(2), 1, 0)) %>%
    mutate(BOOKS3 = ifelse(BOOKS %in% c(3), 1, 0)) %>%
    mutate(EDCAT41 = ifelse(EDCAT4 %in% c(1), 1, 0)) %>%
    mutate(EDCAT42 = ifelse(EDCAT4 %in% c(2), 1, 0)) %>%
    mutate(EDCAT43 = ifelse(EDCAT4 %in% c(3), 1, 0)) %>%
    mutate(EDCAT44 = ifelse(EDCAT4 %in% c(4), 1, 0)) %>% 
    # clean VEMETHOD variable
    mutate(VEMETHOD = trimws(as.factor(as.character(VEMETHOD)), "r"))
  
  # Export.
  write_csv(df1, file = paste0("Data\\prepped\\Data_", i, "_v2.csv"))
}

# Sample size check.
rm(list=ls())
df <- read_csv(file = paste0("Data\\prepped\\Data_Q1_v2.csv"))
df %>% group_by(CNTRYID) %>% summarize(n = n()) %>% View()
df <- read_csv(file = paste0("Data\\prepped\\Data_Q4_v2.csv"))
df %>% group_by(CNTRYID) %>% summarize(n = n()) %>% View()

#