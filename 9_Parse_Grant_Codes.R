#################################
## Parse Grant Codes
##
## 2022-12-08
#################################


#clear workspace
rm(list=ls())

pacman::p_load(dplyr, 
               tidyr, 
               readxl, 
               stringr, 
               googlesheets4)

setwd("~/Documents/NSF_RADS/")


#read in googlesheet with completed responses
complete <- read_sheet("https://docs.google.com/spreadsheets/d/1iLgFTnqf8opc560sHeNy594EE_EJdW2ULcJYJHIYpJI/edit#gid=1076141439", sheet = "Complete_Researcher_Contact_List_withDiscipline", col_types = "c")

#NIH Grant codes
# DP - Institutional Training and Director Program Projects - (0 responded)
# F — fellowships (10 responded)
# K — career development awards (18 responded)
# N — research contracts (0 responded)
# P — program project and research center grants (0 responded)
# R — research project grants - majority
# S — research-related programs (equipment etc.) (0 responded)
# T — training grants (2 responded)
# U — cooperative agreements (13 responded)
# Y — interagency agreements (0 responded)

complete <- complete %>% 
  mutate(GrantCode = case_when(grepl("^[[:digit:]]DP", grant_number) ~ "DP",
                               grepl("^[[:digit:]]F", grant_number) ~ "F",
                               grepl("^[[:digit:]]K", grant_number) ~ "K", 
                               grepl("^[[:digit:]]N", grant_number) ~ "N", 
                               grepl("^[[:digit:]]P", grant_number) ~ "P", 
                               grepl("^[[:digit:]]R", grant_number) ~ "R", 
                               grepl("^[[:digit:]]S", grant_number) ~ "S", 
                               grepl("^[[:digit:]]T", grant_number) ~ "T", 
                               grepl("^[[:digit:]]U", grant_number) ~ "U", 
                               grepl("^[[:digit:]]Y", grant_number) ~ "Y"))

#fix one added that has an NA in front of it
complete$GrantCode[which(complete$grant_number == "NA3R21AA026919-01A1S1")] <- "R"


table_grant_codes <- complete %>% 
  mutate(Responded = ifelse(!is.na(response), "Responded", "Non_Response")) %>% 
  group_by(GrantCode, Responded, funder_name) %>% 
  summarize(count=n()) %>% 
  group_by(GrantCode, funder_name) %>% 
  mutate(Total = sum(count)) %>% 
  pivot_wider(names_from = Responded, 
              values_from = count, 
              values_fill = 0) %>% 
  mutate(Percent_Respond = round(Responded/Total*100,2)) %>% 
  arrange(desc(Percent_Respond))

write.csv(table_grant_codes, file="Respondent_data/Responses_by_grant_codes.csv", row.names=F)
