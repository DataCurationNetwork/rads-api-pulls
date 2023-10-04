#################################
## Add discipline to List of Completed Respondents
##
## 2022-12-08
#################################


#clear workspace
rm(list=ls())

pacman::p_load(dplyr, 
               tidyr, 
               readxl, 
               stringr, 
               googlesheets4, 
               ggplot2)

setwd("~/Documents/NSF_RADS/")

#read in the complete combined emails
alldat <- read.csv(file="Institution_Data_Complete_Emails/ResearcherData_All_CurrentEmp.csv")


#read in googlesheet with completed responses
complete <- read_sheet("https://docs.google.com/spreadsheets/d/1iLgFTnqf8opc560sHeNy594EE_EJdW2ULcJYJHIYpJI/edit#gid=1076141439", sheet = "(Old Version)Complete_Researcher_ContactList", col_types = "c")


#ensure grant numbers match between datasets
#complete$grant_number <- unlist(complete$grant_number)

summary(complete$grant_number %in% alldat$project_id)

complete$grant_number[which(complete$grant_number %in% alldat$project_id == FALSE )]

complete[which(complete$grant_number == "NA3R21AA026919-01A1S1"),] 
#he is psychology, was under a different grant number

#add in discipline to completes
complete$discipline <- alldat$discipline[match(complete$grant_number, alldat$project_id)]

#add in amount
complete$award_amount <- alldat$award_amount[match(complete$grant_number, alldat$project_id)]

#fill in the non-match
complete[which(is.na(complete$discipline)),]
complete$discipline[which(is.na(complete$discipline))] <- "psychology"

complete$award_amount[which(is.na(complete$award_amount))] <- "7147"

complete$award_amount <- as.numeric(complete$award_amount)

summary(complete$award_amount)

#plot the distributions
complete %>% 
  ggplot(aes(x=award_amount)) +
  geom_histogram(bins = 50) +
  facet_grid(funder_name~.)

complete %>% 
  filter(!is.na(...1)) %>% 
  mutate(award_cat = case_when(award_amount <= 250000 ~ "less than $250K", 
                                between(award_amount,250000, 500000) ~ "$250-500K", 
                                between(award_amount, 500000, 1000000) ~ "$500k-1mil", 
                                award_amount > 1000000 ~ "over $1mil"), 
         award_cat = factor(award_cat, levels = c("less than $250K", "$250-500K", "$500k-1mil", "over $1mil"))) %>% 
  group_by(award_cat, funder_name) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = funder_name, 
              values_from = count)

## add the categories to the completed data
complete <- complete %>% 
  mutate(award_cat = case_when(award_amount <= 250000 ~ "less than $250K", 
                               between(award_amount,250000, 500000) ~ "$250-500K", 
                               between(award_amount, 500000, 1000000) ~ "$500k-1mil", 
                               award_amount > 1000000 ~ "over $1mil"), 
         award_cat = factor(award_cat, levels = c("less than $250K", "$250-500K", "$500k-1mil", "over $1mil")))

boxplot(complete$award_amount ~ complete$funder_name)

by(complete$award_amount, list(complete$funder_name), summary)

# add in the grant code as a column 
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

#write out complete list (all population) as a new google sheet
write_sheet(ss = "https://docs.google.com/spreadsheets/d/1iLgFTnqf8opc560sHeNy594EE_EJdW2ULcJYJHIYpJI/edit#gid=1076141439",
            data = complete,
            sheet="Complete_Researcher_Contact_List_withDiscipline")


#Look at response rates across disciplines
response_table <- complete %>% 
  mutate(responded = ifelse(!is.na(...1), "Response", "No_Response")) %>% 
  group_by(responded, Organization) %>% 
  summarize(count=n()) %>% 
  group_by(Organization) %>% 
  mutate(Total = sum(count)) %>% 
  pivot_wider(names_from = responded, 
              values_from = count) %>% 
  mutate(Response_Rate = round(Response/Total*100, 2)) %>% 
  select(Organization, Response, Total, Response_Rate)



#subset to only those who responded
respondents <- complete %>% 
  filter(!is.na(...1))

response_by_discip <- respondents %>% 
  group_by(Organization, discipline) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = Organization, 
              values_from = count, 
              values_fill = 0)

write.csv(response_by_discip, file="Respondent_data/Response_Counts_by_Discipline.csv", row.names = F) 
