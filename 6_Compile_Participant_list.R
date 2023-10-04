#################################
## Compile participant list from complete de-duped participants
##
## 2022-08-16
#################################


#clear workspace
rm(list=ls())

pacman::p_load(dplyr, 
               tidyr, 
               readxl, 
               stringr)



#read in the complete emails
duke <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_Duke_University_20220721.csv")
michigan <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_University_of_Michigan_20220721.csv")
minnesota <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_University_of_Minnesota_20220721.csv")
vt <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_Virginia_Tech_20220721.csv")
cornell <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_Cornell_University_20220816.csv")
washu <- read.csv("data/Institution_Data_Complete_Emails/ResearcherData_Washington_University_20220816.csv")



minnesota <- minnesota %>% 
  mutate(inst_email = ifelse(grepl("umn", pi_email, ignore.case = T), 1, 0))

michigan <- michigan %>% 
  mutate(inst_email = ifelse(grepl("umich", pi_email, ignore.case = T), 1, 0))

vt <- vt %>% 
  mutate(inst_email = ifelse(grepl("vt", pi_email, ignore.case = T), 1, 0))

duke <- duke %>% 
  mutate(inst_email = ifelse(grepl("duke", pi_email, ignore.case = T), 1, 0))

washu <- washu %>% 
  mutate(inst_email = ifelse(grepl("wustl", pi_email, ignore.case = T), 1, 0))

cornell <- cornell %>% 
  mutate(inst_email = ifelse(grepl("cornell", pi_email, ignore.case = T), 1, 0))

#merge the files
alldat <- bind_rows(minnesota, michigan, vt, duke, washu, cornell)

#look at non-inst emails
alldat %>% 
  group_by(institution, inst_email) %>% 
  summarize(count=n()) %>% 
  group_by(institution) %>% 
  mutate(prop = count/sum(count))


alldat %>% 
  filter(inst_email == 0) %>% 
  select(institution, pi_email) %>% 
  View()

#write out the researchers with current emails. Also need to account for duplicates (names were in caps)

alldat1 <-  alldat %>%
  filter(inst_email == 1) %>% 
  filter(!duplicated(pi_email))

alldat1 %>% 
  group_by(institution) %>% 
  summarize(count=n())

write.csv(alldat1, file="data/Institution_Data_Complete_Emails/ResearcherData_All_CurrentEmp.csv", row.names = F)



## Subset to include the relevant columns for the survey
# PI Name 
# PI Email
# Funder
# Institution
# Grant title (only exists for NSF)
# Grant Number
# Year of award

alldat2 <- alldat1 %>% 
  mutate(start_year = substr(start_date, start=1, stop=4), 
         end_year = substr(end_date, start=1, stop=4), 
         pi_name = str_to_title(pi_name)) %>% 
  select(pi_name, pi_email, agency, institution, title, project_id, start_year, end_year) 


write.csv(alldat2, file="data/Institution_Data_Complete_Emails/Complete_Researcher_ContactList.csv", row.names = F)
