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
               googlesheets4)

setwd("data/")

#read in the complete combined emails
alldat <- read.csv(file="Institution_Data_Complete_Emails/ResearcherData_All_CurrentEmp.csv")


#read in googlesheet with completed responses
complete <- read_sheet("https://docs.google.com/spreadsheets/d/1iLgFTnqf8opc560sHeNy594EE_EJdW2ULcJYJHIYpJI/edit#gid=1076141439", sheet = "Complete_Researcher_ContactList", col_types = "c")


#ensure grant numbers match between datasets
#complete$grant_number <- unlist(complete$grant_number)

summary(complete$grant_number %in% alldat$project_id)

complete$grant_number[which(complete$grant_number %in% alldat$project_id == FALSE )]

complete[which(complete$grant_number == "NA3R21AA026919-01A1S1"),] 
#he is psychology, was under a different grant number

#add in discipline to completes
complete$discipline <- alldat$discipline[match(complete$grant_number, alldat$project_id)]

#fill in the non-match
complete[which(is.na(complete$discipline)),]
complete$discipline[which(is.na(complete$discipline))] <- "psychology"


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
