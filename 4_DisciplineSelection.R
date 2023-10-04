################################
## NSF, NIH, and DOE Discipline Selection
##
## 2022-04-21
################################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               googlesheets4)


#setwd("~/Documents/NSF_RADS/")


## Read in Data #################################

#read in google sheets
NIHdis <- read_sheet("https://docs.google.com/spreadsheets/d/1iewpFq5eN7d883m0MyT4B6HS7dTQ9oJMIAJ1gUgL3dE/edit#gid=0", sheet = "NIH")

##STOP and request new token to get the sheet to work (need to check all the boxes on the authentication permission page)

NSFdis <-  read_sheet("https://docs.google.com/spreadsheets/d/1iewpFq5eN7d883m0MyT4B6HS7dTQ9oJMIAJ1gUgL3dE/edit#gid=0", sheet = "NSF")
DOEdis <-  read_sheet("https://docs.google.com/spreadsheets/d/1iewpFq5eN7d883m0MyT4B6HS7dTQ9oJMIAJ1gUgL3dE/edit#gid=0", sheet = "DOE")

#read in data
NIH <- read.csv("data/NIH/CombinedNIH_data_pull.csv")
NSF <- read.csv("data/NSF/CombinedNSF_data_details_20220710.csv")
DOE <- read.csv("data/DOE/DOE_combined.csv")


## Data Cleaning ###########################

#### NIH ############################################
#Gather disciplines for each agency
nihdisp <- lapply(NIHdis[,-c(1:2)], function(x) NIHdis$organization_dept_type[which(x==1)])

nihdisp.df <- data.frame(organization_dept_type = unlist(lapply(NIHdis[,-c(1:2)], function(x) NIHdis$organization_dept_type[which(x==1)]), use.names = F), 
                         discipline = rep(names(nihdisp), times=unlist(lapply(nihdisp, length))))

#assign each grant into a discipline, combining multiple ones
NIH$discipline <- NA

for (i in unique(nihdisp.df$organization_dept_type)) {
  #select associated disciplines, collapse multiple as needed
  currentdisp <- paste(nihdisp.df$discipline[which(nihdisp.df$organization_dept_type==i)], collapse = ",")
  
  #assign to grants
  NIH$discipline[which(NIH$organization_dept_type ==  i)] <- currentdisp
  
}

#For NIH, need to aggregate by unique award number, as currently they are broken out by year
#https://www.era.nih.gov/files/Deciphering_NIH_Application.pdf

#Look at project numbers
projnumsplit <- unlist(lapply(strsplit(NIH$project_num, split = "-"), length))
NIH[which(projnumsplit == 4),] 

#remove first number and anything after the dash
NIH$project_num_apptype <- substr(NIH$project_num, start=1, stop=1)
NIH$project_num_fyear <- lapply(strsplit(NIH$project_num, split = "-"), function(x) x[2])
NIH$project_num_cut <- substr(lapply(strsplit(NIH$project_num, split = "-"), function(x) x[1]), 
                          start=2, stop=nchar(lapply(strsplit(NIH$project_num, split = "-"), function(x) x[1])))

#Some odd project numbers that aren't fitting into this - maybe take same PI name with same project date?



## Look at disciplines by institution
NIHdispbyU <- NIH %>% 
  group_by(organization_org_name, discipline) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = organization_org_name, 
              values_from = count) 


write.csv(NIHdispbyU, file="data/NIH_discipline_by_Institution.csv", row.names = F)


## Departments by discipline category
NIHdispbyDept <- NIH %>% 
  group_by(organization_dept_type, discipline) %>% 
  summarize(count=n()) %>% 
  arrange(discipline)

write.csv(NIHdispbyDept, file="data/NIH_discipline_by_Department.csv", row.names=F)



#### NSF #######################################################

#Gather disciplines for each agency
nsfdisp <- lapply(NSFdis[,-c(1:2,8:11)], function(x) NSFdis$fundProgramName[which(x==1)])

nsfdisp.df <- data.frame(fundProgramName = unlist(lapply(NSFdis[,-c(1:2,8:11)], function(x) NSFdis$fundProgramName[which(x==1)]), use.names = F), 
                         discipline = rep(names(nsfdisp), times=unlist(lapply(nsfdisp, length))))


#check if newly included data is missing disicpline classifications
summary(NSF$fundProgramName %in% NSFdis$fundProgramName)
table(NSF$fundProgramName[which(NSF$fundProgramName %in% NSFdis$fundProgramName == F)]) %>% View

#some are caps
summary(tolower(NSF$fundProgramName) %in% tolower(NSFdis$fundProgramName))

 #assign each grant into a discipline, combining multiple ones
NSF$discipline <- NA

for (i in unique(tolower(nsfdisp.df$fundProgramName))) {
  #select associated disciplines, collapse multiple as needed
  currentdisp <- paste(nsfdisp.df$discipline[which(tolower(nsfdisp.df$fundProgramName)==i)], collapse = ",")
  
  #assign to grants
  NSF$discipline[which(tolower(NSF$fundProgramName) ==  i)] <- currentdisp
  
}


## Look at disciplines by institution
NSFdispbyU <- NSF %>% 
  group_by(institution, discipline) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = institution, 
              values_from = count) 


write.csv(NSFdispbyU, file="data/NSF_discipline_by_Institution.csv", row.names = F)


## Departments by discipline category
NSFdispbyDept <- NSF %>% 
  group_by(fundProgramName, discipline) %>% 
  summarize(count=n()) %>% 
  arrange(discipline)

write.csv(NSFdispbyDept, file="data/NSF_discipline_by_Department.csv", row.names=F)


#### DOE ###################################################


#Gather disciplines for each agency
doedisp <- lapply(DOEdis[,-c(1:2)], function(x) DOEdis$`Program Area`[which(x==1)])

doedisp.df <- data.frame(`Program Area` = unlist(lapply(DOEdis[,-c(1:2)], function(x) DOEdis$`Program Area`[which(x==1)]), use.names = F), 
                         discipline = rep(names(doedisp), times=unlist(lapply(doedisp, length))))

#assign each grant into a discipline, combining multiple ones
DOE$discipline <- NA

for (i in unique(doedisp.df$Program.Area)) {
  #select associated disciplines, collapse multiple as needed
  currentdisp <- paste(doedisp.df$discipline[which(doedisp.df$Program.Area==i)], collapse = ",")
  
  #assign to grants
  DOE$discipline[which(DOE$Program.Area ==  i)] <- currentdisp
  
}


## Look at disciplines by institution
DOEdispbyU <- DOE %>% 
  group_by(Institution, discipline) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = Institution, 
              values_from = count) 


write.csv(DOEdispbyU, file="data/DOE_discipline_by_Institution.csv", row.names = F)


## Departments by discipline category
DOEdispbyDept <- DOE %>% 
  group_by(Program.Area, discipline) %>% 
  summarize(count=n()) %>% 
  arrange(discipline)

write.csv(DOEdispbyDept, file="data/DOE_discipline_by_Department.csv", row.names=F)


## Institutional Data by Discipline #############################

## Need to combine all the agency data, and only take "completed" grants.

NIH1 <- NIH %>% 
  select(organization_org_name, discipline, project_num, contact_pi_name, project_start_date, project_end_date, award_amount, organization_dept_type) %>% 
  mutate(agency = "NIH", 
         project_start_date = as.Date(project_start_date), 
         project_end_date = as.Date(project_end_date)) %>% 
  rename(institution = organization_org_name, 
         project_id = project_num, 
         pi_name = contact_pi_name, 
         start_date = project_start_date, 
         end_date = project_end_date, 
         dept_or_program = organization_dept_type)

NSF1 <- NSF %>% 
  mutate(pi_name = paste(piLastName, piFirstName, sep=", ")) %>% 
  select(institution, discipline, id, pi_name, piEmail, startDate, expDate, fundsObligatedAmt, fundProgramName, title, abstractText, agency) %>% 
  mutate(startDate = as.Date(startDate, format = "%m/%d/%Y"), 
         expDate = as.Date(expDate, format = "%m/%d/%Y"), 
         id = as.character(id)) %>% 
  rename(pi_email = piEmail, 
         project_id = id,
         start_date = startDate, 
         end_date = expDate,
         award_amount = fundsObligatedAmt, 
         abstract = abstractText, 
         dept_or_program = fundProgramName)

DOE1 <- DOE %>% 
  select(Institution, discipline, Award.Number, PI, Start.Date, End.Date, Amount.Awarded.to.Date, Program.Area, Title, Abstract) %>% 
  mutate(agency = "DOE", 
         Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"), 
         End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>% 
  rename(institution = Institution, 
         project_id = Award.Number, 
         pi_name = PI, 
         start_date = Start.Date, 
         end_date = End.Date, 
         award_amount = Amount.Awarded.to.Date, 
         dept_or_program = Program.Area, 
         title = Title, 
         abstract = Abstract)


#merge all together
alldat <- bind_rows(NIH1, NSF1, DOE1)

#ensure only awards that are complete are included (End date < 5/1/2022; note NSF does not have an end date, so current awards may be included)
currentawards <- which(alldat$end_date > "2022-05-01")
longagoawards <- which(alldat$start_date < "2013-01-01")

alldat1 <- alldat[-unique(c(currentawards,longagoawards)),]

#Harmonize Institution name
alldat1 <- alldat1 %>% 
  mutate(institution = case_when(grepl("Cornell", institution, ignore.case=T) ~ "Cornell University", 
                                 grepl("Duke", institution, ignore.case=T) ~ "Duke University", 
                                 grepl("Michigan", institution, ignore.case=T) ~ "University of Michigan", 
                                 grepl("Minnesota", institution, ignore.case=T) ~ "University of Minnesota", 
                                 grepl("Virginia", institution, ignore.case=T) ~ "Virginia Tech", 
                                 grepl("Washington", institution, ignore.case=T) ~ "Washington University"))


#remove NA disciplines
alldat2 <- alldat1 %>% 
  filter(!is.na(discipline)) %>% 
  filter(discipline != "not applicable")

alldispbyU <- alldat2 %>% 
  group_by(institution, discipline) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = institution, 
              values_from = count) %>% 
  arrange(nchar(discipline))



write.csv(alldispbyU, file="data/All_discipline_by_institution_20220721.csv", row.names=F)

# Confirm start and end dates of grants
summary(alldat2$start_date)
summary(alldat2$end_date)

### Write out by Institution ######################

#Do the de-duplication of PIs by this step too. Just take most recent grant. 
for (i in unique(alldat2$institution)) {
  temp <- filter(alldat2, institution == i) %>% 
    arrange(nchar(discipline))
  write.csv(temp, file= paste0("data/Data_by_Institution/ResearcherData_", gsub(" ", "_", i), "_20220721.csv"), row.names = F)
  
  temp1 <- filter(alldat2, institution == i) %>% 
    group_by(pi_name) %>% 
    arrange(desc(end_date)) %>% 
    slice_head() %>% 
    arrange(nchar(discipline))
  write.csv(temp1, file= paste0("data/Data_by_Institution/ResearcherData_", gsub(" ", "_", i), "_dedup", "_20220721.csv"), row.names = F)
}


alldispbyU_dedup <- alldat2 %>% 
  group_by(institution, pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head() %>% 
  group_by(institution, discipline) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = institution, 
              values_from = count) %>% 
  arrange(nchar(discipline))

write.csv(alldispbyU_dedup, file="data/All_discipline_by_institution_deduplicatedPI_20220721.csv", row.names = F)

#write out combined de-duped data
alldat3 <- alldat2 %>% 
  group_by(institution, pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head()

write.csv(alldat3, file="data/Combined_data_deduplicated_20220721.csv", row.names=F)


## Compare new counts with old as NSF were not unduely eliminated

oldalldat <- read.csv("data/Combined_data_deduplicated.csv")

oldalldat %>%  
  group_by(institution, pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head() %>% 
  group_by(institution, agency) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = agency, 
              values_from = count)

alldat2 %>% 
  group_by(institution, pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head() %>% 
  group_by(institution, agency) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = agency, 
              values_from = count)



## Number of Missing Emails by Institution ###################3
alldat2 %>% 
  group_by(institution, pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head() %>% 
  group_by(institution) %>% 
  summarize(Nmissing = sum(is.na(pi_email)), 
            N = n(), 
            pMissing = Nmissing/N)
  


## UMN Data Exploration #######################

#UMN emails
# alldat2 %>% 
#   filter(institution == "University of Minnesota", is.na(pi_email)) %>% 
#   write.csv(file="UMN_missingemails.csv", row.names=F)
# 

#read in Colin's edits
umnemails <- read.csv("data/UMN_missingemails_Colin.csv") %>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y"), 
         end_date = as.Date(end_date, "%m/%d/%y"))

#bind back with data
umndata <- alldat2 %>% 
  filter(institution == "University of Minnesota", !is.na(pi_email)) %>% 
  bind_rows(umnemails)

write.csv(umndata, file="Data_by_Institution/ResearcherData_University_of_Minnesota_emails.csv", row.names = F)


## Look at duplicate PIs

#some PIs have multiple grants - and it looks like NIH enters each submission (renewal/report) as a separate line. 
summary(duplicated(tolower(umndata$pi_name)))
summary(duplicated(umndata$pi_email))
length(unique(umndata$pi_name))

dup_pi <- umndata$pi_name[duplicated(tolower(umndata$pi_name))]

#For survey purposes, just take most recent entry to de-dup
umndata_dedup <- umndata %>% 
  group_by(pi_name) %>% 
  arrange(desc(end_date)) %>% 
  slice_head()

umndata_dedup %>% 
  group_by(discipline) %>% 
  summarize(count=n()) %>% 
  arrange(nchar(discipline))


write.csv(umndata_dedup, file="data/Data_by_Institution/ResearcherData_University_of_Minnesota_emails_dedup.csv", row.names = F)

#How many missing emails?
umndata_dedup %>% 
  filter(!grepl("@", pi_email)) %>% 
  View
