############################################
## RADS Analysis of NSF & NIH Awardees
## API pull attempt
##
##  2022-03-26
##
############################################
#clear workspace
rm(list=ls())

#required libraries
pacman::p_load(dplyr, tidyr, stringr)

#No auth appears to be needed
#source token information (defines key, token and oath)
#source('~/UnsyncedDocuments/API_reference/elsevier_token.R', chdir = TRUE)

#setwd
setwd("data/NSF/")

#read in data
umn <- read.csv(file="raw_data/UMN_NSF_results_20220326.csv")
umndun <- read.csv(file="raw_data/UMNPullbyDUN_20220327.csv")
umich <- read.csv(file="raw_data/UMICH_NSF_results_20220328.csv")
cornell <- read.csv(file="raw_data/Cornell_NSF_results_20220328.csv")
duke <- read.csv(file="raw_data/Duke_NSF_results_20220328.csv")
vt <- read.csv(file="raw_data/VirginaTech_NSF_results_20220328.csv")
washu <- read.csv(file="raw_data/WashU_NSF_results_20220328.csv")

#detailed information
details <- read.csv(file="raw_data/CombinedNSF_data_details_20220719.csv")


#How many umn ones are Unviersity of MN
umn %>% 
  filter(grepl("Minnesota", awardeeName, ignore.case=T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

umndun %>% 
  filter(grepl("Minnesota", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

umich %>% 
  filter(grepl("Michigan", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

cornell %>% 
  filter(grepl("Cornell", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

duke %>% 
  filter(grepl("Duke", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

washu %>% 
  filter(grepl("Washington", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

vt %>% 
  filter(grepl("Virginia", awardeeName, ignore.case = T)) %>% 
  group_by(awardeeName) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))


## Filter to the relevant university 
# CAPS appear to be NASA funding
umn1 <- umn %>% 
  filter(awardeeName == "University of Minnesota-Twin Cities") %>% 
  mutate(institution = "University of Minnesota")

umich1 <- umich %>% 
  filter(awardeeName == "Regents of the University of Michigan - Ann Arbor" |
           awardeeName == "UNIVERSITY OF MICHIGAN" |
           awardeeName == "REGENTS OF THE UNIVERSITY OF MICHIGAN (6309)") %>% 
  mutate(institution = "University of Michigan")

cornell1 <- cornell %>% 
  filter(awardeeName == "Cornell University" | awardeeName == "CORNELL UNIVERSITY") %>% 
  mutate(institution =  "Cornell University")

duke1 <- duke %>% 
  filter(awardeeName == "Duke University") %>% 
  mutate(institution = "Duke University")

washu1 <- washu %>% 
  filter(awardeeName == "Washington University" | awardeeName == "WASHINGTON UNIVERSITY THE (3611)") %>% 
  mutate(institution = "Washington University")

vt1 <- vt %>% 
  filter(awardeeName == "Virginia Polytechnic Institute and State University" | 
           awardeeName == "VIRGINIA TECH") %>% 
  mutate(institution = "Virginia Tech")

#combined universities
alldat <- bind_rows(umn1, umich1, cornell1, duke1, washu1, vt1)

alldat %>% 
  group_by(institution) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

## UPDATE 2022-07-19, This is an OA mandate, so do not restrict
#can restrict to public access mandate
summary(factor(alldat$publicAccessMandate))

alldat2 <- 
alldat %>% 
  #filter(publicAccessMandate == 1) %>% 
  mutate(date_c = as.Date(date, format="%m/%d/%Y"), 
         year = str_extract(date, pattern = "20[[:digit:]]{2}"), 
         id = as.integer(id))


alldat2 %>% 
  group_by(year, institution) %>% 
  summarize(count =n()) %>% 
  pivot_wider(names_from = institution, 
              values_from = count) %>% 
  knitr::kable()


#write out csv with all schools
write.csv(alldat, file="data/NSF/CombinedNSF_data_pull.csv", row.names = F)


## Combine with detailed data pull ##########

#check overlap
summary(alldat2$id %in% details$id)

#which are not in the details?
alldat2[which(alldat2$id %in% details$id == FALSE),] %>% View


#join
pmdetails <- alldat2 %>% 
  mutate(id = as.numeric(id)) %>% 
  left_join(details, by="id")


#disciplines
#environmental science, materials science, psychology, biomedical sciences, and physics

#Look at program
FundProgram <- pmdetails %>% 
  group_by(fundProgramName) %>% 
  summarize(count=n()) 

#write out programs
write.csv(FundProgram, file="data/NSF/FundPrograms.csv", row.names=F)

#write out full dataset 
write.csv(pmdetails, file="data/NSF/CombinedNSF_data_details_20220710.csv", row.names = F)

pmdetails <- read.csv("data/NSF/CombinedNSF_data_details_20220710.csv")
