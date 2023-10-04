############################################
## RADS Analysis of NIH Awardees
## Analysis of API data
##
##  2022-03-30
##
############################################
#clear workspace
rm(list=ls())

#required libraries
pacman::p_load(dplyr, tidyr, stringr)

#setwd
setwd("data/NIH")

#read in data
load("NIHdatapull_20220331.Rdata")


#How many umn ones are Unviersity of MN
UNIVERSITY.OF.MINNESOTA %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 

UNIVERSITY.OF.MICHIGAN.AT.ANN.ARBOR %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))


CORNELL.UNIVERSITY %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

DUKE.UNIVERSITY %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

WASHINGTON.UNIVERSITY %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

VIRGINIA.POLYTECHNIC.INST.AND.ST.UNIV %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

#combined universities
alldat <- UNIVERSITY.OF.MINNESOTA %>% 
  filter(organization_org_name == "UNIVERSITY OF MINNESOTA") %>% 
  bind_rows(UNIVERSITY.OF.MICHIGAN.AT.ANN.ARBOR, 
            DUKE.UNIVERSITY, 
            CORNELL.UNIVERSITY, 
            WASHINGTON.UNIVERSITY, 
            VIRGINIA.POLYTECHNIC.INST.AND.ST.UNIV)

alldat %>% 
  group_by(organization_org_name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))



alldat %>% 
  group_by(fiscal_year, organization_org_name) %>% 
  summarize(count =n()) %>% 
  pivot_wider(names_from = organization_org_name, 
              values_from = count) %>% 
  knitr::kable()


department <- alldat %>% 
  group_by(organization_dept_type) %>% 
  summarize(count=n())

#write out csv with all schools
write.csv(alldat, file="CombinedNIH_data_pull.csv", row.names = F)

#write out departments in the data
write.csv(department, file="DepartmentCounts.csv", row.names = F)

## Combine with detailed data pull ##########

#check overlap
summary(publicmandate$id %in% details$id)

#join
pmdetails <- publicmandate %>% 
  mutate(id = as.numeric(id)) %>% 
  left_join(details, by="id")


#disciplines
#environmental science, materials science, psychology, biomedical sciences, and physics

#Look at program
FundProgram <- pmdetails %>% 
  group_by(fundProgramName) %>% 
  summarize(count=n()) 

#write out programs
write.csv(FundProgram, file="FundPrograms.csv", row.names=F)

#write out full dataset 
write.csv(pmdetails, file="CombinedNSF_data_details.csv", row.names = F)
