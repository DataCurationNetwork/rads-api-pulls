################################
## DOE data compile
##
## 2022-04-21
################################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               knitr, 
               readxl)

#set working directory
setwd("data/DOE")

#load in data
files <- grep("xlsx", list.files(), value = T)

alldat <- data.frame()

for (i in files) {
  temp <- read_excel(path=i)
 alldat <-  bind_rows(alldat, temp)
}


#write out program areas
ProgramArea <- alldat %>% 
  group_by(`Program Area`) %>% 
  summarize(count=n()) 


#Need to limit to >2013
alldat$StartDate_d <- as.Date(alldat$`Start Date`, format = "%m/%d/%Y")

alldat2013 <- alldat %>% 
  filter(StartDate_d > "2013-01-01")

write.csv(ProgramArea, file="DOE_programareas.csv", row.names = F)

#combined data
write.csv(alldat2013, file="DOE_combined.csv", row.names = F)
