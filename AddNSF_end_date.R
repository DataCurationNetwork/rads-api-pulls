#################################
## Add grant end date for NSF
##
## 2022-07-14
#################################
#Add in the anticipated end date for the NSF grants in the final datasets

pacman::p_load(dplyr, 
               tidyr, 
               readxl)

setwd("~/Documents/NSF_RADS/")

#read in NSF details
nsfdeets <- read.csv("NSF/CombinedNSF_data_details_20220712.csv")

#read in completed emails
UMN <- read.csv("Institution_Data_Complete_Emails/ResearcherData_University_of_Minnesota.csv")
Umich <- read.csv("Institution_Data_Complete_Emails/ResearcherData_University_of_Michigan.csv")
VT <- read.csv("Institution_Data_Complete_Emails/ResearcherData_Virginia_Tech.csv")

#set dates as dates
UMN$end_date <- as.Date(UMN$end_date, format = "%m/%d/%y")
Umich$end_date <- as.Date(Umich$end_date, format = "%m/%d/%y")
VT$end_date <- as.Date(VT$end_date, format = "%m/%d/%y")

#match each with end dates
summary(UMN$project_id %in% nsfdeets$id)

#sanity checking
cbind(UMN$project_id[which(UMN$project_id %in% nsfdeets$id)], na.omit(nsfdeets$id[match(UMN$project_id, nsfdeets$id)]))

#Fill in missing NSF end dates
UMN$end_date[which(UMN$project_id %in% nsfdeets$id)] <- 
as.Date(na.omit(nsfdeets$expDate[match(UMN$project_id, nsfdeets$id)]), format = "%m/%d/%Y")

Umich$end_date[which(Umich$project_id %in% nsfdeets$id)] <- 
  as.Date(na.omit(nsfdeets$expDate[match(Umich$project_id, nsfdeets$id)]), format = "%m/%d/%Y")

VT$end_date[which(VT$project_id %in% nsfdeets$id)] <- 
  as.Date(na.omit(nsfdeets$expDate[match(VT$project_id, nsfdeets$id)]), format = "%m/%d/%Y")

write.csv(VT, file="Institution_Data_Complete_Emails/ResearcherData_Virginia_Tech_enddates.csv", row.names = F)
write.csv(UMN, file="Institution_Data_Complete_Emails/ResearcherData_University_of_Minnesota_enddates.csv", row.names = F)
write.csv(Umich, file="Institution_Data_Complete_Emails/ResearcherData_University_of_Michigan_enddates.csv", row.names = F)
