#################################
## Add grant end date for NSF
## Also add in the extra NSF grants without OA mandates
##
## 2022-07-20
#################################
#Add in the anticipated end date for the NSF grants in the final datasets

#clear workspace
rm(list=ls())

pacman::p_load(dplyr, 
               tidyr, 
               readxl)

setwd("data/")

#read in new grants list
updatedgrants <- read.csv("Combined_data_deduplicated_foiaemails_20220721.csv")

#read in completed emails
UMN <- read.csv("Institution_Data_Complete_Emails/Old_data/ResearcherData_University_of_Minnesota.csv")
Umich <- read.csv("Institution_Data_Complete_Emails/Old_data/ResearcherData_University_of_Michigan.csv")
VT <- read.csv("Institution_Data_Complete_Emails/Old_data/ResearcherData_Virginia_Tech.csv")
Duke <- read.csv("Institution_Data_Complete_Emails/Old_data/ResearherData_Duke.csv")
Cornell <- read.csv("Institution_Data_Complete_Emails/ResearherData_cornell.csv")
WashU <- read.csv("Institution_Data_Complete_Emails/ResearherData_washu.csv")

#set dates as dates
UMN$end_date <- as.Date(UMN$end_date, format = "%m/%d/%y")
Umich$end_date <- as.Date(Umich$end_date, format = "%m/%d/%y")
VT$end_date <- as.Date(VT$end_date, format = "%m/%d/%y")
Duke$end_date <- as.Date(Duke$end_date, format = "%m/%d/%y")
Cornell$end_date <- as.Date(Cornell$end_date, format = "%m/%d/%y")
WashU$end_date <- as.Date(WashU$end_date, format = "%m/%d/%y")


## Add in completed emails to the new grant cuts (all NSF regardless of OA mandates, start date cut off of  >2011-01-01 and end date < 2022-05-01)

### Minnesota ####
UMN_update <- updatedgrants %>% 
  filter(institution == "University of Minnesota")

summary(UMN$project_id %in% UMN_update$project_id)
summary(UMN_update$project_id %in% UMN$project_id)

UMN$project_id[which(UMN$project_id %in% UMN_update$project_id==F)][order(UMN$project_id[which(UMN$project_id %in% UMN_update$project_id==F)])]
UMN_update$project_id[which(UMN_update$project_id %in% UMN$project_id==F)][order(UMN_update$project_id[which(UMN_update$project_id %in% UMN$project_id==F)])]


#Match in emails for what we have to the updated lists
missingemails <- which(is.na(UMN_update$pi_email))

for (i in missingemails) {
  if (UMN_update$project_id[i] %in% UMN$project_id) {
    UMN_update$pi_email[i] <- UMN$pi_email[match(UMN_update$project_id[i], UMN$project_id)]
  }
}

summary(is.na(UMN_update$pi_email))
  #Maybe these are different grants for the same person?? Appears to be a newer grant for the same people...

#run again matching on name??
missingemails <- which(is.na(UMN_update$pi_email))

for (i in missingemails) {
  if (trimws(UMN_update$pi_name[i]) %in% UMN$pi_name) {
    UMN_update$pi_email[i] <- UMN$pi_email[match(trimws(UMN_update$pi_name[i]), UMN$pi_name)]
  }
}


summary(is.na(UMN_update$pi_email)) #YAY

## write out
write.csv(UMN_update, file="Institution_Data_Complete_Emails/ResearcherData_University_of_Minnesota_20220721.csv", row.names = F)



## Michigan #####

Umich_update <- updatedgrants %>% 
  filter(institution == "University of Michigan")

summary(Umich$project_id %in% Umich_update$project_id)
summary(Umich_update$project_id %in% Umich$project_id)


#Match in emails for what we have to the updated lists
missingemails <- which(is.na(Umich_update$pi_email))

for (i in missingemails) {
  if (Umich_update$project_id[i] %in% Umich$project_id) {
    Umich_update$pi_email[i] <- Umich$pi_email[match(Umich_update$project_id[i], Umich$project_id)]
  }
}

summary(is.na(Umich_update$pi_email))
#Maybe these are different grants for the same person?? Appears to be a newer grant for the same people...

#run again matching on name??
missingemails <- which(is.na(Umich_update$pi_email))

for (i in missingemails) {
  if (trimws(Umich_update$pi_name[i]) %in% Umich$pi_name) {
    Umich_update$pi_email[i] <- Umich$pi_email[match(trimws(Umich_update$pi_name[i]), Umich$pi_name)]
  }
}


summary(is.na(Umich_update$pi_email)) #YAY

## write out
write.csv(Umich_update, file="Institution_Data_Complete_Emails/ResearcherData_University_of_Michigan_20220721.csv", row.names = F)


### VT #####

VT_update <- updatedgrants %>% 
  filter(institution == "Virginia Tech")

summary(VT$project_id %in% VT_update$project_id)
summary(VT_update$project_id %in% VT$project_id)



#Match in emails for what we have to the updated lists
missingemails <- which(is.na(VT_update$pi_email))

for (i in missingemails) {
  if (VT_update$project_id[i] %in% VT$project_id) {
    VT_update$pi_email[i] <- VT$pi_email[match(VT_update$project_id[i], VT$project_id)]
  }
}

summary(is.na(VT_update$pi_email))

#run again matching on name??
missingemails <- which(is.na(VT_update$pi_email))

for (i in missingemails) {
  if (gsub(" ", "", VT_update$pi_name[i]) %in% gsub(" ", "", VT$pi_name)) {
    VT_update$pi_email[i] <- VT$pi_email[match(gsub(" ", "", VT_update$pi_name[i]), gsub(" ", "", VT$pi_name))]
  }
}

summary(is.na(VT_update$pi_email))

## write out
write.csv(VT_update, file="Institution_Data_Complete_Emails/ResearcherData_Virginia_Tech_20220721.csv", row.names = F)


### Duke ######

Duke_update <- updatedgrants %>% 
  filter(institution == "Duke University")

summary(Duke$project_id %in% Duke_update$project_id)
summary(Duke_update$project_id %in% Duke$project_id)


#Match in emails for what we have to the updated lists
missingemails <- which(is.na(Duke_update$pi_email))

for (i in missingemails) {
  if (Duke_update$project_id[i] %in% Duke$project_id) {
    Duke_update$pi_email[i] <- Duke$pi_email[match(Duke_update$project_id[i], Duke$project_id)]
  }
}

summary(is.na(Duke_update$pi_email))
#Maybe these are different grants for the same person?? Appears to be a newer grant for the same people...

#run again matching on name??
missingemails <- which(is.na(Duke_update$pi_email))

for (i in missingemails) {
  if (trimws(Duke_update$pi_name[i]) %in% Duke$pi_name) {
    Duke_update$pi_email[i] <- Duke$pi_email[match(trimws(Duke_update$pi_name[i]), Duke$pi_name)]
  }
}


summary(is.na(Duke_update$pi_email)) #YAY

## write out
write.csv(Duke_update, file="Institution_Data_Complete_Emails/ResearcherData_Duke_University_20220721.csv", row.names = F)


## Cornell ######
Cornell_update <- updatedgrants %>% 
  filter(institution == "Cornell University")

summary(Cornell$project_id %in% Cornell_update$project_id)
summary(Cornell_update$project_id %in% Cornell$project_id)

Cornell$project_id[which(Cornell$project_id %in% Cornell_update$project_id==F)][order(Cornell$project_id[which(Cornell$project_id %in% Cornell_update$project_id==F)])]
Cornell_update$project_id[which(Cornell_update$project_id %in% Cornell$project_id==F)][order(Cornell_update$project_id[which(Cornell_update$project_id %in% Cornell$project_id==F)])]


#Match in emails for what we have to the updated lists
missingemails <- which(is.na(Cornell_update$pi_email))

for (i in missingemails) {
  if (Cornell_update$project_id[i] %in% Cornell$project_id) {
    Cornell_update$pi_email[i] <- Cornell$pi_email[match(Cornell_update$project_id[i], Cornell$project_id)]
  }
}

summary(is.na(Cornell_update$pi_email))
#Maybe these are different grants for the same person?? Appears to be a newer grant for the same people...

# #run again matching on name??
# missingemails <- which(is.na(Cornell_update$pi_email))
# 
# for (i in missingemails) {
#   if (trimws(Cornell_update$pi_name[i]) %in% Cornell$pi_name) {
#     Cornell_update$pi_email[i] <- Cornell$pi_email[match(trimws(Cornell_update$pi_name[i]), Cornell$pi_name)]
#   }
# }
# 
# 
# summary(is.na(Cornell_update$pi_email)) #YAY

## write out
write.csv(Cornell_update, file="Institution_Data_Complete_Emails/ResearcherData_Cornell_University_20220816.csv", row.names = F)



## Wash U ######

WashU_update <- updatedgrants %>% 
  filter(institution == "Washington University")

summary(WashU$project_id %in% WashU_update$project_id)
summary(WashU_update$project_id %in% WashU$project_id)

WashU$project_id[which(WashU$project_id %in% WashU_update$project_id==F)][order(WashU$project_id[which(WashU$project_id %in% WashU_update$project_id==F)])]
WashU_update$project_id[which(WashU_update$project_id %in% WashU$project_id==F)][order(WashU_update$project_id[which(WashU_update$project_id %in% WashU$project_id==F)])]


#Match in emails for what we have to the updated lists
missingemails <- which(is.na(WashU_update$pi_email))

for (i in missingemails) {
  if (WashU_update$project_id[i] %in% WashU$project_id) {
    WashU_update$pi_email[i] <- WashU$pi_email[match(WashU_update$project_id[i], WashU$project_id)]
  }
}

summary(is.na(WashU_update$pi_email))
#Maybe these are different grants for the same person?? Appears to be a newer grant for the same people...

#run again matching on name??
missingemails <- which(is.na(WashU_update$pi_email))

for (i in missingemails) {
  if (trimws(WashU_update$pi_name[i]) %in% WashU$pi_name) {
    WashU_update$pi_email[i] <- WashU$pi_email[match(trimws(WashU_update$pi_name[i]), WashU$pi_name)]
  }
}


summary(is.na(WashU_update$pi_email)) #YAY

## write out
write.csv(WashU_update, file="Institution_Data_Complete_Emails/ResearcherData_Washington_University_20220816.csv", row.names = F)



