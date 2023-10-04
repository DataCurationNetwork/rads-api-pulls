################################
## Email selection 
##
## 2022-06-03
################################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               googlesheets4, 
               readxl)


#setwd("~/Documents/NSF_RADS/")


## Read in data ##################

#deduplicated data

dat <- read.csv(file="data/Combined_data_deduplicated_20220721.csv")

#NIH FOIA data - https://www.nih.gov/institutes-nih/nih-office-director/office-communications-public-liaison/freedom-information-act-office/contact-information-nih-supported-pis
f1 <- read_excel("NIH_FOIAemails/foia-pi-addresses.xls", skip = 10, na = c("", "UNKNOWN", "Not Available", "-", "N/A"))
f2 <- read_excel("NIH_FOIAemails/FY_2015_Funded_Grants_Contact_Listing.xlsx", na = c("", "UNKNOWN", "Not Available", "-", "N/A"))
f3 <- read_excel("NIH_FOIAemails/FY-17-Mailing-and-Email-Addresses-of-NIH-Principal-Investigators-634-17-SA_RFM_20171219.xlsx", skip = 4, na = c("", "UNKNOWN", "Not Available", "-", "N/A"))
f4 <- read_excel("NIH_FOIAemails/FY-18-mailing-email-eddresses-principal-investigators-61-19-CS-LLs-01242019-approved.xlsx", na = c("", "UNKNOWN", "Not Available", "-", "N/A"))
f5 <- read_excel("NIH_FOIAemails/Mailing-and-Email-Addresses-of-NIH-Principal-Investigators-52-17-RFM-SA-JD-approved_20170124.xlsx", skip = 5, na = c("", "UNKNOWN", "Not Available", "-", "N/A"))

#make sure ns are correct
dat %>% 
  group_by(institution) %>% 
  summarize(count=n())

# merge the foia data, then match to the dat
#harmonize names
names(f1)[which(names(f1) == "Principal Investigator Name")] <- "PI_name"
names(f2)[which(names(f2) == "PI Name (Contact)")] <- "PI_name"
names(f3)[which(names(f3) == "PI Name (Contact)")] <- "PI_name"
names(f4)[which(names(f4) == "Contact PI First Namee")] <- "PI_name"
names(f5)[which(names(f5) == "PI Name (Contact)")] <- "PI_name"

names(f1)[which(names(f1) == "PI Email Address")] <- "PI_email"
names(f2)[which(names(f2) == "PI Email")] <- "PI_email"
names(f3)[which(names(f3) == "PI Email")] <- "PI_email"
names(f4)[which(names(f4) == "Contact PI Email")] <- "PI_email"
names(f5)[which(names(f5) == "PI Email")] <- "PI_email"

names(f1)[which(names(f1) == "Institute or Center")] <- "Institution"
names(f4)[which(names(f4) == "Organization")] <- "Institution"

#combine data files
foia <- bind_rows(f1, f2, f3, f4, f5)

#how many valid emails?
summary(is.na(foia$PI_email))

#how many names from our data are in the foia data?
summary(tolower(dat$pi_name) %in% tolower(foia$PI_name))


#select emails, covert PI name to lowercase for better matching
foiaemail <- foia %>% 
  mutate(PI_name = tolower(PI_name)) %>% 
  filter(!is.na(PI_email)) %>% 
  select(PI_name, PI_email, Institution) 

#select only our institutions
grep("DUKE", foiaemail$Institution, ignore.case = T, value=T)
Institution_list <- c("UNIVERSITY OF MINNESOTA", "UNIVERSITY OF MICHIGAN", "CORNELL UNIVERSITY","WASHINGTON UNIVERSITY", "VIRGINIA POLYTECHNIC INST AND ST UNIV", "DUKE UNIVERSITY")

foiaemail1 <- foiaemail %>% 
  filter(Institution %in% Institution_list)

#remove duplicates
summary(duplicated(foiaemail1))

foiaemail2 <- foiaemail1[!duplicated(foiaemail1),]

summary(tolower(dat$pi_name) %in% tolower(foiaemail1$PI_name))

#add email to data
missingemails <- which(is.na(dat$pi_email))

summary(is.na(dat$pi_email))

for (i in missingemails) {
  if (tolower(dat$pi_name[i]) %in% foiaemail2$PI_name) {
    dat$pi_email[i] <- foiaemail2$PI_email[match(tolower(dat$pi_name[i]), foiaemail2$PI_name)]
  }
}

#Look at missing emails by Institution
dat %>% 
  mutate(missingemail = ifelse(is.na(pi_email), "missing", "present")) %>% 
  group_by(institution, missingemail) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from = missingemail, 
              values_from = count)

#write out data
#main data
write.csv(dat, file="data/Combined_data_deduplicated_foiaemails_20220721.csv", row.names = F)

for (i in unique(dat$institution)) {
  temp <- filter(dat, institution == i)
  write.csv(temp, file=paste0("data/Data_by_Institution/ResearcherData_", gsub(" ", "_", i), "_dedup_foiaemails.csv"), row.names = F)
}


## Check UMN Counts ######

#complete count-wise data missing emails
umn_count <- read.csv("data/Data_by_Institution/ResearcherData_University_of_Minnesota_dedup_foiaemails.csv", strip.white = T)

umn_emails <- read_excel("data/Data_by_Institution/ResearcherData_University_of_Minnesota_emails_complete.xlsx", )

summary(tolower(umn_count$pi_name) %in% tolower(umn_emails$pi_name))
summary(tolower(umn_count$project_id) %in% tolower(umn_emails$project_id))

umn_count[which(tolower(umn_count$project_id) %in% tolower(umn_emails$project_id)==FALSE),] %>% View


#add the NSF ones to the complete email list
umn_emails_complete <- umn_emails %>% 
  mutate(start_date = as.character(start_date)) %>% 
  bind_rows(umn_count[which(tolower(umn_count$project_id) %in% tolower(umn_emails$project_id)==FALSE),])

summary(duplicated(umn_emails_complete$pi_name))
  #ah these are it

dupnames <- umn_emails_complete$pi_name[duplicated(umn_emails_complete$pi_name)]

umn_emails_complete %>% 
  filter(pi_name %in% dupnames) %>% View

#keep the most recent start date for them

umn_emails_complete2 <- umn_emails_complete %>% 
  arrange(pi_name, desc(start_date)) %>% 
  filter(!duplicated(pi_name))


summary(is.na(umn_emails_complete2$pi_email))

#write this out as completed dataset
write.csv(umn_emails_complete2, file="data/Institution_Data_Complete_Emails/ResearcherData_University_of_Minnesota.csv", row.names=F)
