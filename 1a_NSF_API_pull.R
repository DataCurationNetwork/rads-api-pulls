############################################
## RADS Pull of NSF & NIH Awardees
## API pull attempt
##
##  2022-03-26
##
############################################
#clear workspace
rm(list=ls())

#required libraries
pacman::p_load("rjson", "dplyr", "jsonlite", "readxl", "textclean", "foreach", "doParallel")

#No auth appears to be needed
#source token information (defines key, token and oath)
#source('~/UnsyncedDocuments/API_reference/elsevier_token.R', chdir = TRUE)

#setwd
#setwd("~/Documents/NSF_RADS/")

#initially explored parallel calls, but did not handle errors well 
# detectCores()
# registerDoParallel(35)

############################
#using NSF API
# The NSF Award Search web API provides a web API interface to the Research.gov's Research Spending and Results data, which provides NSF research award information from 2007.
#Reference this: https://www.research.gov/common/webapi/awardapisearch-v1.htm#sample-requests

#Reference by Awardee name (University of Minnesota) or by DUNS number 
#UMN TC: 555917996
#UofMich AA: 073133571
#GET http://api.nsf.gov/services/v1/awards.{format}?parameters

# awardeename <- c("university+of+minnesota-twin+cities")
# awardeename <- c("university+of+michigan+ann+arbor")
# awardeename <- c("cornell")
# awardeename <- c("duke")
#awardeename <- c("washington+university")
#awardeeCity <- "louis"
awardeename <- c("virginia+tech")
awardeeCity <- "blacksburg"
#duns <- 073133571
metadata_url <- "http://api.nsf.gov/services/v1/awards.json"

#set up pagination
pagination <-  seq(1, 10000, by=25)

allresults <- data.frame()


for (i in awardeename) {
  for (j in pagination) {
    
    cat("on item ", j, "\n")
    
    metadata_params <- list()
    #metadata_params$key <- paste0('apiKey=',key) 
    metadata_params$awardeeName <- paste0('awardeeName=',i)
    #metadata_params$dunsNumber <- paste0('dunsNumber=',i)
    metadata_params$awardeeCity <- paste0('awardeeCity=',awardeeCity)
    metadata_params$offset <- paste0('offset=',j)
    
    #compiles the parameters into a URL for API pull
    metadatareq <- paste0(metadata_url,"/",  "?",paste(unlist(metadata_params),collapse='&')) 
    
    metadata <- fromJSON(metadatareq)
    #metadata$response$award
    
    allresults <- bind_rows(allresults, metadata$response$award)
  }

}




#write out list of awards
# write.csv(allresults, file="data/NSF/raw_data/UMN_NSF_results_20220328.csv", row.names = FALSE)
# write.csv(allresults, file="data/NSF/raw_data/UMICH_NSF_results_20220328.csv", row.names = FALSE)
# write.csv(allresults, file="data/NSF/raw_data/Duke_NSF_results_20220328.csv", row.names = FALSE)
# write.csv(allresults, file="data/NSF/raw_data/Cornell_NSF_results_20220328.csv", row.names = FALSE)
#write.csv(allresults, file="data/NSF/raw_data/WashU_NSF_results_20220328.csv", row.names = FALSE)
write.csv(allresults, file="data/NSF/raw_data/VirginaTech_NSF_results_20220328.csv", row.names = FALSE)


### Pulls for each award ######
## Read in the combined data file for all grants in the database subject to open access mandates, then pull individual information for them. 

#clear workspace
rm(list=ls())

alldat <- read.csv("data/NSF/raw_data/CombinedNSF_data_pull.csv")

#https://api.nsf.gov/services/v1/awards/1052893.json?printFields=primaryProgram,id,fundProgramName,piEmail,awardAgencyCode,fundAgencyCode
metadata_url <- 'http://api.nsf.gov/services/v1/awards'
awardids <- alldat$id
printFields <- 'primaryProgram,id,fundProgramName,piEmail,awardAgencyCode,fundAgencyCode,projectOutComesReport,abstractText,startDate,expDate'
allresults <- data.frame()

awardids[9774:length(awardids)]

for (i in awardids[9774:length(awardids)]) {
    
    cat("on item", which(awardids == i),"of", length(awardids), "\n")
    
    metadata_params <- list()
    metadata_params$printFields <- paste0('printFields=', printFields)
    
    #compiles the parameters into a URL for API pull
    metadatareq <- paste0(metadata_url,"/",i,".json",  "?",paste(unlist(metadata_params),collapse='&')) 
    
    metadata <- fromJSON(metadatareq)
    #metadata$response$award
    
    allresults <- bind_rows(allresults, metadata$response$award)
  }

alldat[which(alldat$id %in% allresults$id == FALSE),] %>% head
  #the ones that are not in the details are NASA studies. 

write.csv(allresults, "data/NSF/raw_data/CombinedNSF_data_details_20220719.csv", row.names = F)

# #using foreach
# testresults <- foreach(j=seq(1, 70000, by=25), .combine = 'bind_rows', .packages = c('rjson', 'jsonlite', 'dplyr'), .inorder=TRUE) %dopar% {
#   metadata_params <- list()
#   #metadata_params$key <- paste0('apiKey=',key) 
#   metadata_params$awardeeName <- paste0('awardeeName=',awardeename)
#   #metadata_params$dunsNumber <- paste0('dunsNumber=',duns)
#   metadata_params$offset <- paste0('offset=',j)
#   
#   #compiles the parameters into a URL for API pull
#   metadatareq <- paste0(metadata_url,"/",  "?",paste(unlist(metadata_params),collapse='&')) 
#   
#   
#   
#   tryCatch(
#     {metadata <- fromJSON(metadatareq)
#     #metadata$response$award
#     
#     metadata$response$award},
#     error = function(e) {paste("error with page", j)})
# }
# 
# write.csv(testresults, file="UMICHPull_20220328.csv")



