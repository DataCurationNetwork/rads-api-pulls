##############################################
## NIH API attempts
##
## 2022-03-29
##
##############################################
## https://api.reporter.nih.gov/
#############################################
## limit per call is 500
## offset starts at i+1
## max offset is 9,999

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               curl, 
               jsonlite, 
               rjson, 
               repoRter.nih)



## QUick and dirty - use the R package already created: https://github.com/bikeactuary/repoRter.nih

org_names = c("UNIVERSITY OF MINNESOTA", "UNIVERSITY OF MICHIGAN AT ANN ARBOR", "CORNELL UNIVERSITY", "DUKE UNIVERSITY",  "VIRGINIA POLYTECHNIC INST AND ST UNIV")


for (i in org_names) {
  
  req <- make_req(criteria = 
                    list(
                      fiscal_years = 2013:2022,
                      include_active_projects = FALSE,
                      org_names = i
                    ),
                  include_fields = c("Organization", "FiscalYear", "AwardAmount", "ApplId","SubprojectId", "ProjectNum","ProjectNumSplit","ContactPiName","AllText","FullStudySection", "ProjectStartDate","ProjectEndDate"),
                  message = FALSE)
  
  
  res <- get_nih_data(req,
                      max_pages = 50,
                      flatten_result = TRUE)
  
  assign(paste0(gsub(" ", ".", i)), res)
}




## REPULL WASH U to include city too

req <- make_req(criteria = 
                  list(
                    fiscal_years = 2013:2022,
                    include_active_projects = FALSE,
                    org_names = "WASHINGTON UNIVERSITY", 
                    org_cities = "SAINT LOUIS"
                  ),
                include_fields = c("Organization", "FiscalYear", "AwardAmount", "ApplId","SubprojectId", "ProjectNum","ProjectNumSplit","ContactPiName","AllText","FullStudySection", "ProjectStartDate","ProjectEndDate"),
                message = FALSE)


WASHINGTON.UNIVERSITY <- get_nih_data(req,
                    max_pages = 50,
                    flatten_result = TRUE)

save(list=ls(), file="data/NIH/NIHdatapull_20220331.Rdata")

# #example institution search 
# #curl -X POST "https://api.reporter.nih.gov/v2/projects/search" -H "accept: application/json" -H "Content-Type: application/json" -d "{\"criteria\":{\"org_names\":[\"UNIVERSITY OF MINNESOTA\"]},\"include_fields\":[\"ApplId\",\"SubprojectId\",\"FiscalYear\",\"Organization\",\"ProjectNum\",\"ProjectNumSplit\",\"ContactPiName\",\"AllText\",\"FullStudySection\",\"ProjectStartDate\",\"ProjectEndDate\"],\"offset\":0,\"limit\":25,\"sort_field\":\"project_start_date\",\"sort_order\":\"desc\"}"
# 
# # {
# #   "criteria":
# #     {
# #       "org_names": ["UNIVERSITY OF MINNESOTA"]
# #     },
# #   "include_fields": [
# #     "ApplId","SubprojectId","FiscalYear","Organization", "ProjectNum",
# #     "ProjectNumSplit","ContactPiName","AllText","FullStudySection",
# #     "ProjectStartDate","ProjectEndDate"
# #   ],
# #   "offset":0,
# #   "limit":250,
# #   "sort_field":"project_start_date",
# #   "sort_order":"desc"
# # }
# 
# #curl documentation: https://cran.r-project.org/web/packages/curl/vignettes/intro.html#Customizing_requests
# 
# #Need to create a json object with the request, then send 
# org_names <- "UNIVERSITY OF MINNESOTA"
# include_fields <- c("ApplId","SubprojectId","FiscalYear","Organization", "ProjectNum","ProjectNumSplit","ContactPiName","AllText","FullStudySection", "ProjectStartDate","ProjectEndDate")
# offsetnum <- 0
# criteria <- list()
# 
# criteria$org_names <- org_names
# 
# the_req <- list(
#   criteria = criteria,
#   include_fields = include_fields,
#   offset = offsetnum, 
#   limit = 500,
#   sort_field = 'project_start_date',
#   sort_order = 'desc')
# 
# toJSON(the_req)
