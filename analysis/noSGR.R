# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DESCRIPTION: ----
# This script loads the noSGR.CSV file (note there are odd characters in 
# several of the fields, but not in the whyNotUsed field which is the main
# field of interest here) and 
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


## Initialize ----
cat("\014")
setwd(here::here())
library(dplyr)
source("code/GGTHEME.R")

noSGR <- readr::read_csv("data/noSGR.csv") %>%
  mutate(whyNotUsed2=FSA::mapvalues(whyNotUsed,
                                    from=c("no access","no SGR","not English",
                                           "not fish","not focus","not peer-reviewed"),
                                    to=c("other","other","other",
                                         "not fish","other","other")))


## Summary Tables ----
tbl1 <- addmargins(xtabs(~whyNotUsed+year,data=noSGR),margin=2)
round(100*prop.table(tbl1,margin=2),1)
tbl2 <- addmargins(xtabs(~whyNotUsed2+year,data=noSGR),margin=2)
round(100*prop.table(tbl2,margin=2),1)
