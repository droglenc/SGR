# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DESCRIPTION: ----
# This script reads the data file from GoogleSheets, make a local Excel version
# of it. It reads the Hits sheet, wrangles it for later use, and writes it out
# as a CSV file. It reads in the individual year results sheets, wrangles them
# for later use, stacks them so all years are in one data.frame, splits that
# data.frame into those articles that used SGR and those that did not (but we
# examined for use of SGR), and writes those those data.frames out to CSV files.
# These files are used in the analysis scripts.
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

## Initialize ----
cat("\014")
setwd(here::here())
library(dplyr)


## Get Data ----
### Bring the googlesheets to SGR_Review.xlsx in the data folder
fn <- googledrive::as_id("https://docs.google.com/spreadsheets/d/1-hOiMPmVX7dKbWyIi0CTAlU7kPgoFg-kYmiRAoYOu1M/edit#gid=1733151943")
googledrive::drive_download(file=fn,path="data/raw/SGR_Review",overwrite=TRUE)

### Search data
SearchHits <- readxl::read_excel("data/raw/SGR_Review.xlsx",sheet="Hits") %>%
  rename(year=Year,hits=Hits,date=`Date of Search`) %>%
  mutate(num2get30=as.numeric(NA))

### Get all Years Data
#### List all years of data
yrs <- 2009:2011

#### Create a data.frame to receive the results ... must load one year's of data
#### to get the structure of the data.frame.
tmp <- readxl::read_excel("data/raw/SGR_Review.xlsx",sheet="2009") %>%
  select(-ArticleURL,-GSRank,-USE,-RAND)
SGR <- noSGR <- tmp[0,]
rm(tmp)

#### Cycle through the years of data
for (i in yrs) {
  # Read data, remove unneeded variables, change RAND to a list from 1 to
  # last, reduce to only those records it took to get to 30 useable articles
  res <- readxl::read_excel("data/raw/SGR_Review.xlsx",sheet=as.character(i)) %>%
    select(-ArticleURL,-GSRank) %>%
    mutate(RAND=1:nrow(.)) %>%
    filter(RAND<=RAND[which(USE==30)])
  # Add a line to SearchHits that is the number read to get to 30
  SearchHits$num2get30[SearchHits$year==i] <- nrow(res)
  # Separate into the 30 articles read and the others examined but not read.
  tmpSGR <- filter(res,!is.na(USE)) %>%
    select(-USE,-RAND)
  tmpNoSGR <- filter(res,is.na(USE)) %>%
    select(-USE,-RAND)
  # Append to data.frame
  SGR <- rbind(SGR,tmpSGR)
  noSGR <- rbind(noSGR,tmpNoSGR)
}

SGR <- select(SGR,-whyNotUsed,-SGR) %>%
  rename(year=Year)
noSGR <- select(noSGR,-(SGR:context)) %>%
  rename(year=Year)


## Write Data files ----
write.csv(SearchHits,"data/SearchHits.csv",row.names=FALSE,quote=FALSE)
write.csv(SGR,"data/SGR.csv",row.names=FALSE)
write.csv(noSGR,"data/noSGR.csv",row.names=FALSE)

