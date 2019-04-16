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

SGR <- readr::read_csv("data/SGR.csv") %>%
  mutate(stdeqn=factor(stdeqn,levels=c("Y","N")),
         eqnref=factor(FSA::mapvalues(eqnref,from="N",to="None")),
         eqnref=relevel(eqnref,"None"),
         lenwt=factor(lenwt,levels=c("W","L","both")),
         logbase=FSA::mapvalues(logbase,
                                from=c("10.0","e","unsure"),
                                to=c("10","e","unsure")),
         logbase=factor(logbase,levels=c("e","10","unsure")),
         units2=factor(FSA::mapvalues(units,
                          from=c("%","% bw/day","% mass/day","% weight/day",
                                 "% WM/day","%/day","/day","g/day/%",
                                 "none","unsure"),
                          to=c("%","% W/day","% W/day","% W/day",
                               "% W/day","%/day","/day","g/day/%",
                               "none","unsure"))),
         units2=relevel(units2,"%/day")
         )


## Summary Tables ----
tblSE <- addmargins(xtabs(~stdeqn+year,data=SGR),margin=2)
round(100*prop.table(tblSE,margin=2),1)
tblER <- addmargins(xtabs(~eqnref+year,data=SGR),margin=2)
round(100*prop.table(tblER,margin=2),1)
tblLB <- addmargins(xtabs(~logbase+year,data=SGR),margin=2)
round(100*prop.table(tblLB,margin=2),1)
tblUN <- addmargins(xtabs(~units2+year,data=SGR),margin=2)
round(100*prop.table(tblUN,margin=2),1)
tblLW <- addmargins(xtabs(~lenwt+year,data=SGR),margin=2)
round(100*prop.table(tblLW,margin=2),1)
tblAE <- addmargins(xtabs(~context+year,data=SGR),margin=2)
round(100*prop.table(tblAE,margin=2),1)



