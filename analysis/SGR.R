# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DESCRIPTION: ----
#
# Loads SGR.CSV (there are odd characters in several of the fields, but not in
# the fields of interest here).
#
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

## Initialize ----
cat("\014")
setwd(here::here())
library(dplyr)
source("code/GGTHEME.R")




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

