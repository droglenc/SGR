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
                                         "not fish","other","other")),
         whyNotUsed=factor(whyNotUsed),
         whyNotUsed=relevel(whyNotUsed,"not fish"),
         whyNotUsed2=factor(whyNotUsed2))


## Summary Tables ----
tbl1 <- addmargins(xtabs(~whyNotUsed+year,data=noSGR),margin=2)
round(100*prop.table(tbl1,margin=2),1)
tbl2 <- addmargins(xtabs(~whyNotUsed2+year,data=noSGR),margin=2)
round(100*prop.table(tbl2,margin=2),1)

## Summary Figures ----
sum1 <- group_by(noSGR,year,whyNotUsed) %>%
  summarize(freq=n()) %>%
  mutate(perc=freq/sum(freq)*100)

whyNot <- ggplot(sum1,aes(x=year,y=perc,fill=whyNotUsed)) +
  geom_bar(stat="identity",position=position_fill(reverse=TRUE)) +
  scale_y_continuous(name="% of Reasons Article Not Used",
                     labels=scales::percent,
                     expand=expand_scale(mult=0)) +
  scale_x_continuous(name="Publication Year",
                     breaks=seq(2009,2018,1),
                     expand=expand_scale(mult=0.04)) +
  scale_fill_brewer(palette="Greys",direction=-1) +
  theme_SGR +
  theme(legend.position="right")

whyNot
