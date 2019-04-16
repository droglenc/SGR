# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DESCRIPTION: ----
# This script loads the SearchHits.CSV file and plots the number of
# GoogleScholar hits of "specific growth rate" AND fish against year and the
# percentage of articles read per year to get 30 that met our criteria for
# inclusion in the analysis by year.
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


## Initialize ----
cat("\014")
setwd(here::here())
library(dplyr)
source("code/GGTHEME.R")

SearchHits <- readr::read_csv("data/SearchHits.csv") %>%
  mutate(percUseable=30/num2get30*100)
SearchHits


## Summary Plots ----
SH1 <- ggplot(SearchHits,aes(x=year,y=hits)) +
  geom_line(size=1) +
  geom_point(size=2.5,pch=21,bg="white") +
  scale_y_continuous(name="Search Results Returned",
                     limits=c(0,NA),expand=expand_scale(mult=c(0,0.03))) +
  scale_x_continuous(name="Publication Year",
                     expand=expand_scale(mult=0.02)) +
  theme_SGR
SH1

SH2 <- ggplot(SearchHits,aes(x=year,y=percUseable)) +
  geom_line(size=1) +
  geom_point(size=2.5,pch=21,bg="white") +
  scale_y_continuous(name="% Useable Articles",
                     limits=c(0,NA),expand=expand_scale(mult=c(0,0.03))) +
  scale_x_continuous(name="Publication Year",
                     expand=expand_scale(mult=0.02)) +
  theme_SGR
SH2

SH1 + SH2

