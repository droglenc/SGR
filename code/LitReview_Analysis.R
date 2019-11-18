## Must run wrangle.R script prior to this to retrieve the data from
## GoogleScholar and wrangle it into the needed format which is printed
## out to the CSV files that are read in the chunks below. You can run
## wrangle.R with   source(paste0(here::here(),"/code/wrangle.R"))
library(FSA)
library(dplyr)
library(ggplot2)
library(captioner)
wdir <- here::here()
source(paste0(wdir,"/","code/GGTHEME.R"))
figures <- captioner::captioner(prefix="Figure")
# short helper function
RES <- function(x,digits=1) formatC(x,format="f",digits=digits)

## Rate of SGR usage in fisheries literature
## Read SeachHits data with some cleaning and renaming
SearchHits <- readr::read_csv(paste0(wdir,"/data/SearchHits.csv")) %>%
  dplyr::select(-date,-`Other Notes`) %>%
  dplyr::rename(`GS Returns`=hits) %>%
  ## Compute percent useable citations as 30 divided by the number of articles
  ## examined to get to thirty that met our inclusion criterion. Use this to
  ## estimate the total number of returned articles that would have met our
  ## inclusion criterion
  dplyr::mutate(percUseable=30/num2get30*100,
                `Est. Returns`=round(`GS Returns`*percUseable/100,0)) %>%
  as.data.frame()

## Regression of GoogleScholar results vs year
gs_inc <- lm(`GS Returns`~year,data=SearchHits)
sum_gs_inc <- summary(gs_inc)
pval_gs_inc <- sum_gs_inc$coefficients["year","Pr(>|t|)"]
slp_gs_inc <- coef(gs_inc)["year"]
cislp_gs_inc <- confint(gs_inc)["year",]

## Regression of estimated total number of articles that would meet our
## inclusion criterion vs. year
er_inc <- lm(`Est. Returns`~year,data=SearchHits)
sum_er_inc <- summary(er_inc)
pval_er_inc <- sum_er_inc$coefficients["year","Pr(>|t|)"]
slp_er_inc <- coef(er_inc)["year"]
cislp_er_inc <- confint(er_inc)["year",]

## Range of GS articles returned that met inclusion criterion
minPU <- min(SearchHits$percUseable)
minPUyr <- SearchHits$year[SearchHits$percUseable==minPU]
maxPU <- max(SearchHits$percUseable)
maxPUyr <- SearchHits$year[SearchHits$percUseable==maxPU]

## Read the data that indicates why the articles were not included
noSGR <- readr::read_csv(paste0(wdir,"/","data/noSGR.csv")) %>%
  ## Create a new variable with fewer reasons
  mutate(whyNotUsed2=FSA::mapvalues(whyNotUsed,
                                    from=c("not fish","no access","no SGR",
                                           "no SGR eqn","not English","not focus",
                                           "not peer-reviewed",
                                           "presentation abstract",
                                           "review paper","used mass-specific SGR"),
                                    to=c("not fish","no access","no SGR",
                                         "other","no access","other",
                                         "not peer-reviewed","other","other","other")),
         whyNotUsed=factor(whyNotUsed),
         whyNotUsed=relevel(whyNotUsed,"not fish"),
         whyNotUsed2=factor(whyNotUsed2,levels=c("not fish","no SGR",
                                                 "no access","not peer-reviewed",
                                                 "other")))

## Compute percentage of reasons why not included by year
whyNot_t1 <- addmargins(xtabs(~whyNotUsed+year,data=noSGR),margin=2)
whyNot_p1 <- round(100*prop.table(whyNot_t1,margin=2),1)

## Convert wide format to long format data for ease of plotting below 
SearchHits2 <- SearchHits %>%
  select(year,`GS Returns`,`Est. Returns`) %>%
  tidyr::gather("type","returns",2:3) %>%
  mutate(type=factor(type,levels=c("GS Returns","Est. Returns")))

## Create the figure caption
figures(name="SearchHits",
        caption=paste("Number of articles returned from a Google Scholar search",
                      "using 'specific growth rate' AND fish and the estimated",
                      "number of articles that met our inclusion criteria by",
                      "year from 2009-2018."))

## Plot of search results versus year
SH1 <- ggplot(data=SearchHits2,aes(x=year,y=returns,group=type)) +
  geom_line(size=1,aes(linetype=type)) +
  geom_point(size=2,pch=21,bg="white") +
  scale_y_continuous(name="Number of articles",
                     limits=c(0,NA),expand=expand_scale(mult=c(0,0.03))) +
  scale_x_continuous(name="Publication year",
                     breaks=seq(2009,2018,1),
                     minor_breaks=seq(2009,2018,1),
                     expand=expand_scale(mult=0.04)) +
  annotate(geom="label",x=2011,y=3000,size=3,label="Google Scholar returns") +
  annotate(geom="label",x=2016,y=1100,size=3,
           label="Estimated returns that\nmet inclusion criteria") +
  theme_SGR +
  theme(axis.text.x=element_text(angle=45,hjust=1))
SH1

ggsave("Figure1.pdf",SH1,device="pdf",path=paste0(here::here(),"/code/"),
       width=80,height=80,units="mm",dpi=1000,scale=1.5)


## Characteristics of SGR usage
## Read and prepare data
SGR <- readr::read_csv(paste0(wdir,"/data/SGR.csv")) %>%
  mutate(eqnused=NA_character_,
         eqnused=ifelse(correqn=="Y","correct",eqnused),
         eqnused=ifelse(stdeqn=="Y","typical",eqnused),
         eqnused=ifelse(alteqnExplan %in% c("likely typo",
                                            "not multiplied by 100","no logs"),
                        paste("typical, but",alteqnExplan),eqnused),
         eqnused=ifelse(alteqnExplan %in% c("something else"),"something else",
                        eqnused),
         eqnused=factor(eqnused,levels=c("correct","typical",
                                         "typical, but likely typo",
                                         "typical, but not multiplied by 100",
                                         "typical, but no logs",
                                         "something else")),
         eqnref=factor(FSA::mapvalues(eqnref,from="N",to="None")),
         eqnref=relevel(eqnref,"None"),
         lenwt=factor(lenwt,levels=c("W","L","both")),
         logbase=FSA::mapvalues(logbase,
                                from=c("10.0","e","unsure"),
                                to=c("10","e","unsure")),
         logbase=factor(logbase,levels=c("e","10","unsure")),
         units2=factor(FSA::mapvalues(units,
                                      from=c("% bw/d","% bw/day","% mass/day","% mg/d",
                                             "% w/day","% weight/day","% WM/day","%W/day"),
                                      to=rep("% W/day",8))),
         units2=relevel(units2,"%/day"),
         units3=FSA::mapvalues(units2,from=c("% BW^(-d)","% daily growth increase",
                                             "% W/day","%day","/day","cm",
                                             "g/day","g/day/%","g/Kg/day","mm/day"),
                               to=rep("other",10)),
         units3=factor(units3,levels=c("none","unsure","%/day","%","other"))) %>%
  as.data.frame()

## Context results
tblContext <- xtabs(~context,data=SGR)
( ptblContext <- 100*cbind(A=prop.table(tblContext)[["A"]],
                           binCI(tblContext["A"],sum(tblContext),type="wilson")) )
## Length or weight usage
tblLW <- xtabs(~lenwt,data=SGR)
( ptblLW <- 100*cbind(W=prop.table(tblLW)[["W"]],
                      binCI(tblLW["W"],sum(tblLW),type="wilson")) )
## Equation usage rates
( tblEqnUse <- addmargins(xtabs(~eqnused+year,data=SGR),margin=2) )
( ciEqnUse <- 100*DescTools::MultinomCI(tblEqnUse[,"Sum"],method="wilson"))

## Correct papers
SGRcorrect <- filter(SGR,correqn=="Y") %>%
  droplevels()
( SGRcorrRefs <- xtabs(~eqnref,data=SGRcorrect) )

## Incorrect papers
SGRincorrect <- filter(SGR,correqn=="N") %>%
  droplevels()
( SGRincorrRefs <- xtabs(~eqnref,data=SGRincorrect) ) ## Did not use
( SGRincorrUnits <- xtabs(~units3,data=SGRincorrect) )
( ptblNoUnits <- 100*cbind(NONE=sum(prop.table(SGRincorrUnits)[c("none","unsure")]),
                           binCI(sum(SGRincorrUnits[c("none","unsure")]),
                                 sum(SGRincorrUnits),type="wilson")) )
( ciSGRincorrUnits2 <- 100*DescTools::MultinomCI(SGRincorrUnits[c("%/day","%","other")],
                                                 method="wilson"))
