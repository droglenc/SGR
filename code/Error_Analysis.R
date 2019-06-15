library(dplyr)
source(paste0(here::here(),"/","code/GGTHEME.R"))

## Atlantic Salmon (Bell et al. 2010) ... cal FO diet
### Characteristics
as_W0 <- 52.8
as_WF <- 2.75*1000
as_dt <- 55*7
( as_g <- (log(as_WF)-log(as_W0))/as_dt )

### Growth by exponential model
as_t <- 0:as_dt
dat1 <- data.frame(species="Atlantic salmon",t=as_t,W=as_W0*exp(as_g*as_t)) %>%
  mutate(lnW=log(W))

### Estimates of SGR metrics
as_t1 <- 0
as_t2 <- max(dat1$t)
### computed as %/d
as_g1 <- (dat1$lnW[as_t2+1]-dat1$lnW[as_t1+1])/as_dt
formatC(as_g1*100,format="f",digits=3)
as_G1 <- exp(as_g1)-1
formatC(as_G1*100,format="f",digits=3)
# compute as %/month
as_dt2 <- as_dt/30
as_g1m <- (dat1$lnW[as_t2+1]-dat1$lnW[as_t1+1])/as_dt2
formatC(as_g1m*100,format="f",digits=3)
as_G1m <- exp(as_g1m)-1
formatC(as_G1m*100,format="f",digits=3)

### Predicted weights using g1 and G1
dat1$predW_g1 <- dat1$W[as_t1+1]*(1+as_g1)^(dat1$t)
dat1$predW_G1 <- dat1$W[as_t1+1]*(1+as_G1)^(dat1$t)
dat1$predW_pe <- (dat1$predW_g1-dat1$predW_G1)/dat1$predW_G1*100


## Pirarcu
### Characteristics
pir_W0 <- 113.5
pir_WF <- 2630
pir_dt <- 140
( pir_g <- (log(pir_WF)-log(pir_W0))/pir_dt )

### Growth by exponential model
pir_t <- 0:pir_dt
dat2 <- data.frame(species="Pirarcu",t=pir_t,W=pir_W0*exp(pir_g*pir_t)) %>%
  mutate(lnW=log(W))

### Estimates of SGR metrics
pir_t1 <- 0
pir_t2 <- max(dat2$t)
### computed as %/d
pir_g1 <- (dat2$lnW[pir_t2+1]-dat2$lnW[pir_t1+1])/pir_dt
formatC(pir_g1*100,format="f",digits=3)
pir_G1 <- exp(pir_g1)-1
formatC(pir_G1*100,format="f",digits=3)
# compute as %/month
pir_dt2 <- pir_dt/30
pir_g1m <- (dat2$lnW[pir_t2+1]-dat2$lnW[pir_t1+1])/pir_dt2
formatC(pir_g1m*100,format="f",digits=3)
pir_G1m <- exp(pir_g1m)-1
formatC(pir_G1m*100,format="f",digits=3)

### Predicted weights using g1 and G1
dat2$predW_g1 <- dat2$W[pir_t1+1]*(1+pir_g1)^(dat2$t)
dat2$predW_G1 <- dat2$W[pir_t1+1]*(1+pir_G1)^(dat2$t)
dat2$predW_pe <- (dat2$predW_g1-dat2$predW_G1)/dat2$predW_G1*100


### Plot the growth trajectories
dat <- rbind(dat1,dat2)

datW <- dplyr::select(dat,species,t,predW_g1,predW_G1) %>%
  tidyr::gather(SGR,predW,3:4) %>%
  dplyr::mutate(SGR=substr(SGR,7,8),
                SGR=factor(SGR,levels=c("G1","g1")),
                species=factor(species,levels=c("Atlantic salmon",
                                                "Pirarcu")))

datPE <- dplyr::select(dat,species,t,predW_pe) %>%
  tidyr::gather(var,pe,3) %>%
  dplyr::mutate(species=factor(species,levels=c("Atlantic salmon","Pirarcu"))) %>%
  select(-var)

datPE2 <- rbind(dat1[dat1$t==max(dat1$t),],dat2[dat2$t==max(dat2$t),]) %>%
  select(species,t,predW_pe) %>%
  tidyr::gather(var,pe,3) %>%
  dplyr::mutate(species=factor(species,levels=c("Atlantic salmon","Pirarcu")),
                pelbl=paste0(formatC(pe,format="f",digits=2),"%")) %>%
  select(-var)

pW <- ggplot(datW,aes(x=t,y=predW,linetype=SGR,color=species)) +
  geom_line(size=1,alpha=0.5) +
  scale_color_manual(values=c("black","black")) +
  scale_y_continuous(name="Predicted weight (g)",
                     limits=c(0,3000),expand=expand_scale(mult=c(0,0.03))) +
  scale_x_continuous(name="Time elapsed (days)",
                     limits=c(0,400),expand=expand_scale(mult=0.04)) +
  annotate(geom="label",x=80,y=2000,label="pirarcu",size=4) +
  annotate(geom="label",x=320,y=600,label="Atlantic salmon",size=4) +
  theme_SGR

pPE <- ggplot(datPE,aes(x=t,y=pe,group=species)) +
  geom_line(size=1) +
  geom_point(data=datPE2,aes(x=t,y=pe,group=species),
             size=2,pch=21,bg="white") +
  geom_text(data=datPE2,aes(x=t,y=pe,group=species,label=pelbl),
            hjust=c(1.25,-0.25),vjust=c(1,0)) +
  scale_y_continuous(name="Percent error",
                     limits=c(NA,0),expand=expand_scale(mult=0.02)) +
  scale_x_continuous(name="Time elapsed (days)",
                     limits=c(0,400),expand=expand_scale(mult=0.04)) +
  annotate(geom="label",x=75,y=-3,label="pirarcu",size=4) +
  annotate(geom="label",x=290,y=-1,label="Atlantic salmon",size=4) +
  theme_SGR

pcomb <- pW + pPE

ggsave("Figure2.pdf",pcomb,device="pdf",path=paste0(here::here(),"/code/"),
       width=180,height=90,units="mm",dpi=1000,scale=1.5)
