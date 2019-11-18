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
dat2 <- data.frame(species="pirarcu",t=pir_t,W=pir_W0*exp(pir_g*pir_t)) %>%
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




## Nile Tilapia (Azaza et al. 2009) ... SBM0 diet
### Characteristics
til_W0 <- 2.56
til_WF <- 26.77
til_dt <- 45
( til_g <- (log(til_WF)-log(til_W0))/til_dt )

### Growth by exponential model
til_t <- 0:til_dt
dat3 <- data.frame(species="Nile tilapia",t=til_t,W=til_W0*exp(til_g*til_t)) %>%
  mutate(lnW=log(W))

### Estimates of SGR metrics
til_t1 <- 0
til_t2 <- max(dat3$t)
### computed as %/d
til_g1 <- (dat3$lnW[til_t2+1]-dat3$lnW[til_t1+1])/til_dt
formatC(til_g1*100,format="f",digits=3)
til_G1 <- exp(til_g1)-1
formatC(til_G1*100,format="f",digits=3)
# compute as %/month
til_dt2 <- til_dt/30
til_g1m <- (dat3$lnW[til_t2+1]-dat3$lnW[til_t1+1])/til_dt2
formatC(til_g1m*100,format="f",digits=3)
til_G1m <- exp(til_g1m)-1
formatC(til_G1m*100,format="f",digits=3)

### Predicted weights using g1 and G1
dat3$predW_g1 <- dat3$W[til_t1+1]*(1+til_g1)^(dat3$t)
dat3$predW_G1 <- dat3$W[til_t1+1]*(1+til_G1)^(dat3$t)
dat3$predW_pe <- (dat3$predW_g1-dat3$predW_G1)/dat3$predW_G1*100



### Function to computer errors in predicting weight when using Gstar than G
errorInWt <- function(g,deltat) (((1+g)/exp(g))^deltat-1)*100

## Pirarcu example
( pir_Gstar <- pir_g )
( pir_G <- exp(pir_g)-1 )

( pwt_Gstar <- pir_W0*(1+pir_Gstar)^pir_dt )
( pwt_G <- pir_W0*(1+pir_G)^pir_dt )
(pwt_Gstar-pwt_G)/pwt_G*100
errorInWt(pir_Gstar,pir_dt) ## it works

dat <- rbind(dat1,dat2,dat3)

datW <- dplyr::select(dat,species,t,predW_g1,predW_G1) %>%
  tidyr::gather(SGR,predW,3:4) %>%
  dplyr::mutate(SGR=substr(SGR,7,8),
                SGR=factor(SGR,levels=c("G1","g1")),
                species=factor(species,levels=c("Atlantic salmon",
                                                "pirarcu",
                                                "Nile tilapia")))

### Plot the growth trajectories and errors (new Figure 2)
pW <- ggplot() +
  geom_line(data=datW,aes(x=t,y=predW,linetype=SGR,color=species),
            size=1,alpha=0.5) +
  scale_color_manual(values=c("black","black","black")) +
  scale_y_continuous(name="Predicted weight (g)",trans="log10",
                     limits=c(1,3000),expand=expand_scale(mult=c(0,0.03))) +
  annotation_logticks(sides="l") +
  scale_x_continuous(name="Time elapsed (days)",
                     limits=c(0,400),expand=expand_scale(mult=0.04)) +
  annotate(geom="label",x=80,y=2000,label="pirarcu",size=4) +
  annotate(geom="label",x=320,y=600,label="Atlantic salmon",size=4) +
  annotate(geom="label",x=150,y=100,label="Nile tilapia",size=4,hjust=0) +
  geom_line(data=data.frame(x=c(150,49),y=c(90,27)),aes(x=x,y=y),
            arrow=arrow(length=unit(0.05,"inches"),ends="first",type="closed")) +
  theme_SGR


datPE <- dplyr::select(dat,species,t,predW_pe) %>%
  tidyr::gather(var,pe,3) %>%
  dplyr::mutate(species=factor(species,
                levels=c("Atlantic salmon","pirarcu","Nile tilapia"))) %>%
  select(-var)

datPE2 <- rbind(dat1[dat1$t==max(dat1$t),],
                dat2[dat2$t==max(dat2$t),],
                dat3[dat3$t==max(dat3$t),]) %>%
  select(species,t,predW_pe) %>%
  tidyr::gather(var,pe,3) %>%
  dplyr::mutate(species=factor(species,
                  levels=c("Atlantic salmon","pirarcu","Nile tilapia")),
                pelbl=paste0(formatC(pe,format="f",digits=2),"%")) %>%
  select(-var)


## Create generic trajectories of errors
g <- seq(0.01,0.06,0.01)
dt <- seq(0,400)
d <- data.frame(g=rep(g,length(dt)),dt=rep(dt,each=length(g))) %>%
  mutate(err=errorInWt(g,dt)) %>%
  filter(err>-20)

dlastg <- group_by(d,g) %>%
  arrange(g,dt) %>%
  filter(row_number()==n())

pPE <- ggplot(data=d,aes(x=dt,y=err,group=g)) +
  geom_line(size=1,color="gray60") +
  scale_y_continuous(name="Percent error in predicted weight",
                     limits=c(NA,0),expand=expand_scale(mult=0.02)) +
  scale_x_continuous(name="Time elapsed (days)",
                     limits=c(0,450),expand=expand_scale(mult=0.02)) +
  geom_line(data=datPE,aes(x=t,y=pe,group=species),size=1.25) +
  geom_point(data=datPE2,aes(x=t,y=pe,group=species),
             size=2,pch=21,bg="white") +
  geom_label(data=datPE2,aes(x=t,y=pe,group=species,label=species),
             vjust=1.4) +
  geom_text(data=dlastg,aes(x=dt,y=err,group=g),
            label=paste0("g=",formatC(g,format="f",digits=2)),
            hjust=-0.1,vjust=0.05) +
  theme_SGR

pcomb <- pW + pPE

ggsave("Figure2a.pdf",pcomb,device="pdf",path=paste0(here::here(),"/code/"),
       width=180,height=90,units="mm",dpi=1000,scale=1.5)




### Plot the growth trajectories and errors (original Figure 2)
pPE <- ggplot(datPE,aes(x=t,y=pe,group=species)) +
  geom_line(size=1) +
  geom_point(data=datPE2,aes(x=t,y=pe,group=species),
             size=2,pch=21,bg="white") +
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


