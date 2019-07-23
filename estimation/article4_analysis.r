

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
library(polycor)

## This loads the data workspace where skattefunn_data.r is already loaded
load("./data/art3base.RData")

## Overall system settings
options(digits=8)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

## Importing and cleaningemail address and the project number
regdataraw = read.spss("./data/regdata_tommy.sav", to.data.frame=TRUE, labels=F)
regdatazw <- regdataraw[,c("epost","prosjektnummer")] #could add ny_epost too
regdatazw$epost <- str_trim(as.character(regdatazw$epost))
names(regdatazw) <- c("email","pnr")

## Importing Skattefunn data
regdatasf <- sfmain[,c("Prosjektnummmer","Organisasjonsnummer")]
names(regdatasf) <- c("pnr","orgnr") #orgnr is a unique identifier for each firm / same for pnr at project level
datazw = read.csv("./data/skattefunn_10_clean.csv", sep=";",dec=",", header=T, na.strings="-1")

md <- datazw[c("Resp","dc_t0","dc_t1")]
md$Resp <- str_trim(as.character(md$Resp))
colnames(md) <- c("email","dc0","dc1")

## NB / regdf comes from skattefunn_data.r contains all projects, not only endreport
matchvec <- match(md$email,regdf$email)
regdata <- cbind(md,regdf[matchvec,])
datazw <- cbind(datazw,regdata)

## Splitting datazw in first wave and second wave
zw0 <- data.frame(orgnr=datazw$orgnr);zw0$yr <- 2004
zw1 <- data.frame(orgnr=datazw$orgnr);zw1$yr <- 2014

zw0[,3:117] <- datazw[,2:116]
zw1[,3:70] <- datazw[,117:184];zw1 <- zw1[1:263,]

## This dataset contains a panel of projects for each firm laid in time. Financial data to be added to this dataset
year <- c(seq(2002,2015,1)) ; org <- unique(datazw$orgnr)
sfp <- expand.grid(orgnr=org, year = year)
sfp <- merge(sfp, sfmain, by.x=c("orgnr","year"), by.y=c("Organisasjonsnummer","ar_til"), all.x=TRUE)
regnskapadj <- regnskap[ , !(names(regnskap) %in% c("ansatte"))]
regnskapadj <- subset(regnskapadj,driftsinntekt>0)
regnskapadj$pm <- regnskapadj$driftsres/regnskapadj$driftsinntekt
fin <- data.frame(regnskapadj  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(mean(pm,na.rm=TRUE))) ;colnames(fin) <- c("orgnr","pm")
assets <- data.frame(regnskapadj  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(mean(eiendom,na.rm=TRUE))) ;colnames(assets) <- c("orgnr","assets")
debt <- data.frame(regnskapadj  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(mean(gjeld,na.rm=TRUE))) ;colnames(debt) <- c("orgnr","debt")

zw <- datazw
zw$id <- as.character(zw$orgnr)
zw <- merge(zw,fin,by=("orgnr"), all.x=TRUE)
zw <- merge(zw,assets,by=("orgnr"), all.x=TRUE)
zw <- merge(zw,debt,by=("orgnr"), all.x=TRUE)
zw$lassets <- log(zw$assets)


## Creating variables
sense0 <- rowMeans(zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")], na.rm=TRUE)
seize0 <- as.double(rowMeans(zw[,c( "Q19e", "Q20b", "Q19c", "Q25", "Q20")], na.rm=TRUE))
tran0 <- as.double(rowMeans(zw[,c("Q20c", "Q25a", "Q25b", "Q25c")], na.rm=TRUE))
zw$dc0 <- rowMeans(cbind(sense0,seize0,tran0),na.rm=TRUE)

sense1 <- as.double(rowMeans(zw[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q7.3_t1")], na.rm=TRUE))
seize1 <-as.double(rowMeans(zw[,c("Q6.6_t1","Q7.1_t1","Q6.4_t1","Q7.5_t1","Q6.7_t1")], na.rm=TRUE))
tran1 <- as.double(rowMeans(zw[,c("Q7.2_t1","Q7.6_t1","Q7.7_t1","Q7.8_t1")], na.rm=TRUE))
zw$dc1 <- rowMeans(cbind(sense1,seize1,tran1),na.rm=TRUE)

zw$ca0 <-  rowMeans(zw[,c("Q28","Q28a", "Q28b", "Q28c")], na.rm=TRUE)
zw$ca1 <-  rowMeans(zw[,c("Q16.1_t1","Q16.2_t1", "Q16.3_t1", "Q16.4_t1")], na.rm=TRUE)
zw$or0 <-  rowMeans(zw[,c("Q21c", "Q21d", "Q21e", "Q22", "Q22a", "Q22b", "Q22c", "Q22d")], na.rm=TRUE)
zw$or1 <-  rowMeans(zw[,c("Q5.1_t1", "Q5.2_t1", "Q5.3_t1", "Q5.4_t1", "Q5.5_t1", "Q5.6_t1", "Q5.7_t1")], na.rm=TRUE)
zw$dl1 <-  rowMeans(zw[,c("Q8.1_t1", "Q8.2_t1", "Q8.3_t1" , "Q8.4_t1" , "Q8.5_t1", "Q8.6_t1")], na.rm=TRUE)
zw$dyn <-  rowMeans(zw[,c("Q27", "Q27a", "Q27b", "Q27c", "Q27e", "Q27g")], na.rm=TRUE)

zw$ddc <- as.double(zw$dc1-zw$dc0)
zw$dor <-  zw$or1-zw$or0
zw$size <- zw$Q1_t1
zw$age <- zw$Q2_t1
zw$dynhigh <- ifelse(zw$dyn > 6, 1,0)
zw$dynstable <- ifelse(zw$dyn < 6, 1,0)

zw$normdyn <-  scale(zw$dyn)
## Adjusting data set by removing NA for pm
zwadj <- zw[!is.na(zw$pm),]


## Correlations
### (2)  DESCRIPTIVE ANALYSIS EVOUSJON OF CAPABILITIES
cormat <- zw[,c("dl",
                "or0",
                "or1",
                "dc0",
                "dc1",
                "ca0",
                "ca1")]
                ## "size",
                ## "age",
                ## "dyn",
                ## "pm",
                ## "lassets")]



varmean <- apply(cormat,2, mean, na.rm=TRUE)
varsd <- apply(cormat,2, sd, na.rm=TRUE)


art2cor <-cor(cormat, use="complete.obs")

upper<-round(art2cor,2)
upper[upper.tri(art2cor)]<-""
upper<-as.data.frame(upper)
vardesc <- cbind(varmean,varsd)
upper <- cbind(vardesc,upper)
rownames(upper) <- c("1) DL",
                     "2) OR (T=1)",
                     "3) OR (T=2)",
                     "4) DC (T=1)",
                     "5) DC (T=2)",
                     "6) CA (T=1)",
                     "7) CA (T=2)")
                     ## "8) Firm Size",
                     ## "9) Firm Age",
                     ## "10) Env.dynamism",
                     ## "11) Firm PM",
                     ## "12) ln Firm Assets")


art2cor <- xtable(upper)
names(art2cor) <- c("Mean","SD",paste(seq(1:7),sep=","))

art2cor


## Regressions
mod1 <- lm(or1 ~ or0 + dl +  size + age + dyn + pm + lassets, data=zw);summary(mod1)
mod2 <- lm(or1 ~ or0 + dl + dc0 + ddc + size + age + dyn + pm + lassets, data=zw);summary(mod2)
mod1m <- lm(ddc ~ or0 + dl + size + age + dyn + pm + lassets, data=zw);summary(mod1m)
med.out1 <- mediate(mod1m,mod2, treat="dl",mediator="ddc",robustSE=TRUE, sims=1000)
summary(med.out1) ## ADE is insignificant telling us that DL has no direct effect on OR

mod3 <- lm(dc1 ~ dc0 + dl + size + age + dyn + pm + lassets, data=zw);summary(mod3)
mod4 <- lm(ca1 ~ ca0 +  dc0 + ddc + size + age + dyn + pm + lassets, data=zw);summary(mod4)
mod5 <- lm(ca1 ~ ca0 +  dc0 + ddc + or0 + dor + size + age + dyn + pm + lassets, data=zw);summary(mod5)
mod5m <- lm(dor ~ ca0 + ddc + or0 + dl + dc0 + size + age + dyn + pm + lassets, data=zw);summary(mod5m)

med.out5 <- mediate(mod5m,mod5, treat="ddc",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out5) ## ADE is insignificant telling us that DL has no direct effect on OR

mod6 <- lm(dl ~ dc0 +ddc*dyn +  size + age + dyn + pm + lassets, data=zw) ; summary(mod6) ### NB INTERACTION EFFECT
mod6 <- lm(dl ~ dc0 +  size + age + dyn + pm + lassets, data=zw) ; summary(mod6)


## Heckmann Regressions
zw$period <- ifelse(!is.na(zw$Q1_t1),1,0)


heckmodel1 <- selection(period ~ Q35b + dyn + dc0 + lassets + pm, or1 ~ or0 + dl + size + age + dyn + pm + lassets, data=zw)
summary(heckmodel1)

heckmodel2 <- selection(period ~ Q35b + dyn + dc0 + lassets + pm, or1 ~ or0 + dl + dc0 + ddc + size + age + dyn + pm + lassets, data=zw)
summary(heckmodel2)

heckmodel3 <- selection(period ~ Q35b + dyn + dc0 + lassets + pm, dc1 ~ dc0 + dl + size + age + dyn + pm + lassets, data=zw)
summary(heckmodel3)

heckmodel4 <- selection(period ~ Q35b + dyn + dc0 + lassets + pm, ca1 ~ ca0 + dl + dc0 + ddc + size + age + dyn + pm + lassets, data=zw)
summary(heckmodel4)

heckmodel5 <- selection(period ~ Q35b + dyn + dc0 + lassets + pm, ca1 ~ ca0 + dl + dc0 + ddc + or0 + dor + size + age + dyn + pm + lassets, data=zw)
summary(heckmodel5)

## if rho is zero (meaning non significant) / no selection bias

mainres <- list(heckmodel1, heckmodel2,heckmodel3,heckmodel4,heckmodel5,mod6)


## Robustmess / include more observations
mod3 <- lm(dc1 ~ dc0 + dl + size + age + dyn, data=zw);summary(mod3)
mod4 <- lm(ca1 ~ ca0 + dl + dc0 + ddc + or0 + dor + size + age + dyn, data=zw);summary(mod4)
mod4m <- lm(dor ~ ca0 + ddc + or0 + dl + dc0 + size + age + dyn, data=zw);summary(mod1m)

med.out4 <- mediate(mod4m,mod4, treat="ddc",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out4) ## ADE is insignificant telling us that DL has no direct effect on OR


## Extract R2 values

r1 <- round(summary(mod1)$adj.r.squared,3)
r2 <- round(summary(mod2)$adj.r.squared,3)
r3 <- round(summary(mod3)$adj.r.squared,3)
r4 <- round(summary(mod4)$adj.r.squared,3)
r5 <- round(summary(mod5)$adj.r.squared,3)
r6 <- round(summary(mod6)$adj.r.squared,3)

## extract coefficients

mod1_dl <- round(heckmodel1$estimate["dl"],3)
mod2_dl <- round(heckmodel2$estimate["dl"],3)
mod3_dl <- round(heckmodel3$estimate["dl"],3)
mod4_dl <- round(heckmodel3$estimate["dl"],3)
mod5_dl <- round(heckmodel3$estimate["dl"],3)

mod2_dc0 <- round(heckmodel2$estimate["dc0"],3)
mod4_dc0 <- round(heckmodel4$estimate["dc0"],3)
mod5_dc0 <- round(heckmodel5$estimate["dc0"],3)
mod6_dc0 <- round(mod6$coefficients["dc0"],3)

mod2_ddc <- round(heckmodel2$estimate["ddc"],3)
mod4_ddc <- round(heckmodel4$estimate["ddc"],3)
mod5_ddc <- round(heckmodel5$estimate["ddc"],3)

medmod1_ade <- round(med.out1$z0,3)  ## average direct effect
medmod1_acme <- round(med.out1$d0,3) ## average causal mediated effect
medmod1_shmed <- round(med.out1$n.avg,3) ## proportion of effect mediated

medmod5_ade <- round(med.out5$z0,3)  ## average direct effect
medmod5_acme <- round(med.out5$d0,3) ## average causal mediated effect
medmod5_shmed <- round(med.out5$n.avg,3) ## proportion of effect mediated










##########
## mod1 <- lm(ca1 ~ ca0 + dl + or0*dyn + size + age + dyn + pm + lassets,data=zwadj);summary(mod1)
mod1 <- lm(ca1 ~ ca0 + dl + or0 + dor + size + age  + pm + lassets,data=zw);summary(mod1)
mod1 <- lm(ca1 ~ ca0 + dl + or0 + dor + dc0 + ddc + size + age  + pm + lassets,data=zw);summary(mod1)

mod1 <- lm(ca1 ~ ca0 + dl + or0 + dor + dc0 + ddc + size + age  + pm + lassets, data=zw);summary(mod1)
mod1m <- lm(dor ~ ca0 + ddc + or0 + dl + dc0 + size + age + dyn + pm + lassets, data=zw);summary(mod1m)

med.out <- mediate(mod1m,mod1, treat="ddc",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out) ## ADE is insignificant telling us that DL has no direct effect on OR


zwadj <- zw[!is.na(zw$dca),]

mod1 <- lm(dca ~  dl + dor + ddc + size + age  + pm + lassets, data=zwadj);summary(mod1)
mod1m <- lm(dor ~ ddc  + dl + size + age + dyn + pm + lassets, data=zwadj);summary(mod1m)

med.out <- mediate(mod1m,mod1, treat="ddc",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out) ## ADE is insignificant telling us that DL has no direct effect on OR


cor(zw$dc1,zw$dor, use="complete.obs")
zw$dca <- zw$ca1 - zw$ca0


mod1 <- lm(pm ~ ca0 + dor + ddc + size + age + lassets,data=zw);summary(mod1)


capture.output(summary(mod1), file="testreg.xls")
