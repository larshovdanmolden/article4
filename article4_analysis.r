

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
#library(polycor)

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



### Creating variable for determining constant in respondent


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
zw$ma <-  rowMeans(zw[,c("Q24","Q24a", "Q24b", "Q23b","Q23c")], na.rm=TRUE)

zw$ddc <- as.double(zw$dc1-zw$dc0)
zw$dor <-  zw$or1-zw$or0
zw$size <- zw$Q1_t1
zw$age <- zw$Q2_t1
zw$dynhigh <- ifelse(zw$dyn > 4, 1,0)
zw$dynstable <- ifelse(zw$dyn < 4, 1,0)

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


### DATA IMPU
foo <- zw[,c("Q24","Q24a","Q23b","Q23a")]

summary(foo)


pcformal <-  princomp(~.,data=foo, nfactors=1, rotate="oblimin", na.action=na.omit)
summary(pcformal)
### MMA SEM
zw$ma <-  rowMeans(zw[,c("Q24","Q24a","Q23b","Q23a")], na.rm=TRUE)
zw$maorint <- zw$ma*zw$or1

model <- '
# outcome model
ca1 ~  ca0 +  b1*or1 + b2*ma+ dc0 + size + age
# mediator models
or1 ~ a1*dc0 + size + age
ma ~ a2*dc0 + size + age

# indirect effects (IDE)
medVar1DOR  := a1*b1
medVar2MA  := a2*b2
sumIDE := (a1*b1) + (a2*b2)

# total effect
total := (a1*b1) + (a2*b2)
or1 ~~ ma # model correlation between mediators
'

fitfull <- sem(model, data=zw, estimator="ML")
summary(fitfull, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)



model <- '
# outcome model
ca1 ~  ca0  + b2*ma + size + age + pm + lassets

# mediator models
#or1 ~ a1*dc0 + size + age
ma ~ a2*dc0

# indirect effects (IDE)
#medVar1DOR  := a1*b1
#medVar2MA  := a2*b2
#sumIDE := (a1*b1) + (a2*b2)

# total effect
#total := (a1*b1) + (a2*b2)
#or1 ~~ ma # model correlation between mediators
'

fitma <- sem(model, data=zw)


summary(fitma, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)


model <- '
# outcome model
ca1 ~  ca0  + or1 + size + age + pm + lassets

# mediator models
or1 ~ a1*dc0 + size + age
#ma ~ a2*dc0

# indirect effects (IDE)
#medVar1DOR  := a1*b1
#medVar2MA  := a2*b2
#sumIDE := (a1*b1) + (a2*b2)

# total effect
#total := (a1*b1) + (a2*b2)
#or1 ~~ ma # model correlation between mediators
'

fitor <- sem(model, data=zw)
summary(fitor, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)



