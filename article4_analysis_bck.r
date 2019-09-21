

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
library(polycor)
install.packages("missForest")
library(missForest)

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



### MMA SEM
zw$ma <-  rowMeans(zw[,c("Q24","Q24a","Q23b")], na.rm=TRUE)
zw$maorint <- zw$ma*zw$or1

model <- '
# outcome model
ca1 ~  ca0 +  b1*or1 + b2*ma + ma:or1 + size + age

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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)






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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)




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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
















## Regressions
mod1 <- lm(or1 ~ or0 + dl +  size + age + dyn + pm + lassets, data=zw);summary(mod1)
mod2 <- lm(or1 ~ or0 + dl + dc0 + ddc + size + age + dyn + pm + lassets, data=zw);summary(mod2)
mod1m <- lm(ddc ~ or0 + dl + size + age + dyn + pm + lassets, data=zw);summary(mod1m)
med.out1 <- mediate(mod1m,mod2, treat="dl",mediator="ddc",robustSE=TRUE, sims=1000)
summary(med.out1) ## ADE is insignificant telling us that DL has no direct effect on OR

mod3 <- lm(dc1 ~ dc0 + dl + size + age + dyn + pm + lassets, data=zw);summary(mod3)
mod4 <- lm(ca1 ~ ca0 +  dc0 + ddc + size + age + dyn + pm + lassets, data=zw);summary(mod4)
mod5 <- lm(ca1 ~ ca0 +  dc0 + dc1 + or0 + dor + size + age + dyn + pm + lassets, data=zw);summary(mod5)
mod5m <- lm(dor ~ ca0 + dc1 + or0 + dl + dc0 + size + age + dyn + pm + lassets, data=zw);summary(mod5m)

med.out5 <- mediate(mod5m,mod5, treat="dc1",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out5) ## ADE is insignificant telling us that DL has no direct effect on OR




install.packages("mma")
library(mma)

###setting variables
### adjusting dataset
zwadj <- zw[,c("ca1","ca0","dc0","dc1","or0","or1","ma","size","age")]

y <- zwadj[,1]
x <- zwadj[,2:length(zwadj)]

data.bin<-data.org(x,y,pred=3,mediator=c(5,6),jointm=list(n=1,j1=c("or1","ma")), predref="M",alpha=0.4,alpha2=0.4)



data("weight_behavior")

xx=weight_behavior[,2:14]
yy=weight_behavior[,1]

data.bin<-mma::data.org(xx,yy,pred=2,mediator=c(1,3:13),
                   jointm=list(n=1,j1=c
                    ("tvhours","cmpthours",
                  "cellhours")), predref="M",
                   alpha=0.4,alpha2=0.4)

zw$dyn <-  rowMeans(zw[,c("Q27a", "Q27b", "Q27c")], na.rm=TRUE)
summary(zw$dyn)
zw$dyn <- zw$Q27a

zw$dynhigh <- ifelse(zw$dyn > 4.667, 1,0)
zwhigh <- subset(zw,zw$dynhigh==1)
zwlow <- subset(zw,zw$dynhigh==0)

nrow(zwhigh)
nrow(zwlow)

mod5 <- lm(ca1 ~ ca0 +  dc0 + dc1 + or0 + dor + size + age  + pm + lassets, data=zwhigh);summary(mod5)
mod5m <- lm(dor ~ ca0 + dc1 + or0 + dc0 + size + age + dyn + pm + lassets, data=zwhigh);summary(mod5m)

med.out5hi <- mediate(mod5m,mod5, treat="dc1",mediator="dor",robustSE=TRUE, sims=1000)

mod5 <- lm(ca1 ~ ca0 +  dc0 + dc1 + or0 + dor + size + age  + pm + lassets, data=zwlow);summary(mod5)
mod5m <- lm(dor ~ ca0 + dc1 + or0  + dc0 + size + age  + pm + lassets, data=zwlow);summary(mod5m)

med.out5lw <- mediate(mod5m,mod5, treat="dc1",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out5lw)
summary(med.out5hi)





### MULTPLE MEDIATION
mod5 <- lm(ca1 ~ ca0 +  dc0 + dc1 + or0 + dor + size + age + dyn + pm + lassets, data=zw);summary(mod5)
mod5m <- lm(dor ~ ca0 + dc1 + or0 + dl + dc0 + size + age + dyn + pm + lassets, data=zw);summary(mod5m)

med.out5 <- mediate(mod5m,mod5, treat="dc1",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out5) ## ADE is insignificant telling us that DL has no direct effect on OR

zwadj <- subset(zw,!is.na(zw$or1))
Xnames <- c("ca0", "size","age")

m.med <- multimed(outcome = "ca1", med.main ="dor", med.alt ="ma", treat = "dc1", covariates = Xnames,                   data = zwadj, sims = 100)

summary(m.med)


plot(m.med, type = "point")
"Q24b","Q23a","Q23b","Q23c"

### MMA SEM
zw$ma <-  rowMeans(zw[,c("Q24","Q24a","Q23b")], na.rm=TRUE)
zw$maorint <- zw$ma*zw$or1

model <- '
# outcome model
ca1 ~  ca0 +  b1*or1 + b2*ma + ma:or1 + size + age

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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)






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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)




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

fit <- sem(model, data=zw)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)












boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")


library(semPlot)
semPaths(fit)
semPaths(fit,what="equality","est",style="lisrel",layout="tree", sizeLat=8, edge.label.cex = 0.9, ask =FALSE)

#####
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




### TESTING DC CONSTRUCT

mod1 <- lm(ca1~ ca0 + pm + lassets + dc1 + or1 , data=zw);summary(mod1)
mod1 <- lm(ca1~ + pm , data=zw);summary(mod1)

## Expoting to mplus
mplusexport <- zw[,c("Q19","Q19a","Q19b","Q19c","Q19d","Q20d",
                     "Q19e","Q20b","Q25","Q20",
                     "Q20c","Q25a","Q25b","Q25c",
                     "Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q7.3_t1",
                     "Q6.6_t1","Q7.1_t1","Q7.5_t1","Q6.7_t1",
                     "Q7.2_t1","Q7.6_t1","Q7.7_t1","Q7.8_t1","Q28","Q28a","Q28b","Q28c",
                     "Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1",
                     "Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c","Q22d",
                     "Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.4_t1","Q5.5_t1","Q5.6_t1","Q5.7_t1",
                     "Q8.1_t1","Q8.2_t1","Q8.3_t1","Q8.4_t1","Q8.5_t1","Q8.6_t1",
                     "Q27","Q27a","Q27b","Q27c","Q27e","Q27g","Q24", "Q24a","Q23a", "Q23b", "Q23c","size","age","dynhigh","pm","lassets")]

mplusexport <- zw[,c("Q19a", "Q19c", "Q20b", "Q20c", "Q19b", "Q19d", "Q20d", "Q20e",  "Q6.2_t1", "Q6.4_t1", "Q7.1_t1", "Q7.2_t1", "Q6.3_t1", "Q6.5_t1",  "Q7.3_t1", "Q7.4_t1", "Q28", "Q28a", "Q28b", "Q28c", "Q16.1_t1",  "Q16.2_t1", "Q16.3_t1", "Q16.4_t1", "Q21c", "Q21d", "Q21e", "Q5.4_t1",  "Q5.6_t1", "Q5.7_t1", "Q23a", "Q23b", "Q23c", "Q24", "Q24a",  "Q24b", "Q24c", "Q24d", "Q8.7_t1", "Q8.8_t1", "Q8.9_t1", "size",  "age", "dyn", "pm", "lassets")]

mplusexport <- subset(mplusexport, !is.na(Q5.4_t1))
nrow(mplusexport)



mplusexport[is.na(mplusexport)] <- -9999

write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport.csv", row.names=FALSE, col.names=FALSE, sep=",")

####



1 <- lm(ca1~ ca0 + pm + lassets + dc1 + or1 , data=zw);summary(mod1)
mod1 <- lm(ca1~ + pm , data=zw);summary(mod1)

## Expoting to mplus
mplusexport <- zw[,c("Q19a", "Q19b", "Q19c", "Q19d", #DC0
                     "Q20b","Q20c","Q20d", "Q20e", #DC0

                     "Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q6.6_t1", #DC1
                     "Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1", #DC1
                     "Q28","Q28a","Q28b","Q28c","Q28d", "Q28e", #CA0
                     "Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1", #CA1
                     "Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c","Q22d", #OC0
                     "Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.4_t1","Q5.5_t1","Q5.6_t1","Q5.7_t1", #OC1
                     "Q23a","Q23b","Q23c","Q24","Q24a","Q24b","Q24c","Q24d",  #MA
                     "Q27","Q27a","Q27b","Q27c","Q27e","Q27g", #DYN
                     "size","age","dynhigh","pm","lassets")]

mplusexport <- subset(mplusexport, !is.na(Q5.1_t1))
mplusexport[is.na(mplusexport)] <- -9999

write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport1.csv", row.names=FALSE, col.names=FALSE, sep=",")

#### IMPUTATION
#### ADDING INDEX MEAN TO MISSING

vsense0i <- zwadj[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]
vsense0 <- zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")]

vsense0imp <- missForest(vsense0)

#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
### Balancing sample by including only lines with observations in both time periods
zwadj <- subset(zw,!is.na(zw$Q4.1_t1))
nrow(zwadj)

library(Hmisc)



vsense0 <- missForest(zwadj[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")])
vseize0 <-  missForest(zwadj[,c( "Q19e", "Q20b", "Q25")],variablewise = TRUE)
vtran0 <-  missForest(zwadj[,c("Q20c", "Q25a", "Q25b", "Q25c")] )
vsense1 <-  missForest(zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q7.3_t1")] )
vseize1 <-  missForest(zwadj[,c("Q6.6_t1","Q7.1_t1","Q7.5_t1","Q6.7_t1")] )
vtran1 <-   missForest(zwadj[,c("Q7.2_t1","Q7.6_t1","Q7.7_t1","Q7.8_t1")] )
vca0 <-   missForest(zwadj[,c("Q28","Q28a", "Q28b", "Q28c")])
vca1 <-   missForest(zwadj[,c("Q16.1_t1","Q16.2_t1", "Q16.3_t1", "Q16.4_t1")])
vor0 <-   missForest(zwadj[,c("Q21c", "Q21d", "Q21e", "Q22", "Q22a", "Q22b", "Q22c", "Q22d")])
vor1 <-   missForest(zwadj[,c("Q5.1_t1", "Q5.2_t1", "Q5.3_t1", "Q5.4_t1", "Q5.5_t1", "Q5.6_t1", "Q5.7_t1")])
vdl1 <-   missForest(zwadj[,c("Q8.1_t1", "Q8.2_t1", "Q8.3_t1" , "Q8.4_t1" , "Q8.5_t1", "Q8.6_t1")])
vdyn <-   missForest(zwadj[,c("Q27", "Q27a", "Q27b", "Q27c", "Q27e", "Q27g")])
vma  <-   missForest(zwadj[,c("Q24","Q24a","Q23a","Q23b","Q23c")])

vcontrols <- zwadj[,c("size","age","dynhigh","pm","lassets")]

nrow(vma$ximp)


vdc <- zwadj[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d",
                "Q20b","Q20c","Q20d", "Q20e")]


mplusexport <- cbind(vsense0$ximp,vseize0$ximp,vtran0$ximp,vsense1$ximp,vseize1$ximp,vtran1$ximp,vca0$ximp,vca1$ximp,vor0$ximp,vor1$ximp,vdl1$ximp,vdyn$ximp,vma$ximp,vcontrols)

mplusexport <- vdc

mplusexport[is.na(mplusexport)] <- -9999

write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport1.csv", row.names=FALSE, col.names=FALSE, sep=",")

### Function to impute with average of the index creates
imputezw <- function(vec){
    meanVec <- rowMeans(vec, na.rm=TRUE)
    for (i in 1:nrow(vec)){
        for (j in 1:ncol(vec)){
            if(is.na(vec[i,j])){vec[i,j] <- meanVec[i]}
        }
    }
    return(vec)
}

vsense0 <- imputezw(zwadj[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")])
vseize0 <-  imputezw(zwadj[,c( "Q19e", "Q20b", "Q25")])
vtran0 <-  imputezw(zwadj[,c("Q20c", "Q25a", "Q25b", "Q25c")] )
vsense1 <-  imputezw(zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q7.3_t1")] )
vseize1 <-  imputezw(zwadj[,c("Q6.6_t1","Q7.1_t1","Q7.5_t1","Q6.7_t1")] )
vtran1 <-  imputezw (zwadj[,c("Q7.2_t1","Q7.6_t1","Q7.7_t1","Q7.8_t1")] )
vca0 <-   imputezw(zwadj[,c("Q28","Q28a", "Q28b", "Q28c", "Q28d", "Q28e")])
vca1 <-  imputezw (zwadj[,c("Q16.1_t1","Q16.2_t1", "Q16.3_t1", "Q16.4_t1")])
vor0 <-  imputezw (zwadj[,c("Q21c", "Q21d", "Q21e", "Q22", "Q22a", "Q22b", "Q22c", "Q22d")])
vor1 <-  imputezw (zwadj[,c("Q5.1_t1", "Q5.2_t1", "Q5.3_t1", "Q5.4_t1", "Q5.5_t1", "Q5.6_t1", "Q5.7_t1")])
vdl1 <-  imputezw (zwadj[,c("Q8.1_t1", "Q8.2_t1", "Q8.3_t1" , "Q8.4_t1" , "Q8.5_t1", "Q8.6_t1")])
vdyn <-  imputezw (zwadj[,c("Q27", "Q27a", "Q27b", "Q27c", "Q27e", "Q27g")])
vma  <-  imputezw (zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d","Q23a","Q23b","Q23c")])

vcontrols <- zwadj[,c("size","age","pm","lassets")]

mplusexport <- cbind(vsense0,vseize0,vtran0,vsense1,vseize1,vtran1,vca0,vca1,vor0,vor1,vdl1,vdyn,vma,vcontrols)

mplusexport[is.na(mplusexport)] <- -9999

write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport1.csv", row.names=FALSE, col.names=FALSE, sep=",")


foo <- imputezw(vsense0)
head(foo)
head(vsense0)


vsense0
foo
nrow(foo)

vcontrols <- zwadj[,c("size","age","dynhigh","pm","lassets")]












zw$ddc <- as.double(zw$dc1-zw$dc0)
zw$dor <-  zw$or1-zw$or0
zw$size <- zw$Q1_t1
zw$age <- zw$Q2_t1
zw$dynhigh <- ifelse(zw$dyn > 4, 1,0)
zw$dynstable <- ifelse(zw$dyn < 4, 1,0)




####


mod1 <- lm(ca1~ ca0 + or1 + ma , data=zw);summary(mod1)
mod1 <- lm(ca1~ ca0 + or1*ma , data=zw);summary(mod1)
mod1 <- lm(ca1~ ca0 + or1*ma, data=zw);summary(mod1)
mod1 <- lm(dca~  + or1*ma + dc1, data=zw);summary(mod1)

mod1 <- lm(ca1~ ca0 + or1 + dc1*dyn, data=zw);summary(mod1)

zw$dca <- zw$ca1-zw$ca0






mod1 <- lm(ca1 ~ dc0 + dl + size + age + dyn, data=zw);summary(mod3)
mod4 <- lm(ca1 ~ ca0 +  dc1 + or0 + dor + size + age + dyn, data=zw);summary(mod4)
mod4m <- lm(dor ~ ca0 + ddc + or0 + dl + dc0 + size + age + dyn, data=zw);summary(mod1m)

med.out4 <- mediate(mod4m,mod4, treat="ddc",mediator="dor",robustSE=TRUE, sims=1000)
summary(med.out4)











cor(zw$dc0,zw$dyn0, use="complete.obs")

model <- "

ma ~ dc0
or1 ~ dc0
ca1  ~ ca0  + or1 + ma + dc1


"

fit <- sem(model, data = zw, estimator="WLSMV")

summary(fit,fit.measures=TRUE)















write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport.csv", row.names=FALSE, col.names=FALSE, sep=",")

## Testing dynamism as moderator

zwdyn <- zw[,c("Q27a", "Q27b", "Q27c")]
fa1 <- factanal(na.omit(zwdyn),1)
fa1


zw$dyn <-  rowMeans(zw[,c("Q27a", "Q27b", "Q27c")], na.rm=TRUE)

mod3 <- lm(ca1 ~ ca0 + dyn*dc0 + dor + dc0, data=zw);summary(mod3)


mod3 <- lm(ca1 ~ ca0 + dc0*dyn + dc0*dyn2 + dc0+ dor + dc0, data=zw);summary(mod3)


## SEM Robustness for estimating correlation between key constructs

require(lavaan)
require(semPlot)


model <- "

#SENSE0 =~ Q19+ Q19a+ Q19b+  Q19d+ Q20d
#SEIZE0 =~  Q19e+ Q20b+ Q19c+ Q25
#TRAN0 =~ Q20c+ Q25a+ Q25b+ Q25c
#DC0 =~ SENSE0 + SEIZE0 + TRAN0
DC0  =~ Q19+ Q19a+ Q19b+  Q19d+ Q20d +Q19e+ Q20b+ Q19c+ Q20c+ Q25a+ Q25b

#SENSE1 =~ Q6.1_t1+Q6.2_t1+Q6.3_t1+Q6.4_t1+Q6.5_t1+Q7.3_t1
#SEIZE1 =~Q6.6_t1+Q7.1_t1+Q6.4_t1+Q7.5_t1+Q6.7_t1
#TRAN1 =~ Q7.2_t1+Q7.6_t1+Q7.7_t1+Q7.8_t1

#DC1 =~ SENSE1 + SEIZE1 + TRAN1
#DC1  =~Q6.1_t1+Q6.2_t1+Q6.3_t1+Q6.4_t1+Q6.5_t1+Q7.3_t1+Q6.6_t1+Q7.1_t1+Q6.4_t1+Q7.5_t1+Q6.7_t1+Q7.2_t1+Q7.6_t1+Q7.7_t1+Q7.8_t1

CA0 =~ Q28+Q28a+ Q28b+ Q28c
#CA1 =~ Q16.1_t1+Q16.2_t1+ Q16.3_t1+ Q16.4_t1
OR0 =~ Q21c+ Q21d+ Q21e+ Q22+ Q22a+ Q22b+ Q22c+ Q22d
#OR1 =~ Q5.1_t1+ Q5.2_t1+ Q5.3_t1+ Q5.4_t1+ Q5.5_t1+ Q5.6_t1+ Q5.7_t1
MA =~ Q24+ Q24a+ Q23a+ Q23b+ Q23c


OR1 ~ DC0
MA  ~ DC0

CA0 ~  MA + OR0 + DC0

#MA ~~ OR0

"





fit <- sem(model, data = zw)



summary(fit,fit.measures=TRUE)


semPaths(fit)
semPaths(fit,what="equality","est",style="lisrel",layout="tree", sizeLat=8, edge.label.cex = 0.9, ask =FALSE)


for (i in 1:1000000){
    print(i)
}


x1 <- runif(10000, 1, 1000)
x2 <- runif(10000, -1, 1)
x3 <- runif(1000, 800, 1000)
plot(x1, cex=0.1, col="green")
points(x2, cex=0.4, col="blue")
points(x3, cex=0.4, col="red")

x4 <- GBM(1,0.2,N=1000)
x5 <- GBM(1,0.2,N=1000)

plot(x4,type="l")
lines(x5, col="red")

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

zw$dsense <- (sense1-sense0)
zw$dseize <- (seize1-seize0)
zw$dtran <- (tran1-tran0)




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


### Function to impute with average of the index creates
imputezw <- function(vec){
    meanVec <- rowMedians(vec, na.rm=TRUE) #coud be rowMeans
    for (i in 1:nrow(vec)){
        for (j in 1:ncol(vec)){
            if(is.na(vec[i,j])){vec[i,j] <- meanVec[i]}
        }
    }
    return(vec)
}

zwadj <- subset(zw,!is.na(Q4.1_t1))


zwadj[is.na(zwadj)] <- NA

nrow(zwadj)

dc01 <- imputezw(zwadj[,c("Q19a", "Q19c", "Q20b", "Q20c")])
dc02 <-  imputezw(zwadj[,c( "Q19b","Q19d","Q20d", "Q20e")])
dc11 <-  imputezw(zwadj[,c("Q6.2_t1","Q6.4_t1", "Q6.7_t1","Q7.1_t1","Q7.2_t1")] )
dc12 <-  imputezw(zwadj[,c("Q6.1_t1","Q6.3_t1","Q7.4_t1")] )

dc11 <-  imputezw(zwadj[,c("Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1")] )
dc12 <-  imputezw(zwadj[,c("Q6.3_t1","Q7.3_t1","Q7.4_t1")] )


ca0 <-  imputezw(zwadj[,c("Q28","Q28a","Q28b","Q28c")] )
ca1 <-   imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1")] )

or0 <-   imputezw(zwadj[,c( "Q21c","Q21d","Q21e")])
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")])

ma01 <-   imputezw(zwadj[,c("Q23a","Q23b","Q23c")])
ma02 <-   imputezw(zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d")])

vdl1 <-  imputezw(zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])



vcontrols <- zwadj[,c("size","age","dyn","pm","lassets")]

maint <- ma01*vcontrols$dyn;names(maint) <- c("maint1","maint2","maint3")
orint <- or1*vcontrols$dyn;names(orint) <- c("orint1","orint2","orint3")
zwadj <- cbind(dc01,dc02,dc11,dc12,ca0,ca1,or0,or1,ma01,ma02,vdl1,vcontrols)
nrow(zwadj)

mplusexport <- zwadj
mplusexport[is.na(mplusexport)] <- -9999
write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport.csv", row.names=FALSE, col.names=FALSE, sep=",")


### PRINCOMP

dc1 <-  round(imputezw(zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q6.6_t1","Q6.7_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")] ),0)
ca1 <-  round(imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1")]),0)
ca0 <- round(imputezw(zwadj[,c("Q28","Q28a","Q28b","Q28c")]),0)
ma <- round(imputezw(zwadj[,c("Q8.7_t1", "Q8.8_t1", "Q8.9_t1")]),0)
or1 <- round(imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")]),0)

pdc1 <- dc1[which(!is.na(dc1[,1])),]
pdc <- fa.poly(pdc1,2)
pdc

 pca0 <- ca0[which(!is.na(ca0[,1])),]
pdc <- fa.poly(pca0,1)
pdc

pca1 <- ca1[which(!is.na(ca1[,1])),]
pdc <- fa.poly(pca1,1)
pdc

pma <- ma[which(!is.na(ma[,1])),]
pma <- fa.poly(pma,1)
pma

por1 <- or1[which(!is.na(or1[,1])),]
por1 <- fa.poly(por1,1)
por1





pdc1 <- round(pdc1,0)

pdc1est <- prcomp(pdc1, scale=TRUE)
pdc1est

pdc1 <- dc11[which(!is.na(dc11[,1])),]
pdc2 <- dc12[which(!is.na(dc12[,1])),]

foo <- fa.poly(pdc1,2)
foo



summary(foo)
pdc1est <- prcomp(pdc1, scale=TRUE)
pdc1est
pdc1est <- princomp(pdc1)
pdc1est$loadings
summary(pdc1est)

pdc2est <- princomp(pdc2)
pdc2est$loadings
summary(pdc2est)

library(psych)
library(GPArotation)
library(semPLS)


## DIRECT MODEL DC TO CA
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.3_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1")#CA1
           #"Q21c","Q21d","Q21e", #OC0
           #"Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           #"Q8.7_t1", "Q8.8_t1", "Q8.9_t1")
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC01",5),
             rep("DC02",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3))
           #  rep("OR0",3),
           #  rep("OR1",3),
           #  rep("MA01",3))
           #  rep("MA01",3))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("CA0","DC01","DC02")
dv <- c("CA1","CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")


ECSI <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ecsi <- sempls(model = ECSI, data = zwadj, wscheme = "centroid",maxit=1000)
ecsi

pathDiagram(ecsi, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


ecsiBoot <- bootsempls(ecsi, nboot = 200, start = "ones", verbose = FALSE)
ecsiBoot

semPLS::rSquared(ecsi)
semPLS::gof(ecsi)




## PARTIAL MODEL MA 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.3_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           #"Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC01",5),
             rep("DC02",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("OR1",3),
           #  rep("MA01",3))
             rep("MA01",3))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("MA01","CA0","DC01","DC01","DC02","DC02")
dv <- c("CA1","CA1","CA1","MA01","CA1","MA01")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")


ECSI <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ecsi <- sempls(model = ECSI, data = zwadj, wscheme = "centroid",maxit=1000)
ecsi

pathDiagram(ecsi, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


ecsiBoot <- bootsempls(ecsi, nboot = 200, start = "ones", verbose = FALSE)
ecsiBoot

semPLS::rSquared(ecsi)
semPLS::gof(ecsi)





## PARTIAL MODEL OR 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC0
                                        #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.3_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1") #OC1
           #"Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC01",4),
             rep("DC02",4),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("OR1",3))
           #  rep("MA01",3))
           #  rep("MA01",5))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("OR1","CA0","DC01","DC01","DC02","DC02")
dv <- c("CA1","CA1","CA1","OR1","CA1","OR1")

sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

ECSI <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ecsi <- sempls(model = ECSI, data = zwadj, wscheme = "centroid",maxit=1000)
ecsi

pathDiagram(ecsi, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


ecsiBoot <- bootsempls(ecsi, nboot = 200, start = "ones", verbose = FALSE)
ecsiBoot

semPLS::rSquared(ecsi)
semPLS::gof(ecsi)



##FULL MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.3_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC01",5),
             rep("DC02",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("OR1",3),
           # rep("MA01",3))
             rep("MA01",3))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("MA01","CA0","DC01","DC01","DC02","DC02","OR1","DC01","DC02")
dv <- c("CA1","CA1","CA1","MA01","CA1","MA01","CA1","OR1","OR1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")


ECSI <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ecsi <- sempls(model = ECSI, data = zwadj, wscheme = "centroid",maxit=1000)
ecsi

pathDiagram(ecsi, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


ecsiBoot <- bootsempls(ecsi, nboot = 200, start = "ones", verbose = FALSE)
ecsiBoot

semPLS::rSquared(ecsi)
semPLS::gof(ecsi)
