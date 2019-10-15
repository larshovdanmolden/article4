

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
library(reshape)
library(huxtable)
library(semPLS)
###install.packages("huxtable")
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

zw$size <- zw$Q1_t1
zw$age <- zw$Q2_t1


## Imputation function
imputezw <- function(vec){
    meanVec <- rowMeans(vec, na.rm=TRUE) #coud be rowMeans
    for (i in 1:nrow(vec)){
        for (j in 1:ncol(vec)){
            if(is.na(vec[i,j])){vec[i,j] <- meanVec[i]}
        }
    }
    return(vec)
}



extboot <- function(bootobj,n,nam,r){ #remembaer space between Letter and " ("P ")
    ##this function extracts estimates and standard errors for a bootstrapped object from PLS Sem

    c1 <- attr(bootobj$t,"path")
    c2 <- bootobj$t0
    c3 <- c(apply(bootobj$t,2,sd))
    res <- data.frame(c1,c2,c3);colnames(res) <- c("name",paste(nam,"est",sep="_"),paste(nam,"se",sep="_"))
    res <- res[(nrow(res)-n):nrow(res),];  rownames(res) <- NULL
    toMatch <- c(r, "DC")
    res <- res[(grep(paste(toMatch,collapse="|"),res$name)),]
    res$t <- res[,2] / res[,3]
    res <- res[,c(1,2,4)]
    res[,2:3] <- round(res[,2:3],3)


    return(res)
}

## including only observations at both times
zwadj <- subset(zw,!is.na(Q4.1_t1))
rownames(zwadj) <- NULL





## Running median imputations
dc01 <-  imputezw(zwadj[,c("Q6.2_t1","Q6.4_t1", "Q6.5_t1","Q7.1_t1","Q7.2_t1")] )
dc02 <-  imputezw(zwadj[,c("Q6.1_t1","Q7.3_t1","Q7.4_t1")] )
dc00 <-  imputezw(zwadj[,c("Q19", "Q20d","Q20e")] )
ca0 <-  imputezw(zwadj[,c("Q28","Q28a","Q28c")] )
ca1 <-   imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.4_t1")] )
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1","Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.5_t1")])
or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c")])
ma01 <-  imputezw(zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])
ma02 <-  imputezw(zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d")])
m0 <- imputezw(zwadj[,c("Q16a","Q16b","Q17a","Q17b")])
m1 <- imputezw(zwadj[,c("Q4.1_t1","Q4.2_t1","Q4.4_t1","Q4.5_t1")])
#m0 <- imputezw(zwadj[,c("Q18a","Q18b","Q18d")])
#m1 <- imputezw(zwadj[,c("Q4.7_t1","Q4.8_t1","Q4.9_t1")])


complist <- list(DC_P=dc01,DC_X=dc02,CA_t=ca0,CA=ca1,P=or1,R=ma01)
vcontrols <- zwadj[,c("size","age","dyn","pm","lassets")]
zwadj <- cbind(dc01,dc02,dc00,ca0,ca1,or1,or0,ma01,m0,m1,vcontrols)

#### Testing constructs


m0 <- zwadj[,c("Q18a","Q18b","Q18d")]
m1 <- zwadj[,c("Q4.7_t1","Q4.8_t1","Q4.9_t1")]



m0c <- na.omit(m0)
m1c <- na.omit(m1)

pdc1est <- princomp(m0c)
pdc1est$loadings

pdc1est <- princomp(m1c)
pdc1est$loadings

######## ADJUSTING DATASET WITH DELTA

zwadj$d1 <- zwadj$Q5.4_t1 - zwadj$Q21c
zwadj$d2 <- zwadj$Q5.6_t1 - zwadj$Q21d
zwadj$d3 <- zwadj$Q5.7_t1 - zwadj$Q21e

zwadj$m1 <- zwadj$Q4.1_t1 - zwadj$Q16a
zwadj$m2 <- zwadj$Q4.2_t1 - zwadj$Q16b
zwadj$m3 <- zwadj$Q4.4_t1 - zwadj$Q17a
zwadj$m4 <- zwadj$Q4.5_t1 - zwadj$Q17b

## zwadj$d1 <- zwadj$Q6.1_t1 - zwadj$Q19
## zwadj$d2 <- zwadj$Q6.2_t1 - zwadj$Q19a
## zwadj$d3 <- zwadj$Q6.3_t1 - zwadj$Q19b
## zwadj$d4 <- zwadj$Q6.4_t1 - zwadj$Q19c
## zwadj$d5 <- zwadj$Q6.5_t1 - zwadj$Q19d
## zwadj$d6 <- zwadj$Q6.6_t1 - zwadj$Q19e
## zwadj$d7 <- zwadj$Q6.7_t1 - zwadj$Q20
## zwadj$d8 <- zwadj$Q7.1_t1 - zwadj$Q20b
## zwadj$d9 <- zwadj$Q7.2_t1 - zwadj$Q20c
## zwadj$d10 <- zwadj$Q7.3_t1 - zwadj$Q20d
## zwadj$d11 <- zwadj$Q7.4_t1 -zwadj$Q20e


 ## zwadj$d4 <- zwadj$Q5.1_t1 - zwadj$Q22
 ## zwadj$d5 <- zwadj$Q5.2_t1 - zwadj$Q22a
 ## zwadj$d6 <- zwadj$Q5.3_t1 - zwadj$Q22b
 ## zwadj$d7 <- zwadj$Q5.5_t1 - zwadj$Q22c
names(zwadj)
summary(zwadj$dyn)
nboot <- 200
zwadjh <- subset(zwadj,dyn>4);nrow(zwadjh)
zwadjl <- subset(zwadj,dyn<=4);nrow(zwadjl)




### test model SEM

model <- '
# outcome model
#DC =~  Q6.2_t1 + Q6.4_t1 + Q7.1_t1 + Q7.2_t1 + Q6.5_t1
DC =~  Q6.1_t1 + Q7.3_t1 + Q7.4_t1
DC0 =~ Q19 + Q20d + Q20e
CA0 =~ Q28 + Q28a + Q28c
CA1 =~ Q16.1_t1 + Q16.2_t1 + Q16.4_t1
#dR =~ d1 + d2 + d3
#dM =~  m1 + m2 + m3 + m4
R1 =~ Q5.4_t1 + Q5.6_t1 + Q5.7_t1
#M1 =~ Q4.1_t1 + Q4.2_t1 + Q4.4_t1 + Q4.5_t1
R0 =~ Q21c + Q21d + Q21e
#M0 =~ Q16a + Q16b + Q17a + Q17b

CA1 ~ CA0 + DC + R1 + DC0  +  size + pm + lassets
#M1 ~ DC + M0
R1 ~ DC + R0
DC  ~ DC0


#R1 ~~ M1
#R0 ~~ M0
'

fit <- sem(model, data=zwadj)


summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)






### delta P MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           # "Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
    "Q5.4_t1","Q5.6_t1","Q5.7_t1",
   #"Q4.7_t1","Q4.8_t1","Q4.9_t1",#OC1
    "Q4.1_t1","Q4.2_t1","Q4.4_t1","Q4.5_t1",

    #"d1","d2","d3",
    #"m1","m2","m3","m4",

           "size","age")

latents <- c(rep("DC",3),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("dR",3),
             rep("dM",4),
           #  rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","dR","DC","dM","DC","DC")
dv <- c("CA1","CA1","CA1","CA1","dR","CA1","dM","CA1")
iv <- c("CA0","SIZE","AGE","DC","PM","ASS","dR","DC")
dv <- c("CA1","CA1","CA1","CA1","CA1","CA1","CA1","dR")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

FULL <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
full <- sempls(model = FULL, data = zwadj, wscheme = "centroid",maxit=1000)
full

f <- semPLS::gof(full)
f2 <- semPLS::gof(full)

f
f2

pathDiagram(full, file = "full", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

fullBoot <- bootsempls(full, nboot = nboot, start = "ones", verbose = FALSE)

fullout <- extboot(fullBoot,9,"P_LOW",c("dM ","dR "))
fullout

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
pl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
ph <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)

pl
ph

pathDiagram(base, file = "base", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

pBootl <- bootsempls(pl, nboot = nboot, start = "ones", verbose = FALSE)
pBooth <- bootsempls(ph, nboot = nboot, start = "ones", verbose = FALSE)

PL <- extboot(pBootl,9,"P_LOW","P ")
PH <- extboot(pBooth,9,"P_HIGH","P ")
P <- merge(PL,PH , by="name"); colnames(P) <- c("Path","LOW","t","HIGH","t")
PL


### R MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           # "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "size","age")

latents <- c(rep("DC",5),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","R","DC")
dv <- c("CA1","CA1","CA1","CA1","R")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

BASE <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
base <- sempls(model = BASE, data = zwadj, wscheme = "centroid",maxit=1000)

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
rl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
rh <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)

rBootl <- bootsempls(rl, nboot = nboot, start = "ones", verbose = FALSE)
rBooth <- bootsempls(rh, nboot = nboot, start = "ones", verbose = FALSE)




RL <- extboot(rBootl,9,"R_LOW",c("R ","P "))
RH <- extboot(rBooth,9,"R_HIGH",c("R ","P "))
R <- merge(RL,RH , by="name"); colnames(R) <- c("Path","LOW","t","HIGH","t")
RL



### R and P  MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
            "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "size","age")

latents <- c(rep("DC",5),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("P",3),
             rep("P",3),
             rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","R","DC","P","DC")
dv <- c("CA1","CA1","CA1","CA1","R","CA1","P")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

BASE <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
base <- sempls(model = BASE, data = zwadj, wscheme = "centroid",maxit=1000)

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
rl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
rh <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)

rBootl <- bootsempls(rl, nboot = nboot, start = "ones", verbose = FALSE)
rBooth <- bootsempls(rh, nboot = nboot, start = "ones", verbose = FALSE)

#baseBoot <- bootsempls(base, nboot = nboot, start = "ones", verbose = FALSE)
#full <- extboot(baseBoot,9,"Mean",c("R ","P "))
#full

BL <- extboot(rBootl,9,"R_LOW",c("R ","P "))
BH <- extboot(rBooth,9,"R_HIGH",c("R ","P "))
B <- merge(BL,BH , by="name"); colnames(R) <- c("Path","LOW","t","HIGH","t")


R
P
B







names(zwadj)
summary(zwadj$dyn)
nboot <- 200
zwadjh <- subset(zwadj,dyn>4);nrow(zwadjh)
zwadjl <- subset(zwadj,dyn<=4);nrow(zwadjl)

### delta P MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
            "Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           #"Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "d2","d3","d6","d7",
           #"Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "size","age")

latents <- c(rep("DC",4),
           #  rep("DC_S",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",4),
           #  rep("MA01",3))
           #  rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","DC","P","DC")
dv <- c("CA1","CA1","CA1","CA1","CA1","P")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

BASE <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
base <- sempls(model = BASE, data = zwadj, wscheme = "centroid",maxit=1000)
base

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
pl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
ph <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)
pl

#pathDiagram(base, file = "base", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

pBootl <- bootsempls(pl, nboot = nboot, start = "ones", verbose = FALSE)
pBooth <- bootsempls(ph, nboot = nboot, start = "ones", verbose = FALSE)
#baseBoot <- bootsempls(base, nboot = nboot, start = "ones", verbose = FALSE)
#baseBoot


PL <- extboot(pBootl,9,"P_LOW","P ")
PH <- extboot(pBooth,9,"P_HIGH","P ")
P <- merge(PL,PH , by="name"); colnames(P) <- c("Path","LOW","t","HIGH","t")
P

BS <- extboot(baseBoot,9,"base","P ")
BS
### R MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           # "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "size","age")

latents <- c(rep("DC",5),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","DC","DC","R","DC")
dv <- c("CA1","CA1","CA1","CA1","CA1","CA1","R")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

BASE <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
base <- sempls(model = BASE, data = zwadj, wscheme = "centroid",maxit=1000)

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
rl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
rh <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)

rBootl <- bootsempls(rl, nboot = nboot, start = "ones", verbose = FALSE)
rBooth <- bootsempls(rh, nboot = nboot, start = "ones", verbose = FALSE)




RL <- extboot(rBootl,9,"R_LOW",c("R ","P "))
RH <- extboot(rBooth,9,"R_HIGH",c("R ","P "))
R <- merge(RL,RH , by="name"); colnames(R) <- c("Path","LOW","t","HIGH","t")
RL



### R and P  MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
            "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "size","age")

latents <- c(rep("DC",5),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("P",3),
             rep("P",3),
             rep("R",3),

             "SIZE","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")

iv <- c("CA0","SIZE","AGE","DC","DC","R","DC","P","DC")
dv <- c("CA1","CA1","CA1","CA1","CA1","CA1","R","CA1","P")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

BASE <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
base <- sempls(model = BASE, data = zwadj, wscheme = "centroid",maxit=1000)

BASEl <- plsm(data = zwadjl, strucmod = sm, measuremod = mm)
rl <- sempls(model = BASEl, data = zwadjl, wscheme = "centroid",maxit=1000)

BASEh <- plsm(data = zwadjh, strucmod = sm, measuremod = mm)
rh <- sempls(model = BASEh, data = zwadjh, wscheme = "centroid",maxit=1000)

rBootl <- bootsempls(rl, nboot = nboot, start = "ones", verbose = FALSE)
rBooth <- bootsempls(rh, nboot = nboot, start = "ones", verbose = FALSE)

baseBoot <- bootsempls(base, nboot = nboot, start = "ones", verbose = FALSE)
full <- extboot(baseBoot,9,"Mean",c("R ","P "))
full

BL <- extboot(rBootl,9,"R_LOW",c("R ","P "))
BH <- extboot(rBooth,9,"R_HIGH",c("R ","P "))
B <- merge(BL,BH , by="name"); colnames(R) <- c("Path","LOW","t","HIGH","t")


R
P
B








names(complist[[1]])

##Factor extraction full model based on insight from Polychoric PCA
items <- c("Q6.2_t1","Q6.4_t1", "Q7.2_t1", #DC1 "Q7.1_t1""Q6.5_t1",
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC2
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OR1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1") #MA

latents <- c(rep("DC_P",3),
             rep("DC_X",3),
             rep("CA_t",3),
             rep("CA",3),
             rep("P",3),
             rep("R",3))


mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA_t","DC_P","DC_P","DC_X","DC_X","P","DC_P","DC_X")
dv <- c("CA","CA","CA","R","CA","R","CA","P","P")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

FULL <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
full <- sempls(model = FULL, data = zwadj, wscheme = "centroid",maxit=1000)
full

pathDiagram(full, file = "full", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


fullCI <- data.frame(semPLS::dgrho(full))
##fullR <- semPLS::rSquared(ecsi)
##fullGOF <- semPLS::gof(ecsi)
fullCI
constructs <- unique(latents)
cfull <- full$coefficients[grep("^lam",rownames(full$coefficients)),]
cfull$num <- substring(rownames(cfull),7,7)
cfull$fac <- substring(rownames(cfull),5,5)
cfull$cname <- sub("\\->.*", "", cfull$Path)
cfull
grep("^CA_t $",cfull$cname,fixed=FALSE)
cfull$cname

extcomp <- function(cname,cfull){
    ## Function to extract loadings from a PLS object

    cest <-  cfull[grep(paste("^",cname," $",sep=""),cfull$cname,fixed=FALSE),]$Estimate
    cnam <-  rep(cname,length(cest))
   # iname <- paste(cnam,"_",cfull$num,sep="")
    output <- data.frame(cnam,cest)
    return(output)
    }

falpha <- function(cons){
    ## Function to extract alpha from a set of indicators
    res <- psych::alpha(cons)$total$std.alpha
    return(res)
    }

## extracting alpha from constructs
lalpha <- lapply(complist,falpha)
calpha <- melt(lalpha)
colnames(calpha) <- c("alpha","cnam")#;calpha$cnam <- toupper(calpha$cnam)

## extracting factor loadings
ctab <- lapply(constructs,extcomp, cfull=cfull)
ctab <- melt(ctab)
ctab$iname <- paste(ctab$cnam,"^",cfull$num,sep="")
ctab
calpha


## Extracting DG rho
fullCI$cnam <- rownames(fullCI)
cci <- fullCI[,c(3,1)];colnames(cci) <- c("cnam","rho")
ctab <- merge(ctab,calpha,by="cnam",all.x=TRUE)
ctab <- merge(ctab,cci,by="cnam",all.x=TRUE)

## replacing C_t with C_{t-1}
ctab$cnam <- gsub('_t', '_{t-1}', ctab$cnam)
ctab$iname <- gsub('_t', '_{t-1}', ctab$iname)

ctab
## Creating factor output table

ht <- hux(
    Factor = paste("$",ctab$cnam,"$",sep=""),
    Items = ctab$iname,
    Loading = round(ctab$value,3),
    Alpha = round(ctab$alpha,3),
    CI = round(ctab$rho,3),
    add_colnames = TRUE
)

f <- c(4,7,12,15,18,21)

bold(ht)[1,]           <- TRUE
bottom_border(ht)[1,]  <- 0.7
bottom_border(ht)[f,]  <- 0.2
align(ht)[,2]          <- 'right'
right_padding(ht)      <- 15
left_padding(ht)       <- 15
bottom_padding(ht)     <- 6
top_padding(ht) <- 6
width(ht)              <- 0.90
number_format(ht)[,c(3,4,5)]      <- 2
ht$Alpha[duplicated(ht$Alpha)] <- NA
ht$CI[duplicated(ht$CI)] <- NA
ht$Factor[duplicated(ht$Factor)] <- NA
escape_contents(ht)[, c(1,2)] <- FALSE
add_footnote(ht, "Loadings estimated using both polychoric correlation matrix and regular principal component with continous variables. Both displayed similar results. Standard loadings reported here", border = 0.2)
ht



## Evaluating PLS model https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
## PARTIAL MODEL MA 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1","Q7.2_t1","Q6.5_t1", #DC1 #"Q7.1_t1"
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           #"Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC_P",4),
             rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA_{t-1}",3),
             rep("CA1",3),
           #  rep("OR0",3),
           #  rep("OR1",3),
           #  rep("MA01",3))
             rep("R",3))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA_{t-1}","DC_P","DC_P","DC_X","DC_X")
dv <- c("CA1","CA1","CA1","R","CA1","R")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")


MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma


## DIRECT MODEL DC TO CA
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1")#CA1
           #"Q21c","Q21d","Q21e", #OC0
           #"Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           #"Q8.7_t1", "Q8.8_t1", "Q8.9_t1")
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC_P",5),
             rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA_{t-1}",3),
             rep("CA",3))
           #  rep("OR0",3),
           #  rep("OR1",3),
           #  rep("MA01",3))
           #  rep("MA01",3))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("CA_{t-1}","DC_P","DC_X")
dv <- c("CA","CA","CA")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")


DIR <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
dir <- sempls(model = DIR, data = zwadj, wscheme = "centroid",maxit=1000)
dir



## Partial MODEL OR 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC0
                                        #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1") #OC1
           #"Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           #"size","pm","age","lassets")

latents <- c(rep("DC_P",4),
             rep("DC_X",4),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA_{t-1}",3),
             rep("CA",3),
           #  rep("OR0",3),
             rep("P",3))
           #  rep("MA01",3))
           #  rep("MA01",5))
            # "SZ","PM","AGE","ASS")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("P","CA_{t-1}","DC_P","DC_P","DC_X","DC_X")
dv <- c("CA","CA","CA","P","CA","P")

sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

OR <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
or <- sempls(model = OR, data = zwadj, wscheme = "centroid",maxit=1000)



### Summary stats


pathDiagram(dir, file = "dir", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
pathDiagram(full, file = "full", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
pathDiagram(or, file = "or", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
pathDiagram(ma, file = "ma", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

dirBoot <- bootsempls(dir, nboot = 200, start = "ones", verbose = FALSE)
fullBoot <- bootsempls(full, nboot = 200, start = "ones", verbose = FALSE)
orBoot <- bootsempls(or, nboot = 200, start = "ones", verbose = FALSE)
maBoot <- bootsempls(ma, nboot = 200, start = "ones", verbose = FALSE)

extboot <- function(bootobj,n,nam){
    ##this function extracts estimates and standard errors for a bootstrapped object from PLS Sem

    c1 <- attr(bootobj$t,"path")
    c2 <- bootobj$t0
    c3 <- c(apply(bootobj$t,2,sd))
    res <- data.frame(c1,c2,c3);colnames(res) <- c("name",paste(nam,"est",sep="_"),paste(nam,"se",sep="_"))
    res <- res[(nrow(res)-n):nrow(res),];  rownames(res) <- NULL
    return(res)
}

dirRes <- extboot(dirBoot,2,"dir")
fullRes <- extboot(fullBoot,8,"full")
orRes <-  extboot(orBoot,5,"or")
maRes <-  extboot(maBoot,5,"ma")

plmres <- merge(fullRes,dirRes,by="name",all.x=TRUE)
plmres <- merge(plmres,orRes,by="name",all.x=TRUE)
plmres <- merge(plmres,maRes,by="name",all.x=TRUE)

plmres <- plmres[c(1,2,5,4,3,7,6,9,8),]
plmres
plmres
plmest <- plmres[,c(1,4,6,8,2)]
plmse <- plmres[,c(1,5,7,9,3)]

plmt <- round(plmest[,c(2:5)] / plmse[,c(2:5)],2)
plmt <- data.frame(plmest$name,plmt)
plmest[,c(2:5)] <- round(plmest[,c(2:5)],3)
plmt

plmFig <- data.frame(plmest[,c(1,5)],plmt[,5])
plmFig


extFig <- function(df){
    name <- df[,1]
    mvec <- c(1:nrow(df))
    rvec <- c(1:nrow(df))

    for (i in 1:nrow(df)){
        x <- df[i,3]
        m <- ""
        if(x>2.576){m="***"}
        if(x<2.576 & x>1.960){m="**"}
        if(x<1.960 & x>1.670){m="*"}

        mvec[i] <- m
        rvec[i] <- paste("$",df[i,2],"^{",m,"}","$",sep="")



    }
    df$ast <- mvec
    df$res <- rvec
    return(df)
    }




figOut <- extFig(plmFig)
figOut














foo[1,5]
plmFig[3,3]

plmest
#### Finding GOF and N
gofdir <- round(semPLS::gof(dir)[3],2)
gofor <- round(semPLS::gof(or)[3],2)
gofma <- round(semPLS::gof(ma)[3],2)
goffull <- round(semPLS::gof(full)[3],2)

N <- 260


str(gofdir)
gofdir[3]

plmest
plmt
consName <- c("$DC_X \rightarrow CA$"," ",
              "$DC_X \rightarrow CA$"," ",
              " ",
              "$DC_P \rightarrow P$"," ",
              "$DC_P \rightarrow R$"," ",
              " ",
              "$DC_X \rightarrow P$"," ",
              "$DC_X \rightarrow R$"," ",
              " ",

              " ",
              "$P \rightarrow CA$"," ",
              "$R \rightarrow CA$"," ",
              " ",
              "$CA_{t-1} \rightarrow CA$"
              " ",
              "Goodnes of fit")

consLabel <- c("$DC_P \rightarrow CA$",
              "$DC_X \rightarrow CA$",
              "$DC_P \rightarrow P$",
              "$DC_P \rightarrow R$",
              "$DC_X \rightarrow P$",
              "$DC_X \rightarrow R$",
              "$P \rightarrow CA$",
              "$R \rightarrow CA$",
              "$CA_{t-1} \rightarrow CA$"
              )
plmest$name <- as.character(plmest$name)


consName <- as.character(plmest[,1])
consName <- consName[c(2,5,4,3,7,6,9,8,1)]

nameMat <- data.frame(consLabel,consName); colnames(nameMat) <- c("cname","name")
nameMat$cname <- as.character(nameMat$cname)

resest <- merge(plmest,nameMat)
resse <- merge(plmse,nameMat)
resest[,2:5] <- round(resest[,2:5],3)
resse[,2:5] <- round(resse[,2:5],3)


foo <- resest[1,2]
resest
rest <- round(resest[,2:5]/resse[,2:5],3)
rest
resest

str(resest)
fullRes <- data.frame(matrix(NA,ncol=6,nrow=18))
fullRes

fullRes[c(seq(1,nrow(fullRes),2)),] <- resest[c(1:nrow(resest)),]
fullRes[c(seq(2,nrow(fullRes),2)),c(2,3,4,5)] <- resse[c(1:nrow(resse)),c(2,3,4,5)]

fullRes[c(seq(2,nrow(fullRes),2)),c(2,3,4,5)] <- paste("(",format(unlist(rest)),")",sep="")
foo <- apply(rest,2

fullRes
xtable(fullRes)
seq(2,nrow(fullRes),2)

resest[c(1,2,3),]


fullRes


plmest[,1]
extvec <- c(
dirMod <- c(

length(dirBoot)-1
dirBoot[1,1]
str(dirBoot)

foo <- data.frame(dirBoot)
str(dirBoot)

t0 <- dirBoot$t0
t <- dirBoot$t
tt <-(t)
apply(tt, 2,sd)

f1 <- t0[1]
f2 <- t[,1]
f3 <- sqrt((f1-f2)^2)
mean(f3)
sd(f2)
t
head(t)
    attr(t,"path")

    apply(




## Evaluating PLS model https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
## PARTIAL MODEL MA 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           #"Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "pm","size")

latents <- c(#rep("DC_P",5),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "PM","SIZE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA0","P","PM","SIZE")
dv <- c("CA1","CA1","CA1","CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

sm
MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma0 <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma0



pathDiagram(ma0, file = "ma0", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")



getwd()
maBoot0 <- bootsempls(ma0, nboot = 200, start = "ones", verbose = FALSE)
maBoot0
0.128/0.06282





## Evaluating PLS model https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
## PARTIAL MODEL MA 2DC
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1","Q7.2_t1", #DC1
           #"Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "pm","size","lassets","age")

latents <- c(rep("DC_L",3),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "PM","SIZE","ASS","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA0","P","DC_L","DC_L","DC_L","PM","SIZE","ASS","AGE")
dv <- c("CA1","CA1","CA1","CA1","R","P","CA1","CA1","CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

sm
MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma1 <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma1



pathDiagram(ma1, file = "ma1n", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")



getwd()
maBoot <- bootsempls(ma, nboot = 200, start = "ones", verbose = FALSE)
maBoot
0.128/0.06282


items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           #"Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "pm","size","lassets","age")

latents <- c(rep("DC_C",3),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "PM","SIZE","ASS","AGE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA0","P","DC_C","DC_C","DC_C","PM","SIZE","ASS","AGE")
dv <- c("CA1","CA1","CA1","CA1","R","P","CA1","CA1","CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

sm
MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma2 <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma2

maBoot2 <- bootsempls(ma2, nboot = 200, start = "ones", verbose = FALSE)
maBoot2

pathDiagram(ma2, file = "ma2", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

0.1367/0.0636



#### FULL MODEL
items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           #"Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "pm","size")

latents <- c(rep("DC_X",3),
           #  rep("DC_X",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "PM","SIZE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA0","P","DC_X","DC_X","DC_X","PM","SIZE")
dv <- c("CA1","CA1","CA1","CA1","R","P","CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

sm
MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma3 <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma3

maBoot3 <- bootsempls(ma3, nboot = 200, start = "ones", verbose = FALSE)
maBoot3

zwadjh <- subset(zwadj,dyn<4)
ma3h <- sempls(model = MA, data = zwadjh, wscheme = "centroid",maxit=1000)
ma3h

maBoot3h <- bootsempls(ma3h, nboot = 200, start = "ones", verbose = FALSE)
maBoot3h

pathDiagram(ma3, file = "ma2", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

0.1424/0.0636




model <-

items <- c(#"Q19a","Q19c", "Q20b", "Q20c", #DC01
           #"Q19b","Q19d","Q20d", "Q20e", #DC02
           "Q6.2_t1","Q6.4_t1","Q7.2_t1", #DC1
           "Q6.1_t1","Q7.3_t1","Q7.4_t1", #DC1
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           #"Q21c","Q21d","Q21e", #OC0
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OC1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1",
           #"Q24","Q24a","Q24b","Q24c","Q24d")
           "pm","size")

latents <- c(rep("DC_L",3),
             rep("DC_C",3),
           #  rep("DC11",4),
           #  rep("dc12",4),
             rep("CA0",3),
             rep("CA1",3),
           #  rep("OR0",3),
             rep("P",3),
           #  rep("MA01",3))
             rep("R",3),

             "PM","SIZE")

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("R","CA0","P","DC_C","DC_C","DC_C","PM","SIZE","DC_L","DC_L","DC_L")
dv <- c("CA1","CA1","CA1","CA1","R","P","CA1","CA1","CA1","R","P")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

MA <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
ma2 <- sempls(model = MA, data = zwadj, wscheme = "centroid",maxit=1000)
ma2

maBoot2 <- bootsempls(ma2, nboot = 200, start = "ones", verbose = FALSE)
maBoot2



zwadjh <- subset(zwadj,dyn>4.5)
nrow(zwadjh)
ma2h <- sempls(model = MA, data = zwadjh, wscheme = "centroid",maxit=1000)
ma2h

maBoot2h <- bootsempls(ma2h, nboot = 200, start = "ones", verbose = FALSE)
maBoot2h


pathDiagram(ma2, file = "ma2", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
mplusexport <- zwadj
mplusexport[is.na(mplusexport)] <- -9999
write.table(mplusexport,"/Users/larshovdanmolden/Documents/git/article4/estimation/mplusexport2.csv", row.names=FALSE, col.names=FALSE, sep=",")
names(zwadj)
names(mplusexport)
head(mplusexport)
summary(mplusexport)
summary(zwadj)
model <- "


DCP  =~ Q6.2_t1+Q6.4_t1+ Q7.2_t1
#DCX  =~ Q6.1_t1 + Q7.3_t1 + Q7.4_t1
CA0 =~  Q28+Q28a+Q28c
CA1 =~           Q16.1_t1+Q16.2_t1+Q16.4_t1
P =~        Q5.4_t1+Q5.6_t1+Q5.7_t1
R =~           Q8.7_t1+ Q8.8_t1+ Q8.9_t1
PM =~     pm
SZ =~     size
ASS =~  lassets
#AGE =~ age


P ~ DCP
R ~ DCP
#R ~ DCX
#P ~ DCX

CA1 ~ CA0 + P + R + PM + DCP + ASS + SZ




"





fit1 <- sem(model, data = zwadj)



summary(fit1,fit.measures=TRUE)


semPaths(fit)
semPaths(fit,what="equality","est",style="lisrel",layout="tree", sizeLat=8, edge.label.cex = 0.9, ask =FALSE)




model <- "


#DCP  =~ Q6.2_t1+Q6.4_t1+ Q7.2_t1
DCX  =~ Q6.1_t1 + Q7.3_t1 + Q7.4_t1
CA0 =~  Q28+Q28a+Q28c
CA1 =~           Q16.1_t1+Q16.2_t1+Q16.4_t1
P =~        Q5.4_t1+Q5.6_t1+Q5.7_t1
R =~           Q8.7_t1+ Q8.8_t1+ Q8.9_t1
PM =~     pm
SZ =~     size
ASS =~  lassets
#AGE =~ age


#P ~ DCP
#R ~ DCP
R ~ DCX
P ~ DCX

CA1 ~ CA0 + P + R + PM + DCX + ASS + SZ




"





fit2 <- sem(model, data = zwadj)



summary(fit2,fit.measures=TRUE)






model <- "


DCP  =~ Q6.2_t1+Q6.4_t1+ Q7.2_t1
DCX  =~ Q6.1_t1 + Q7.3_t1 + Q7.4_t1
CA0 =~  Q28+Q28a+Q28c
CA1 =~           Q16.1_t1+Q16.2_t1+Q16.4_t1
P =~        Q5.4_t1+Q5.6_t1+Q5.7_t1
R =~           Q8.7_t1+ Q8.8_t1+ Q8.9_t1
PM =~     pm
SZ =~     size
ASS =~  lassets
#AGE =~ age


P ~ DCP
R ~ DCP

R ~ DCX
P ~ DCX

CA1 ~ CA0 + P + R + PM + DCX+ DCP+ ASS + SZ


R~~P
DCP~~DCX

"





fit3 <- sem(model, data = zwadj)



summary(fit3,fit.measures=TRUE)
