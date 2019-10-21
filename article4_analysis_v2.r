

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


setwd("/Users/larshovdanmolden/Documents/git/article4")
alldc <- zwadj[,c("Q19a", "Q19c", "Q19d", "Q20b", "Q20c","Q19", "Q20d","Q20e","Q19b","Q20")]

## Running median imputations
dc01 <-  imputezw(zwadj[,c("Q6.2_t1","Q6.4_t1", "Q6.5_t1","Q7.1_t1","Q7.2_t1")] )
dc02 <-  imputezw(zwadj[,c("Q6.1_t1","Q7.3_t1","Q7.4_t1")] )
dc001 <- imputezw(zwadj[,c("Q19a", "Q19c", "Q19d", "Q20b", "Q20c")] )
dc002 <-  imputezw(zwadj[,c("Q19", "Q20d","Q20e")] )
dc002 <-  imputezw(zwadj[,c("Q19c", "Q20b", "Q20c")])
ca0 <-  imputezw(zwadj[,c("Q28","Q28a","Q28c")] )
ca1 <-   imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.4_t1")] )
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1","Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.5_t1")])
or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c")])
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")])
or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e")])
ma01 <-  imputezw(zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])
ma02 <-  imputezw(zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d")])
m0 <- imputezw(zwadj[,c("Q16a","Q16b","Q17a","Q17b")])
m1 <- imputezw(zwadj[,c("Q4.1_t1","Q4.2_t1","Q4.4_t1","Q4.5_t1")])
m1 <- imputezw(zwadj[,c("Q4.4_t1","Q4.5_t1","Q4.6_t1")])
#m0 <- imputezw(zwadj[,c("Q18a","Q18b","Q18d")])
#m1 <- imputezw(zwadj[,c("Q4.7_t1","Q4.8_t1","Q4.9_t1")])


complist <- list(DC_P=dc01,DC_X=dc02,CA_t=ca0,CA=ca1,P=or1,R=ma01)
vcontrols <- zwadj[,c("size","age","dyn","pm","lassets")]
zwadj <- cbind(dc01,dc02,dc001,dc002,ca0,ca1,or1,or0,ma01,m0,m1,vcontrols)


######## ADJUSTING DATASET WITH DELTA

zwadj$d1 <- zwadj$Q5.4_t1 - zwadj$Q21c
zwadj$d2 <- zwadj$Q5.6_t1 - zwadj$Q21d
zwadj$d3 <- zwadj$Q5.7_t1 - zwadj$Q21e

zwadj$m1 <- zwadj$Q4.1_t1 - zwadj$Q16a
zwadj$m2 <- zwadj$Q4.2_t1 - zwadj$Q16b
zwadj$m3 <- zwadj$Q4.4_t1 - zwadj$Q17a
zwadj$m4 <- zwadj$Q4.5_t1 - zwadj$Q17b

names(zwadj)
summary(zwadj$dyn)
nboot <- 200
zwadjh <- subset(zwadj,dyn>4);nrow(zwadjh)
zwadjl <- subset(zwadj,dyn<=4);nrow(zwadjl)

DC1 <- c("Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1")
DC2 <- c("Q6.1_t1","Q7.3_t1","Q7.4_t1")
#DC01 <- c("Q19", "Q20d","Q20e")
#DC01 <- c("Q19a", "Q19c", "Q19d", "Q20b", "Q20c")
DC01 <- c("Q19c", "Q20b", "Q20c")
CA0 <- c("Q28","Q28a","Q28c")
CA1 <- c("Q16.1_t1","Q16.2_t1","Q16.4_t1")
R <- c("Q5.4_t1","Q5.6_t1","Q5.7_t1")
M <- c("Q4.1_t1","Q4.2_t1","Q4.4_t1","Q4.5_t1")
R0 <- c("Q21c","Q21d","Q21e")
M0 <- c("Q16a","Q16b","Q17a","Q17b")
#R <- c("d1","d2","d3")
#M <- c("m1","m2","m3","m4")

CONT <- c("size","age","lassets","pm")

##### Direct only
items <- c(DC01,CA0,CA1,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),toupper(CONT))
mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

DIR <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
dir <- sempls(model = DIR, data = zwadj, wscheme = "centroid",maxit=1000)

pathDiagram(dir, file = "direct", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

dirfit <- semPLS::gof(dir)
dirBoot <- bootsempls(dir, nboot = nboot, start = "ones", verbose = FALSE)
dirout <- extboot(dirBoot,9,"",c("^M ","R "))
dirout

##### R as  mediators
items <- c(DC01,CA0,CA1,R,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","R","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","R")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
RMED <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
rmed <- sempls(model = RMED, data = zwadj, wscheme = "centroid",maxit=1000)

pathDiagram(rmed, file = "rmed", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

rmedfit <- semPLS::gof(rmed)
rmedBoot <- bootsempls(rmed, nboot = nboot, start = "ones", verbose = FALSE)
rmedout <- extboot(rmedBoot,9,"",c("^M ","R "))



##### M as  mediators
items <- c(DC01,CA0,CA1,M,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("M",length(M)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","M","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","M")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
MMED <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
mmed <- sempls(model = MMED, data = zwadj, wscheme = "centroid",maxit=1000)

pathDiagram(mmed, file = "mmed", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

mmedfit <- semPLS::gof(mmed)
mmedBoot <- bootsempls(mmed, nboot = nboot, start = "ones", verbose = FALSE)
mmedout <- extboot(mmedBoot,9,"",c("^M ","R "))


##### Both mediators
items <- c(DC01,CA0,CA1,R,M,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("M",length(M)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","R","DC","M","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","R","CA1","M")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
BOTH <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
both <- sempls(model = BOTH, data = zwadj, wscheme = "centroid",maxit=1000)

pathDiagram(both, file = "both", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

bothfit <- semPLS::gof(both)
bothBoot <- bootsempls(both, nboot = nboot, start = "ones", verbose = FALSE)
bothout <- extboot(bothBoot,9,"",c("^M ","R "))


##### Both mediators with link between
items <- c(DC01,CA0,CA1,R,M,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("M",length(M)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","R","DC","M","DC","M")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","R","CA1","M","R")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
LINK <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
link <- sempls(model = LINK, data = zwadj, wscheme = "centroid",maxit=1000)

pathDiagram(link, file = "link", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

linkfit <- semPLS::gof(link)
linkBoot <- bootsempls(link, nboot = nboot, start = "ones", verbose = FALSE)
linkout <- extboot(linkBoot,9,"",c("^M ","R "))



###### SUMMARY OF FINDINGS
dirfit
rmedfit
mmedfit
bothfit
linkfit

dirout
rmedout
mmedout
bothout
linkout




DC1 <- c("Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1")
DC2 <- c("Q6.1_t1","Q7.3_t1","Q7.4_t1")
#DC01 <- c("Q19", "Q20d","Q20e")
#DC01 <- c("Q19a", "Q19c", "Q19d", "Q20b", "Q20c")
DC01 <- c("Q19c", "Q20b", "Q20c")
CA0 <- c("Q28","Q28a","Q28c")
CA1 <- c("Q16.1_t1","Q16.2_t1","Q16.4_t1")
R <- c("Q5.4_t1","Q5.6_t1","Q5.7_t1")
M <- c("Q4.1_t1","Q4.2_t1","Q4.4_t1","Q4.5_t1")
R0 <- c("Q21c","Q21d","Q21e")
M0 <- c("Q16a","Q16b","Q17a","Q17b")
M <- c("Q4.4_t1","Q4.5_t1","Q4.6_t1")


##### Both mediators with proactiveness as M
items <- c(DC01,CA0,CA1,R,M,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("M",length(M)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","R","DC","M","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","R","CA1","M")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
BOTH <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
both <- sempls(model = BOTH, data = zwadj, wscheme = "centroid",maxit=1000)
both
pathDiagram(both, file = "both_with_proactiveness_as_M", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


bothfit <- semPLS::gof(both)
bothBoot <- bootsempls(both, nboot = nboot, start = "ones", verbose = FALSE)
bothout <- extboot(bothBoot,9,"",c("^M ","R "))
bothout

##### Both mediators with path
items <- c(DC01,CA0,CA1,R,M,R0,M0,CONT)
latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("M",length(M)),rep("R0",length(R0)),rep("M0",length(M0)),toupper(CONT))

mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c(toupper(CONT),"CA0","DC","R","DC","M","DC","M0","R0","DC","DC")
dv <- c(rep("CA1",length(CONT)),"CA1","CA1","CA1","R","CA1","M","M","R","M0","R0")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
BOTH <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
both <- sempls(model = BOTH, data = zwadj, wscheme = "centroid",maxit=1000)
both
pathDiagram(both, file = "bothdct0", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

bothfit <- semPLS::gof(both)
bothBoot <- bootsempls(both, nboot = nboot, start = "ones", verbose = FALSE)
bothout <- extboot(bothBoot,15,"",c("M ","R ","M0 ","R0 "))

bothout



###### SUMMARY OF FINDINGS
dirfit
rmedfit
mmedfit
bothfit
bothfith
bothfitl
linkfit

dirout
rmedout
mmedout
bothout
dynout
linkout

plsLoadings(both)


plot(dc01[,1],dc001[,1])
plot(zwadj$d1)

## opening in the DC literature saying that DC needs to affect CA and that mindset another mechanism in

Organizational posture (EO started as posture) - attitudes
(Organizational behaviour)

Definitions
Are there other definitiosn that opens for other mediators than resources











