

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
library(reshape)
library(huxtable)
library(semPLS)
library(mice)
library(mma)
library(car)
library(lmtest)
#install.packages("semPLSModelSelection", repos="http://R-Forge.R-project.org")
library("semPLSModelSelection")
                                        #library(polycor)

## This loads the data workspace where skattefunn_data.r is already loaded
load("./data/art3base.RData")
rm(list=setdiff(ls(), c("sfmain","regdf","regnskap")))
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


## Imputation functionxx
imputezw <- function(vec){
    meanVec <- rowMeans(vec, na.rm=TRUE) #coud be rowMeans
    for (i in 1:nrow(vec)){
        for (j in 1:ncol(vec)){
            if(is.na(vec[i,j])){vec[i,j] <- meanVec[i]}
        }
    }
    return(vec)
}

imputezw <- function(vec){
   tempData <- mice(vec,m=10,maxit=50,meth='pmm',seed=500)
   return(complete(tempData,1))
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
#alldc <- zwadj[,c("Q19","Q19a", "Q19b","Q19c", "Q19d", "Q20","Q20b", "Q20c", "Q20d","Q20e")]
#alldc0 <- zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q6.7_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")]

alldc <- zwadj[,c("Q19","Q19c", "Q19d", "Q20b", "Q20c", "Q20d","Q20e")]
alldc0 <- zwadj[,c("Q6.1_t1","Q6.4_t1","Q6.5_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")]
## Running median imputation

## Running median imputation
#dc02 <-  imputezw(zwadj[,c("Q6.4_t1", "Q6.5_t1","Q7.1_t1","Q7.2_t1")] )
#dc02 <-  imputezw(zwadj[,c("Q6.1_t1","Q7.3_t1","Q7.4_t1")] )
#dc002 <- imputezw(zwadj[,c("Q19c", "Q19d", "Q20b", "Q20c")] )
dc02 <-  imputezw(alldc)
dc002 <-  imputezw(alldc0)
#dc002 <-  imputezw(zwadj[,c("Q19", "Q20d","Q20e")] )
#dc002 <-  imputezw(zwadj[,c("Q19d","Q19c", "Q20b", "Q20c","Q20e")])
ca0 <-  imputezw(zwadj[,c("Q28","Q28a","Q28c")] )
ca1 <-   imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.4_t1")] )
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1","Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.5_t1")])
or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c")])
#or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")])
#or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e")])
or1 <-   imputezw(zwadj[,c("Q5.1_t1","Q5.2_t1","Q5.3_t1")]) ## CHANGED FOR EXPLOITAINON
or0 <-   imputezw(zwadj[,c("Q22","Q22a","Q22b")])

#ma01 <-  imputezw(zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])
#ma02 <-  imputezw(zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d")])
## m0 <- imputezw(zwadj[,c("Q16a","Q16b","Q17a","Q17b")])
#m0 <- imputezw(zwadj[,c("Q18","Q17a","Q17b")])
m1 <- imputezw(zwadj[,c("Q4.4_t1","Q4.5_t1","Q4.6_t1")])
m2 <- imputezw(zwadj[,c("Q4.1_t1","Q4.2_t1","Q4.3_t1")])
#dyn  <- imputezw(zwadj[,c("Q27a","Q27b","Q27c")])
m0 <- imputezw(zwadj[,c("Q18","Q17a","Q17b")])
#m1 <- imputezw(zwadj[,c("Q4.7_t1","Q4.8_t1","Q4.9_t1")])
#dcm <- dc002 ;colnames(dcm)  <-  paste("dcm",seq(1,length(dc002),1),sep="") ## Average DC between two periods
dcm <- (dc002+dc02)/2 ;colnames(dcm)  <-  paste("dcm",seq(1,length(dc002),1),sep="") ## Average DC between two periods
#complist <- list(DC_P=dc01,DC_X=dc02,CA_t=ca0,CA=ca1,P=or1,R=ma01)
vcontrols <- zwadj[,c("size","age","pm","lassets","debt","assets","dyn")]
#zwadj <- cbind(dc01,dc02,dc001,dc002,ca0,ca1,or1,or0,ma01,m0,m1,vcontrols,dyn)
zwadj <- cbind(dc002,dc02,ca0,ca1,m1,m0,or0, or1,vcontrols,dcm)
#zwadj <- cbind(ca0,ca1,m1,m0,or0, or1,dcm)
zwadj$de <- zwadj$debt/zwadj$assets

#### NO IMP
zwadj <- subset(zw,!is.na(Q4.1_t1))
rownames(zwadj) <- NULL

setwd("/Users/larshovdanmolden/Documents/git/article4")
#alldc <- zwadj[,c("Q19","Q19a", "Q19b","Q19c", "Q19d", "Q20","Q20b", "Q20c", "Q20d","Q20e")]
#alldc0 <- zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q6.7_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")]

alldc <- zwadj[,c("Q19","Q19c", "Q19d", "Q20b", "Q20c", "Q20d","Q20e")]
alldc0 <- zwadj[,c("Q6.1_t1","Q6.4_t1","Q6.5_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")]
## Running median imputation

## Running median imputation
#dc02 <-  (zwadj[,c("Q6.4_t1", "Q6.5_t1","Q7.1_t1","Q7.2_t1")] )
#dc02 <-  (zwadj[,c("Q6.1_t1","Q7.3_t1","Q7.4_t1")] )
#dc002 <- (zwadj[,c("Q19c", "Q19d", "Q20b", "Q20c")] )
dc02 <-  (alldc)
dc002 <-  (alldc0)
#dc002 <-  (zwadj[,c("Q19", "Q20d","Q20e")] )
#dc002 <-  (zwadj[,c("Q19d","Q19c", "Q20b", "Q20c","Q20e")])
ca0 <-  (zwadj[,c("Q28","Q28a","Q28c")] )
ca1 <-   (zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.4_t1")] )
or1 <-   (zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1","Q5.1_t1","Q5.2_t1","Q5.3_t1","Q5.5_t1")])
or0 <-   (zwadj[,c("Q21c","Q21d","Q21e","Q22","Q22a","Q22b","Q22c")])
#or1 <-   (zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")])
#or0 <-   (zwadj[,c("Q21c","Q21d","Q21e")])
or1 <-   (zwadj[,c("Q5.1_t1","Q5.2_t1","Q5.3_t1")]) ## CHANGED FOR EXPLOITAINON
or0 <-   (zwadj[,c("Q22","Q22a","Q22b")])

#ma01 <-  (zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])
#ma02 <-  (zwadj[,c("Q24","Q24a","Q24b","Q24c","Q24d")])
## m0 <- (zwadj[,c("Q16a","Q16b","Q17a","Q17b")])
#m0 <- (zwadj[,c("Q18","Q17a","Q17b")])
m1 <- (zwadj[,c("Q4.4_t1","Q4.5_t1","Q4.6_t1")])
m2 <- (zwadj[,c("Q4.1_t1","Q4.2_t1","Q4.3_t1")])
#dyn  <- (zwadj[,c("Q27a","Q27b","Q27c")])
m0 <- (zwadj[,c("Q18","Q17a","Q17b")])
#m1 <- (zwadj[,c("Q4.7_t1","Q4.8_t1","Q4.9_t1")])
#dcm <- dc002 ;colnames(dcm)  <-  paste("dcm",seq(1,length(dc002),1),sep="") ## Average DC between two periods
dcm <- (dc002+dc02)/2 ;colnames(dcm)  <-  paste("dcm",seq(1,length(dc002),1),sep="") ## Average DC between two periods
#complist <- list(DC_P=dc01,DC_X=dc02,CA_t=ca0,CA=ca1,P=or1,R=ma01)
vcontrols <- zwadj[,c("size","age","pm","lassets","debt","assets","dyn")]
#zwadj <- cbind(dc01,dc02,dc001,dc002,ca0,ca1,or1,or0,ma01,m0,m1,vcontrols,dyn)
zwadj <- cbind(dc002,dc02,ca0,ca1,m1,m0,or0, or1,vcontrols,dcm)
#zwadj <- cbind(ca0,ca1,m1,m0,or0, or1,dcm)
zwadj$de <- zwadj$debt/zwadj$assets

zwexp<-zwadj

zwexp[is.na(zwexp)] <- -99
#zwexp <- subset(zwexp,pm>-5)
write.csv(zwexp,"zwad_noimp.csv",quote = FALSE)

#DC1 <- c("Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1")
#DC01 <- c("Q6.1_t1","Q7.3_t1","Q7.4_t1")
#DC01 <- c("Q19c", "Q19d", "Q20b", "Q20c")
#DC01 <- c("Q19a", "Q19c", "Q19d", "Q20b", "Q20c")
#DC01 <- c("Q19c", "Q20b", "Q20c")
CA0 <- c("Q28","Q28a","Q28c")
CA1 <- c("Q16.1_t1","Q16.2_t1","Q16.4_t1")
#R <- c("Q5.4_t1","Q5.6_t1","Q5.7_t1")
#M <- c("Q4.4_t1","Q4.5_t1","Q4.6_t1")
R0 <- c("Q21c","Q21d","Q21e")
M0 <- c("Q18","Q17a","Q17b")
DC01  <- names(dcm)
#DC01 <- c("Q19","Q19c", "Q19d", "Q20b", "Q20c", "Q20d","Q20e")
CONT <- c("size","assets","pm")
CONTm <- c("size","assets")
#CONT <- c("pm","de","dyn","size")
#or0 <-   imputezw(zwadj[,c("Q21c","Q21d","Q21e")])
R <-c("Q5.1_t1","Q5.2_t1","Q5.3_t1") ## CHANGED FOR EXPLOITAINON
R0 <-c("Q22","Q22a","Q22b")

CONT <- c("size","age")
CONTm <- c("size","age")


plswrap <-function(zwadj,CONT,CONTm){

  ### MODELLING THE SEMPLS
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

    ##### R as  mediators
      items <- c(DC01,CA0,CA1,R,R0,CONT)
      latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("R0",length(R0)),toupper(CONT))
      mm <- cbind(latents,items); colnames(mm) <- c("source","target")
      iv <- c(toupper(CONT),toupper(CONTm),"CA0","DC","R","DC","R0")
      dv <- c(rep("CA1",length(CONT)),rep("R",length(CONTm)),"CA1","CA1","CA1","R","R")
      sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
      RMED <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
      rmed <- sempls(model = RMED, data = zwadj, wscheme = "centroid",maxit=1000)
      pathDiagram(rmed, file = "rmed", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
      rmedfit <- semPLS::gof(rmed)
      rmedBoot <- bootsempls(rmed, nboot = nboot, start = "ones", verbose = FALSE)
      rmedout <- extboot(rmedBoot,15,"",c("^M ","R "))

    ##### M as  mediators
      items <- c(DC01,CA0,CA1,M,M0,CONT)
      latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("M",length(M)),rep("M0",length(M0)),toupper(CONT))
      mm <- cbind(latents,items); colnames(mm) <- c("source","target")
      iv <- c(toupper(CONT),toupper(CONTm),"CA0","DC","M","DC","M0")
      dv <- c(rep("CA1",length(CONT)),rep("M",length(CONTm)),"CA1","CA1","CA1","M","M")
      sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
      MMED <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
      mmed <- sempls(model = MMED, data = zwadj, wscheme = "centroid",maxit=1000)
      pathDiagram(mmed, file = "mmed", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
      mmedfit <- semPLS::gof(mmed)
      mmedBoot <- bootsempls(mmed, nboot = nboot, start = "ones", verbose = FALSE)
      mmedout <- extboot(mmedBoot,16,"",c("^M ","R "))

    ##### Both mediators
      items <- c(DC01,CA0,CA1,R,M,R0,M0,CONT)
      latents <- c(rep("DC",length(DC01)),rep("CA0",length(CA0)),rep("CA1",length(CA1)),rep("R",length(R)),rep("M",length(M)),rep("R0",length(R0)),rep("M0",length(M0)),toupper(CONT))
      mm <- cbind(latents,items); colnames(mm) <- c("source","target")
      iv <- c(toupper(CONT),toupper(CONTm),toupper(CONTm),"CA0","R","DC","M","DC","M0","R0","DC")
      dv <- c(rep("CA1",length(CONT)),rep("R",length(CONTm)),rep("M",length(CONTm)),"CA1","CA1","R","CA1","M","M","R","CA1")
      sm <- cbind(iv,dv);colnames(sm) <- c("source","target")
      BOTH <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
      both <- sempls(model = BOTH, data = zwadj, wscheme = "centroid",maxit=1000)
      pathDiagram(both, file = "both", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")
      bothfit <- semPLS::gof(both)
      bothBoot <- bootsempls(both, nboot = nboot, start = "ones", verbose = FALSE)
      bothout <- extboot(bothBoot,20,"",c("^M ","R "))
      
      

  ### Model fit
  ## https://rdrr.io/rforge/semPLSModelSelection/man/ic.html
    qsq <- round(c(qSquared(dir, d=4)[length(qSquared(dir, d=4))],
             qSquared(rmed, d=4)[length(qSquared(rmed, d=4))],
             qSquared(mmed, d=4)[length(qSquared(mmed, d=4))],
             qSquared(both, d=4)[length(qSquared(both, d=4))]),2)

    rsq <- round(c(rSquared(dir)[length(rSquared(dir))],
              rSquared(rmed)[length(rSquared(rmed))],
              rSquared(mmed)[length(rSquared(mmed))],
              rSquared(both)[length(rSquared(both))]),2)

    gof <- round(c(gof(dir)[3],
             gof(rmed)[3],
             gof(mmed)[3],
             gof(both)[3]),2)

    bic <-round(c(ic(dir, LV = "CA1",criteria = "BIC"),
            ic(rmed, LV = "CA1",criteria = "BIC"),
            ic(mmed, LV = "CA1",criteria = "BIC"),
            ic(both, LV = "CA1",criteria = "BIC")),1)

    n_obs <-round(c(dir$N-length(dir$incomplete),
                    rmed$N-length(rmed$incomplete),
                    mmed$N-length(mmed$incomplete),
                    both$N-length(both$incomplete)),0)

    dgr <- round(dgrho(both)[,1],2)

  #### CORRELATION AND VIF TESTING
    fact <- data.frame(both$factor_scores)
    cormatLV <- fact[,c("CA1",
                  "CA0",
                  "DC",
                  "R",
                  "R0",
                  "M",
                  "M0")]
    allLV <- names(cormatLV)
    allCONT <- (unique(c(CONT,CONTm)))
    cormatC <- fact[,toupper(allCONT)]
    cormat <- zwadj[,allCONT]

    ### Making indexes for mean values
      sdindLV <- c(sd(rowMeans(zwadj[,CA1]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,CA0]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,DC01]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,R]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,R0]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,M]),na.rm=TRUE),
                   sd(rowMeans(zwadj[,M0]),na.rm=TRUE))

      mindLV <- c(mean(rowMeans(zwadj[,CA1]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,CA0]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,DC01]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,R]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,R0]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,M]),na.rm=TRUE),
                       mean(rowMeans(zwadj[,M0]),na.rm=TRUE))


      mindC <- apply(cormat,2, mean, na.rm=TRUE)
      sdindC <- apply(cormat,2, sd, na.rm=TRUE)

      descm <- c(mindLV,mindC);names(descm) <- c(allLV,allCONT)
      descsd <- c(sdindLV,sdindC);names(descsd) <- c(allLV,allCONT)
      descf <-cbind(descm,descsd); colnames(descf)<-c("mean","sd")

      cormat <-cbind(cormatLV,cormatC)
      cormat <- cor(cormat,use="complete.obs")

      upper<-round(cormat,2)
      upper[upper.tri(cormat)]<-""
      cormat<-as.data.frame(upper)
      desccor <- cbind(descf,cormat)

    ### VIF
    #Chin (1998) considers Dillon-Goldstein’s rho to be a
    #better indicator than Cronbach’s alpha. As a rule of
    #thumb Dillon-Goldstein’s rho values higher than 0.70
    #suggest unidimensionality.
      mod1 <- lm(CA1 ~ ., data=fact) ; summary(mod1)
      resvif <- vif(mod1)




  ###### RESULT OUTPUT
    resmat  <- data.frame()
    resmatmed = data.frame()  ## res for mediator paths


        of <-function(mod,relvec){ ## Function toget row number of path
          relmat=data.frame(pt=relvec,nr=NA)
          for(i in 1:length(relvec)){

            f1=mod
            f2=relvec[i]
            relmat[i,2] <- eval(parse(text=paste("which(",f1,"$name=='",f2,"')",sep="")))
          }
          colnames(relmat) = c("path","rnr")
          return(relmat)
        }


      ### Mod1
          m=2
          of1 <- of("dirout",c("DC -> CA1"))

          resmat[1,1] <- as.character(dirout[of1$rnr[1],1])
          resmat[1,m] <- (dirout[of1$rnr[1],2])
          resmat[2,m] <- (dirout[of1$rnr[1],3])
          resmat[11,m] <- rsq[m-1]
          resmat[12,m] <- qsq[m-1]
          resmat[13,m] <- gof[m-1]
          resmat[14,m] <- bic[m-1]
          resmat[15,m] <- n_obs[m-1]

          resmatmed[2,m-1] <- NA
          resmatmed[2,m] <- NA


      ### Mod2
          m=3
          of2 <- of("rmedout",c("DC -> CA1","R -> CA1","DC -> R"))
          resmat[1,1] <- as.character(rmedout[of2$rnr[1],1])
          resmat[1,m] <- (rmedout[of2$rnr[1],2])
          resmat[2,m] <- (rmedout[of2$rnr[1],3])

          resmat[3,1] <- as.character(rmedout[of2$rnr[2],1])
          resmat[3,m] <- (rmedout[of2$rnr[2],2])
          resmat[4,m] <- (rmedout[of2$rnr[2],3])

          resmat[7,1] <- as.character(rmedout[of2$rnr[3],1])
          resmat[7,m] <- (rmedout[of2$rnr[3],2])
          resmat[8,m] <- (rmedout[of2$rnr[3],3])

          resmat[11,m] <- rsq[m-1]
          resmat[12,m] <- qsq[m-1]
          resmat[13,m] <- gof[m-1]
          resmat[14,m] <- bic[m-1]
          resmat[15,m] <- n_obs[m-1]
          rmedm <- extboot(rmedBoot,20,"",c("M0 ","R0 "))

          of2m <- of("rmedm",c("R0 -> R"))
          resmatmed[1,1] <- as.character(rmedm[of2m$rnr[1],1])
          resmatmed[1,m] <- (rmedm[of2m$rnr[1],2])
          resmatmed[2,m] <- (rmedm[of2m$rnr[1],3])


      ### Mod3
          m=4
          of3 <- of("mmedout",c("DC -> CA1","M -> CA1","DC -> M"))
          resmat[1,1] <- as.character(mmedout[of3$rnr[1],1])
          resmat[1,m] <- (mmedout[of3$rnr[1],2])
          resmat[2,m] <- (mmedout[of3$rnr[1],3])

          resmat[5,1] <- as.character(mmedout[of3$rnr[2],1])
          resmat[5,m] <- (mmedout[of3$rnr[2],2])
          resmat[6,m] <- (mmedout[of3$rnr[2],3])

          resmat[9,1] <- as.character(mmedout[of2$rnr[3],1])
          resmat[9,m] <- (mmedout[of3$rnr[3],2])
          resmat[10,m] <- (mmedout[of3$rnr[3],3])


          resmat[11,m] <- rsq[m-1]
          resmat[12,m] <- qsq[m-1]
          resmat[13,m] <- gof[m-1]
          resmat[14,m] <- bic[m-1]
          resmat[15,m] <- n_obs[m-1]

          mmedm <- extboot(mmedBoot,20,"",c("M0 ","R0 "))
          of3m <- of("mmedm",c("M0 -> M"))
          resmatmed[3,1] <- as.character(mmedm[of3m$rnr[1],1])
          resmatmed[3,m] <- (mmedm[of3m$rnr[1],2])
          resmatmed[4,m] <- (mmedm[of3m$rnr[1],3])

      ### Mod4
          m=5
          of4 <- of("bothout",c("DC -> CA1","R -> CA1","M -> CA1","DC -> R","DC -> M"))
          resmat[1,1] <- as.character(bothout[of4$rnr[1],1])
          resmat[1,m] <- (bothout[of4$rnr[1],2])
          resmat[2,m] <- (bothout[of4$rnr[1],3])

          resmat[3,1] <- as.character(bothout[of4$rnr[2],1])
          resmat[3,m] <- (bothout[of4$rnr[2],2])
          resmat[4,m] <- (bothout[of4$rnr[2],3])

          resmat[5,1] <- as.character(bothout[of4$rnr[3],1])
          resmat[5,m] <- (bothout[of4$rnr[3],2])
          resmat[6,m] <- (bothout[of4$rnr[3],3])

          resmat[7,1] <- as.character(bothout[of4$rnr[4],1])
          resmat[7,m] <- (bothout[of4$rnr[4],2])
          resmat[8,m] <- (bothout[of4$rnr[4],3])

          resmat[9,1] <- as.character(bothout[of4$rnr[5],1])
          resmat[9,m] <- (bothout[of4$rnr[5],2])
          resmat[10,m] <- (bothout[of4$rnr[5],3])

          resmat[11,m] <- rsq[m-1]
          resmat[12,m] <- qsq[m-1]
          resmat[13,m] <- gof[m-1]
          resmat[14,m] <- bic[m-1]
          resmat[15,m] <- n_obs[m-1]

          bothm <- extboot(bothBoot,20,"",c("M0 ","R0 "))
          of4m <- of("bothm",c("R0 -> R","M0 -> M"))
          resmatmed[1,1] <- as.character(bothm[of4m$rnr[1],1])
          resmatmed[1,m] <- (bothm[of4m$rnr[1],2])
          resmatmed[2,m] <- (bothm[of4m$rnr[1],3])
          resmatmed[3,1] <- as.character(bothm[of4m$rnr[2],1])
          resmatmed[3,m] <- (bothm[of4m$rnr[2],2])
          resmatmed[4,m] <- (bothm[of4m$rnr[2],3])

    # Adding names to GOF stats
          resmat[11:14,1] = c("rsq","qsq","gof","bic")
          colnames(resmat) = c("path","m1","m2","m3","m4")
          colnames(resmatmed) = c("path","m1","m2","m3","m4")

    # Extracting factor loadings
          loadings <-both$outer_loadings
          ldcm <- round(loadings[,"DC"],2) ; ldcm <- ldcm[ ldcm != 0 ]
          lcap <- round(loadings[,"R"] ,2); lcap <- lcap[ lcap != 0 ]
          lcog <- round(loadings[,"M"],2) ; lcog <- lcog[ lcog != 0 ]
          lca <- round(loadings[,"CA1"],2) ; lca <- lca[ lca!= 0 ]


return(list(extrfact=fact,resmatmed=resmatmed,resmat=resmat,dgr=dgr,sdind=sdindC,mind=mindC,cormat=desccor,descf=descf,resvif=resvif,
            models=list(dir=dir,rmed=rmed,mmed=mmed,both=both),
            modelout=list(dirout=dirout,rmedout=rmedout,mmedout=mmedout,bothout=bothout),
            loadings=list(ldcm=ldcm,lcap=lcap,lcog=lcog,lca=lca)))

}


#### Create interaction terms
RI <-(zwadj[,R])
MI <-(zwadj[,M])

RI[is.na(RI)] <- 3
MI[is.na(MI)] <- 3

mkint <-function(V1,V2){
  res<-data.frame()
  i=1
  for (a in 1:ncol(V2)){
    for(b in 1:ncol(V1)){
      res[1:nrow(V2),i] <- sqrt(V1[,a] * V2[,b])
      i=i+1
      print(i)
    }
  }
    colnames(res) <-paste("int",c(1:9),sep="")
    return(res)
}

zwadj<-zwadj[,1:47]
INT <-mkint(RI,MI)
zwadj<-cbind(zwadj,INT)
INTn <-names(INT)
head(zwadj)  
#### remove extreme pm values
zwadj2<-subset(zwadj,pm>-5)
plot(zwadj$pm)
nrow(zwadj2)
nboot=200
main<-plswrap(zwadj, c("size","age"), c())
mainf <-plswrap(zwadj2,c("size","lassets","pm","age"), c("size","lassets","age","pm"))

save.image("./data/resart4.RData")

main$resmat
mainf$resmat
main$cormat
mainf$cormat


names(main$models)

main$models$both

### ON PLS SEM
##https://www.researchgate.net/profile/Harald_Van_Heerde/publication/319360558_Addressing_Endogeneity_in_Marketing_Models/links/5ac297a145851584fa773930/Addressing-Endogeneity-in-Marketing-Models.pdf


### COPULA TESTING
# R code for correcting for endogeneity in the Corporate Reputation Data PLS model
# using Gaussian Copula Approach as descripted in Park and Gupta (2012)

# PLEASE CITE AS:
# Hult, G. T. M., J. F. Hair, D. Proksch, M. Sarstedt, A. Pinkwart, & C. M. Ringle (2018).
# Addressing Endogeneity in International Marketing Applications of Partial
# Least Squares Structural Equation Modeling. Journal of International Marketing,
# forthcoming.

# Set directory -> REPLACE WITH THE DIRECTORY INCLUDING THE EXAMPLE CSV FILE
# ON YOUR COMPUTER
#setwd ("C:/endo")

# Load required libraries -> PLEASE INSTALL THE "CAR" PACKAGE IF YOU HAVE NOT
# ALREADY. SEE https://www.r-bloggers.com/how-to-install-packages-on-r-screenshots/
# FOR INSTRUCTIONS HOW TO INSTALL A PACKAGE
library(KScorrect)

## READ MORE http://dse.univr.it/ssdev/documents/material2013/slidesBlauwSanne.pdf
#Run the he KolmogorovñSmirnov test with Lilliefors correction


# Function to create Gausian Copula
# From Gui, Raluca, Markus Meierer, and Rene Algesheimer (2017),
# "R Package REndo: Fitting Linear Models with Endogenous Regressors using
# Latent Instrumental Variables (Version 1.3)," https://cran.r-project.org/web/packages/REndo/
createCopula <- function(P){
  H.p <- stats::ecdf(P)
  H.p <- H.p(P)
  H.p <- ifelse(H.p==0,0.0000001,H.p)
  H.p <- ifelse(H.p==1,0.9999999,H.p)
  U.p <- H.p
  p.star <- stats::qnorm(U.p)
  return(p.star)
}

# Function to calculate corrected p-values for regression based on bootstrapped standard errors
bootstrapedSignificance <- function(dataset, bootstrapresults, numIndependentVariables, numCopulas){
  for (i in 1:nrow(summary(bootstrapresults))){
    t <- summary(bootstrapresults)[i, "original"] / summary(bootstrapresults)[i, "bootSE"]
    # df = n (number of observations) - k (number of independent variables + copulas) - 1
    pvalue <- 2 * pt(-abs(t),df=nrow(dataset)-numIndependentVariables-numCopulas-1)
    cat("Pr(>|t|)", rownames(summary(bootstrapresults))[i], ": ", pvalue, "\n")
  }
}

# Read data (extracted standardized latent variable scores from PLS model)
#CRDdata = read.csv2("CRP_dataset_std.csv", header=TRUE, sep=";", dec=".", stringsAsFactors=FALSE);
CRDdata <- main$extrfact
RCA1 <- CRDdata[,"CA1"]
RCA0 <- CRDdata[,"CA0"]
RDC <- CRDdata[,"DC"]
RR <- CRDdata[,"R"]
RM <- CRDdata[,"M"]
RR0 <- CRDdata[,"R0"]
RM0 <- CRDdata[,"M0"]

### Run the he KolmogorovñSmirnov test with Lilliefors correction / the independent only
LcKS(RDC, "pnorm", nreps = 4999)
LcKS(RM, "pnorm", nreps = 4999)
LcKS(RR, "pnorm", nreps = 4999)
LcKS(RCA0, "pnorm", nreps = 4999)

# Calculate standard regression
stdModel <- lm(CA1 ~ CA0 + DC + M + RR)
summary(stdModel);

# Calculate copulas for indpndent variables within model
CA0_star <- createCopula(RCA0)
DC_star <- createCopula(RDC)
RR_star <- createCopula(RR)
M_star <- createCopula(RM)

# Set bootstrapping rounds
# FOR TESTING PURPOSE, WE RECOMMEND SETTING THIS VALUE TO 100; FOR REPORTING THE FINAL
# RESULTS WE RECOMMEND SETTING IT TO 10000
bootrounds = 100

# Calculate Results
# Include Copula for CA0
# Normal regression
copulaResults1 <- lm(RCA1 ~ RCA0 + RDC + RM+RR+ M_star + 0)
summary(copulaResults1)
# Bootstrap Standard Errors
bootCopulaResults1 <- Boot(copulaResults1, R=bootrounds)
summary(bootCopulaResults1)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults1, 3, 1)

# Include Copula for LIKE
# Normal copula regression
copulaResults2 <- lm (RCA1 ~ RCA0 + RDC + RM +RR+ DC_star + 0)
summary(copulaResults2)
# Bootstrap standard errors
bootCopulaResults2 <- Boot(copulaResults2, R=bootrounds)
summary(bootCopulaResults2)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults2, 3, 1)

# Include Copula for CUSA
# Normal copula regression
copulaResults3 <- lm (RCA1 ~ RCA0 + RDC + RM + RR + M_star + 0)
summary(copulaResults3)
# Bootstrap standard errors
bootCopulaResults3 <- Boot(copulaResults3, R=bootrounds)
summary(bootCopulaResults3)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults3, 3, 1)

# Include Copula for LIKE and COMP
# Normal copula regression
copulaResults4 <- lm (RCA1 ~ RCA0 + RDC + RM + RR + M_star + DC_star + RR_star )
summary(copulaResults4)
# Bootstrap standard errors
bootCopulaResults4 <- Boot(copulaResults4, R=bootrounds)
summary(bootCopulaResults4)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults4, 4, 4)

### RESET RAMSEY TEST FOR FUNCTIONAL FORM
### sig test stat indicates non linearity
resettest(CA1 ~ CA0 + DC + M + RR)
resettest(R ~ + DC + R0)
resettest(M ~ + DC + M0)






# Include Copula for LIKE and CUSA
# Normal copula regression
copulaResults5 <- lm (CUSL ~ COMP + LIKE + CUSA + LIKE_star + CUSA_star + 0)
summary(copulaResults5)
# Bootstrap standard errors
bootCopulaResults5 <- Boot(copulaResults5, R=bootrounds)
summary(bootCopulaResults5)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults5, 3, 2)

# Include Copula for COMP and CUSA
# Normal copula regression
copulaResults6 <- lm (CUSL ~ COMP + LIKE + CUSA + COMP_star + CUSA_star + 0)
summary(copulaResults6)
# Bootstrap standard errors
bootCopulaResults6 <- Boot(copulaResults6, R=bootrounds)
summary(bootCopulaResults6)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults6, 3, 2)

# Include Copula for LIKE, COMP and CUSA
# Normal copula regression
copulaResults7 <- lm (CUSL ~ COMP + LIKE + CUSA + COMP_star + LIKE_star + CUSA_star + 0)
summary(copulaResults7)
# Bootstrap standard errors
bootCopulaResults7 <- Boot(copulaResults7, R=bootrounds)
summary(bootCopulaResults7)
# Calculate corrected p-values based on bootstrapped standard errors
bootstrapedSignificance(CRDdata, bootCopulaResults7, 3, 3)













## res for mediator paths
resmatmed = data.frame()


bothout
resmat

dirout[1,1]



### Coud be run to test interaction
#### SEMINR
library(seminr)

CA0 <- c("Q28","Q28a","Q28c")
CA1 <- c("Q16.1_t1","Q16.2_t1","Q16.4_t1")
R <- c("Q5.1_t1","Q5.2_t1","Q5.3_t1")
M <- c("Q4.4_t1","Q4.5_t1","Q4.6_t1")
R0 <- c("Q22","Q22a","Q22b")
M0 <- c("Q18","Q17a","Q17b")
DC01  <- names(dcm)
#DC01 <- c("Q19c", "Q19d", "Q20b", "Q20c")
#CONT <- c("size","age","lassets","dyn","pm")
#size  <- "size"

drops <- c("lassets","pm","assets","debt","dyn","de")
zw3 <- zwadj[ , !(names(zwadj) %in% drops)]
names(zw3)

zw4 <- subset(zwadj, pm>-5)

measurements <- constructs(
  reflective("DC",DC01),
  reflective("CA1",CA1),
  reflective("RR",R),
  reflective("R0",R0),
  reflective("M",M),
  reflective("M0",M0),
  reflective("CA0",CA0),
  composite("size",single_item("size")),
#  composite("lassets",single_item("lassets")),
 # composite("pm",single_item("pm")),
  composite("age",single_item("age")),
 # composite("dyn","dyn"),
 interaction_term("RR", "M", method =  orthogonal, weights = mode_A)
)


# Quickly create multiple paths "from" and "to" sets of constructs
structure <- relationships(
  paths(from = c("DC", "M", "RR","RR*M","CA0","size","age"),to = "CA1"),
  paths(from = c("DC", "M0"),to = "M"),
  paths(from = c("DC", "R0"),to = "RR")
)



# Dynamically compose SEM models from individual parts
pls_model <- estimate_pls(data = zw3, measurements, structure)
summary(pls_model)

foo<-PLSc(pls_model)
names(foo)
foo$path_coef
foo$outer_weights

boot_estimates <- bootstrap_model(foo, nboot = 500, cores = 2)
summary(boot_estimates)
# Use multi-core parallel processing to speed up bootstraps
boot_estimates2 <- bootstrap_model(pls_model, nboot = 500, cores = 2)
summary(boot_estimates2)

cf <- confidence_interval(boot_seminr_model = boot_estimates, from = "DC", through = "M", to = "CA1", alpha = 0.05)
cf

structure <- relationships(
  paths(from = c("DC", "R", "M", "CA0","size","age"),to = "CA1"),
  paths(from = c("DC"),to = "R"),
  paths(from = c("DC"),to = "M")
)



# Dynamically compose SEM models from individual parts
pls_modelint <- estimate_pls(data = zw3, measurements, structure)
summary(pls_modelint)
plot_scores(pls_modelint)
# Use multi-core parallel processing to speed up bootstraps
boot_estimatesint <- bootstrap_model(pls_modelint, nboot = 100, cores = 2)
summary(boot_estimatesint)







measurements <- constructs(
  reflective("DC",DC01),
  reflective("CA1",CA1),
  reflective("R",R),
  reflective("R0",R0),
  #reflective("M",M),
  #reflective("M0",M0),
  reflective("CA0",CA0),
  composite("size","size"),
  composite("assets","assets"),
  composite("debt","debt"),
  composite("age","age")
  #composite("pm","pm"),
 interaction_term("R", "M", method =  orthogonal, weights = mode_A)
)



# Quickly create multiple paths "from" and "to" sets of constructs
structure <- relationships(
    paths(from = c("DC", "R", "CA0","size","age","debt","assets"),to = "CA1"),
    paths(from = c("DC", "R0"),to = "R")
    )



# Dynamically compose SEM models from individual parts
pls_model <- estimate_pls(data = zwadj, measurements, structure)
summary(pls_model)

# Use multi-core parallel processing to speed up bootstraps
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)




#### EXTRA
### Model set 1: Path dependency in mediator and with DC mid way

zwadj <- subset(zwadj,!is.na(zwadj$ca0))
zwadj <- subset(zwadj,!is.na(zwadj$r))


mod1 <- lm(ca1 ~ ca0 + r + dcm + size + age + assets + pm, data=zwadj);summary(mod1)
mod1m <- lm(r ~  + dcm + r0 + size + age + assets + pm, data=zwadj);summary(mod1m)
med.out1 <- mediate(mod1m,mod1, treat="dcm",mediator="r",robustSE=TRUE, sims=1000)
summary(med.out1) ## ADE is insignificant telling us that DL has no direct effect on OR

mod1 <- lm(ca1 ~ ca0 + dcm + m + size + age + assets + pm, data=zwadj);summary(mod1)
mod1m <- lm(m ~ + dcm + m0 + size + age + assets + pm, data=zwadj);summary(mod1m)
med.out1 <- mediate(mod1m,mod1, treat="dcm",mediator="m",robustSE=TRUE, sims=1000)
summary(med.out1) ## ADE is insignificant telling us that DL has no direct effect on OR


x=zwadj[,c("ca0","size","age","assets","pm","m","r")]
y=zwadj[,"ca1"]
pred=zwadj$dcm
nrow(zwadj)

data.bin<-data.org(x,y,mediator=6:7,jointm=list(n=1,j1=c(6,7)),pred=pred, predref="M",alpha=0.05,alpha2=0.05)
summary(data.bin)

## data.bin<-data.org(x,y,mediator=7,pred=pred, predref="M",alpha=0.05,alpha2=0.1)
## summary(data.bin)


temp1<-mma::boot.med(data=data.bin,n=2,n2=4,nonlinear=TRUE)
temp1
summary(temp1)

data.bin<-data.org(x,y,mediator=7:8,pred=pred, predref="M",alpha=0.1,alpha2=0.1)
summary(data.bin)

temp1<-mma::boot.med(data=data.bin,n=2,n2=4,nonlinear=FALSE)
temp1
summary(temp1)


## interpreting https://openresearchsoftware.metajnl.com/articles/10.5334/jors.160/
## example from business
##Chen CC, Chiu SF. An integrative model linking supervisor support and organizational citizenship behavior. Journal of Business and Psychology. 2008;23:1–10. [Google Scholar]


data.contx<-data.org(x,y,pred=pred,contmed=c(5:6),
                     alpha=0.4,alpha2=0.4)

summary(data.contx)

temp1<-mma::boot.med(data=data.contx,n=1,n2=2)
temp1
summary(temp1)
data('weight_behavior')
x=weight_behavior[,2:14]
y=weight_behavior[,1]



data("weight_behavior")
x=weight_behavior[,2:14]
y=weight_behavior[,15]
data.bin<-data.org(x,y,pred=2,mediator=c(1,3:13), jointm=list(n=1,j1=c("tvhours","cmpthours","cellhours")), predref="F",alpha=0.4,alpha2=0.4)

x=weight_behavior[,c(3:14)]
pred=weight_behavior[,2]
y=weight_behavior[,15]

data.b.b.2.1<-data.org(x,y,mediator=5:12,jointm=list(n=1,j1=c(5,7,9)),
pred=pred,predref="M", alpha=0.4,alpha2=0.4)

> summary(data.bin)
######## ADJUSTING DATASET WITH DELTA



zwadj$d1 <- zwadj$Q5.4_t1 - zwadj$Q21c
zwadj$d2 <- zwadj$Q5.6_t1 - zwadj$Q21d
zwadj$d3 <- zwadj$Q5.7_t1 - zwadj$Q21e


#zwbck  <- zwadj


zwadj <- zwbck
### Making indexes for further analysis

zwadj$m  <- rowMeans(m1)
zwadj$r  <- rowMeans(or1)
zwadj$m0  <- rowMeans(m0)
zwadj$r0  <- rowMeans(or0)
zwadj$ca1  <- rowMeans(ca1)
zwadj$ca0  <- rowMeans(ca0)
zwadj$dc0  <- rowMeans(dc002)
zwadj$dc1  <- rowMeans(dc02)
zwadj$dcm  <- (zwadj$dc1+zwadj$dc0)/2
zwadj$ddc  <- zwadj$dc1-zwadj$dc0
zwadj$dm  <- (zwadj$m-zwadj$m0)/zwadj$m0
zwadj$dr  <- (zwadj$r-zwadj$r0)/zwadj$r0



am1 <- round(psych::alpha(m1)$total[1],2)
am1
