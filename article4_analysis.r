

setwd("/Users/larshovdanmolden/Documents/git/article3")
source("./estimation/packages.r")
source("./estimation/article_functions.r")

require("MASS")
library(sampleSelection)
library(mediation)
library(reshape)
library(huxtable)
install.packages("huxtable")
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


## ## Creating variables
## sense0 <- rowMeans(zw[,c("Q19", "Q19a", "Q19b", "Q19c", "Q19d", "Q20d")], na.rm=TRUE)
## seize0 <- as.double(rowMeans(zw[,c( "Q19e", "Q20b", "Q19c", "Q25", "Q20")], na.rm=TRUE))
## tran0 <- as.double(rowMeans(zw[,c("Q20c", "Q25a", "Q25b", "Q25c")], na.rm=TRUE))
## zw$dc0 <- rowMeans(cbind(sense0,seize0,tran0),na.rm=TRUE)

## sense1 <- as.double(rowMeans(zw[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q7.3_t1")], na.rm=TRUE))
## seize1 <-as.double(rowMeans(zw[,c("Q6.6_t1","Q7.1_t1","Q6.4_t1","Q7.5_t1","Q6.7_t1")], na.rm=TRUE))
## tran1 <- as.double(rowMeans(zw[,c("Q7.2_t1","Q7.6_t1","Q7.7_t1","Q7.8_t1")], na.rm=TRUE))
## zw$dc1 <- rowMeans(cbind(sense1,seize1,tran1),na.rm=TRUE)

## zw$ca0 <-  rowMeans(zw[,c("Q28","Q28a", "Q28b", "Q28c")], na.rm=TRUE)
## zw$ca1 <-  rowMeans(zw[,c("Q16.1_t1","Q16.2_t1", "Q16.3_t1", "Q16.4_t1")], na.rm=TRUE)
## zw$or0 <-  rowMeans(zw[,c("Q21c", "Q21d", "Q21e", "Q22", "Q22a", "Q22b", "Q22c", "Q22d")], na.rm=TRUE)
## zw$or1 <-  rowMeans(zw[,c("Q5.1_t1", "Q5.2_t1", "Q5.3_t1", "Q5.4_t1", "Q5.5_t1", "Q5.6_t1", "Q5.7_t1")], na.rm=TRUE)
## zw$dl1 <-  rowMeans(zw[,c("Q8.1_t1", "Q8.2_t1", "Q8.3_t1" , "Q8.4_t1" , "Q8.5_t1", "Q8.6_t1")], na.rm=TRUE)
## zw$dyn <-  rowMeans(zw[,c("Q27", "Q27a", "Q27b", "Q27c", "Q27e", "Q27g")], na.rm=TRUE)
## zw$ma <-  rowMeans(zw[,c("Q24","Q24a", "Q24b", "Q23b","Q23c")], na.rm=TRUE)

## zw$ddc <- as.double(zw$dc1-zw$dc0)
## zw$dor <-  zw$or1-zw$or0
## zw$size <- zw$Q1_t1
## zw$age <- zw$Q2_t1
## zw$dynhigh <- ifelse(zw$dyn > 4, 1,0)
## zw$dynstable <- ifelse(zw$dyn < 4, 1,0)

## zw$normdyn <-  scale(zw$dyn)
## ## Adjusting data set by removing NA for pm
## #zwadj <- zw[!is.na(zw$pm),]


## ## Correlations
## ### (2)  DESCRIPTIVE ANALYSIS EVOUSJON OF CAPABILITIES
## cormat <- zw[,c("dl",
##                 "or0",
##                 "or1",
##                 "dc0",
##                 "dc1",
##                 "ca0",
##                 "ca1")]
##                 ## "size",
##                 ## "age",
##                 ## "dyn",
##                 ## "pm",
##                 ## "lassets")]



## varmean <- apply(cormat,2, mean, na.rm=TRUE)
## varsd <- apply(cormat,2, sd, na.rm=TRUE)


## art2cor <-cor(cormat, use="complete.obs")

## upper<-round(art2cor,2)
## upper[upper.tri(art2cor)]<-""
## upper<-as.data.frame(upper)
## vardesc <- cbind(varmean,varsd)
## upper <- cbind(vardesc,upper)
## rownames(upper) <- c("1) DL",
##                      "2) OR (T=1)",
##                      "3) OR (T=2)",
##                      "4) DC (T=1)",
##                      "5) DC (T=2)",
##                      "6) CA (T=1)",
##                      "7) CA (T=2)")
##                      ## "8) Firm Size",
##                      ## "9) Firm Age",
##                      ## "10) Env.dynamism",
##                      ## "11) Firm PM",
##                      ## "12) ln Firm Assets")


## art2cor <- xtable(upper)
## names(art2cor) <- c("Mean","SD",paste(seq(1:7),sep=","))

## art2cor


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

## including only observations at both times
zwadj <- subset(zw,!is.na(Q4.1_t1))
rownames(zwadj) <- NULL

#### Princomp with only observed no imputation
dc1 <-  zwadj[,c("Q6.1_t1","Q6.2_t1","Q6.3_t1","Q6.4_t1","Q6.5_t1","Q6.6_t1","Q6.7_t1","Q7.1_t1","Q7.2_t1","Q7.3_t1","Q7.4_t1")]
dc1 <-  dc1[which(!is.na(dc1)),]
pdc <- fa.poly(dc1,2)
pdc

dc1adj <- cbind(dc11,dc12)
dc1adj <- round(dc1adj[which(!is.na(dc1adj[,1])),],0)
pdcimp <- fa.poly(dc1adj,2)
pdcimp
which(is.na(dc1adj[,1]))

ca1 <-  zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1")]
ca1 <-  ca1[which(!is.na(ca1)),]
pca1 <- fa.poly(ca1,2)
pca1

ca1adj <-  round(imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.3_t1","Q16.4_t1")]),0)
pca1imp <- fa.poly(ca1adj,2)
pca1imp

ca0 <-  zwadj[,c("Q28","Q28a","Q28b","Q28c")]
ca0 <-  ca0[which(!is.na(ca0)),]
pca0 <- fa.poly(ca0,2)
pca0

ca0adj <- round(imputezw(zwadj[,c("Q28","Q28a","Q28b","Q28c")]),0)
pca0imp <- fa.poly(ca0adj,2)
pca0imp

or1 <-   zwadj[,c("Q5.1_t1", "Q5.2_t1", "Q5.3_t1", "Q5.4_t1", "Q5.6_t1", "Q5.7_t1")]
or1 <-  or1[which(!is.na(or1)),]
por1 <- fa.poly(or1,2)
por1

or1adj <-   round(imputezw(zwadj[,c("Q5.4_t1", "Q5.6_t1", "Q5.7_t1")]),0)
por1imp <- fa.poly(or1adj,1)
por1imp

ma1 <-  zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")]
ma1 <-  ma1[which(!is.na(ma1)),]
pma1 <- fa.poly(ma1,1)
pma1


ma1adj <-   round(imputezw(zwadj[,c("Q8.7_t1", "Q8.8_t1", "Q8.9_t1")]),0)
pma1imp <- fa.poly(ma1adj,1)
pma1imp





## Running median imputations
dc01 <-  imputezw(zwadj[,c("Q6.2_t1","Q6.4_t1", "Q6.5_t1","Q7.1_t1","Q7.2_t1")] )
dc02 <-  imputezw(zwadj[,c("Q6.1_t1","Q6.3_t1","Q7.4_t1")] )
ca0 <-  imputezw(zwadj[,c("Q28","Q28a","Q28c")] )
ca1 <-   imputezw(zwadj[,c("Q16.1_t1","Q16.2_t1","Q16.4_t1")] )
or1 <-   imputezw(zwadj[,c("Q5.4_t1","Q5.6_t1","Q5.7_t1")])
ma01 <-  imputezw(zwadj[,c( "Q8.7_t1", "Q8.8_t1", "Q8.9_t1")])
complist <- list(dc01=dc01,dc02=dc02,ca0=ca0,ca1=ca1,or1=or1,ma1=ma01)
vcontrols <- zwadj[,c("size","age","dyn","pm","lassets")]
zwadj <- cbind(dc01,dc02,ca0,ca1,or0,or1,ma01,vcontrols)


names(complist[[1]])

##Factor extraction full model based on insight from Polychoric PCA
items <- c("Q6.2_t1","Q6.4_t1", "Q7.1_t1","Q7.2_t1","Q6.5_t1", #DC1
           "Q6.1_t1","Q6.3_t1","Q7.4_t1", #DC2
           "Q28","Q28a","Q28c", #CA0
           "Q16.1_t1","Q16.2_t1","Q16.4_t1", #CA1
           "Q5.4_t1","Q5.6_t1","Q5.7_t1", #OR1
           "Q8.7_t1", "Q8.8_t1", "Q8.9_t1") #MA

latents <- c(rep("DC01",5),
             rep("DC02",3),
             rep("CA0",3),
             rep("CA1",3),
             rep("OR1",3),
             rep("MA1",3))


mm <- cbind(latents,items); colnames(mm) <- c("source","target")
iv <- c("MA1","CA0","DC01","DC01","DC02","DC02","OR1","DC01","DC02")
dv <- c("CA1","CA1","CA1","MA1","CA1","MA1","CA1","OR1","OR1")
sm <- cbind(iv,dv);colnames(sm) <- c("source","target")

FULL <- plsm(data = zwadj, strucmod = sm, measuremod = mm)
full <- sempls(model = FULL, data = zwadj, wscheme = "centroid",maxit=1000)
full

pathDiagram(full, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")

fullBoot <- bootsempls(full, nboot = 200, start = "ones", verbose = FALSE)
fullBoot

fullCI <- data.frame(semPLS::dgrho(full))
fullR <- semPLS::rSquared(ecsi)
fullGOF <- semPLS::gof(ecsi)

constructs <- unique(latents)
cfull <- full$coefficients[grep("^lam",rownames(full$coefficients)),]
cfull$num <- substring(rownames(cfull),7,7)

extcomp <- function(cname,cfull){
    ## Function to extract loadings from a PLS object

    cest <-  cfull[grep(paste("^",cname,sep=""),cfull$Path),]$Estimate
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
colnames(calpha) <- c("alpha","cnam");calpha$cnam <- toupper(calpha$cnam)
calpha


## extracting factor loadings
ctab <- lapply(constructs,extcomp, cfull=cfull)
ctab <- melt(ctab)
ctab$iname <- paste(ctab$cnam,"_",cfull$num,sep="")
ctab
calpha


## Extracting DG rho
fullCI$cnam <- rownames(fullCI)
cci <- fullCI[,c(3,1)];colnames(cci) <- c("cnam","rho")
ctab <- merge(ctab,calpha,by="cnam",all.x=TRUE)
ctab <- merge(ctab,cci,by="cnam")
## Creating factor output table


ht <- hux(
    Factor = ctab$cnam,
    Items = ctab$iname,
    Loading = round(ctab$value,3),
    Alpha = round(ctab$alpha,3),
    CI = round(ctab$rho,3),
    add_colnames = TRUE
)
f <- which(cfull$num==1)+1

cfull

f
ht
bold(ht)[1,]           <- TRUE
#bottom_border(ht)[1,]  <- 2
bottom_border(ht)[f,]  <- 2
align(ht)[,2]          <- 'right'
right_padding(ht)      <- 10
left_padding(ht)       <- 10
width(ht)              <- 0.35
number_format(ht)[,c(3,4,5)]      <- 2
ht$Alpha[duplicated(ht$Alpha)] <- NA
ht$CI[duplicated(ht$CI)] <- NA
ht$Factor[duplicated(ht$Factor)] <- NA

f
ht
quick_pdf(ht, file = 'motorcarsdata.pdf')
ht

class(ht)

ht
ht$Alpha[duplicated(ht$Alpha)] <- NA
ctab
class(ctab)


    constructs
ctab <- lapply(

foo <- extcomp(constructs[1],cfull)
str(foo)
foo
cname <- constructs[1]
cname

cfull[grep(paste("^",cname, sep=""),cfull$Path),]$Estimate

cdc1 <-


    paste("^",cname,sep="")
foo

grep("^lam",rownames(full$coefficients))





pathDiagram(full, file = "ecsiStructure", full = FALSE, edge.labels = "both", output.type = "graphics", digits = 2,graphics.fmt="pdf")


ecsiBoot <- bootsempls(ecsi, nboot = 200, start = "ones", verbose = FALSE)
ecsiBoot

semPLS::rSquared(ecsi)
semPLS::gof(ecsi)
semPLS::dgrho(ecsi)
semPLS::dgrho(ecsi)




## Evaluating PLS model https://cran.r-project.org/web/packages/semPLS/vignettes/semPLS-intro.pdf
