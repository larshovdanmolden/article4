
### Removes certain uninteresting variables after do.call to merge list of frames
cleanres <- function(df){
    tokeep <- which(df$term=="value")
    df2 <- data.frame(df[tokeep,])
    return(df2)
    }


### Stacks results on top of eachother
stackres <- function(ls,sign){

    names <- names(ls)
    templist1 <- list()
    templist2 <- list()

    for(i in 1:length(ls)){
        templist1[[i]] <- ls[[i]][,c(1,3,6)]
        templist2[[i]] <- ls[[i]][,c(1,3)]
    }

    res <- data.frame(do.call("cbind",templist1))
    estv <- grep("estimate",colnames(res))
    pv <- grep("p.value",colnames(res))

    beta <- res[,estv];colnames(beta) <- paste("coef.",names, sep="")
    sig <- res[,pv];colnames(sig) <- paste("p.",names, sep="")
    q <- res[,1]

    finres1 <- cbind(q,beta,sig)
    finres2 <- cbind(q,beta)

    if(sign){return(finres1)} else {return(finres2)}

 }


### Extract certain variables from a panel and make sum
extsfress <- function(var,nam){

    res <- data.frame(sfp  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(sum(!! var,na.rm=TRUE)))
    colnames(res) <- c("orgnr",nam)

    return(res)
    }

### Extract certain variables from a panel and make means
extsfresm <- function(var,nam){

    res <- data.frame(sfp  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(mean(!! var,na.rm=TRUE)))
    colnames(res) <- c("orgnr",nam)

    return(res)
    }

### Function to run many poisson regressions to test pairwise with a dependent
po <- function(df){


    mod <- glm(spin~., data=df, family="poisson")
    ext <- grep("Q",names(mod$coefficients))
    res.coef <- mod$coefficients[ext]
    res.p <- summary(mod)$coefficients[ext,4]

    return(list(coef=res.coef,p=res.p))
    }



### function to build moderator plots
meplot <- function(model,var1,var2,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=3,me_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black", rnum=FALSE){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
                                         ylim=c(min(lwr),max(upr)),
      # ylim=c(-0.25,0),
       xlab = xlab,
       ylab = ylab,
       main = main)
  lines(z0, dy.dx, lwd = me_lwd, lty = me_lty, col = me_col)
 # lines(z0, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
 # lines(z0, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)

  if(rnum){return(dy.dx)}
}


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
