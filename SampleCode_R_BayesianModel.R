# 20200213
# This sample code is to purduce bayesian hierarchical logistic model for meta-analysis

#install.packages("BRugs", configure.args = c(Rmpfr = "--with-openbugs=/Users/jiax/.wine/drive_c/Program Files (x86)/OpenBUGS/OpenBUGS323"))


library(readxl)
library(tidyverse)
#loads the BRugs package
library(BRugs)
library(rjags)
library(coda)
library(R2OpenBUGS) 
library(stringr)
library("colorRamps")
library(forestplot)
library(rmeta)

D <- read_excel("~/Documents/ccf/IBTR_whole Meta/BTreatmentMete/list.xlsx")

D <- D %>% separate(`Years Conducted`, c("A", "B"))

D$midyear <- (as.numeric(D$A) + as.numeric(D$B))/2
D$midyear[13] <- "2007 approximate"
D$midyear[14] <- "2007 approximate"


D <- D[ order( D$"midyear", decreasing=FALSE ), ]
D$midDur <- parse_number(D$midyear)
D$treatment <- as.factor(D$...3)
D$rate <- D$`5-year incidencerate`
D$RecurN <- D$rate * D$`Number of Patients`/100
D$Ntotal <- D$`Number of Patients`
D$RecurN <- round(D$RecurN)
D$N.margin = D$Ntotal

D$Trial[c(2,4,6,8,10,12,14)] <- D$Trial[c(1,3,5,7,9,11,13)]

A <- list()
for(i in 1:length(levels(D$treatment))){
  A[[i]] <- D[ which(D$treatment==levels(D$treatment)[i]), ]
}

######################
### Meta-analysis ####
sigmoid <- function(psi){ out <- 1/(1+exp(-psi)); return(out) }
logit <- function(a0){ out <- log(a0) - log(1-a0); return(out) }
boa.hpd <- function(x, alpha){
  n <- length(x); m <- max(1, ceiling(alpha * n))
  y <- sort(x); a <- y[1:m]; b <- y[(n - m + 1):n];
  i <- order(b - a)[1]
  structure(c(a[i], b[i]), names = c("Lower Bound", "Upper Bound")) }

### treatment ###
bugdata <- list(n.1=length(A[[1]]$RecurN), NRec.1=A[[1]]$RecurN, N.1=A[[1]]$Ntotal, 
                n.2=length(A[[2]]$RecurN), NRec.2=A[[2]]$RecurN, N.2=A[[2]]$Ntotal,
                a=0, b=1/200, c=10) 

inits <- list(mu.1=rep(0,bugdata$n.1), M.1=logit( sum(A[[1]]$RecurN)/sum(A[[1]]$Ntotal)), sd.M.1=1,
              mu.2=rep(0,bugdata$n.2), M.2=logit( sum(A[[2]]$RecurN)/sum(A[[2]]$Ntotal)), sd.M.2=1)

#mu.1 the theta for each study, M.1 is the average of theta over all the studye, 

#write.datafile(bugdata,"bdata.txt"); bugsData(inits,"inits.txt")

m <- jags.model("UnivariateBinMetaReg-1.txt", data=bugdata, inits=inits, n.chains=4, quiet=FALSE)
update(m, 10000, progress.bar="none")
S <- coda.samples(m, c("pi.1","pi.2",
                       "pr.1","pr.2",
                       "M.1","M.2"), n.iter=30000, progress.bar="none")

sumS <- summary(S) 
#saveRDS(S,"postSamps-1.RDS")
xlsx::write.xlsx(sumS[[1]], file="post-sum-1.xlsx", sheetName="moments", row.names=TRUE ); AP=TRUE
xlsx::write.xlsx(sumS[[2]], file="post-sum-1.xlsx", append=AP, sheetName="quantiles", row.names=TRUE )


# ### time by treatment ###
# ### Time constant ###
# 
# bugdata <- list(n.1=length(A[[1]]$RecurN), NRec.1=A[[1]]$RecurN, N.1=A[[1]]$N.margin, x.1=A[[1]]$midDur-2000, 
#                 n.2=length(A[[2]]$RecurN), NRec.2=A[[2]]$RecurN, N.2=A[[2]]$N.margin, x.2=A[[2]]$midDur-2000,
#                 a=0, b=1/200, c=10, d=0, e=0.001) 
# 
# inits <- list(mu.1=rep(0,bugdata$n.1), M.1=logit( sum(A[[1]]$RecurN)/sum(A[[1]]$N.margin) ), sd.M.1=1,
#               mu.2=rep(0,bugdata$n.2), M.2=logit( sum(A[[2]]$RecurN)/sum(A[[2]]$N.margin) ), sd.M.2=1, 
#               Beta=0)
# bugsData(bugdata,"bdata.txt"); bugsData(inits,"inits.txt")
# 
# m <- jags.model("UnivariateBinMetaReg-2.txt", data=bugdata, inits=inits, n.chains=4, quiet=FALSE)
# 
# update(m, 10000, progress.bar="none")
# S <- coda.samples(m, c("pi.1","pi.2",
#                        "pr.1","pr.2",
#                        "M.1","M.2",
#                        "Beta"), n.iter=30000, progress.bar="none")
# sumS <- summary(S) #; geweke.diag(S)
# saveRDS(S,"postSamps-2.RDS")
# xlsx::write.xlsx(sumS[[1]], file="post-sum-2.xlsx", sheetName="moments", row.names=TRUE ); AP=TRUE
# xlsx::write.xlsx(sumS[[2]], file="post-sum-2.xlsx", append=AP, sheetName="quantiles", row.names=TRUE )


######################################

sS <- function(S,tar,i=NA,N.chain=4){
  if(!is.na(i)){  u <- which( colnames(head(S)[[1]]) == paste0(tar,"[",i,"]") )
  } else{  u <- which( colnames(head(S)[[1]]) == tar ) 
  }
  out <- S[[1]][,u]; for(k in 2:N.chain){ out <- c(out, S[[k]][,u]) }
  return(out)
}
sigmoid <- function(psi){ out <- 1/(1+exp(-psi)); return(out) }
logit <- function(a0){ out <- log(a0) - log(1-a0); return(out) }
IP <- function(P,X){ return( X%*%P ) }
HPD <- function(x, alpha){ out1 <- boa.hpd(x,alpha); out2 <- median(x); return( c(out1[1],out2,out1[2]) ) }

Prob <- function(X,Pars){
  #out <- apply( sigmoid( apply(Pars,MARGIN=1,FUN=IP,X=X) ), MARGIN=2, FUN=quantile, prob=c(0.025,0.5,0.975) )
  out <- apply( sigmoid( apply(Pars,MARGIN=1,FUN=IP,X=X) ), MARGIN=2, FUN=HPD, alpha=0.05 )
  return(out)
}

boa.hpd <- function(x, alpha){
  n <- length(x); m <- max(1, ceiling(alpha * n))
  y <- sort(x); a <- y[1:m]; b <- y[(n - m + 1):n];
  i <- order(b - a)[1]
  structure(c(a[i], b[i]), names = c("Lower Bound", "Upper Bound")) }

######################################################################### 

Pars.1 <-  sS(S,"M.1")
Pars.2 <-  sS(S,"M.2")
Pars <- list( Pars.1,Pars.2)
IN <- 0.25
LWD <- 3
Leg.tx <- unique(paste0(D$Trial,"-",str_sub(D$midyear)))
ss <- sample(1:length(Leg.tx),length(Leg.tx),replace=FALSE)
Col <- matlab.like(length(Leg.tx))[ss]; names(Col) <- Leg.tx

### Data Plots ####
pdf("margin-Data2.pdf",width=10, height=6)
par(mfrow=c(1,2)) 
for(Ii in 1:2){
  leg.tx <- paste0(A[[Ii]]$Trial,"-",str_sub(A[[Ii]]$"midyear"))
  col.tx <- sapply(X=leg.tx, FUN=function(X,cl) cl[which(names(cl)==X)], cl=Col )
  plot(-10,-10,xlab="",ylab="",cex.axis=1.6,cex.main=2.1,lwd=3, xlim=c( range(parse_number(D$midyear))[1]-2,range(parse_number(D$midyear))[2]+2), ylim=c(0,0.07),
       main=paste0("Treatment ",levels(D$treatment)[Ii]))
  mtext("5-years IBTR",side=2,line=2.7,cex=1.6)
  mtext("enrollment interval mid-point",side=1,line=3.4,cex=1.6)
  o <- order(A[[Ii]]$Ntotal, decreasing=TRUE)
  symbols(x=parse_number(A[[Ii]]$midyear)[o], y=c(A[[Ii]]$RecurN/A[[Ii]]$Ntotal)[o], circles=sqrt(c(A[[Ii]]$Ntotal/pi)[o]), inches=IN, ann=F, bg=col.tx, fg=NULL, add=TRUE)
  legend("topleft",leg.tx,col=col.tx,lwd=4,cex=0.8)
  }

dev.off()

## get forest plot data

Prob <- function(Pars){
  #out <- apply( sigmoid( apply(Pars,MARGIN=1,FUN=IP,X=X) ), MARGIN=2, FUN=quantile, prob=c(0.025,0.5,0.975) )
  out <- apply( sigmoid(Pars), MARGIN=2, FUN=HPD, alpha=0.05 )
  return(out)
}

pi1 <-  HPD(sigmoid(Pars[[1]]),0.05) 
pi2 <-  HPD(sigmoid(Pars[[2]]),0.05) 

levels(D$treatment)
c(7,7)

fout <- as.data.frame(rbind(pi1, pi2))
colnames(fout) <- c("95% Lower Bound", "5-year IBTR" ,        "95% Upper Bound")
rownames(fout) <- c("PBI","WBI")
xlsx::write.xlsx(fout, file="PosteriorEstimates.xlsx", sheetName="quantiles", row.names=TRUE )


tabletext. <- rbind( c("Treatment","N"), cbind(levels(D$treatment),c(7,7)))
f <- as.data.frame(rbind(c(NA, NA, NA),  pi1, pi2))
colnames(tabletext.) <- c("Treatment","N")



pdf("ForestPlots.pdf", onefile = F,width=6, height=3)
forestplot(tabletext.,
           f$V2,
           f$`Lower Bound`,
           f$`Upper Bound`,
           zero=3,
           title = "",
           col=meta.colors(box="darkblue",
                           line="darkblue"),
           digitsize=1.3, xlow=0, xhigh=0.35,
           graphwidth=unit(4,"inches"),
           boxsize = as.vector(c(1,1)))
grid.text("Forest Plots - IBTR", .5, .9, gp=gpar(cex=1))
dev.off()

