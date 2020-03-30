#h.degree,pt,mis.specified
rm(list=ls())
args=commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

# h.degree=0 
# pt=0.7 
# mis.specified=1
# rand.seed=6
set.seed(rand.seed)

library(mvtnorm)

#setwd("C:/Users/Shuxi ZENG/Dropbox/Third Year/OW_Randomization/Code")

source("Crude.R")
source("IPWC.R")
source("OW.R")
source("LinearR.R")
#source("DoubleRobust.R")

###Parameter fixed
#n<-60
p<-10
nsim<-2000
#Treatment effect
gamma<-0
##signal_level: Control the explain power of feature, R^2
signal_level=1.0

#Prob of being treated
#pt=0.5

##Control whether the model is correctly specified
##If correctly, then use linear outcome model
##If not, then add quadratic interaction term
#mis.specified=0

##Control the degree of heterogeneout treatment effect 
#h.degree=0.5
##Store Results
RMSE=NULL
MC_SD=NULL
EST_SD=NULL
CRATE=NULL
BIAS=NULL

n.grid=seq(50,200,by=10)

for (n in n.grid){

EST<-SE<-COVER<-matrix(NA,nsim,4)
colnames(EST)<-colnames(SE)<-colnames(COVER)<-c("UNADJ","IPW","OW","LR")

for(i in 1:nsim){
  x<-rmvnorm(n,rep(0,p),diag(1,p))
  z<-rbinom(n,1,pt)
  ##Use hypergeometric to ensure existence of treat/control
  # z=rep(0,n)
  # z[sample(1:n,round(n*pt),replace=F)]=1
  bsq<-2/(1+2+2^2+2^3+2^4)
  betasq<-rep(c(bsq,2*bsq,2^2*bsq,2^3*bsq,2^4*bsq),each=p/5)
  
  deltasq<-2
  betasq=(signal_level* deltasq^2)*betasq/sum(betasq)
  beta<-sqrt(betasq) 
  
  ##
  beta.tau=rep(1,p)
  beta.tau=sqrt(signal_level* deltasq^2)*beta.tau

  
  ##Add second order term if misspecified
  if (mis.specified==1){
  x.inter=x[,1:(p-1)]*x[,2:p]
  
  beta.quad<-rep(bsq,p-1)
  beta.quad=(signal_level* deltasq^2)*beta.quad/sum(beta.quad)
  beta.quad=sqrt(beta.quad)
  
  y<-as.numeric(x%*%beta+x%*%beta.tau*z*h.degree+x.inter%*%beta.quad+gamma*z+rnorm(n,0,sqrt(deltasq)))
  ##Correctly Specified
  }else{
    y<-as.numeric(x%*%beta+x%*%beta.tau*z*h.degree+gamma*z+rnorm(n,0,sqrt(deltasq)))
  }
  
  W=cbind(1,x)
  ##Finite sample gamma
  gamma_finite=mean(x%*%beta.tau*h.degree)+gamma
  
  # UNADJ
  res.crude <- Crude(y=y, z=z, W=W)
  EST[i,1] <- res.crude$tau
  SE[i,1]<-res.crude$se
  COVER[i,1]<-(gamma<EST[i,1]+qnorm(0.975)*SE[i,1])&(gamma>EST[i,1]-qnorm(0.975)*SE[i,1])
  
  # IPW
  res.IPWC <- IPWC(y.all=y, z.all=z, W.all=W, q.all=0)
  EST[i,2] <- res.IPWC$tau
  SE[i,2] <- res.IPWC$se
  COVER[i,2]<-(gamma<EST[i,2]+qnorm(0.975)*SE[i,2])&(gamma>EST[i,2]-qnorm(0.975)*SE[i,2])
  
  # OW
  res.OW <- OW(y=y, z=z, W=W)
  EST[i,3] <- res.OW$tau
  SE[i,3] <- res.OW$se
  COVER[i,3]<-(gamma<EST[i,3]+qnorm(0.975)*SE[i,3])&(gamma>EST[i,3]-qnorm(0.975)*SE[i,3])
  
  #Supply the one without intercept
  res.LR <- LR(y=y, z=z, W=x)
  EST[i,4] <- res.LR$tau
  SE[i,4] <- res.LR$se
  COVER[i,4]<-(gamma<EST[i,4]+qt(0.975,n-(p+1)*2)*SE[i,4])&(gamma>EST[i,4]-qt(0.975,n-(p+1)*2)*SE[i,4])
  #COVER[i,4]<-(gamma_finite<EST[i,4]+qt(0.975,n-(p+1)*2)*SE[i,4])&(gamma_finite>EST[i,4]-qt(0.975,n-(p+1)*2)*SE[i,4])
  
  #DR
  #Supply the one without intercept
  # res.DR <- DR(y=y, z=z, W=x)
  # EST[i,5] <- res.DR$tau
  # SE[i,5] <- res.DR$se
  # 
  
  if(i%%500==0)print(i)
}

# Results
# Filter numeric error
SE[SE>100]=NA
RMSE=rbind(RMSE,c(n,sqrt(colMeans((EST-gamma)^2,na.rm=T))))
MC_SD=rbind(MC_SD,c(n,apply(EST,2,FUN=function(x){sd(x,na.rm=T)})))
EST_SD=rbind(EST_SD,c(n,colMeans(SE,na.rm=T)))
CRATE=rbind(CRATE,c(n,colMeans(COVER,na.rm=T)))
BIAS=rbind(BIAS,c(n,abs(colMeans(EST,na.rm=T)-gamma)))
print(paste(n," finished"))
}
# colMeans(EST)
# colMeans(SE)
# apply(EST,2,sd)
# colMeans(matrix(gamma, nrow(EST), ncol(EST)) <= EST + qnorm(0.975)*SE & 
#            matrix(gamma, nrow(EST), ncol(EST)) >= EST - qnorm(0.975)*SE)
setwd("cont/")
save(RMSE,MC_SD,EST_SD,CRATE,BIAS,file=paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
setwd("..")
source("plot_results_cont.R")