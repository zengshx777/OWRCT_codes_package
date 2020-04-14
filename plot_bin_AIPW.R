#rare.degree,h.degree,pt,mis.specified
rm(list=ls())
args=commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
# rare.degree=-2.197 
# h.degree=0 
# pt=0.5 
# mis.specified=0 
# rand.seed=12
set.seed(rand.seed)

library(mvtnorm)
#setwd("C:/Users/Shuxi ZENG/Dropbox/Third Year/OW_Randomization/Code")

source("Crude.R")
source("IPWC.R")
source("OW.R")
source("LinearR.R")
source("PS_AIPW.R")
#source("DoubleRobust.R")

###Parameter fixed
p<-10
nsim<-2000
#Treatment effect; SET AT 0
gamma<-0
##signal_level: Control the explain power of feature, R^2
signal_level=1
logit=1
# #Prob of being treated
# pt=0.5
# #Rare degree: 0(50%),0.8(68%),3 (95%)
# rare.degree=0
# ##Control whether the model is correctly specified
# ##If correctly, then use linear outcome model
# ##If not, then add quadratic interaction term
# mis.specified=0
# ##Control the degree of heterogeneout treatment effect 
# h.degree=0.3

##Store Results
RMSE_DIFF=NULL
MC_SD_DIFF=NULL
EST_SD_DIFF=NULL
CRATE_DIFF=NULL
BIAS_DIFF=NULL

RMSE_ODDS=NULL
MC_SD_ODDS=NULL
EST_SD_ODDS=NULL
CRATE_ODDS=NULL
BIAS_ODDS=NULL

RMSE_RISK=NULL
MC_SD_RISK=NULL
EST_SD_RISK=NULL
CRATE_RISK=NULL
BIAS_RISK=NULL

NON_CONVER=NULL


#n.grid=c(seq(50,200,by=10),500)
n.grid=c(50,100,200,500)


for (n in n.grid){
  
  EST_DIFF<-SE_DIFF<-matrix(NA,nsim,5)
  colnames(EST_DIFF)<-colnames(SE_DIFF)<-c("UNADJ","IPW","OW","LR","AIPW")
  
  EST_RISK<-SE_RISK<-matrix(NA,nsim,5)
  colnames(EST_RISK)<-colnames(SE_RISK)<-c("UNADJ","IPW","OW","LR","AIPW")
  
  EST_ODDS<-SE_ODDS<-matrix(NA,nsim,5)
  colnames(EST_ODDS)<-colnames(SE_ODDS)<-c("UNADJ","IPW","OW","LR","AIPW")
  
  ##True Values
  true.p.1=numeric(nsim)
  true.p.0=numeric(nsim)
  
  for(i in 1:nsim){
    x<-rmvnorm(n,rep(0,p),diag(1,p))
    z<-rbinom(n,1,pt)
    ##Use hypergeometric to ensure existence of treat/control
    # z=rep(0,n)
    # z[sample(1:n,round(n*pt),replace=F)]=1
    bsq<-2/(1+2+2^2+2^3+2^4)
    betasq<-rep(c(bsq,2*bsq,2^2*bsq,2^3*bsq,2^4*bsq),each=p/5)
    
    deltasq<-1
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
      
      p.mean<-as.numeric(rare.degree+x%*%beta+x%*%beta.tau*z*h.degree+x.inter%*%beta.quad+gamma*z)
      true.mean.1=as.numeric(rare.degree+x%*%beta+x%*%beta.tau*h.degree+x.inter%*%beta.quad+gamma)
      true.mean.0=as.numeric(rare.degree+x%*%beta+x.inter%*%beta.quad)
      ##Correctly Specified
    }else{
      p.mean<-as.numeric(rare.degree+x%*%beta+x%*%beta.tau*z*h.degree+gamma*z)
      true.mean.1=as.numeric(rare.degree+x%*%beta+x%*%beta.tau*h.degree+gamma)
      true.mean.0=as.numeric(rare.degree+x%*%beta)
      
    }
    
    W<-cbind(1,x)
    
    if(logit==1){
      true.p.1[i]=mean(1/(1+exp(-true.mean.1)))
      true.p.0[i]=mean(1/(1+exp(-true.mean.0)))
      p.y=1/(1+exp(-p.mean))
    }else{
      true.p.1[i]=mean(pnorm(true.mean.1))
      true.p.0[i]=mean(pnorm(true.mean.0))
      p.y=pnorm(p.mean)
    }
    ###Generate Binary Outcome
    ##Generate from Bernouli trial
    ##True model is logit model
    
    
    y=unlist(lapply(p.y,FUN=function(x){sample(c(1,0),1,prob=c(x,1-x))}))
    
    
    
    
    # UNADJ
    res.crude <- Crude(y=y, z=z, W=W,binary=1)
    EST_DIFF[i,1] <- res.crude$mean_diff
    EST_RISK[i,1] <- res.crude$log_risk_ratio
    EST_ODDS[i,1] <- res.crude$log_odds_ratio
    
    SE_DIFF[i,1]<-res.crude$se_mean_diff
    SE_RISK[i,1]<-res.crude$se_risk_ratio
    SE_ODDS[i,1]<-res.crude$se_odds_ratio
    
    
    # IPW
    res.IPWC <- IPWC(y.all=y, z.all=z, W.all=W, q.all=0,binary=1)
    EST_DIFF[i,2] <- res.IPWC$mean_diff
    EST_RISK[i,2] <- res.IPWC$log_risk_ratio
    EST_ODDS[i,2] <- res.IPWC$log_odds_ratio
    
    SE_DIFF[i,2]<-res.IPWC$se_mean_diff
    SE_RISK[i,2]<-res.IPWC$se_risk_ratio
    SE_ODDS[i,2]<-res.IPWC$se_odds_ratio
    
    # OW
    res.OW <- OW(y=y, z=z, W=W,binary=1)
    EST_DIFF[i,3] <- res.OW$mean_diff
    EST_RISK[i,3] <- res.OW$log_risk_ratio
    EST_ODDS[i,3] <- res.OW$log_odds_ratio
    
    SE_DIFF[i,3]<-res.OW$se_mean_diff
    SE_RISK[i,3]<-res.OW$se_risk_ratio
    SE_ODDS[i,3]<-res.OW$se_odds_ratio
    
    #LR
    #Supply the one without intercept
    res.LR <- LR(y=y, z=z, W=x,binary=1)
    #NA happen if not converged
    EST_DIFF[i,4] <- res.LR$mean_diff
    EST_RISK[i,4] <- res.LR$log_risk_ratio
    EST_ODDS[i,4] <- res.LR$log_odds_ratio
    
    SE_DIFF[i,4]<-res.LR$se_mean_diff
    SE_RISK[i,4]<-res.LR$se_risk_ratio
    SE_ODDS[i,4]<-res.LR$se_odds_ratio
    
    #AIPW 
    #Supply the one without intercept
    res.AIPW <- AIPW(y=y, z=z, W=x,binary=1)
    #NA happen if not converged
    EST_DIFF[i,5] <- res.AIPW$mean_diff
    EST_RISK[i,5] <- res.AIPW$log_risk_ratio
    EST_ODDS[i,5] <- res.AIPW$log_odds_ratio
    
    SE_DIFF[i,5]<-res.AIPW$se_mean_diff
    SE_RISK[i,5]<-res.AIPW$se_risk_ratio
    SE_ODDS[i,5]<-res.AIPW$se_odds_ratio
    
    
    if(i%%500==0)print(i)
  }
  
  
  
  # # Results
  complete.id=which(apply(EST_DIFF,1,FUN=function(x){all(!is.na(x[1:4]))}))
  
  ##Use MC approximate true parameter
  true_diff=mean(true.p.1)-mean(true.p.0)
  true_risk=log(mean(true.p.1)/mean(true.p.0))
  true_odds=log(mean(true.p.1)/(1-mean(true.p.1)))-log(mean(true.p.0)/(1-mean(true.p.0)))
  
  #Remove non finite one
  EST_DIFF[is.infinite(EST_DIFF)]=NA
  EST_RISK[is.infinite(EST_RISK)]=NA
  EST_ODDS[is.infinite(EST_ODDS)]=NA
  
  RMSE_DIFF=rbind(RMSE_DIFF,c(n,sqrt(colMeans((EST_DIFF[complete.id,]-true_diff)^2,na.rm=T))))
  RMSE_RISK=rbind(RMSE_RISK,c(n,sqrt(colMeans((EST_RISK[complete.id,]-true_risk)^2,na.rm=T))))
  RMSE_ODDS=rbind(RMSE_ODDS,c(n,sqrt(colMeans((EST_ODDS[complete.id,]-true_odds)^2,na.rm=T))))
  
  MC_SD_DIFF=rbind(MC_SD_DIFF,c(n,apply(EST_DIFF[complete.id,],2,FUN=function(x){sd(x,na.rm = T)})))
  MC_SD_RISK=rbind(MC_SD_RISK,c(n,apply(EST_RISK[complete.id,],2,FUN=function(x){sd(x,na.rm = T)})))
  MC_SD_ODDS=rbind(MC_SD_ODDS,c(n,apply(EST_ODDS[complete.id,],2,FUN=function(x){sd(x,na.rm = T)})))
  
  SE_DIFF[SE_DIFF>10]=NA
  SE_RISK[SE_RISK>10]=NA
  SE_ODDS[SE_ODDS>10]=NA
  
  EST_SD_DIFF=rbind(EST_SD_DIFF,c(n,colMeans(SE_DIFF[complete.id,],na.rm=T)))
  EST_SD_RISK=rbind(EST_SD_RISK,c(n,colMeans(SE_RISK[complete.id,],na.rm=T)))
  EST_SD_ODDS=rbind(EST_SD_ODDS,c(n,colMeans(SE_ODDS[complete.id,],na.rm=T)))
  
  CRATE_DIFF=rbind(CRATE_DIFF,c(n,colMeans(matrix(true_diff, nrow(EST_DIFF), ncol(EST_DIFF)) <= EST_DIFF + qnorm(0.975)*SE_DIFF & 
                                             matrix(true_diff, nrow(EST_DIFF), ncol(EST_DIFF)) >= EST_DIFF - qnorm(0.975)*SE_DIFF,na.rm=T)))
  
  CRATE_RISK=rbind(CRATE_RISK,c(n,colMeans(matrix(true_risk, nrow(EST_RISK), ncol(EST_RISK)) <= EST_RISK + qnorm(0.975)*SE_RISK & 
                                             matrix(true_risk, nrow(EST_RISK), ncol(EST_RISK)) >= EST_RISK - qnorm(0.975)*SE_RISK,na.rm=T)))
  
  CRATE_ODDS=rbind(CRATE_ODDS,c(n,colMeans(matrix(true_odds, nrow(EST_ODDS), ncol(EST_ODDS)) <= EST_ODDS + qnorm(0.975)*SE_ODDS & 
                                             matrix(true_odds, nrow(EST_ODDS), ncol(EST_ODDS)) >= EST_ODDS - qnorm(0.975)*SE_ODDS,na.rm=T)))
  
  BIAS_DIFF=rbind(BIAS_DIFF,c(n,abs(colMeans(EST_DIFF[complete.id,],na.rm=T)-true_diff)))
  BIAS_RISK=rbind(BIAS_RISK,c(n,abs(colMeans(EST_RISK[complete.id,],na.rm=T)-true_risk)))
  BIAS_ODDS=rbind(BIAS_ODDS,c(n,abs(colMeans(EST_ODDS[complete.id,],na.rm=T)-true_odds)))
  #Non convergence time
  NON_CONVER=rbind(NON_CONVER,c(n,nsim-length(complete.id)))
  
  print(paste(n," finished"))
}
##Save data 
setwd("binary/")
save(RMSE_RISK,RMSE_DIFF,RMSE_ODDS,
     MC_SD_DIFF,MC_SD_RISK,MC_SD_ODDS,
     EST_SD_DIFF,EST_SD_RISK,EST_SD_ODDS,
     CRATE_DIFF,CRATE_ODDS,CRATE_RISK,
     BIAS_DIFF,BIAS_RISK,BIAS_ODDS,NON_CONVER,
     file=paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
# ##Make Figures
# setwd("..")
# source("plot_results_binary.R")