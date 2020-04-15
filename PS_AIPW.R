#propensity model
library(PSweight)

AIPW<-function(y, z, W,binary=0,filter_numeric_error=T){
  p=ncol(W)
  if(is.null(colnames(W))){
  form.ps <- as.formula(paste("z","~",paste(paste('W',1:p,sep="."),collapse = "+"),sep=""))
  # Outcome model
  form.out<- as.formula(paste("y","~",paste(paste('W',1:p,sep="."),collapse = "+"),sep=""))
  rct.data=data.frame(y=y,z=z,W=W)
  }else{
    form.ps <- as.formula(paste("z","~",paste(colnames(W),collapse = "+"),sep=""))
    form.out<- as.formula(paste("y","~",paste(colnames(W),collapse = "+"),sep=""))
    rct.data=as.data.frame(cbind(y,z,W))
  }
  
  
  if (binary==0){
    aipw<-PSweight(weight = 'ATE',ps.formula = form.ps,yname = 'y',data=rct.data,augmentation = T,out.formula = form.out,family = 'gaussian')
    #invisible(capture.output(aipw_summary<-summary(aipw,type='DIF',contrast=c(-1,1))))
    #est=aipw_summary$inference[1]
    #se=aipw_summary$inference[2]
    est=aipw$muhat[2]-aipw$muhat[1]
    v=c(-1,1)
    se=sqrt(t(v)%*%aipw$covmu%*%v)
    
    return(list(tau=est, se=se))
  }else{
    
    if(filter_numeric_error){
      numerical_error=tryCatch({
        aipw<-PSweight(weight = 'ATE',ps.formula = form.ps,yname = 'y',data=rct.data,augmentation = T,out.formula = form.out,family = 'binomial')
      },warning=function(w){return(TRUE)
      })
    }else{
      aipw<-PSweight(weight = 'ATE',ps.formula = form.ps,yname = 'y',data=rct.data,augmentation = T,out.formula = form.out,family = 'binomial')
      numerical_error=aipw
    }
    if (filter_numeric_error&(class(numerical_error)[1]=="logical")){
      #print("numerical error or not converged")
      return(list(mean_diff=NA,log_odds_ratio=NA,
                  log_risk_ratio=NA,se_risk_ratio=NA,
                  se_odds_ratio=NA,se_mean_diff=NA))
    }else{
      #invisible(capture.output(aipw_summary_rd<-summary(aipw,type='DIF',contrast=c(-1,1))))
      #invisible(capture.output(aipw_summary_rr<-summary(aipw,type='RR',contrast=c(-1,1))))
      #invisible(capture.output(aipw_summary_or<-summary(aipw,type='OR',contrast=c(-1,1))))
      
      
      ##Transform back to Log
      # mean_diff=aipw_summary_rd$inference[1]
      # se_diff=aipw_summary_rd$inference[2]
      # 
      # log_risk_ratio=log(aipw_summary_rr$inference[1])
      # se_risk=(1/aipw_summary_rr$inference[1])*aipw_summary_rr$inference[2]
      
      # log_odds_ratio=log(aipw_summary_or$inference[1])
      # se_odds=(1/aipw_summary_or$inference[1])*aipw_summary_or$inference[2]
      # Potential bug in psweight
      
      mean_diff=aipw$muhat[2]-aipw$muhat[1]
      v=c(-1,1)
      se_diff=sqrt(t(v)%*%aipw$covmu%*%v)
      
      log_risk_ratio=log(aipw$muhat[2])-log(aipw$muhat[1])
      v=c(-1/aipw$muhat[1],1/aipw$muhat[2])
      se_risk=sqrt(t(v)%*%aipw$covmu%*%v)
      
      v=c(-1/(aipw$muhat[1]*(1-aipw$muhat[1])),1/(aipw$muhat[2]*(1-aipw$muhat[2])))
      se_odds=sqrt(t(v)%*%aipw$covmu%*%v)
      log_odds_ratio=log(aipw$muhat[2]/(1-aipw$muhat[2]))-log(aipw$muhat[1]/(1-aipw$muhat[1]))
      
      return(list(mean_diff=mean_diff,log_odds_ratio=log_odds_ratio,
                  log_risk_ratio=log_risk_ratio,se_risk_ratio=se_risk,
                  se_odds_ratio=se_odds,se_mean_diff=se_diff))
    }
    
  }
  
}
