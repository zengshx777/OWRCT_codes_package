library(sandwich)
###Linear Regression
LR <- function(y, z, W,binary=0,filter_numeric_error=T){
  ###Linear Regression with interaction
  ###center the covariate
  ###W do not include intercept
  if (binary==0){
    W=scale(W,scale=FALSE)
    lr_model=lm(y~z+W+W*z)
    #Singular case return NA.
    if(filter_numeric_error&any(is.na(coef(lr_model)))){return(list(tau=NA,se=NA))}
    #point estimation
    tau=lr_model$coefficients['z']
    
    # variance estimate
    # Sandwich estimator 
    # Wiston Lin recommend use HC0, we use HC1 for better coverage rate
    vcv <- vcovHC(lr_model, method="white1",type = "HC1")
    se=sqrt(vcv['z','z'])
    # if(binary==1)
    # {
    #   lr_model=glm(y~z+W+W*z,)
    # }
    
    return(list(tau=tau, se=se))
  }
  ##Binary Case
  else{
    ##W not intercept
    W=scale(W,scale=FALSE)
    if(filter_numeric_error){
      numerical_error=tryCatch({
        lr_model=glm(y~z+W+W*z,family = binomial(link = "logit"))
      },warning=function(w){return(TRUE)
      })
    }
    else{
      lr_model=glm(y~z+W+W*z,family = binomial(link = "logit"))
      numerical_error=lr_model
    }
    ##Check model converge
    if (filter_numeric_error&(class(numerical_error)[1]=="logical"||(!lr_model$converged))){
      #print("numerical error or not converged")
      return(list(mean_diff=NA,log_odds_ratio=NA,
                  log_risk_ratio=NA,se_risk_ratio=NA,
                  se_odds_ratio=NA,se_mean_diff=NA))
    }else{
      
      n=length(z)
      
      m1=predict.glm(lr_model,newdata=data.frame(z=rep(1,n),W=W), type="response")
      m0=predict.glm(lr_model,newdata=data.frame(z=rep(0,n),W=W),type="response")
      
      ##Calculate Asympotic variance with Delta formula
      X.matrix=model.matrix(lr_model)
      X.0=X.matrix
      X.0[,'z']=0
      X.1=X.matrix
      X.1[,'z']=1
      X.m=rbind(X.1,X.0)
      
      #Sandwich Estimator
      Cov=vcovHC(lr_model, method="white1",type = "HC1")
      Mean.Matrix=rbind(c(m1*(1-m1)/n,rep(0,n)),c(rep(0,n),m0*(1-m0)/n))
      if (ncol(Cov)!=ncol(X.m))
      {
        X.m=X.m[,1:ncol(Cov)]
      }
      ##Covariance Matrix of m1 and m0
      Cov.m=Mean.Matrix%*%X.m%*%Cov%*%t(X.m)%*%t(Mean.Matrix)
      
      #log_odds_ratio=mean(log(m1/(1-m1)))-mean(log(m0/(1-m0)))
      mu1=mean(m1)
      mu0=mean(m0)
      
      log_odds_ratio=log(mu1/(1-mu1))-log(mu0/(1-mu0))
      log_risk_ratio=log(mu1/mu0)
      mean_diff=mu1-mu0
      
      ##Delta Method Gradient Vector
      grad_risk=c(1/mu1,-1/mu0)
      grad_odds=c(1/(mu1*(1-mu1)),-1/(mu0*(1-mu0)))
      grad_diff=c(1,-1)
      
      ##Calculate Asymptotic Variance
      v_risk=t(grad_risk)%*%Cov.m%*%grad_risk
      v_odds=t(grad_odds)%*%Cov.m%*%grad_odds
      v_diff=t(grad_diff)%*%Cov.m%*%grad_diff
      
      se_risk=sqrt(v_risk)
      se_odds=sqrt(v_odds)
      se_diff=sqrt(v_diff)
      
      return(list(mean_diff=mean_diff,log_odds_ratio=log_odds_ratio,
                  log_risk_ratio=log_risk_ratio,se_risk_ratio=se_risk,
                  se_odds_ratio=se_odds,se_mean_diff=se_diff))
      
    }
  }
}
