###Unadjusted one
Crude <- function(y, z, W,binary=0,log_scale=1){
  
  # module for checking balance
  diff <- function(cov, z){
    # cov: covariate vector 
    # z: treatment status
    
    v1 <- cov[z == 1]
    v0 <- cov[z == 0]
    mu1 <- mean(v1)
    mu0 <- mean(v0)
    n1 <- length(v1)
    n0 <- length(v0)
    delta <- abs(mu1 - mu0)
    # tstat <- delta / sqrt(var(v1)/n1 + var(v0)/n0)
    tstat <- delta / sqrt(var(v1) + var(v0))
    
    # return the absolute standardized difference
    return(tstat)
  }
  
  if (binary==0){
  
  # summary statistics
  n1 <- sum(z)
  n0 <- sum(1-z)
  mu1.h <- sum(z*y) / n1
  mu0.h <- sum((1-z)*y) / n0
  
  # point estimate
  tau <- mu1.h - mu0.h
  
  # variance estimate
  V <- var(y[z==1])/n1 + var(y[z==0])/n0
  se <- sqrt(V)
  
  # check balance
  asd <- rep(NA, ncol(W)-1)
  for(k in 1:(ncol(W)-1)){
    asd[k] <- diff(cov = W[,k+1], z = z)
  }

  
  return(list(tau=tau, se=se, asd = asd))
  }else{
    # summary statistics
    n1 <- sum(z)
    n0 <- sum(1-z)
    mu1 <- sum(z*y) / n1
    mu0 <- sum((1-z)*y) / n0
    
    # point estimate
    if(log_scale==1){
      log_odds_ratio=log(mu1/(1-mu1))-log(mu0/(1-mu0))
      log_risk_ratio=log(mu1/mu0)
      mean_diff=mu1-mu0
    }
    else{
      log_odds_ratio=(mu1*(1-mu0))/((1-mu1)*mu0)
      log_risk_ratio=mu1/mu0
      mean_diff=mu1-mu0
    }
    
    
    #Ignore the correlation in Y1,Y0
    Cov.m=diag(c(var(y[z==1])/n1,var(y[z==0])/n0))
    
    ##Delta Method Gradient Vector
    if (log_scale==1){
      grad_risk=c(1/mu1,-1/mu0)
      grad_odds=c(1/(mu1*(1-mu1)),-1/(mu0*(1-mu0)))
      grad_diff=c(1,-1)
    }
    else{
      grad_risk=c(1/mu0,-mu1/mu0^2)
      grad_odds=c((1-mu0)/((1-mu1)^2*mu0),-mu1/((1-mu1)*mu0^2))
      grad_diff=c(1,-1)
    }
    
    
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