OW <- function(y, z, W,var_method=1,binary=0){
###y:outcome
###z:treatment status
###W:Components in logistic regression for propensity score model
###var_method: 1 using parametric information 
###            2 plugged the true propensity score
###            3 no parametric information, influence function
    
  # module for checking balance in the weighted sample
  diff <- function(cov, z, ps){
    # cov: covariate
    # z: treatment status
    # ps: estimated propensity scores
    
    v1 <- cov[z == 1]
    v0 <- cov[z == 0]
    w1 <- 1-ps[z == 1]
    w0 <- ps[z == 0]
    n1 <- length(v1)
    n0 <- length(v0)
    delta <- abs(sum(v1*w1) / sum(w1) - sum(v0*w0) / sum(w0))
    # tstat <- delta / sqrt(var(v1)/n1 + var(v0)/n0)
    tstat <- delta / sqrt(var(v1) + var(v0))
    
    # return the absolute standardized difference
    return(tstat)
  }
  if (binary==0){
  
  # summary statistics
  n1 <- sum(z)
  n0 <- sum(1-z)
  n <- n0 + n1
 
  # estimate ps
  fit <- glm(z ~ -1 + W, family = binomial(link = "logit"))
  if(!fit$converged){
    return(list(tau=NA, se=NA, asd = NA))
    }
  e.h <- as.numeric(fit$fitted.values)
  
  # point estimate
  mu1.h <- sum(z*y*(1-e.h)) / sum(z*(1-e.h))
  mu0.h <- sum((1-z)*y*e.h) / sum((1-z)*e.h)
  tau <- mu1.h - mu0.h
  
  # variance estimate
  # influence function 
  # theta <- sum(e.h*(1-e.h)) / n
  # H.b <- ((z*(y-mu1.h) + (1-z)*(y-mu0.h))
  #         * (e.h*(1-e.h))) * W
  # H.b <- colSums(H.b) / n
  # E.bb <- crossprod(sqrt(e.h*(1-e.h)) * W) / n
  # I.i <- z*(1-e.h)*(y-mu1.h) - (1-z)*e.h*(y-mu0.h) -
  #   (z-e.h) * c(t(H.b) %*% solve(E.bb) %*% t(W))
  # V <- sum(I.i^2) / (n*theta)^2
  # se <- sqrt(V)
  
  ###Variance Estimate from Empirical Sandwich Method
  # ###Exploit parametric information
  # Use the property that E(Z|X)=e
  if(var_method==1){
  theta <- sum(e.h*(1-e.h)) / n
  b.11=sum(e.h*((1-e.h)^2)*(y-mu1.h)^2*z)/ n1
  b.22=sum((e.h^2)*(1-e.h)*(y-mu0.h)^2*(1-z))/ n0
  a.13=crossprod((e.h^2)*(1-e.h)*(y-mu1.h)*z,W)/ n1
  a.23=crossprod(e.h*((1-e.h)^2)*(y-mu0.h)*(1-z),W)/ n0
  b.13=crossprod(e.h*((1-e.h)^2)*(y-mu1.h)*z,W)/ n1
  b.23=crossprod((e.h^2)*(1-e.h)*(y-mu0.h)*(1-z),W)/ n0
  a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
   
   
   V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
     (a.13+a.23)%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
  
  
  if (is.na(V)||V<0)
  {
    ###Handle Exception Case
    theta <- sum(e.h*(1-z)) / n 
    b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
    b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
    a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
    a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
    b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
    b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
    b.33=crossprod(abs(z-e.h)* W) / n
    
    V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
         (a.13+a.23)%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
    
  }
   se<-sqrt(V)
  }else if(var_method==2){
  ##Plug in true parameter value e.h=r
    # theta <-r*(1-r)
    # b.11=r*(1-r)^2*sum((y-mu1.h)^2*z)/ n1
    # b.22=r^2*(1-r)*sum((y-mu0.h)^2*(1-z))/ n0
    # a.13=r^2*(1-r)*crossprod((y-mu1.h)*z,W)/ n1
    # a.23=r*(1-r)^2*crossprod((y-mu0.h)*(1-z),W)/ n0
    # b.13=r*(1-r)^2*crossprod((y-mu1.h)*z,W)/ n1
    # b.23=r^2*(1-r)*crossprod((y-mu0.h)*(1-z),W)/ n0
    # a.33.inv=solve(crossprod(W)*r*(1-r) /n)
    # 
    # 
    # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
    #      (a.13+a.23)%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
    # se<-sqrt(V)
  }else if(var_method==3){
  # ##Did not exploit parameteric assumption
  # theta <- sum(e.h*(1-e.h)) / n #Same with Influence function derivation by Frank
  # #theta <- sum(e.h*(1-z)) / n 
  # b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
  # b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
  # a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
  # a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
  # b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
  # b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
  # a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
  # b.33=crossprod(abs(z-e.h)* W) / n
  # 
  # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
  #      (a.13+a.23)%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
  # se<-sqrt(V)
  }else{
    ##Did not exploit parameteric assumption totally
    # theta <- sum(e.h*(1-z)) / n 
    # b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
    # b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
    # a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
    # a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
    # b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
    # b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
    # a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
    # b.33=crossprod(abs(z-e.h)* W) / n
    # 
    # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
    #      (a.13+a.23)%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
    # se<-sqrt(V) 
  }
  # check balance
  asd <- rep(NA, ncol(W)-1)
  for(k in 1:(ncol(W)-1)){
    asd[k] <- diff(cov = W[,k+1], z = z, ps = e.h)
  }
  
  return(list(tau=tau, se=se, asd = asd))
  }else{
    # summary statistics
    n1 <- sum(z)
    n0 <- sum(1-z)
    n <- n0 + n1
    
    # estimate ps
    fit <- glm(z ~ -1 + W, family = binomial(link = "logit"))
    e.h <- as.numeric(fit$fitted.values)
    if(!fit$converged){
      return(list(mean_diff=NA,log_odds_ratio=NA,
                  log_risk_ratio=NA,se_risk_ratio=NA,
                  se_odds_ratio=NA,se_mean_diff=NA))
    }
    # point estimate
    mu1.h <- sum(z*y*(1-e.h)) / sum(z*(1-e.h))
    mu0.h <- sum((1-z)*y*e.h) / sum((1-z)*e.h)
    log_odds_ratio=log(mu1.h/(1-mu1.h))-log(mu0.h/(1-mu0.h))
    log_risk_ratio=log(mu1.h/mu0.h)
    mean_diff=mu1.h-mu0.h
    
    ###Variance Estimate from Empirical Sandwich Method
    # ###Exploit parametric information
    # Use the property that E(Z|X)=e
    if(var_method==1){
      theta <- sum(e.h*(1-e.h)) / n
      b.11=sum(e.h*((1-e.h)^2)*(y-mu1.h)^2*z)/ n1
      b.22=sum((e.h^2)*(1-e.h)*(y-mu0.h)^2*(1-z))/ n0
      a.13=crossprod((e.h^2)*(1-e.h)*(y-mu1.h)*z,W)/ n1
      a.23=crossprod(e.h*((1-e.h)^2)*(y-mu0.h)*(1-z),W)/ n0
      b.13=crossprod(e.h*((1-e.h)^2)*(y-mu1.h)*z,W)/ n1
      b.23=crossprod((e.h^2)*(1-e.h)*(y-mu0.h)*(1-z),W)/ n0
      a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
      

      V1=(b.11-2*a.13%*%a.33.inv%*%t(b.13)+a.13%*%a.33.inv%*%t(a.13))/(theta^2*n)
      V0=(b.22-2*a.23%*%a.33.inv%*%t(b.23)+a.23%*%a.33.inv%*%t(a.23))/(theta^2*n)
      V12=(-a.13%*%a.33.inv%*%t(b.23)-b.13%*%a.33.inv%*%t(a.23)+a.13%*%a.33.inv%*%t(a.23))/(theta^2*n)
      
      Cov.m=matrix(c(V1,V12,V12,V0),2,2)
      
      
      ##Delta Method Gradient Vector
      grad_risk=c(1/mu1.h,-1/mu0.h)
      grad_odds=c(1/(mu1.h*(1-mu1.h)),-1/(mu0.h*(1-mu0.h)))
      grad_diff=c(1,-1)
      
      ##Calculate Asymptotic Variance
      v_risk=t(grad_risk)%*%Cov.m%*%grad_risk
      v_odds=t(grad_odds)%*%Cov.m%*%grad_odds
      v_diff=t(grad_diff)%*%Cov.m%*%grad_diff
      
      se_risk=sqrt(v_risk)
      se_odds=sqrt(v_odds)
      se_diff=sqrt(v_diff)
      
      
      
      if (is.na(v_risk)||is.na(v_odds)||is.na(v_diff)||v_risk<0||v_odds<0||v_diff<0)
      {
        ###Handle Exception Case: Use different Version
        theta <- sum(e.h*(1-z)) / n 
        b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
        b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
        a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
        a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
        b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
        b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
        b.33=crossprod(abs(z-e.h)* W) / n
      
        V1=(b.11-2*a.13%*%a.33.inv%*%t(b.13)+a.13%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13))/(theta^2*n)
        V0=(b.22-2*a.23%*%a.33.inv%*%t(b.23)+a.23%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.23))/(theta^2*n)
        V12=(-a.13%*%a.33.inv%*%t(b.23)-b.13%*%a.33.inv%*%t(a.23)+a.13%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.23))/(theta^2*n)
        
        Cov.m=matrix(c(V1,V12,V12,V0),2,2)
        
        
        ##Delta Method Gradient Vector
        grad_risk=c(1/mu1.h,-1/mu0.h)
        grad_odds=c(1/(mu1.h*(1-mu1.h)),-1/(mu0.h*(1-mu0.h)))
        grad_diff=c(1,-1)
        
        ##Calculate Asymptotic Variance
        v_risk=t(grad_risk)%*%Cov.m%*%grad_risk
        v_odds=t(grad_odds)%*%Cov.m%*%grad_odds
        v_diff=t(grad_diff)%*%Cov.m%*%grad_diff
        
        se_risk=sqrt(v_risk)
        se_odds=sqrt(v_odds)
        se_diff=sqrt(v_diff)
        
        
        
      }
    }else if(var_method==2){
      ##Plug in true parameter value e.h=r
      # theta <-r*(1-r)
      # b.11=r*(1-r)^2*sum((y-mu1.h)^2*z)/ n1
      # b.22=r^2*(1-r)*sum((y-mu0.h)^2*(1-z))/ n0
      # a.13=r^2*(1-r)*crossprod((y-mu1.h)*z,W)/ n1
      # a.23=r*(1-r)^2*crossprod((y-mu0.h)*(1-z),W)/ n0
      # b.13=r*(1-r)^2*crossprod((y-mu1.h)*z,W)/ n1
      # b.23=r^2*(1-r)*crossprod((y-mu0.h)*(1-z),W)/ n0
      # a.33.inv=solve(crossprod(W)*r*(1-r) /n)
      # 
      # 
      # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
      #      (a.13+a.23)%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
      # se<-sqrt(V)
    }else if(var_method==3){
      # ##Did not exploit parameteric assumption
      # theta <- sum(e.h*(1-e.h)) / n #Same with Influence function derivation by Frank
      # #theta <- sum(e.h*(1-z)) / n 
      # b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
      # b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
      # a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
      # a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
      # b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
      # b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
      # a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
      # b.33=crossprod(abs(z-e.h)* W) / n
      # 
      # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
      #      (a.13+a.23)%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
      # se<-sqrt(V)
    }else{
      ##Did not exploit parameteric assumption totally
      # theta <- sum(e.h*(1-z)) / n 
      # b.11=sum(((1-e.h)^2)*(y-mu1.h)^2*z)/ n
      # b.22=sum((e.h^2)*(y-mu0.h)^2*(1-z))/ n
      # a.13=crossprod(e.h*(1-e.h)*(y-mu1.h)*z,W)/ n
      # a.23=crossprod(e.h*(1-e.h)*(y-mu0.h)*(1-z),W)/ n
      # b.13=crossprod((1-e.h)^2*(y-mu1.h)*z,W)/ n
      # b.23=crossprod(e.h^2*(y-mu0.h)*(1-z),W)/ n
      # a.33.inv=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
      # b.33=crossprod(abs(z-e.h)* W) / n
      # 
      # V=(b.11+b.22-2*(a.13+a.23)%*%a.33.inv%*%t(b.13+b.23)+
      #      (a.13+a.23)%*%a.33.inv%*%b.33%*%a.33.inv%*%t(a.13+a.23))/(theta^2*n)
      # se<-sqrt(V) 
    }
    
  }
  return(list(mean_diff=mean_diff,log_odds_ratio=log_odds_ratio,
              log_risk_ratio=log_risk_ratio,se_risk_ratio=se_risk,
              se_odds_ratio=se_odds,se_mean_diff=se_diff))
  
}