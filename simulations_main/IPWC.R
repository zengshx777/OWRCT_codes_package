IPWC <- function(y.all, z.all, W.all, q.all,binary=0,log_scale=1){
  library(MASS)
  
  # module for checking balance in the weighted sample
  diff <- function(cov, z, ps){
    # cov: covariate
    # z: treatment status
    # ps: estimated propensity scores
    
    v1 <- cov[z == 1]
    v0 <- cov[z == 0]
    w1 <- 1 / ps[z == 1]
    w0 <- 1 /(1-ps[z == 0])
    n1 <- length(v1)
    n0 <- length(v0)
    delta <- abs(sum(v1*w1) / sum(w1) - sum(v0*w0) / sum(w0))
    # tstat <- delta / sqrt(var(v1)/n1 + var(v0)/n0)
    tstat <- delta / sqrt(var(v1) + var(v0))
    
    # return the absolute standardized difference
    return(tstat)
  }
  
  # module for calculating the estimates
  GetTau <- function(y.all, z.all, W.all, e.h.all ,q){
    # y: subset of y's
    # z: subset of z's
    # W: subset of W
    # e: ps estimates
    # q: specific cutoff value
    
    keep <- ((e.h.all >= q/100) & (e.h.all <= (1-q/100)))
    ptrim <- 1 - mean(keep)
    y <- y.all[keep]
    z <- z.all[keep]
    W <- W.all[keep,]
    fit.h <- glm(z ~ -1 + W, family = binomial(link = "logit"))
    
    e.h <- as.numeric(fit.h$fitted.values)
    
    # summary statistics
    n1 <- sum(z)
    n0 <- sum(1-z)
    n <- n0 + n1
    
    # point estimate
    mu1.h <- sum(z*y/e.h) / sum(z/e.h)
    mu0.h <- sum((1-z)*y/(1-e.h)) / sum((1-z)/(1-e.h))
    tau <- mu1.h - mu0.h
    
    # variance estimate
    # H.b <- (z*(y-mu1.h)*(1-e.h)/e.h + (1-z)*(y-mu0.h)*e.h/(1-e.h))*W
    # H.b <- colSums(H.b) / n
    # E.bb <- crossprod(sqrt(e.h*(1-e.h)) * W) / n
    # I.i <- z*(y-mu1.h)/e.h - (1-z)*(y-mu0.h)/(1-e.h) - 
    #   (z-e.h) * c(t(H.b) %*% ginv(E.bb) %*% t(W))
    # V <- sum(I.i^2) / n^2
    # se <- sqrt(V)
    
    ##Variance formula by Williamson
    w1=mean(z/e.h)
    w0=mean((1-z)/(1-e.h))
    V_un=mean(z*(y-mu1.h)^2/(e.h^2))/w1^2+mean((1-z)*(y-mu0.h)^2/((1-e.h)^2))/w0^2
    v=crossprod(((1-e.h)/e.h)*(y-mu1.h)*z,W)/(n*w1)+crossprod((e.h/(1-e.h))*(y-mu0.h)*(1-z),W)/(n*w0)
    M1=solve(crossprod(sqrt(e.h*(1-e.h)) * W) / n)
    M2=M1%*%(crossprod(abs(z-e.h)* W) / n)%*%M1
    V=(V_un-v%*%(2*M1-M2)%*%t(v))/n
    se=sqrt(V)
    
    
    # check balance
    asd <- rep(NA, ncol(W)-1)
    for(k in 1:(ncol(W)-1)){
      asd[k] <- diff(cov = W[,k+1], z = z, ps = e.h)
    }
    return(list(tau = tau, se = se, asd = asd, ptrim = ptrim))
  }
  if(binary==0){
    # estimate ps
    fit.all <- glm(z.all ~ -1 + W.all, family = binomial(link = "logit"))
    if(!fit.all$converged){
      return(list(tau=NA, se=NA, asd = NA,ptrim=NA))
    }
    e.h.all <- as.numeric(fit.all$fitted.values)
    
    TAU <- SE <- PTRIM <- rep(NA, length(q.all))
    ASD <- matrix(NA, ncol(W.all)-1, length(q.all))
    for(j in 1:length(q.all)){
      q <- q.all[j]
      res.q <- GetTau(y.all, z.all, W.all, e.h.all ,q)
      TAU[j] <- res.q$tau
      SE[j] <- res.q$se
      ASD[,j] <- res.q$asd
      PTRIM[j] <- res.q$ptrim
    }
    return(list(tau=TAU, se=SE, asd=ASD, ptrim = PTRIM))
  }
  else{
    # summary statistics
    
    n1 <- sum(z.all)
    n0 <- sum(1-z.all)
    n <- n0 + n1
    
    # estimate ps
    fit <- glm(z.all ~ -1 + W.all, family = binomial(link = "logit"))
    if(!fit$converged){
      return(list(mean_diff=NA,log_odds_ratio=NA,
                  log_risk_ratio=NA,se_risk_ratio=NA,
                  se_odds_ratio=NA,se_mean_diff=NA))
    }
    e.h <- as.numeric(fit$fitted.values)
    
    # point estimate
    mu1.h <- sum(z.all*y.all/e.h) / sum(z.all/e.h)
    mu0.h <- sum((1-z.all)*y.all/(1-e.h)) / sum((1-z.all)/(1-e.h))
    # point estimate
    if(log_scale==1){
      log_odds_ratio=log(mu1.h/(1-mu1.h))-log(mu0.h/(1-mu0.h))
      log_risk_ratio=log(mu1.h/mu0.h)
      mean_diff=mu1.h-mu0.h
    }
    else{
      log_odds_ratio=(mu1.h*(1-mu0.h))/((1-mu1.h)*mu0.h)
      log_risk_ratio=mu1.h/mu0.h
      mean_diff=mu1.h-mu0.h
    }
    
    ##Follow Williamson Notations
    ##Delta Method Gradient Vector
    ##Delta Method Gradient Vector
    if (log_scale==1){
      ##Delta Method Gradient Vector
      K_risk=c(1/mu1.h,1/mu0.h)
      K_odds=c(1/(mu1.h*(1-mu1.h)),1/(mu0.h*(1-mu0.h)))
      K_diff=c(1,-1)
    }
    else{
      K_risk=c(1/mu0.h,-mu1.h/mu0.h^2)
      K_odds=c((1-mu0.h)/((1-mu1.h)^2*mu0.h),-mu1.h/((1-mu1.h)*mu0.h^2))
      K_diff=c(1,-1)
    }
    
    ##Asymptotic Variance formula by Williamson
    w1=mean(z.all/e.h)
    w0=mean((1-z.all)/(1-e.h))
    M1=solve(crossprod(sqrt(e.h*(1-e.h)) * W.all) / n)
    M2=M1%*%(crossprod(abs(z.all-e.h)* W.all) / n)%*%M1
    
    V_un=mean(z.all*(y.all-mu1.h)^2/(e.h^2))/w1^2*K_odds[1]^2+
      mean((1-z.all)*(y.all-mu0.h)^2/((1-e.h)^2))/w0^2*K_odds[2]^2
    v=crossprod(((1-e.h)/e.h)*(y.all-mu1.h)*z.all,W.all)/(n*w1)*K_odds[1]+
      crossprod((e.h/(1-e.h))*(y.all-mu0.h)*(1-z.all),W.all)/(n*w0)*K_odds[2]
    v_odds=(V_un-v%*%(2*M1-M2)%*%t(v))/n
    
    V_un=mean(z.all*(y.all-mu1.h)^2/(e.h^2))/w1^2*K_diff[1]^2+
      mean((1-z.all)*(y.all-mu0.h)^2/((1-e.h)^2))/w0^2*K_diff[2]^2
    v=crossprod(((1-e.h)/e.h)*(y.all-mu1.h)*z.all,W.all)/(n*w1)*K_diff[1]+
      crossprod((e.h/(1-e.h))*(y.all-mu0.h)*(1-z.all),W.all)/(n*w0)*K_diff[2]
    v_diff=(V_un-v%*%(2*M1-M2)%*%t(v))/n
    
    
    V_un=mean(z.all*(y.all-mu1.h)^2/(e.h^2))/w1^2*K_risk[1]^2+
      mean((1-z.all)*(y.all-mu0.h)^2/((1-e.h)^2))/w0^2*K_risk[2]^2
    v=crossprod(((1-e.h)/e.h)*(y.all-mu1.h)*z.all,W.all)/(n*w1)*K_risk[1]+
      crossprod((e.h/(1-e.h))*(y.all-mu0.h)*(1-z.all),W.all)/(n*w0)*K_risk[2]
    v_risk=(V_un-v%*%(2*M1-M2)%*%t(v))/n
    
    se_risk=sqrt(v_risk)
    se_odds=sqrt(v_odds)
    se_diff=sqrt(v_diff)
    
    return(list(mean_diff=mean_diff,log_odds_ratio=log_odds_ratio,
                log_risk_ratio=log_risk_ratio,se_risk_ratio=se_risk,
                se_odds_ratio=se_odds,se_mean_diff=se_diff))
    
  }
}
