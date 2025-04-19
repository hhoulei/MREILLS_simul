
DataGeneration <- function(Nvar,g,sigmax,alpha,omega,beta,sigmaxlow,sigmaxup,bXU,
                           sigmay,sigmaylow,sigmayup,bYU){
  
  Sigma_episx <- diag(Nvar-1)
  diag(Sigma_episx) <- sigmax
  episx <- mvrnorm(g,rep(0,Nvar-1),Sigma = Sigma_episx)
  betaGX <- (alpha[,-Nvar] + matrix(omega,ncol=1) %*% matrix(bXU,nrow=1) + episx) %*% solve(diag(Nvar-1)-beta[-Nvar,-Nvar])
  sebetaGX <- matrix(runif((Nvar-1)*g,sigmaxlow,sigmaxup),nrow=g)
  
  episy <- rnorm(g,0,sigmay)
  betaGY <- betaGX %*% beta[-Nvar,Nvar]+omega*bYU+alpha[,Nvar]+episy
  sebetaGY <- runif(g,sigmaylow,sigmayup)
  
  data <- list(betaGX=betaGX,
               sebetaGX=sebetaGX,
               betaGY=c(betaGY),
               sebetaGY=sebetaGY)
  return(data)
}

ParaGenerations <- function(E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
                            bXU,sigmay,sigmaylow,sigmayup,bYU,causaleffect,Ecor){
  
  Nedge <- Nvar*Nvar
  Ntruth <- floor(Nvar*1)
  
  alpha_list0 <- list()
  for(oa in 1:(Nvar-1)){
    sig1 <- matrix(Ecor*0.2*0.2,nrow = E,ncol = E)
    diag(sig1) <- 0.2^2
    alpha1 <- mvrnorm(g,rep(0,E),sig1)
    alpha_list0[[oa]] <- alpha1
  }
  
  alpha_list <- list()
  for(e in 1:E){
    alpha1 <- matrix(NA,nrow=g,ncol=Nvar)
    for(oa in 1:(Nvar-1)){
      alpha1[,oa] <- alpha_list0[[oa]][,e]
    }
    alpha1[,Nvar] <- gammaY[[e]]
    alpha_list[[e]] <- alpha1
  }
  
  beta_list <- list()
  for(e in 1:E){
    beta1 <- matrix(runif(Nedge,-1,1),ncol=Nvar)
    beta1[lower.tri(beta1)] <- 0
    diag(beta1) <- 0
    beta1[,Nvar] <- causaleffect[[e]]
    beta_list[[e]] <- beta1
  }
  
  para_list <- list()
  for(e in 1:E){
    
    NOonce <- list(sigmax=sigmax[e],
                   alpha=alpha_list[[e]],
                   omega=omega[[e]],
                   beta=beta_list[[e]],
                   sigmaxlow=sigmaxlow[e],
                   sigmaxup=sigmaxup[e],
                   bXU=bXU[[e]],
                   sigmay=sigmay[e],
                   sigmaylow=sigmaylow[e],
                   sigmayup=sigmayup[e],
                   bYU=bYU[e]
    )
    
    para_list[[e]] <- NOonce
  }
  
  return(para_list)
}
