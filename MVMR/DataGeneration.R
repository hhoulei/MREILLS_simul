
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
                            bXU,sigmay,sigmaylow,sigmayup,bYU){
  
  Nedge <- Nvar*Nvar
  Ntruth <- floor(Nvar*0.3)
  
  alpha_list <- list()
  for(e in 1:E){
    alpha1 <- NULL
    for(oa in 1:Nvar){
      alpha1 <- cbind(alpha1,runif(g,0.05,0.2))
    }
    alpha1[,Nvar] <- gammaY[[e]]
    alpha_list[[e]] <- alpha1
  }
  
  beta_list <- list()
  for(e in 1:E){
    beta1 <- matrix(runif(Nedge,-1,1),ncol=Nvar)
    beta1[lower.tri(beta1)] <- 0
    diag(beta1) <- 0
    beta1[,Nvar] <- c(rep(0.2,Ntruth),rep(0,Nvar-Ntruth))
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
