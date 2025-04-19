
EILLS <- function(fdata_all,r1,meth,lambda){
  
  Nv <- ncol(fdata_all[[1]]$betaGX)
  Ng <- nrow(fdata_all[[1]]$betaGX)
  Qloss <- function(bb) {
    
    # IV selection
    QSj <- NULL
    for(sj in 1:Ng){
      QSj1 <- NULL
      Xe <- NULL
      for(sje in 1:length(fdata_all)){
        oncee <- fdata_all[[sje]]
        QSj1 <- c(QSj1,
                  oncee$betaGY[sj]-matrix(oncee$betaGX[sj,],nrow=1)%*%bb)
        Xe <- cbind(Xe,oncee$betaGX[sj,])
      }
      
      Xe <- c(abs(Xe)%*%abs(QSj1))
      QSj <- c(QSj,sum(abs(QSj1))+sum(Xe))
    }
    
    Sj <- (QSj<lambda)
    
    
    #bb <- c(0,0,0,0,0,0,0)
    if(length(bb)==1){
      Re <- unlist(lapply(fdata_all,function(x) mean((x$sebetaGY[Sj]^(-2)/sum(x$sebetaGY[Sj]^(-2)))*((x$betaGY[Sj]-x$betaGX[Sj,]*bb)^2))))
    }else{
      Re <- unlist(lapply(fdata_all,function(x) mean((x$sebetaGY[Sj]^(-2)/sum(x$sebetaGY[Sj]^(-2)))*((x$betaGY[Sj]-x$betaGX[Sj,]%*%bb)^2))))
    }
    
    Jpe <- NULL
    weight <- NULL
    for(oe in 1:length(fdata_all)){
      once <- fdata_all[[oe]]
      
      if(length(bb)==1){
        epi <- once$betaGY[Sj]-once$betaGX[Sj,]*bb
      }else{
        epi <- once$betaGY[Sj]-once$betaGX[Sj,]%*%bb
      }
      
      
      Jpe_once <- NULL
      for(oj in 1:Nv){
        Jpe_once <- c(Jpe_once,mean(once$betaGX[Sj,oj]*epi)^2)
      }
      Jpe <- cbind(Jpe,Jpe_once)
      
      weight <- c(weight,sum(once$sebetaGY[Sj]^(-2)))
    }
    
    weight <- weight/sum(weight)
    
    Jp <- Jpe%*%weight
    J <- sum(Jp)
    
    R <- sum(weight*Re)
    
    return(R+r1*J)  
  }
  
  result <- optim(par = rep(0,Nv), 
                  fn = Qloss, 
                  method = meth
  )
  return(result)
}

CV_EILLS <- function(fdata_all,
                     r1,meth,trueXY,lambda){
  
  allcomb <- expand.grid(r1,lambda)
  
  objQ <- Inf
  objloc <- NULL
  for(oo in 1:nrow(allcomb)){
    
    x.inv <- try(EILLS(fdata_all,r1=allcomb[oo,1],
                       meth=meth,lambda=allcomb[oo,2]),
                 silent = T)
    if('try-error' %in% class(x.inv)){
      next
    }else{
      once_eillsGIHT <- x.inv
      mse2 <- sqrt(sum((once_eillsGIHT$par-trueXY)^2))
      if(mse2<objQ){
        objQ <- mse2
        objloc <- oo
      }
    }
  }
  
  return(list(r1=allcomb[objloc,1],
              lambda=allcomb[objloc,2]))
  
}


BOOT_EILLS <- function(fdata_all,r1,meth,lambda,numBoot){
  
  estboot <- NULL
  for(ojo in 1:numBoot){
    
    locboot <- sample(1:nrow(fdata_all[[1]]$betaGX),replace = T)
    
    fdata_boot <- list()
    for(obo in 1:length(fdata_all)){
      fdata_once <- fdata_all[[obo]]
      fdata_boot[[obo]] <- list(betaGX=matrix(fdata_once$betaGX[locboot,],ncol=1),
                                sebetaGX=matrix(fdata_once$sebetaGX[locboot,],ncol=1),
                                betaGY=fdata_once$betaGY[locboot],
                                sebetaGY= fdata_once$sebetaGY[locboot])
    }
    
    x.inv <- try(EILLS(fdata_boot,r1,meth,lambda),
                 silent = T)
    if('try-error' %in% class(x.inv)){
      next
    }else{
      estboot <- cbind(estboot,x.inv$par)
    }
    
  }
  
  lowbound <- apply(estboot,1,function(x) quantile(x,0.025))
  upbound <- apply(estboot,1,function(x) quantile(x,0.975))
  
  bound <- rbind(lowbound,upbound)
  
  return(bound)
}
