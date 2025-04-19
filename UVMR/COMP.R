
COMP <- function(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
                 bXU,sigmay,sigmaylow,sigmayup,bYU,r1,numBoot,causaleffect,Ecor){
  
  cat('ii=',ii,'\n')
  
  para_list <- ParaGenerations(E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
                               bXU,sigmay,sigmaylow,sigmayup,bYU,causaleffect,Ecor)
  
  fdata_all <- list()
  for(oi in 1:length(para_list)){
    fdata <- DataGeneration(Nvar,g,
                            sigmax=para_list[[oi]]$sigmax,
                            alpha=para_list[[oi]]$alpha,
                            omega=para_list[[oi]]$omega,
                            beta=para_list[[oi]]$beta,
                            sigmaxlow=para_list[[oi]]$sigmaxlow,
                            sigmaxup=para_list[[oi]]$sigmaxup,
                            bXU=para_list[[oi]]$bXU,
                            sigmay=para_list[[oi]]$sigmay,
                            sigmaylow=para_list[[oi]]$sigmaylow,
                            sigmayup=para_list[[oi]]$sigmayup,
                            bYU=para_list[[oi]]$bYU)
    
    fdata_all[[oi]] <- fdata
  }
  
  
  trueXY <- para_list[[1]]$beta[-Nvar,Nvar]
  pvalueall <- NULL
  for(ov in 1:(Nvar-1)){
    pp <- lapply(fdata_all, function(x) cor.test(x$betaGY-x$betaGX%*%trueXY,x$betaGX[,ov])$p.value)
    pvalueall <- rbind(pvalueall,unlist(pp))
  }
  rownames(pvalueall) <- paste0('x',1:(Nvar-1))
  pvalueall
  
  ##### IVW meta #####
  mrmeta <- MRMETA(fdata_all)
  mrmeta
  
  ##### meta IVW #####
  metamr <- METAMR(fdata_all)
  metamr
  
  ##### invariant MR ############
  
  cv.para1 <- CV_EILLS(fdata_all,
                       r1=r1,meth='L-BFGS-B',
                       trueXY,lambda=seq(0.4,2,0.1))
  cv.para1
  
  # "L-BFGS-B" "CG"
  res_eills <- EILLS(fdata_all,r1=cv.para1$r1,meth='L-BFGS-B',lambda=cv.para1$lambda)
  res_eills

  res_eills$bound <- BOOT_EILLS(fdata_all,r1=cv.para1$r1,meth='L-BFGS-B',
                                lambda=cv.para1$lambda,numBoot)
  res_eills
  
  res_eills <- cbind(res_eills$par,t(res_eills$bound))
  colnames(res_eills) <- c('est','low','up')
  res_eills <- as.data.frame(res_eills)
  
  return(list(para_list,
              mrmeta=mrmeta,
              metamr=metamr,
              res_eills=res_eills,
              cv.para=cv.para1,
              pvalueall=pvalueall))
  
}
