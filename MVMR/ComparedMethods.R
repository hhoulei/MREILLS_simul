
MR_BMA <- function(obj){
  
  BMAoutput <- summarymvMR_SSS(obj,kmin=1, kmax=20, max_iter=1000, 
                               sigma=0.5, prior_prob=0.5, print=F)
  permutbma <- create.permutations(BMAoutput,nrepeat = 100,save.matrix = F)
  resp <- calculate.p(BMAoutput,permutbma)
  res <- cbind(BMAoutput@BMAve_Estimate,resp[,2])
  res <- as.data.frame(res)
  colnames(res) <- c('est','pval')
  
  return(res)
}

MRMETA <- function(fdata_all){
  
  Nv <- ncol(fdata_all[[1]]$betaGX)
  mr_beta1 <- NULL
  mr_se1 <- NULL
  mr_beta2 <- NULL
  mr_se2 <- NULL
  mr_beta3 <- NULL
  mr_se3 <- NULL
  mr_beta4 <- NULL
  mr_se4 <- NULL
  mr_beta5 <- NULL
  mr_se5 <- NULL
  mr_beta6 <- NULL
  mr_se6 <- NULL
  for(ok in 1:length(fdata_all)){
    
    #cat('ok1=',ok,'\n')
    
    obj <- mr_mvinput(fdata_all[[ok]]$betaGX,
                      fdata_all[[ok]]$sebetaGX,
                      fdata_all[[ok]]$betaGY,
                      fdata_all[[ok]]$sebetaGY)
    res1 <- mr_mvivw(obj)
    res2 <- mr_mvegger(obj)
    res3 <- mr_mvlasso(obj)
    res4 <- mr_mvmedian(obj)
    #res5 <- mr_mvcML(obj,DP=F,n=10000)
    
    data_mv_ex <- cbind(fdata_all[[ok]]$betaGX,
                        fdata_all[[ok]]$sebetaGX,
                        fdata_all[[ok]]$betaGY,
                        fdata_all[[ok]]$sebetaGY)
    data_mv_ex <- as.data.frame(data_mv_ex)
    colnames(data_mv_ex) <- c(paste0('betaX',1:Nv),
                              paste0('betaX',1:Nv,'se'),
                              'betaY',
                              'betaYse')
    res6 <- mvmr_horse(data_mv_ex)
    
    mr_beta1 <- rbind(mr_beta1,res1@Estimate)
    mr_se1 <- rbind(mr_se1,res1@StdError)
    mr_beta2 <- rbind(mr_beta2,res2@Estimate)
    mr_se2 <- rbind(mr_se2,res2@StdError.Est)
    mr_beta3 <- rbind(mr_beta3,res3@Estimate)
    mr_se3 <- rbind(mr_se3,res3@StdError)
    mr_beta4 <- rbind(mr_beta4,res4@Estimate)
    mr_se4 <- rbind(mr_se4,res4@StdError)
    mr_beta6 <- rbind(mr_beta6,res6$MR_Estimate$Estimate)
    mr_se6 <- rbind(mr_se6,res6$MR_Estimate$SD)
    
    x.inv <- try(mr_mvcML(obj,DP=F,n=10000),
                 silent = T)
    if('try-error' %in% class(x.inv)){
      mr_beta5 <- rbind(mr_beta5,NA)
      mr_se5 <- rbind(mr_se5,NA)
    }else{
      res5 <- x.inv
      mr_beta5 <- rbind(mr_beta5,res5@Estimate)
      mr_se5 <- rbind(mr_se5,res5@StdError) 
    }

    
  }
  mrmeta1 <- NULL
  mrmeta2 <- NULL
  mrmeta3 <- NULL
  mrmeta4 <- NULL
  mrmeta5 <- NULL
  mrmeta6 <- NULL
  for(ok in 1:ncol(mr_beta1)){
    
    #cat('ok2=',ok,'\n')
    
    once1 <- metagen(mr_beta1[,ok],mr_se1[,ok])
    mrmeta1 <- rbind(mrmeta1,
                     c(once1$TE.common, once1$pval.common,
                       once1$TE.random, once1$pval.random))
    once2 <- metagen(mr_beta2[,ok],mr_se2[,ok])
    mrmeta2 <- rbind(mrmeta2,
                     c(once2$TE.common, once2$pval.common,
                       once2$TE.random, once2$pval.random))
    once3 <- metagen(mr_beta3[,ok],mr_se3[,ok])
    mrmeta3 <- rbind(mrmeta3,
                     c(once3$TE.common, once3$pval.common,
                       once3$TE.random, once3$pval.random))
    once4 <- metagen(mr_beta4[,ok],mr_se4[,ok])
    mrmeta4 <- rbind(mrmeta4,
                     c(once4$TE.common, once4$pval.common,
                       once4$TE.random, once4$pval.random))
    once5 <- metagen(mr_beta5[,ok],mr_se5[,ok])
    mrmeta5 <- rbind(mrmeta5,
                     c(once5$TE.common, once5$pval.common,
                       once5$TE.random, once5$pval.random))
    once6 <- metagen(mr_beta6[,ok],mr_se6[,ok])
    mrmeta6 <- rbind(mrmeta6,
                     c(once6$TE.common, once6$pval.common,
                       once6$TE.random, once6$pval.random))
    
  }
  colnames(mrmeta1) <- colnames(mrmeta2) <- colnames(mrmeta3) <- 
    colnames(mrmeta4) <- colnames(mrmeta5) <- 
    colnames(mrmeta6) <- c('TE.common','pval.common',
                           'TE.random','pval.random')
  
  mrmeta <- list(mrmeta1=mrmeta1,
                 mrmeta2=mrmeta2,
                 mrmeta3=mrmeta3,
                 mrmeta4=mrmeta4,
                 mrmeta5=mrmeta5,
                 mrmeta6=mrmeta6)
  names(mrmeta) <- c('MVIVW','MVEgger','MVLasso','MVMedian','MVcML','MVHorse')
  return(mrmeta)
}

METAMR <- function(fdata_all){
  
  Nvar <- ncol(fdata_all[[1]]$betaGX)+1
  
  betaXall <- list()
  sebetaXall <- list()
  betaYall <- NULL
  sebetaYall <- NULL
  for(on in 1:(Nvar-1)){
    once1 <- NULL
    once2 <- NULL
    for(om in 1:length(fdata_all)){
      once1 <- cbind(once1,fdata_all[[om]]$betaGX[,on])
      once2 <- cbind(once2,fdata_all[[om]]$sebetaGX[,on])
    }
    betaXall[[on]] <- once1
    sebetaXall[[on]] <- once2
  }
  for(om in 1:length(fdata_all)){
   
    betaYall <- cbind(betaYall,fdata_all[[om]]$betaGY)
    sebetaYall <- cbind(sebetaYall,fdata_all[[om]]$sebetaGY)
  }
  
  XGmeta <- list()
  YGmeta <- NULL
  for(om in 1:length(betaXall)){
    once <- betaXall[[om]]
    seonce <- sebetaXall[[om]]
    
    res <- NULL
    for(on in 1:nrow(once)){
      
      #cat('omx=',om,' onx=',on,'\n')
      
      once0 <- metagen(once[on,],seonce[on,])
      res <- rbind(res, c(once0$TE.common, once0$seTE.common,
                          once0$TE.random, once0$seTE.random))
    }
    colnames(res) <- c('TE.common','se.common',
                       'TE.random','se.random')
    XGmeta[[om]] <- as.data.frame(res)
  }
  res <- NULL
  for(on in 1:nrow(betaYall)){
    #cat('ony=',on,'\n')
    once0 <- metagen(betaYall[on,],sebetaYall[on,])
    res <- rbind(res, c(once0$TE.common, once0$seTE.common,
                        once0$TE.random, once0$seTE.random))
  }
  colnames(res) <- c('TE.common','se.common',
                     'TE.random','se.random')
  YGmeta <- as.data.frame(res)
  
  betaXGm1 <- NULL
  sebetaXGm1 <- NULL
  betaXGm2 <- NULL
  sebetaXGm2 <- NULL
  for(om in 1:length(XGmeta)){
    betaXGm1 <- cbind(betaXGm1,XGmeta[[om]]$TE.common)
    sebetaXGm1 <- cbind(sebetaXGm1,XGmeta[[om]]$se.common)
    betaXGm2 <- cbind(betaXGm2,XGmeta[[om]]$TE.random)
    sebetaXGm2 <- cbind(sebetaXGm2,XGmeta[[om]]$se.random)
  }
  
  
  obj1 <- mr_mvinput(betaXGm1,
                     sebetaXGm1,
                     YGmeta$TE.common,
                     YGmeta$se.common)
  res11 <- mr_mvivw(obj1)
  res12 <- mr_mvegger(obj1)
  res13 <- mr_mvlasso(obj1)
  res14 <- mr_mvmedian(obj1)
  res15 <- mr_mvcML(obj1,DP=F,n=10000)
  res16 <- MR_BMA(obj1)
  
  data_mv_ex1 <- cbind(betaXGm1,
                      sebetaXGm1,
                      YGmeta$TE.common,
                      YGmeta$se.common)
  data_mv_ex1 <- as.data.frame(data_mv_ex1)
  colnames(data_mv_ex1) <- c(paste0('betaX',1:(Nvar-1)),
                            paste0('betaX',1:(Nvar-1),'se'),
                            'betaY',
                            'betaYse')
  res17 <- mvmr_horse(data_mv_ex1)
  
  
  obj2 <- mr_mvinput(betaXGm2,
                     sebetaXGm2,
                     YGmeta$TE.random,
                     YGmeta$se.random)
  res21 <- mr_mvivw(obj2)
  res22 <- mr_mvegger(obj2)
  res23 <- mr_mvlasso(obj2)
  res24 <- mr_mvmedian(obj2)
  #res25 <- mr_mvcML(obj2,n=10000)
  res26 <- MR_BMA(obj2)
  
  data_mv_ex2 <- cbind(betaXGm2,
                      sebetaXGm2,
                      YGmeta$TE.random,
                      YGmeta$se.random)
  data_mv_ex2 <- as.data.frame(data_mv_ex2)
  colnames(data_mv_ex2) <- c(paste0('betaX',1:(Nvar-1)),
                            paste0('betaX',1:(Nvar-1),'se'),
                            'betaY',
                            'betaYse')
  res27 <- mvmr_horse(data_mv_ex2)
  
  
  x.inv <- try(mr_mvcML(obj2,DP=F,n=10000),
               silent = T)
  if('try-error' %in% class(x.inv)){
    res5 <- data.frame(res15@Estimate,res15@Pvalue,rep(NA,Nvar-1),rep(NA,Nvar-1)) 
  }else{
    res25 <- x.inv
    res5 <- data.frame(res15@Estimate,res15@Pvalue,res25@Estimate,res25@Pvalue) 
  }
  
  res1 <- data.frame(res11@Estimate,res11@Pvalue,res21@Estimate,res21@Pvalue)
  res2 <- data.frame(res12@Estimate,res12@Pvalue.Est,res22@Estimate,res22@Pvalue.Est)
  res3 <- data.frame(res13@Estimate,res13@Pvalue,res23@Estimate,res23@Pvalue)
  res4 <- data.frame(res14@Estimate,res14@Pvalue,res24@Estimate,res24@Pvalue)
  #res5 <- data.frame(res15@Estimate,res15@Pvalue,res25@Estimate,res25@Pvalue) 
  res6 <- data.frame(res16$MR_Estimate$Estimate,
                     ifelse(res16$MR_Estimate$`2.5% quantile`>0 | res16$MR_Estimate$`97.5% quantile`<0,1,0),
                     res26$MR_Estimate$Estimate,
                     ifelse(res26$MR_Estimate$`2.5% quantile`>0 | res26$MR_Estimate$`97.5% quantile`<0,1,0))
  res7 <- data.frame(res17$MR_Estimate$Estimate,
                     ifelse(res17$MR_Estimate$`2.5% quantile`>0 | res17$MR_Estimate$`97.5% quantile`<0,1,0),
                     res27$MR_Estimate$Estimate,
                     ifelse(res27$MR_Estimate$`2.5% quantile`>0 | res27$MR_Estimate$`97.5% quantile`<0,1,0))
  
  colnames(res1) <- colnames(res2) <- colnames(res3) <- 
    colnames(res4) <- colnames(res5) <- colnames(res6) <- colnames(res7) <- 
    c('TE.common','pval.common','TE.random','pval.random')
  
  
  metamr <- list(res1=res1,
                 res2=res2,
                 res3=res3,
                 res4=res4,
                 res5=res5,
                 res6=res6,
                 res7=res7)
  names(metamr) <- c('MVIVW','MVEgger','MVLasso','MVMedian','MVcML','MVBMA','MVHorse')
  return(metamr)
}
