# 


########################## sc2 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(MRAID)
library(cause)
library(R2jags)

source('mr_horse.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=2
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=1000
r1 <- 3
Ecor=0
mc=50

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","MRAID","cause","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {

  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,causaleffect=0,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code5_sc2_b0_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list=ls())
gc()

########################## sc2 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(MRAID)
library(cause)
library(R2jags)

source('mr_horse.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=2
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=1000
r1 <- 3
mc=50
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","MRAID","cause","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {

  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,causaleffect=0.1,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code5_sc2_b0.1_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list=ls())
gc()

########################## sc2 balance ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(MRAID)
library(cause)
library(R2jags)

source('mr_horse.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=2
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,-0.5,0.5),rep(0,g-ggY)),
               c(runif(ggY,-0.5,0.5),rep(0,g-ggY)),
               c(runif(ggY,-0.5,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=1000
r1 <- 3
Ecor=0
mc=50

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","MRAID","cause","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {

  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,causaleffect=0,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code5_sc2_b0_g_',g,'_E_',E,'_Ecor_',Ecor,'balance.Rdata'))

rm(list=ls())
gc()

########################## sc2 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(MRAID)
library(cause)
library(R2jags)

source('mr_horse.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=2
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,-0.5,0.5),rep(0,g-ggY)),
               c(runif(ggY,-0.5,0.5),rep(0,g-ggY)),
               c(runif(ggY,-0.5,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)),
              c(runif(gomega,0,0),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=1000
r1 <- 3
mc=50
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","MRAID","cause","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {

  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,causaleffect=0.1,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code5_sc2_b0.1_g_',g,'_E_',E,'_Ecor_',Ecor,'balance.Rdata'))

rm(list=ls())
gc()

