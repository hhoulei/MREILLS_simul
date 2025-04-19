
########################## sc1 ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=3
Nvar=9
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc1_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()

########################## sc2 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(R2jags)
source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=9
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
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc2_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()
########################## sc3 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(R2jags)
source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=9
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0),rep(0,g-ggY)),
               c(runif(ggY,0,0),rep(0,g-ggY)),
               c(runif(ggY,0,0),rep(0,g-ggY)))
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
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc3_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()

########################## sc4 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)
source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=9
g=100
gomega <- floor(g*0.8)
ggY <- floor(g*0.8)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc4_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()


########################## sc5 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)
source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=9
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
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc5_g_',g,'_E_',E,'_Ecor_',Ecor,'balance.Rdata'))

rm(list = ls())
gc()


########################## sc1 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)
source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')


E=3
Nvar=4
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=20
Ecor=0

result <- NULL
cl <- makeCluster(mc)
registerDoParallel(cl)
#registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
stopCluster(cl)
save(result,file=paste0('code2_sc6_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()
########################## sc7 Ecor ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=3
Nvar=9
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0.2

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc7_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()


########################## sc8 Ecor ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=3
Nvar=9
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0.6

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc8_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()


######################### sc9 Ecor ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=8
Nvar=9
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc9_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()

####################### sc10 Ecor ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=3
Nvar=9
g=300
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc10_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()


###################### sc11 Ecor ##########


library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)
library(R2jags)

source('mr_horse.R')
source('summary_mvMR_SSS.R')
source('summary_mvMR_BF.R')
source('DataGeneration.R')
source('MR_EILLS.R')
source('ComparedMethods.R')
source('COMP.R')

E=3
Nvar=16
g=100
gomega <- floor(g*0.3)
ggY <- floor(g*0.3)
gammaY <- list(c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)),
               c(runif(ggY,0,0.5),rep(0,g-ggY)))
sigmax <- runif(E,0.01,0.05)
omega <- list(c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)),
              c(runif(gomega,0,0.5),rep(0,g-gomega)))
sigmaxlow <- rep(0.01,E)
sigmaxup <- rep(0.05,E)
bXU <- list(runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8),
            runif(Nvar-1,0.5,0.8))
sigmay<- runif(E,0.05,0.1)
sigmaylow<- rep(0.05,E)
sigmayup<- rep(0.1,E)
bYU <- runif(E,0.5,0.8)
numBoot=500
r1 <- 0.5
mc=70
Ecor=0

result <- NULL
# cl <- makeCluster(mc)
# registerDoParallel(cl)
registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,Ecor)
}
#stopCluster(cl)
save(result,file=paste0('code2_sc11_g_',g,'_E_',E,'_Ecor_',Ecor,'.Rdata'))

rm(list = ls())
gc()
