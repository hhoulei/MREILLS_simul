########################## sc1 ##########

library(MASS)
library(MendelianRandomization)
library(meta)
library(foreach)
library(doParallel)
library(doMC)

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
numBoot=100
r1 <- 0.5
lambda <- 5
mc=20

result <- NULL
cl <- makeCluster(mc)
registerDoParallel(cl)
#registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash"),
                  .errorhandling =  "remove"
) %dopar% {
  source('summary_mvMR_SSS.R')
  source('summary_mvMR_BF.R')
  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,lambda=lambda,numBoot)
}
stopCluster(cl)
save(result,file=paste0('code5_sc1_g_',g,'_E_',E,'_var_',Nvar,'.Rdata'))
