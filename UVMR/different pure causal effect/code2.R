# 
########################## sc1 ##########

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
numBoot=1000
r1 <- 3
Ecor=0
mc=50
causaleffect <- list(rep(0.1,Nvar),
                     rep(0.2,Nvar),
                     rep(0.3,Nvar))

result <- NULL

registerDoMC(mc)
result <- foreach(ii=1:200,
                  .packages = c("meta","MendelianRandomization","MASS",
                                "combinat","hash","MRAID","cause","R2jags"),
                  .errorhandling =  "remove"
) %dopar% {

  COMP(ii,E,Nvar,g,gammaY,sigmax,omega,sigmaxlow,sigmaxup,
       bXU,sigmay,sigmaylow,sigmayup,bYU,r1=r1,numBoot,causaleffect,Ecor)
}

save(result,file=paste0('code2_sc1_b0_g_',g,'_E_',E,'_Ecor_',Ecor,'differentcausal.Rdata'))

rm(list=ls())
gc()
