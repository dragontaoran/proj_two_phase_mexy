args = commandArgs(TRUE)
wd = args[1]

source("~/research/two_phase_mexy/simulation/SY2011_algorithm.R")
library(MASS)

NSIM = 1000
NJOB = 10
p = 1
rho_set = c(-0.5, 0, 0.5)
muu_set = c(0, 0.5)
muw_set = c(0, 0.5)
n = 1000
n2 = 400
alpha = 0.3
beta = 0.4

setwd(wd)

for (rho in rho_set) {
    for (muu in muu_set) {
        for (muw in muw_set) {
            fn_out = paste0("SY2011_rho", rho, "_muu", muu, "_muw", muw, ".RData")
            
            results = matrix(NA, nrow=NSIM*NJOB, ncol=2)
            colnames(results) = c("Effect", "SE")
            
            for (njob in 1:NJOB) {
                set.seed(12345+5000*njob)
                
                nsim = 1
                while (nsim <= NSIM) {
                    
                    ### generate data
                    simX = rnorm(n)
                    epsilon = rnorm(n)
                    simY = alpha+beta*simX+epsilon
                    error = mvrnorm(n, mu=c(muu,muw), Sigma=matrix(c(1, rho, rho, 1), nrow=2))
                    
                    simS = rbinom(n, 1, p)
                    simU = simS*error[,2]
                    simW = simS*error[,1]
                    simY_tilde = simY+simW
                    simX_tilde = simX+simU
                    
                    id_phase2 = sample(n, n2)
                    simY[-id_phase2] = NA
                    simX[-id_phase2] = NA
                    
                    res = audit.correct2(simY_tilde, simY, simX_tilde, simX)
                    index = NSIM*njob-NSIM+nsim
                    results[index,1] = res$CorrectEst
                    results[index,2] = sqrt(res$VarCorrectEst)
                    nsim = nsim+1
                }    
            }
            
            # colMeans(results)
            # sd(results[,2])
            save(list=c("results"), file=fn_out)             
        }
    }
}
