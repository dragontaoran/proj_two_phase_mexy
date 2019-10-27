args = commandArgs(TRUE)
wd = args[1]

source("~/research/two_phase_mexy/simulation/CC2000_algorithm.R")
library(MASS)

NSIM = 1000
NJOB = 10
p_set = c(0.6, 1)
rho_set = c(0.3, 0.5)
sigma_set = c(0.5, 1)
n = 1000
n2 = 400
alpha = 0.3
beta = 0.4
gamma = 0.5
pX2 = 0.5
design_set = c("srs", "ssrs")

setwd(wd)

for (design in design_set) {
    for (rho in rho_set) {
        for (p in p_set) {
            for (sigma in sigma_set) {
                fn_out = paste0("CC2000_", design, "_rho", rho, "_p", p, "_sigma", sigma, ".RData")
                
                results = matrix(NA, nrow=NSIM*NJOB, ncol=4)
                colnames(results) = c("Effect_X1", "SE_X1", "Effect_X2", "SE_X2")
                
                for (njob in 1:NJOB) {
                    set.seed(12345+5000*njob)
                    
                    nsim = 1
                    while (nsim <= NSIM) {
                        
                        ### generate data
                        simX = rnorm(n)
                        simX2 = rbinom(n, 1, pX2)
                        epsilon = rnorm(n)
                        simY = alpha+beta*simX+gamma*simX2+epsilon
                        
                        indX2_0 = which(simX2 == 0)
                        nX2_0 = length(indX2_0)
                        indX2_1 = which(simX2 == 1)
                        nX2_1 = length(indX2_1)
                        
                        error = matrix(NA, n, 2)
                        error[indX2_0,] = mvrnorm(nX2_0, mu=c(0,0), Sigma=matrix(c(sigma_set[1], sigma_set[1]*rho_set[1], sigma_set[1]*rho_set[1], sigma_set[1]), nrow=2))
                        error[indX2_1,] = mvrnorm(nX2_1, mu=c(0,0), Sigma=matrix(c(sigma, sigma*rho, sigma*rho, sigma), nrow=2))
                        
                        simS = rep(NA, n)
                        simS[indX2_0] = rbinom(nX2_0, 1, p_set[1])
                        simS[indX2_1] = rbinom(nX2_1, 1, p)
                        
                        simU = simS*error[,2]
                        simW = simS*error[,1]
                        simY_tilde = simY+simW
                        simX_tilde = simX+simU
                        
                        if (design == "srs") {
                            id_phase2 = sample(n, n2)
                        } else if (design == "ssrs") {
                            id_phase2 = c(sample(indX2_0, n2/2), sample(indX2_1, n2/2))
                        }
                        
                        simY[-id_phase2] = NA
                        simX[-id_phase2] = NA
                        
                        res = chen2(simY, cbind(simX, simX2), simY_tilde, cbind(simX_tilde, simX2))
                        index = NSIM*njob-NSIM+nsim
                        results[index,c(1,3)] = res$est[-1]
                        results[index,c(2,4)] = res$se[-1]
                        nsim = nsim+1
                    }    
                }
                
                # colMeans(results)
                # sd(results[,2])
                save(list=c("results"), file=fn_out)                   
            }
        }        
    }    
}

