args = commandArgs(TRUE)
wd = args[1]

source("~/research/two_phase_mexy/simulation/SY2011_algorithm.R")
library(MASS)

NSIM = 100
NJOB = 100
p = 0.6
rho = 0.3
n = 1000
# n2_set = c(50, 100, 200, 300)
n2_set = c(25)
alpha = 0.3
beta = 0.4

setwd(wd)

for (n2 in n2_set) {
	fn_out = paste0("SY2011_n2_", n2, ".RData")
	
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
	        error = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1, rho, rho, 1), nrow=2))
	        
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
