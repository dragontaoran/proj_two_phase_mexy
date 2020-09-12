# ### intsall/update TwoPhaseReg
# library(devtools)
# 
# ## from github
# install_github("dragontaoran/TwoPhaseReg")
# 
# ## from 2525 and LH office desktop
# install("C:/Users/taor2/Dropbox/two_phase/TwoPhaseReg", local=FALSE)
#
# ## from laptop
# install("C:/Users/Ran Tao/Dropbox/two_phase/TwoPhaseReg", local=FALSE)
# ### intsall/update TwoPhaseReg

args = commandArgs(TRUE)
njob = as.integer(args[1])
rho = as.numeric(args[2])
p = as.numeric(args[3])
hn_scale = as.numeric(args[4])
nsieve = as.integer(args[5])
wd = args[6]

library(TwoPhaseReg)
library(MASS)
library(splines)

NSIM = 40
n = 1000
n2 = 400
alpha = 0.3
beta = 0.4

setwd(wd)
fn_out = paste0(njob, ".RData")

set.seed(12345+5000*njob)

results = matrix(NA, nrow=NSIM, ncol=4)
colnames(results) = c("Effect_X1", "SE_X1", "Effect_X2", "SE_X2")

nsim = 1
while (nsim <= NSIM) {
    
	# timeStart = Sys.time()

    ### generate data
    simX1 = rnorm(n)
	simX2 = rnorm(n)
    epsilon = rnorm(n)
    simY = alpha+beta*simX1+beta*simX2+epsilon
    error = mvrnorm(n, mu=c(0,0,0), Sigma=matrix(c(1, rho, rho, rho, 1, rho, rho, rho, 1), nrow=3))
    
    simS = rbinom(n, 1, p)
    simU1 = simS*error[,2]
	simU2 = simS*error[,3]
    simW = simS*error[,1]
    simY_tilde = simY+simW
    simX1_tilde = simX1+simU1
	simX2_tilde = simX2+simU2
    
    id_phase2 = sample(n, n2)

    simY[-id_phase2] = NA
    simX1[-id_phase2] = NA
	simX2[-id_phase2] = NA
    
    # cubic basis
    Bspline1 = bs(simX1_tilde, df=nsieve, degree=3, Boundary.knots=range(simX1_tilde), intercept=TRUE)
	Bspline2 = bs(simX2_tilde, df=nsieve, degree=3, Boundary.knots=range(simX2_tilde), intercept=TRUE)
	Bspline = matrix(NA, nrow=n, ncol=nsieve^2)
	for (i in 1:nsieve) {
		for (j in 1:nsieve) {
			Bspline[,i*nsieve-nsieve+j] = Bspline1[,i]*Bspline2[,j]
		} 
	}
    colnames(Bspline) = paste("bs", 1:ncol(Bspline), sep="")
    # cubic basis
    
    data = data.frame(Y_tilde=simY_tilde, X1_tilde=simX1_tilde, X2_tilde=simX2_tilde, Y=simY, X1=simX1, X2=simX2,  Bspline)
    ### generate data
    
    res = smle_MEXY(Y="Y", X=c("X1", "X2"), Y_tilde="Y_tilde", X_tilde=c("X1_tilde", "X2_tilde"), Bspline=colnames(Bspline), data=data, hn_scale=0.1)

	# timeEnd = Sys.time()
	# timeEnd - timeStart

	if (sum(is.na(res$coefficients)) == 0) {
        results[nsim,] = c(res$coefficients[2,1:2], res$coefficients[3,1:2])
        nsim = nsim+1
        if (nsim%%10 == 0) {
            print(paste(nsim, "replicates done."))
        }
    }
}

# colMeans(results)
# sd(results[,2])
save(list=c("results"), file=fn_out)
