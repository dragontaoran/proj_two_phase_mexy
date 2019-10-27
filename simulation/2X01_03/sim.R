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
sigma = as.numeric(args[7])
design = args[8]

library(TwoPhaseReg)
library(MASS)
library(splines)

NSIM = 1000
n = 1000
n2 = 400
alpha = 0.3
beta = 0.4
gamma = 0.5
pX2 = 0.25
sigma0 = 0.5
p0 = 0.6
rho0 = 0.3

setwd(wd)
fn_out = paste0(njob, ".RData")

results = matrix(NA, nrow=NSIM, ncol=4)
colnames(results) = c("Effect_X1", "SE_X1", "Effect_X2", "SE_X2")

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
    error[indX2_0,] = mvrnorm(nX2_0, mu=c(0,0), Sigma=matrix(c(sigma0, sigma0*rho0, sigma0*rho0, sigma0), nrow=2))
    error[indX2_1,] = mvrnorm(nX2_1, mu=c(0,0), Sigma=matrix(c(sigma, sigma*rho, sigma*rho, sigma), nrow=2))
    
    simS = rep(NA, n)
    simS[indX2_0] = rbinom(nX2_0, 1, p0)
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
    
    # cubic basis
    Bspline_0 = bs(simX_tilde[indX2_0], df=nsieve, degree=3, Boundary.knots=range(simX_tilde[indX2_0]), intercept=TRUE)
    Bspline_1 = bs(simX_tilde[indX2_1], df=nsieve, degree=3, Boundary.knots=range(simX_tilde[indX2_1]), intercept=TRUE)
    Bspline = matrix(0, n, 2*nsieve)
    Bspline[indX2_0,1:nsieve] = Bspline_0
    Bspline[indX2_1,(nsieve+1):(2*nsieve)] = Bspline_1
    colnames(Bspline) = paste("bs", 1:(2*nsieve), sep="")
    # cubic basis
    
    data = data.frame(Y_tilde=simY_tilde, X_tilde=simX_tilde, Y=simY, X=simX, X2=simX2, Bspline)
    ### generate data
    
    res = smle_MEXY(Y="Y", X="X", Y_tilde="Y_tilde", X_tilde="X_tilde", Z="X2", Bspline=colnames(Bspline), data=data, hn_scale=hn_scale)
    if (sum(is.na(res$coefficients)) == 0) {
        results[nsim,1:2] = res$coefficients[2,1:2]
        results[nsim,3:4] = res$coefficients[3,1:2]
        nsim = nsim+1
        if (nsim%%10 == 0) {
            print(paste(nsim, "replicates done."))
        }
    }
}    

# colMeans(results)
# sd(results[,2])
save(list=c("results"), file=fn_out)                   


