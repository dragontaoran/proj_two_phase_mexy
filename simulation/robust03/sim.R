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

NSIM = 1000
n = 1000
n2 = 400
alpha = 0.3
beta = 0.4

setwd(wd)
fn_out = paste0(njob, ".RData")

set.seed(12345+5000*njob)

results = matrix(NA, nrow=NSIM, ncol=2)
colnames(results) = c("Effect", "SE")

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
    simX_tilde = simX*exp(simU)
    
    id_phase2 = sample(n, n2)

    simY[-id_phase2] = NA
    simX[-id_phase2] = NA
    
    # # histogram basis
    # Bspline = matrix(NA, nrow=n, ncol=nsieve)
    # cut_x_tilde = cut(simX_tilde, breaks=quantile(simX_tilde, probs=seq(0, 1, 1/nsieve)), include.lowest = TRUE)
    # for (i in 1:nsieve) {
    #     Bspline[,i] = as.numeric(cut_x_tilde == names(table(cut_x_tilde))[i])
    # }
    # colnames(Bspline) = paste("bs", 1:nsieve, sep="")
    # # histogram basis
    
    # # linear basis
    # Bspline = bs(simX_tilde, df=nsieve, degree=1, Boundary.knots=range(simX_tilde), intercept=TRUE)
    # colnames(Bspline) = paste("bs", 1:nsieve, sep="")
    # # linear basis
    
    # # quadratic basis
    # Bspline = bs(simX_tilde, df=nsieve, degree=2, Boundary.knots=range(simX_tilde), intercept=TRUE)
    # colnames(Bspline) = paste("bs", 1:nsieve, sep="")
    # # quadratic basis
    
    # cubic basis
    Bspline = bs(simX_tilde, df=nsieve, degree=3, Boundary.knots=range(simX_tilde), intercept=TRUE)
    colnames(Bspline) = paste("bs", 1:nsieve, sep="")
    # cubic basis
    
    data = data.frame(Y_tilde=simY_tilde, X_tilde=simX_tilde, Y=simY, X=simX, Bspline)
    ### generate data
    
    res = smle_MEXY(Y="Y", X="X", Y_tilde="Y_tilde", X_tilde="X_tilde", Bspline=colnames(Bspline), data=data, hn_scale=hn_scale)
    if (sum(is.na(res$coefficients)) == 0) {
        results[nsim,] = res$coefficients[2,1:2]
        nsim = nsim+1
        if (nsim%%10 == 0) {
            print(paste(nsim, "replicates done."))
        }
    }
}

# colMeans(results)
# sd(results[,2])
save(list=c("results"), file=fn_out)
