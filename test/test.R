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

library(TwoPhaseReg)
library(MASS)
library(splines)
NSIM = 100
n = 500
n2 = 100
nsieve = 10

args = commandArgs(TRUE)
njob = as.integer(args[1])

fn_out = paste0("res/test_", njob, ".Rdata")

set.seed(12345+5000*njob)

results_est = matrix(NA, nrow=NSIM, ncol=2)
colnames(results_est) = c("intercept", "X")
results_se = matrix(NA, nrow=NSIM, ncol=2)
colnames(results_se) = c("intercept", "X")

nsim = 1
while (nsim <= NSIM) {
    
    ### generate data
    
    ## SY_2011 setup
    
    # case 1: errors in Y and X
    alpha = 6
    beta = -0.01
    simX = rnorm(n, 200, 50)
    epsilon = rnorm(n, 0, 0.5)
    simY = alpha+beta*simX+epsilon
    error = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(0.5^2, 0.5*0.5*20, 0.5*0.5*20, 20^2), nrow=2))
    simU = error[,2]
    simW = error[,1]
    simY_tilde = simY+simW
    simX_tilde = simX+simU
    id_phase2 = c(sample(n, n2))
    # case 1: errors in Y and X
    
    # # case 2: error in X only
    # alpha = 6
    # beta = -0.01
    # simX = rnorm(n, 200, 50)
    # epsilon = rnorm(n, 0, 0.5)
    # simY = alpha+beta*simX+epsilon
    # error = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(0, 0, 0, 20^2), nrow=2))
    # simU = error[,2]
    # simW = error[,1]
    # simY_tilde = simY+simW
    # simX_tilde = simX+simU
    # id_phase2 = c(sample(n, n2))
    # # case 2: error in X only
    
    # SY_2011 setup
     
    # naive setup
    
    # # case 1: simple random sampling
    # alpha = 0.3
    # beta = 0.5
    # simX = rnorm(n)
    # epsilon = rnorm(n)
    # simY = alpha+beta*simX+epsilon
    # error = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(0, 0, 0, 1), nrow=2))
    # 
    # simU = error[,2]
    # simW = error[,1]
    # simY_tilde = simY+simW
    # simX_tilde = simX+simU
    # id_phase2 = c(sample(n, n2))
    # # case 1: simple random sampling
    
    # # case 2: outcome-dependent sampling
    # alpah = 0.3
    # beta = 0.5
    # simX_tilde = rnorm(n)
    # error = rnorm(n)
    # simX = simX_tilde-error
    # epsilon = rnorm(n)
    # simY = alpha+beta*simX+epsilon
    # simY_tilde = simY
    # order_Y = order(simY_tilde)
    # id_phase2 = c(order_Y[1:(n2/2)], order_Y[(n-n2/2+1):n])
    # # case 2: outcome-dependent sampling
    
    # naive setup
    
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
    
    # quadratic basis
    Bspline = bs(simX_tilde, df=nsieve, degree=2, Boundary.knots=range(simX_tilde), intercept=TRUE)
    colnames(Bspline) = paste("bs", 1:nsieve, sep="")
    # quadratic basis

    
    data = data.frame(Y_tilde=simY_tilde, X_tilde=simX_tilde, Y=simY, X=simX, Bspline)
    ### generate data
    
    # rescale data
    data$Y_tilde = data$Y_tilde-alpha
    data$Y = data$Y - alpha
    data$X_tilde = data$X_tilde/10
    data$X = data$X/10
    # rescale data
    
    res = smle_MEXY(Y="Y", X="X", Y_tilde="Y_tilde", X_tilde="X_tilde", Bspline=colnames(Bspline), data = data, hn_scale=0.5)
    if (sum(is.na(res$coefficients)) == 0) {
        results_est[nsim,] = t(res$coefficients[,1])
        results_se[nsim,] = t(res$coefficients[,2])
        
        # rescale results
        results_est[nsim,1] = results_est[nsim,1]+alpha
        results_est[nsim,2] = results_est[nsim,2]/10
        results_se[nsim,2] = results_se[nsim,2]/10
        # rescale results
        
        nsim = nsim+1
        if (nsim%%10 == 0) {
            print(paste(nsim, "replicates done."))
        }
    }
}

# colMeans(results_est)
# apply(results_est, 2, sd)
# colMeans(results_se)

save(list=c("results_est", "results_se"), file=fn_out)
