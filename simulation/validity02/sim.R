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
nsieve = 10

#### notations are adopted from Shepherd and Yu (2011)
n = 1000
sigma_u = 50
beta0 = 6
beta1 = -0.01
sigma_ep = 0.5
sigma_x = 50
mu_x = 200
p_y = 0.2

args = commandArgs(TRUE)
njob = as.integer(args[1])
n2 = as.numeric(args[2])
p_s = as.numeric(args[3])
rho_u_ustar = as.numeric(args[4])

fn_out = paste0("res/n2_", n2, "_p_s_", p_s, "_rho_u_ustar_", rho_u_ustar, "/", njob, ".Rdata")

set.seed(12345+5000*njob)

results = matrix(NA, nrow=NSIM, ncol=2)
colnames(results) = c("Effect", "SE")

nsim = 1
while (nsim <= NSIM) {
    
    ### generate data
    
    sim_x = rnorm(n, mu_x, sigma_x)
    sim_ep = rnorm(n, 0, sigma_ep)
    sim_y = beta0+beta1*sim_x+sim_ep

    sim_uy = rnorm(n)
	sim_sy = rbinom(n, 1, p_y)
    sim_error = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(0.5^2, rho_u_ustar*0.5*sigma_u, rho_u_ustar*0.5*sigma_u, sigma_u^2), nrow=2))
    sim_ustar = sim_error[,1]
    sim_u = sim_error[,2]
	sim_s = rbinom(n, 1, p_s)

    sim_w = sim_x+sim_s*sim_u
	sim_ystar = sim_y+sim_sy*sim_uy+sim_s*sim_ustar

    id_phase2 = c(sample(n, n2))
    sim_y[-id_phase2] = NA
    sim_x[-id_phase2] = NA
    
    # # histogram basis
    # bspline = matrix(NA, nrow=n, ncol=nsieve)
    # cut_w = cut(sim_w, breaks=quantile(sim_w, probs=seq(0, 1, 1/nsieve)), include.lowest = TRUE)
    # for (i in 1:nsieve) {
    #     bspline[,i] = as.numeric(cut_w == names(table(cut_w))[i])
    # }
    # colnames(bspline) = paste("bs", 1:nsieve, sep="")
    # # histogram basis
    
    # # linear basis
    # bspline = bs(sim_w, df=nsieve, degree=1, Boundary.knots=range(sim_w), intercept=TRUE)
    # colnames(bspline) = paste("bs", 1:nsieve, sep="")
    # # linear basis
    
    # # quadratic basis
    # bspline = bs(sim_w, df=nsieve, degree=2, Boundary.knots=range(sim_w), intercept=TRUE)
    # colnames(bspline) = paste("bs", 1:nsieve, sep="")
    # # quadratic basis
    
    # cubic basis
    bspline = bs(sim_w, df=nsieve, degree=3, Boundary.knots=range(sim_w), intercept=TRUE)
    colnames(bspline) = paste("bs", 1:nsieve, sep="")
    # cubic basis

    data = data.frame(ystar=sim_ystar, w=sim_w, y=sim_y, x=sim_x, bspline)
    ### generate data
    
    # rescale data
	x_scale = 10
    data$ystar = data$ystar-beta0
    data$y = data$y-beta0
    data$w = data$w/x_scale
    data$x = data$x/x_scale
    # rescale data
    
    res = smle_MEXY(Y="y", X="x", Y_tilde="ystar", X_tilde="w", Bspline=colnames(bspline), data=data, hn_scale=0.05)
    if (sum(is.na(res$coefficients)) == 0) {
        results[nsim,] = res$coefficients[2,1:2]
        
        # rescale results
        results[nsim,] = results[nsim,]/x_scale
        # rescale results
        
        nsim = nsim+1
        if (nsim%%10 == 0) {
            print(paste(nsim, "replicates done."))
        }
    }
}

save(list="results", file=fn_out)

