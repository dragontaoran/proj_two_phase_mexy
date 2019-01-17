## adopted from http://biostat.mc.vanderbilt.edu/wiki/Main/DataAuditSimulationCode

## audit.correct2() takes information from an audit, and corrects naive estimates when there are errors in the
## predictor and outcome variables which are potentially correlated. This function employs the methods given in
## Section 2.3, and returns the estimate CorrectEst and its variance as outlined in Section 2.3.


audit.correct2 <- function(ystar, y, w, v, x, s, p) {
    
    N <- length(y)
    mod.atten <- lm(ystar~w)
    gamma1 <- mod.atten$coeff[2]
    var.gamma1 <- summary(mod.atten)$coeff[2,2]^2
    
    var.w <- var(w)
    mu.w <- mean(w)
    
    p.est <- mean(s[v==1])
    u.est <- w[s*v==1]-x[s*v==1]
    var.u <- ifelse(sum(s*v)==0, 0, ifelse(sum(s*v)==1, u.est^2, var(u.est)*((sum(s*v)-1)/sum(s*v))))
    var.x <- var(x[v==1])*((sum(v)-1)/sum(v))
    mu.x <- mean(x[v==1])
    
    resid.y <- y[v*s==1]-ystar[v*s==1]
    resid.x <- x[v*s==1]-w[v*s==1]
    cov.est <- ifelse(sum(s*v)==0, 0, ifelse(sum(s*v)==1, resid.y*resid.x, sum(resid.y*resid.x)/(sum(s*v)-1)))
    mu.star <- mean(resid.y)
    
    fixed <- (gamma1*(var.x+var.u*p.est)-p.est*cov.est)/var.x
    
    check <- sum(s*v)
    
    if (check>0) {
        A <- matrix(0,7,7)
        A[1,1] <- 1
        A[1,2] <- mean(w)
        A[2,1] <- mean(w)
        A[2,2] <- sum(w^2)/N
        A[3,3] <- sum(v)/N
        A[4,3] <- 2*sum(v*(x-mu.x))/N
        A[4,4] <- sum(v)/N
        A[5,5] <- sum(v)/N
        A[6,6] <- sum(s*v)/N
        A[7,7] <- sum(s*v)/N
        
        psi <- cbind((ystar-mod.atten$coeff[1]-gamma1*w), 
                     (ystar-mod.atten$coeff[1]-gamma1*w)*w,
                     (x-mu.x)*v,
                     ((x-mu.x)^2-var.x)*v,
                     (s-p.est)*v,
                     ((w-x)^2-var.u)*s*v,
                     ((ystar-y)*(w-x)-cov.est)*s*v)
        B <- t(psi)%*%psi/N
        var.theta <- solve(A)%*%B%*%solve(A)/N
        
        dg <- c(0, (var.x+p*var.u)/var.x, 0, -(gamma1*var.u*p.est-p.est*cov.est)/var.x^2, (gamma1*var.u-cov.est)/var.x, gamma1*p.est/var.x, -p.est/var.x)
        
        var.fixed <- dg%*%var.theta%*%dg 
    } else {
        var.fixed <- var.gamma1
    }
    
    return(list("CorrectEst"=fixed,"VarCorrectEst"=var.fixed))
}

#### notations are adopted from Shepherd and Yu (2011)
library(MASS)
NSIM = 100
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

fn_out = paste0("res/SY2011_n2_", n2, "_p_s_", p_s, "_rho_u_ustar_", rho_u_ustar, "/", njob, ".Rdata")

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
    sim_v = rep(0, n)
    sim_v[id_phase2] = 1
    
    res = audit.correct2(sim_ystar, sim_y, sim_w, sim_v, sim_x, sim_s, p_s)
    results[nsim,1] = res$CorrectEst
    results[nsim,2] = sqrt(res$VarCorrectEst)
	nsim = nsim+1
}

save(list=c("results"), file=fn_out)

