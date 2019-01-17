## adopted from http://biostat.mc.vanderbilt.edu/wiki/Main/DataAuditSimulationCode

## audit.correct2() takes information from an audit, and corrects naive estimates when there are errors in the
## predictor and outcome variables which are potentially correlated. This function employs the methods given in
## Section 2.3, and returns the estimate CorrectEst and its variance as outlined in Section 2.3.

audit.correct2 = function(ystar, y, w, x) {
    
    N = length(y)
    mod.atten = lm(ystar~w)
    gamma1 = mod.atten$coeff[2]
    var.gamma1 = summary(mod.atten)$coeff[2,2]^2
    
    var.w = var(w)
    mu.w = mean(w)
    
    v = as.numeric(!is.na(y))
    if (mean(v == as.numeric(!is.na(x))) != 1) {
        stop("Error: Missingness in y and x are not the same!")
    }
    s = rep(0, N)
    s[which(w!=x)] = 1
    x[v==0] = 0
    y[v==0] = 0
    
    p.est = mean(s[v==1])
    u.est = w[s*v==1]-x[s*v==1]
    var.u = ifelse(sum(s*v)==0, 0, ifelse(sum(s*v)==1, u.est^2, var(u.est)*((sum(s*v)-1)/sum(s*v))))
    var.x = var(x[v==1])*((sum(v)-1)/sum(v))
    mu.x = mean(x[v==1])
    
    resid.y = y[v*s==1]-ystar[v*s==1]
    resid.x = x[v*s==1]-w[v*s==1]
    cov.est = ifelse(sum(s*v)==0, 0, ifelse(sum(s*v)==1, resid.y*resid.x, sum(resid.y*resid.x)/(sum(s*v)-1)))
    mu.star = mean(resid.y)
    
    fixed = (gamma1*(var.x+var.u*p.est)-p.est*cov.est)/var.x
    
    check = sum(s*v)
    
    if (check > 0) {
        A = matrix(0,7,7)
        A[1,1] = 1
        A[1,2] = mean(w)
        A[2,1] = mean(w)
        A[2,2] = sum(w^2)/N
        A[3,3] = sum(v)/N
        A[4,3] = 2*sum(v*(x-mu.x))/N
        A[4,4] = sum(v)/N
        A[5,5] = sum(v)/N
        A[6,6] = sum(s*v)/N
        A[7,7] = sum(s*v)/N
        
        psi = cbind((ystar-mod.atten$coeff[1]-gamma1*w), (ystar-mod.atten$coeff[1]-gamma1*w)*w, (x-mu.x)*v, ((x-mu.x)^2-var.x)*v, (s-p.est)*v, ((w-x)^2-var.u)*s*v, ((ystar-y)*(w-x)-cov.est)*s*v)
        B = t(psi)%*%psi/N
        var.theta = solve(A)%*%B%*%solve(A)/N
        
        dg = c(0, (var.x+p.est*var.u)/var.x, 0, -(gamma1*var.u*p.est-p.est*cov.est)/var.x^2, (gamma1*var.u-cov.est)/var.x, gamma1*p.est/var.x, -p.est/var.x)
        
        var.fixed = dg%*%var.theta%*%dg 
    } else {
        var.fixed = var.gamma1
    }
    
    return(list("CorrectEst"=fixed, "VarCorrectEst"=var.fixed))
}
