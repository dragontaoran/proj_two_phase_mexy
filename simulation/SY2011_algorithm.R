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



audit.correct.cov = function(ystar, w, z, x, y) {
    
    N = length(ystar)
    v = as.numeric(!is.na(y))
    if (mean(v == as.numeric(!is.na(x))) != 1) {
        stop("Error: Missingness in y and x are not the same!")
    }
    nv = sum(v)
    
    if(!is.matrix(z)) z = matrix(z, ncol=1)
    if(!is.matrix(w)) w = matrix(w, ncol=1)
    if(!is.matrix(x)) x = matrix(x, ncol=1)
    
    dim.z = ncol(z)
    dim.w = ncol(w)
    
    t = matrix(NA, nrow=N, ncol=dim.w)
    for (j in 1:ncol(w)) { 
        t[,j] = ifelse(v==1, w[,j]-x[,j], NA)
    }
    tstar = ifelse(v==1, ystar-y, NA) ### In practice we can't differentiate between tstar1 and tstar2, but that doesn't matter.
    
    mu.w = apply(w, 2, mean)
    sigma.ww = cov(w, w)
    mu.z = apply(z, 2, mean)
    sigma.zz = cov(z, z) 
    sigma.wz = cov(w, z)
    sigma.tt = cov(t, t, use="complete.obs")
    mu.ystar = mean(ystar)
    sigma.wystar = cov(w, ystar)
    sigma.zystar = cov(z, ystar)
    sigma.ttstar = cov(t, tstar, use="complete.obs")
    
    psi.mu.ystar = ystar-mu.ystar
    
    psi.mu.w = matrix(NA, N, dim.w)
    junk = matrix(NA, nrow=N, ncol=(dim.w^2))
    junk1 = matrix(NA, nrow=N, ncol=dim.w^2)
    psi.sigma.wystar = matrix(NA, nrow=N, ncol=dim.w)
    for (j in 1:dim.w) {
        psi.mu.w[,j] = w[,j]-mu.w[j]
        psi.sigma.wystar[,j] = psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
    }
    for (j in 1:dim.w) {
        for (k in j:dim.w) {
            junk[,k+dim.w*(j-1)] = psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k] # Would sum to 0 if multiply by (N-1)/N. Similar elsewhere
            junk1[,k+dim.w*(j-1)] = ifelse(v==1, t[,j]*t[,k]-sigma.tt[j,k], 0)
        }
    }
    psi.sigma.ww = junk
    psi.sigma.tt = junk1
    if (dim.w >= 2) {
        psi.sigma.ww = junk[,-which(is.na(junk[1,]))]
        psi.sigma.tt = junk1[,-which(is.na(junk[1,]))]
    }
    
    psi.mu.z = matrix(NA, N, dim.z)
    junk = matrix(NA, nrow=N, ncol=(dim.z^2))
    psi.sigma.zystar = matrix(NA, nrow=N, ncol=dim.z)
    for (j in 1:dim.z) {
        psi.mu.z[,j] = z[,j]-mu.z[j]
        psi.sigma.zystar[,j] = psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
    }
    for (j in 1:dim.z) {
        for (k in j:dim.z) {
            junk[,k+dim.z*(j-1)] = psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
        }
    }
    psi.sigma.zz = junk
    if (dim.z >= 2) {
        psi.sigma.zz = junk[,-which(is.na(junk[1,]))]
    }
    
    psi.sigma.wz = matrix(NA, nrow=N, ncol=dim.w*dim.z)
    for (j in 1:dim.w) {
        for (k in 1:dim.z) {
            psi.sigma.wz[,k+dim.z*(j-1)] = psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
        }
    }
    psi.sigma.ttstar = matrix(0, nrow=N, ncol=dim.w) #### Typically, we only identify a single dimensional tstar=ystar-y.
    for (j in 1:dim.w) {
        psi.sigma.ttstar[,j] = ifelse(v==1, t[,j]*tstar-sigma.ttstar[j], 0)
    }
    
    psi = cbind(psi.mu.w,
                psi.sigma.ww,
                psi.mu.z,
                psi.sigma.zz,
                psi.sigma.wz,
                psi.sigma.tt,
                psi.mu.ystar,
                psi.sigma.wystar,
                psi.sigma.zystar,
                psi.sigma.ttstar)
    
    B = t(psi)%*%psi/N
    
    dim.A = dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1+dim.w+dim.z+dim.w
    
    A = matrix(0, dim.A, dim.A)
    for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z)) {
        A[j,j] = 1
    }
    for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w))) {
        A[j,j] = nv/N
    }
    for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1):(dim.A-dim.w)) {
        A[j,j] = 1
    }
    for (j in (dim.A-dim.w+1):dim.A) {
        A[j,j] = nv/N
    }
    
    var.theta = solve(A)%*%B%*%solve(A)/N
    
    var.theta.diag = diag(var.theta)
    
    C = matrix(NA, nrow=dim.w+dim.z, ncol=dim.w+dim.z)
    C[1:dim.w,1:dim.w] = sigma.ww-sigma.tt
    C[1:dim.w,(dim.w+1):(dim.w+dim.z)] = sigma.wz
    C[(dim.w+1):(dim.w+dim.z),1:dim.w] = t(sigma.wz)
    C[(dim.w+1):(dim.w+dim.z),(dim.w+1):(dim.w+dim.z)] = sigma.zz
    
    D = c(sigma.wystar-sigma.ttstar, sigma.zystar)
    
    C.inv = solve(C)
    
    est = C.inv%*%D
    
    dbeta.dmu.w = matrix(0, nrow=dim.w+dim.z, ncol=dim.w)
    
    dbeta.dsigma.ww = dbeta.dsigma.tt = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.ww)[2])
    for (j in 1:dim(psi.sigma.ww)[2]) {
        junk = rep(0, dim(psi.sigma.ww)[2])
        junk[j] = 1
        Dmat = Dmat1 = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[1,1:dim.w] = junk[1:dim.w]
        if (dim.w >= 2) {
            Dmat[2,2:dim.w] = junk[(dim.w+1):(2*dim.w-1)]
            if (dim.w >= 3) {
                Dmat[3,3:dim.w] = junk[(2*dim.w):(3*dim.w-3)]
                if (dim.w >= 4) {
                    Dmat[4,4:dim.w] = junk[(3*dim.w-2):(4*dim.w-6)]
                    if (dim.w >= 5) {
                        Dmat[5,5:dim.w] = junk[(4*dim.w-5):(5*dim.w-10)]
                        if (dim.w == 6) {
                            Dmat[6,6:dim.w] = junk[(5*dim.w-9):(6*dim.w-13)]
                        }
                    }
                }
            }
        }
        if (sum(diag(Dmat)) == 0) {
            Dmat = Dmat+t(Dmat)
        }
        dbeta.dsigma.ww[,j] = -C.inv%*%Dmat%*%C.inv%*%D
        dbeta.dsigma.tt[,j] = -dbeta.dsigma.ww[,j]
    }
    
    dbeta.dmu.z = matrix(0, nrow=dim.w+dim.z, ncol=dim.z)
    
    dbeta.dsigma.zz = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.zz)[2])
    for (j in 1:dim(psi.sigma.zz)[2]) {
        junk = rep(0, dim(psi.sigma.zz)[2])
        junk[j] = 1
        Dmat = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)] = junk[1:dim.z]
        if (dim.z >= 2){
            Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)] = junk[(dim.z+1):(2*dim.z-1)]
            if (dim.z >= 3) {
                Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)] = junk[(2*dim.z):(3*dim.z-3)]
                if (dim.z >= 4) {
                    Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)] = junk[(3*dim.z-2):(4*dim.z-6)]
                    if (dim.z >= 5) {
                        Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)] = junk[(4*dim.z-5):(5*dim.z-10)]
                        if (dim.z == 6) {
                            Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)] = junk[(5*dim.z-9):(6*dim.z-13)]
                        }
                    }
                }
            }
        }
        if (sum(diag(Dmat)) == 0) {
            Dmat = Dmat+t(Dmat)
        }
        dbeta.dsigma.zz[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    
    dbeta.dsigma.wz = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.wz)[2])
    for (j in 1:dim(psi.sigma.wz)[2]) {
        junk = rep(0, dim(psi.sigma.wz)[2])
        junk[j] = 1
        Dmat12 = matrix(junk, nrow=dim.w, ncol=dim.z, byrow=TRUE)
        Dmat21 = t(Dmat12)
        Dmat = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)] = Dmat12
        Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w] = Dmat21
        dbeta.dsigma.wz[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    
    dbeta.dmu.ystar = matrix(0, nrow=dim.w+dim.z, ncol=1)
    
    stuff = matrix(0, nrow=dim.w+dim.z, ncol=dim.w+dim.z)
    diag(stuff) = 1
    dbeta.dsigma.wzystar = C.inv%*%stuff
    
    stuff = matrix(0, nrow=dim.w+dim.z, ncol=dim.w)
    diag(stuff) = -1
    dbeta.dsigma.ttstar = C.inv%*%stuff
    
    dg.theta = cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt, 
                     dbeta.dmu.ystar, dbeta.dsigma.wzystar, dbeta.dsigma.ttstar)
    
    var.est = dg.theta%*%var.theta%*%t(dg.theta)
    
    return(list("CorrectEst"=est, "VarCorrectEst"=var.est))
    
}



#  p.cat = Probability of being in a particular category (if there are 3 categories,  this is a vector of length 3)
#  cat = Category an observation is classified into
#  poss.cat = unique categories (same length and order as p.cat)
audit.correct.cov.vdependent = function(ystar, w, z, x, y, p.cat, cat, poss.cat) {
    
    N = length(ystar)
    v = as.numeric(!is.na(y))
    if (mean(v == as.numeric(!is.na(x))) != 1) {
        stop("Error: Missingness in y and x are not the same!")
    }
    nv = sum(v)
    
    if(!is.matrix(z)) z = matrix(z, ncol=1)
    if(!is.matrix(w)) w = matrix(w, ncol=1)
    if(!is.matrix(x)) x = matrix(x, ncol=1)
    
    dim.z = ncol(z)
    dim.w = ncol(w)
    
    t = matrix(NA, nrow=N, ncol=dim.w)
    for (j in 1:ncol(w)) { 
        t[,j] = ifelse(v==1, w[,j]-x[,j], NA)
    }
    tstar = ifelse(v==1, ystar-y, NA)         ### In practice we can't differentiate between tstar1 and tstar2, but that doesn't matter.
    
    mu.w = apply(w, 2, mean)
    sigma.ww = cov(w, w)
    mu.z = apply(z, 2, mean)
    sigma.zz = cov(z, z) 
    sigma.wz = cov(w, z)
    sigma.tt = NULL                                  ####  Right now this only works when X is composed of 1 variable
    for (j in 1:length(p.cat)) {
        sigma.tt[j] = cov(t[cat==poss.cat[j]], t[cat==poss.cat[j]], use="complete.obs")
    }
    mu.ystar = mean(ystar)
    sigma.wystar = cov(w, ystar)
    sigma.zystar = cov(z, ystar)
    sigma.ttstar = NULL
    for (j in 1:length(p.cat)) {
        sigma.ttstar[j] = cov(t[cat==poss.cat[j]], tstar[cat==poss.cat[j]], use="complete.obs")
    }
    psi.mu.ystar = ystar-mu.ystar
    
    psi.mu.w = matrix(NA, N, dim.w)
    junk = matrix(NA, nrow=N, ncol=(dim.w^2))
    psi.sigma.wystar = matrix(NA, nrow=N, ncol=dim.w)
    for (j in 1:dim.w) {
        psi.mu.w[,j] = w[,j]-mu.w[j]
        psi.sigma.wystar[,j] = psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
    }
    for (j in 1:dim.w) {
        for (k in j:dim.w) {
            junk[,k+dim.w*(j-1)] = psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k]   #  Would sum to 0 if multiply by (N-1)/N.  Similar elsewhere
        }
    }
    psi.sigma.ww = junk
    if (dim.w >= 2) {
        psi.sigma.ww = junk[,-which(is.na(junk[1,]))]
    }
    junk1 = matrix(NA, nrow=N, ncol=length(p.cat))
    for (j in 1:length(p.cat)) {
        junk1[,j] = ifelse(v==1&cat==poss.cat[j], t*t-sigma.tt[j], 0)
    }
    psi.sigma.tt = junk1
    
    psi.mu.z = matrix(NA, N, dim.z)
    junk = matrix(NA, nrow=N, ncol=(dim.z^2))
    psi.sigma.zystar = matrix(NA, nrow=N, ncol=dim.z)
    for (j in 1:dim.z) {
        psi.mu.z[,j] = z[,j]-mu.z[j]
        psi.sigma.zystar[,j] = psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
    }
    for (j in 1:dim.z) {
        for (k in j:dim.z) {
            junk[,k+dim.z*(j-1)] = psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
        }
    }
    psi.sigma.zz = junk
    if (dim.z >= 2) {
        psi.sigma.zz = junk[,-which(is.na(junk[1,]))]
    }
    
    psi.sigma.wz = matrix(NA, nrow=N, ncol=dim.w*dim.z)
    for (j in 1:dim.w) {
        for (k in 1:dim.z) {
            psi.sigma.wz[,k+dim.z*(j-1)] = psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
        }
    }

    psi.sigma.ttstar = matrix(0, nrow=N, ncol=length(p.cat))    ### Currently only works for 1 variable in X
    for (j in 1:length(p.cat)) {
        psi.sigma.ttstar[,j] = ifelse(v==1&cat==poss.cat[j], t*tstar-sigma.ttstar[j], 0)
    }
    
    psi = cbind(psi.mu.w,
                psi.sigma.ww,
                psi.mu.z,
                psi.sigma.zz,
                psi.sigma.wz,
                psi.sigma.tt,
                psi.mu.ystar,
                psi.sigma.wystar,
                psi.sigma.zystar,
                psi.sigma.ttstar)
    
    B = t(psi)%*%psi/N
    
    dim.A = dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1+dim.w+dim.z+length(p.cat)
    
    A = matrix(0, dim.A, dim.A)
    for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z) ) {
        A[j,j] = 1
    }
    for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat))) {
        A[j,j] = sum(cat==poss.cat[j-(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z)]&v==1)/N
    }
    for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1):(dim.A-length(p.cat))) {
        A[j,j] = 1
    }
    for (j in (dim.A-length(p.cat)+1):dim.A) {
        A[j,j] = sum(cat==poss.cat[j-(dim.A-length(p.cat))]&v==1)/N
    }
    
    var.theta = solve(A)%*%B%*%solve(A)/N
    
    var.theta.diag = diag(var.theta)
    
    C = matrix(NA, nrow=dim.w+dim.z, ncol=dim.w+dim.z)
    C[1:dim.w, 1:dim.w] = sigma.ww-sigma.tt%*%p.cat             ####  Only works with 1 variable in X
    C[1:dim.w, (dim.w+1):(dim.w+dim.z)] = sigma.wz
    C[(dim.w+1):(dim.w+dim.z), 1:dim.w] = t(sigma.wz)
    C[(dim.w+1):(dim.w+dim.z), (dim.w+1):(dim.w+dim.z)] = sigma.zz
    
    D = c(sigma.wystar-sigma.ttstar%*%p.cat, sigma.zystar)
    
    C.inv = solve(C)
    
    est = C.inv%*%D
    C.inv%*%D
    
    dbeta.dmu.w = matrix(0, nrow=dim.w+dim.z, ncol=dim.w)
    
    dbeta.dsigma.ww = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.ww)[2])
    for (j in 1:dim(psi.sigma.ww)[2]) {
        junk = rep(0, dim(psi.sigma.ww)[2])
        junk[j] = 1
        Dmat = Dmat1 = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[1,1:dim.w] = junk[1:dim.w]
        if (dim.w >= 2){
            Dmat[2,2:dim.w] = junk[(dim.w+1):(2*dim.w-1)]
            if (dim.w>=3) {
                Dmat[3,3:dim.w] = junk[(2*dim.w):(3*dim.w-3)]
                if (dim.w>=4) {
                    Dmat[4,4:dim.w] = junk[(3*dim.w-2):(4*dim.w-6)]
                    if (dim.w>=5) {
                        Dmat[5,5:dim.w] = junk[(4*dim.w-5):(5*dim.w-10)]
                        if (dim.w==6) {
                            Dmat[6,6:dim.w] = junk[(5*dim.w-9):(6*dim.w-15)]
                        }
                    }
                }
            }
        }
        if (sum(diag(Dmat)) == 0)  {
            Dmat = Dmat+t(Dmat)
        }
        dbeta.dsigma.ww[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    dbeta.dsigma.tt = matrix(NA, nrow=dim.w+dim.z, ncol=length(p.cat))       #### Doesn't work if more than one variable in X
    for (j in 1:length(p.cat)) {
        Dmat = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[1,1] = -p.cat[j]
        dbeta.dsigma.tt[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    
    dbeta.dmu.z = matrix(0, nrow=dim.w+dim.z, ncol=dim.z)
    
    dbeta.dsigma.zz = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.zz)[2])
    for (j in 1:dim(psi.sigma.zz)[2]) {
        junk = rep(0, dim(psi.sigma.zz)[2])
        junk[j] = 1
        Dmat = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)] = junk[1:dim.z]
        if (dim.z >= 2){
            Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)] = junk[(dim.z+1):(2*dim.z-1)]
            if (dim.z >= 3) {
                Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)] = junk[(2*dim.z):(3*dim.z-3)]
                if (dim.z >= 4) {
                    Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)] = junk[(3*dim.z-2):(4*dim.z-6)]
                    if (dim.z >= 5) {
                        Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)] = junk[(4*dim.z-5):(5*dim.z-10)]
                        if (dim.z == 6) {
                            Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)] = junk[(5*dim.z-9):(6*dim.z-15)]
                        }
                    }
                }
            }
        }
        if (sum(diag(Dmat)) == 0)  {
            Dmat = Dmat+t(Dmat)
        }
        dbeta.dsigma.zz[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    
    dbeta.dsigma.wz = matrix(NA, nrow=dim.w+dim.z, ncol=dim(psi.sigma.wz)[2])
    for (j in 1:dim(psi.sigma.wz)[2]) {
        junk = rep(0, dim(psi.sigma.wz)[2])
        junk[j] = 1
        Dmat12 = matrix(junk, nrow=dim.w, ncol=dim.z, byrow=TRUE)
        Dmat21 = t(Dmat12)
        Dmat = matrix(0, dim.w+dim.z, dim.w+dim.z)
        Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)] = Dmat12
        Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w] = Dmat21
        dbeta.dsigma.wz[,j] = -C.inv%*%Dmat%*%C.inv%*%D
    }
    
    dbeta.dmu.ystar = matrix(0, nrow=dim.w+dim.z, ncol=1)
    
    stuff = matrix(0, nrow=dim.w+dim.z, ncol=dim.w+dim.z)
    diag(stuff) = 1
    dbeta.dsigma.wzystar = C.inv%*%stuff
    
    stuff = matrix(0, nrow=dim.w+dim.z, ncol=length(p.cat))
    for (j in 1:length(p.cat)){
        stuff[1,j] = -p.cat[j]
    }
    dbeta.dsigma.ttstar = C.inv%*%stuff
    
    
    dg.theta = cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt,
                    dbeta.dmu.ystar, dbeta.dsigma.wzystar, dbeta.dsigma.ttstar)
    
    var.est = dg.theta%*%var.theta%*%t(dg.theta)
    
    return(list("CorrectEst"=est, "VarCorrectEst"=var.est))
}
