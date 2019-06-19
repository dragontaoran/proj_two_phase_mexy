
audit.correct.cov<-function(ystar,w,z,v,x,y) {

  N<-length(ystar)
  nv<-sum(v)

  if(!is.matrix(z)) z = matrix(z,ncol=1)
  if(!is.matrix(w)) w = matrix(w,ncol=1)
  if(!is.matrix(x)) x = matrix(x,ncol=1)
    
  dim.z<-ncol(z)
  dim.w<-ncol(w)
  
  t<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:ncol(w)) { 
    t[,j]<-ifelse(v==1,w[,j]-x[,j],NA)
  }
  tstar<-ifelse(v==1,ystar-y,NA)         ### In practice we can't differentiate between tstar1 and tstar2, but that doesn't matter.
  
  mu.w<-apply(w,2,mean)
  sigma.ww<-cov(w,w)
  mu.z<-apply(z,2,mean)
  sigma.zz<-cov(z,z) 
  sigma.wz<-cov(w,z)
  sigma.tt<-cov(t,t,use="complete.obs")
  mu.ystar<-mean(ystar)
  sigma.wystar<-cov(w,ystar)
  sigma.zystar<-cov(z,ystar)
  sigma.ttstar<-cov(t,tstar,use="complete.obs")


  psi.mu.ystar<-ystar-mu.ystar

  psi.mu.w<-matrix(NA,N,dim.w)
  junk<-matrix(NA,nrow=N,ncol=(dim.w^2))
  junk1<-matrix(NA,nrow=N,ncol=dim.w^2)
  psi.sigma.wystar<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:dim.w) {
    psi.mu.w[,j]<-w[,j]-mu.w[j]
    psi.sigma.wystar[,j]<-psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
  }
  for (j in 1:dim.w) {
    for (k in j:dim.w) {
      junk[,k+dim.w*(j-1)]<-psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k]   #  Would sum to 0 if multiply by (N-1)/N.  Similar elsewhere
      junk1[,k+dim.w*(j-1)]<-ifelse(v==1,t[,j]*t[,k]-sigma.tt[j,k],0)
    }
  }
  psi.sigma.ww<-junk
  psi.sigma.tt<-junk1
  if (dim.w>=2) {
    psi.sigma.ww<-junk[,-which(is.na(junk[1,]))]
    psi.sigma.tt<-junk1[,-which(is.na(junk[1,]))]
  }

  psi.mu.z<-matrix(NA,N,dim.z)
  junk<-matrix(NA,nrow=N,ncol=(dim.z^2))
  psi.sigma.zystar<-matrix(NA,nrow=N,ncol=dim.z)
  for (j in 1:dim.z) {
    psi.mu.z[,j]<-z[,j]-mu.z[j]
    psi.sigma.zystar[,j]<-psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
  }
  for (j in 1:dim.z) {
    for (k in j:dim.z) {
      junk[,k+dim.z*(j-1)]<-psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
    }
  }
  psi.sigma.zz<-junk
  if (dim.z>=2) {
    psi.sigma.zz<-junk[,-which(is.na(junk[1,]))]
  }

  psi.sigma.wz<-matrix(NA,nrow=N,ncol=dim.w*dim.z)
  for (j in 1:dim.w) {
    for (k in 1:dim.z) {
      psi.sigma.wz[,k+dim.z*(j-1)]<-psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
    }
  }
  psi.sigma.ttstar<-matrix(0,nrow=N,ncol=dim.w)           #### Typically, we only identify a single dimensional tstar=ystar-y.
  for (j in 1:dim.w) {
    psi.sigma.ttstar[,j]<-ifelse(v==1,t[,j]*tstar-sigma.ttstar[j],0)
#    for (k in 1:dim.w) {
#       psi.sigma.ttstar[,k+dim.w*(j-1)]<-v*(s[,j]*u[,j]*s[,k]*ustar[,k]-sigma.ttstar[j,k])
#    }
  }


  psi<-cbind( psi.mu.w,
             psi.sigma.ww,
             psi.mu.z,
             psi.sigma.zz,
             psi.sigma.wz,
             psi.sigma.tt,
             psi.mu.ystar,
             psi.sigma.wystar,
             psi.sigma.zystar,
             psi.sigma.ttstar)

  B<-t(psi)%*%psi/N

  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1+dim.w+dim.z+dim.w

  A<-matrix(0,dim.A,dim.A)
  for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z) ) {
    A[j,j]<-1
  }
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w))) {
    A[j,j]<-nv/N
  }
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1):(dim.A-dim.w)) {
    A[j,j]<-1
  }
  for (j in (dim.A-dim.w+1):dim.A) {
    A[j,j]<-nv/N
  }

  var.theta<-solve(A)%*%B%*%solve(A)/N

  var.theta.diag<-diag(var.theta)

  C<-matrix(NA,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  C[1:dim.w,1:dim.w]<-sigma.ww-sigma.tt
  C[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-sigma.wz
  C[(dim.w+1):(dim.w+dim.z),1:dim.w]<-t(sigma.wz)
  C[(dim.w+1):(dim.w+dim.z),(dim.w+1):(dim.w+dim.z)]<-sigma.zz

  D<-c(sigma.wystar-sigma.ttstar,sigma.zystar)

  C.inv<-solve(C)

  est<-C.inv%*%D
  C.inv%*%D

  dbeta.dmu.w<- matrix(0,nrow=dim.w+dim.z,ncol=dim.w)

  dbeta.dsigma.ww<-dbeta.dsigma.tt<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.ww)[2])
  for (j in 1:dim(psi.sigma.ww)[2]) {
    junk<-rep(0,dim(psi.sigma.ww)[2])
    junk[j]<-1
    Dmat<-Dmat1<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1:dim.w]<-junk[1:dim.w]
    if (dim.w>=2){
      Dmat[2,2:dim.w]<-junk[(dim.w+1):(2*dim.w-1)]
      if (dim.w>=3) {
        Dmat[3,3:dim.w]<-junk[(2*dim.w):(3*dim.w-3)]
        if (dim.w>=4) {
          Dmat[4,4:dim.w]<-junk[(3*dim.w-2):(4*dim.w-6)]
          if (dim.w>=5) {
            Dmat[5,5:dim.w]<-junk[(4*dim.w-5):(5*dim.w-10)]
            if (dim.w==6) {
              Dmat[6,6:dim.w]<-junk[(5*dim.w-9):(6*dim.w-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.ww[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
    dbeta.dsigma.tt[,j]<- - dbeta.dsigma.ww[,j]
  }

  dbeta.dmu.z<- matrix(0,nrow=dim.w+dim.z,ncol=dim.z)

  dbeta.dsigma.zz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.zz)[2])
  for (j in 1:dim(psi.sigma.zz)[2]) {
    junk<-rep(0,dim(psi.sigma.zz)[2])
    junk[j]<-1
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)]<-junk[1:dim.z]
    if (dim.z>=2){
      Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)]<-junk[(dim.z+1):(2*dim.z-1)]
      if (dim.z>=3) {
        Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)]<-junk[(2*dim.z):(3*dim.z-3)]
        if (dim.z>=4) {
          Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)]<-junk[(3*dim.z-2):(4*dim.z-6)]
          if (dim.z>=5) {
            Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)]<-junk[(4*dim.z-5):(5*dim.z-10)]
            if (dim.z==6) {
              Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)]<-junk[(5*dim.z-9):(6*dim.z-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.zz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dsigma.wz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.wz)[2])
  for (j in 1:dim(psi.sigma.wz)[2]) {
    junk<-rep(0,dim(psi.sigma.wz)[2])
    junk[j]<-1
    Dmat12<-matrix(junk,nrow=dim.w,ncol=dim.z,byrow=TRUE)
    Dmat21<-t(Dmat12)
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-Dmat12
    Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w]<-Dmat21
    dbeta.dsigma.wz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dmu.ystar<- matrix(0,nrow=dim.w+dim.z,ncol=1)

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  diag(stuff)<-1
  dbeta.dsigma.wzystar<- C.inv%*%stuff

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w)
  diag(stuff)<- -1
  dbeta.dsigma.ttstar<- C.inv%*%stuff

  dg.theta<-cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt,
                  dbeta.dmu.ystar, dbeta.dsigma.wzystar, dbeta.dsigma.ttstar)

  var.est<-dg.theta%*%var.theta%*%t(dg.theta)
  
  return(list("CorrectEst"=est,"VarCorrectEst"=var.est))

}








#  p.cat = Probability of being in a particular category (if there are 3 categories, this is a vector of length 3)
#  cat = Category an observation is classified into
#  poss.cat = unique categories (same length and order as p.cat)

audit.correct.cov.vdependent<-function(ystar,w,z,v,x,y,p.cat,cat,poss.cat) {

  N<-length(ystar)
  nv<-sum(v)

  if(!is.matrix(z)) z = matrix(z,ncol=1)
  if(!is.matrix(w)) w = matrix(w,ncol=1)
  if(!is.matrix(x)) x = matrix(x,ncol=1)
    
  dim.z<-ncol(z)
  dim.w<-ncol(w)
  
  t<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:ncol(w)) { 
    t[,j]<-ifelse(v==1,w[,j]-x[,j],NA)
  }
  tstar<-ifelse(v==1,ystar-y,NA)         ### In practice we can't differentiate between tstar1 and tstar2, but that doesn't matter.

  mu.w<-apply(w,2,mean)
  sigma.ww<-cov(w,w)
  mu.z<-apply(z,2,mean)
  sigma.zz<-cov(z,z) 
  sigma.wz<-cov(w,z)
  sigma.tt<-NULL                                  ####  Right now this only works when X is composed of 1 variable
  for (j in 1:length(p.cat)) {
    sigma.tt[j]<-cov(t[cat==poss.cat[j]],t[cat==poss.cat[j]],use="complete.obs")
  }
  mu.ystar<-mean(ystar)
  sigma.wystar<-cov(w,ystar)
  sigma.zystar<-cov(z,ystar)
  sigma.ttstar<-NULL
  for (j in 1:length(p.cat)) {
    sigma.ttstar[j]<-cov(t[cat==poss.cat[j]],tstar[cat==poss.cat[j]],use="complete.obs")
  }
  psi.mu.ystar<-ystar-mu.ystar

  psi.mu.w<-matrix(NA,N,dim.w)
  junk<-matrix(NA,nrow=N,ncol=(dim.w^2))
#  junk1<-matrix(NA,nrow=N,ncol=dim.w^2)
  psi.sigma.wystar<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:dim.w) {
    psi.mu.w[,j]<-w[,j]-mu.w[j]
    psi.sigma.wystar[,j]<-psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
  }
  for (j in 1:dim.w) {
    for (k in j:dim.w) {
      junk[,k+dim.w*(j-1)]<-psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k]   #  Would sum to 0 if multiply by (N-1)/N.  Similar elsewhere
#      junk1[,k+dim.w*(j-1)]<-ifelse(v==1,t[,j]*t[,k]-sigma.tt[j,k],0)
    }
  }
  psi.sigma.ww<-junk
#  psi.sigma.tt<-junk1                                       #### Currently only works for 1 variable in X
  if (dim.w>=2) {
    psi.sigma.ww<-junk[,-which(is.na(junk[1,]))]
#    psi.sigma.tt<-junk1[,-which(is.na(junk[1,]))]
  }
  junk1<-matrix(NA,nrow=N,ncol=length(p.cat))
  for (j in 1:length(p.cat)) {
    junk1[,j]<-ifelse(v==1&cat==poss.cat[j],t*t-sigma.tt[j],0)
  }
  psi.sigma.tt<-junk1

  psi.mu.z<-matrix(NA,N,dim.z)
  junk<-matrix(NA,nrow=N,ncol=(dim.z^2))
  psi.sigma.zystar<-matrix(NA,nrow=N,ncol=dim.z)
  for (j in 1:dim.z) {
    psi.mu.z[,j]<-z[,j]-mu.z[j]
    psi.sigma.zystar[,j]<-psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
  }
  for (j in 1:dim.z) {
    for (k in j:dim.z) {
      junk[,k+dim.z*(j-1)]<-psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
    }
  }
  psi.sigma.zz<-junk
  if (dim.z>=2) {
    psi.sigma.zz<-junk[,-which(is.na(junk[1,]))]
  }

  psi.sigma.wz<-matrix(NA,nrow=N,ncol=dim.w*dim.z)
  for (j in 1:dim.w) {
    for (k in 1:dim.z) {
      psi.sigma.wz[,k+dim.z*(j-1)]<-psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
    }
  }
#  psi.sigma.ttstar<-matrix(0,nrow=N,ncol=dim.w)           #### Typically, we only identify a single dimensional tstar=ystar-y.
#  for (j in 1:dim.w) {
#    psi.sigma.ttstar[,j]<-ifelse(v==1,t[,j]*tstar-sigma.ttstar[j],0)
#  }
  psi.sigma.ttstar<-matrix(0,nrow=N,ncol=length(p.cat))    ### Currently only works for 1 variable in X
  for (j in 1:length(p.cat)) {
    psi.sigma.ttstar[,j]<-ifelse(v==1&cat==poss.cat[j],t*tstar-sigma.ttstar[j],0)
  }

  psi<-cbind( psi.mu.w,
             psi.sigma.ww,
             psi.mu.z,
             psi.sigma.zz,
             psi.sigma.wz,
             psi.sigma.tt,
             psi.mu.ystar,
             psi.sigma.wystar,
             psi.sigma.zystar,
             psi.sigma.ttstar)

  B<-t(psi)%*%psi/N

#  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1+dim.w+dim.z+dim.w    ### only works for single X
  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1+dim.w+dim.z+length(p.cat)
  
  A<-matrix(0,dim.A,dim.A)
  for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z) ) {
    A[j,j]<-1
  }
#  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w))) {
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat))) {
#    A[j,j]<-nv/N
    A[j,j]<-sum(cat==poss.cat[j-(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z)]&v==1)/N
  }
#  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1):(dim.A-dim.w)) {
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1):(dim.A-length(p.cat))) {
    A[j,j]<-1
  }
  for (j in (dim.A-length(p.cat)+1):dim.A) {
#  for (j in (dim.A-dim.w+1):dim.A) {
#    A[j,j]<-nv/N
    A[j,j]<-sum(cat==poss.cat[j-(dim.A-length(p.cat))]&v==1)/N
  }

  var.theta<-solve(A)%*%B%*%solve(A)/N

  var.theta.diag<-diag(var.theta)

  C<-matrix(NA,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  C[1:dim.w,1:dim.w]<-sigma.ww-sigma.tt%*%p.cat             ####  Only works with 1 variable in X
  C[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-sigma.wz
  C[(dim.w+1):(dim.w+dim.z),1:dim.w]<-t(sigma.wz)
  C[(dim.w+1):(dim.w+dim.z),(dim.w+1):(dim.w+dim.z)]<-sigma.zz

  D<-c(sigma.wystar-sigma.ttstar%*%p.cat,sigma.zystar)

  C.inv<-solve(C)

  est<-C.inv%*%D
  C.inv%*%D

  dbeta.dmu.w<- matrix(0,nrow=dim.w+dim.z,ncol=dim.w)

  dbeta.dsigma.ww<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.ww)[2])
  for (j in 1:dim(psi.sigma.ww)[2]) {
    junk<-rep(0,dim(psi.sigma.ww)[2])
    junk[j]<-1
    Dmat<-Dmat1<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1:dim.w]<-junk[1:dim.w]
    if (dim.w>=2){
      Dmat[2,2:dim.w]<-junk[(dim.w+1):(2*dim.w-1)]
      if (dim.w>=3) {
        Dmat[3,3:dim.w]<-junk[(2*dim.w):(3*dim.w-3)]
        if (dim.w>=4) {
          Dmat[4,4:dim.w]<-junk[(3*dim.w-2):(4*dim.w-6)]
          if (dim.w>=5) {
            Dmat[5,5:dim.w]<-junk[(4*dim.w-5):(5*dim.w-10)]
            if (dim.w==6) {
              Dmat[6,6:dim.w]<-junk[(5*dim.w-9):(6*dim.w-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.ww[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
#    dbeta.dsigma.tt[,j]<- - dbeta.dsigma.ww[,j]
  }
  dbeta.dsigma.tt<-matrix(NA,nrow=dim.w+dim.z,ncol=length(p.cat))       #### Doesn't work if more than one variable in X
  for (j in 1:length(p.cat)) {
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1]<- -p.cat[j]
    dbeta.dsigma.tt[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }
  
  dbeta.dmu.z<- matrix(0,nrow=dim.w+dim.z,ncol=dim.z)

  dbeta.dsigma.zz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.zz)[2])
  for (j in 1:dim(psi.sigma.zz)[2]) {
    junk<-rep(0,dim(psi.sigma.zz)[2])
    junk[j]<-1
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)]<-junk[1:dim.z]
    if (dim.z>=2){
      Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)]<-junk[(dim.z+1):(2*dim.z-1)]
      if (dim.z>=3) {
        Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)]<-junk[(2*dim.z):(3*dim.z-3)]
        if (dim.z>=4) {
          Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)]<-junk[(3*dim.z-2):(4*dim.z-6)]
          if (dim.z>=5) {
            Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)]<-junk[(4*dim.z-5):(5*dim.z-10)]
            if (dim.z==6) {
              Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)]<-junk[(5*dim.z-9):(6*dim.z-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.zz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dsigma.wz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.wz)[2])
  for (j in 1:dim(psi.sigma.wz)[2]) {
    junk<-rep(0,dim(psi.sigma.wz)[2])
    junk[j]<-1
    Dmat12<-matrix(junk,nrow=dim.w,ncol=dim.z,byrow=TRUE)
    Dmat21<-t(Dmat12)
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-Dmat12
    Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w]<-Dmat21
    dbeta.dsigma.wz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dmu.ystar<- matrix(0,nrow=dim.w+dim.z,ncol=1)

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  diag(stuff)<-1
  dbeta.dsigma.wzystar<- C.inv%*%stuff

#  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w)
#  diag(stuff)<- -1
  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=length(p.cat))
  for (j in 1:length(p.cat)){
    stuff[1,j]<- -p.cat[j]
  }
  dbeta.dsigma.ttstar<- C.inv%*%stuff


  dg.theta<-cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt,
                  dbeta.dmu.ystar, dbeta.dsigma.wzystar, dbeta.dsigma.ttstar)

  var.est<-dg.theta%*%var.theta%*%t(dg.theta)
  
  return(list("CorrectEst"=est,"VarCorrectEst"=var.est))

}











########################  Trying to extend to more than 1 variable in X


#  p.cat = Probability of being in a particular category (if there are 3 categories, this is a vector of length 3)
#  cat = Category an observation is classified into
#  poss.cat = unique categories (same length and order as p.cat)

audit.correct.cov.vdependent.2plus<-function(ystar,w,z,v,x,y,p.cat,cat,poss.cat) {

  N<-length(ystar)
  nv<-sum(v)

  if(!is.matrix(z)) z = matrix(z,ncol=1)
  if(!is.matrix(w)) w = matrix(w,ncol=1)
  if(!is.matrix(x)) x = matrix(x,ncol=1)
    
  dim.z<-ncol(z)
  dim.w<-ncol(w)
  
  t<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:ncol(w)) { 
    t[,j]<-ifelse(v==1,w[,j]-x[,j],NA)
  }
  tstar<-ifelse(v==1,ystar-y,NA)       

  mu.w<-apply(w,2,mean)
  sigma.ww<-cov(w,w)
  mu.z<-apply(z,2,mean)
  sigma.zz<-cov(z,z) 
  sigma.wz<-cov(w,z)
  sigma.tt<-array(NA,dim=c(dim.w,dim.w,length(p.cat)))
  for (j in 1:length(p.cat)) {
    sigma.tt[,,j]<-cov(t[cat==poss.cat[j],],t[cat==poss.cat[j],],use="complete.obs")
  }
  mu.ystar<-mean(ystar)
  sigma.wystar<-cov(w,ystar)
  sigma.zystar<-cov(z,ystar)
  sigma.ttstar<-array(NA,dim=c(dim.w,1,length(p.cat)))  #cov(t,tstar,use="complete.obs")
  for (j in 1:length(p.cat)) {
    sigma.ttstar[,,j]<-cov(t[cat==poss.cat[j],],tstar[cat==poss.cat[j]],use="complete.obs")
  }
  
  psi.mu.ystar<-ystar-mu.ystar

  psi.mu.w<-matrix(NA,N,dim.w)
  junk<-matrix(NA,nrow=N,ncol=(dim.w^2))
  junk1<-array(NA,dim=c(N,dim.w^2,length(p.cat)))
  psi.sigma.wystar<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:dim.w) {
    psi.mu.w[,j]<-w[,j]-mu.w[j]
    psi.sigma.wystar[,j]<-psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
  }
  for (j in 1:dim.w) {
    for (k in j:dim.w) {
      junk[,k+dim.w*(j-1)]<-psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k]
      for (kk in 1:length(p.cat)) {
        junk1[,k+dim.w*(j-1),kk]<-ifelse(v==1&cat==poss.cat[kk],t[,j]*t[,k]-sigma.tt[j,k,kk],0)
      }
    }
  }
  psi.sigma.ww<-junk
  junk2<-junk1                                  
  if (dim.w>=2) {
    psi.sigma.ww<-junk[,-which(is.na(junk[1,]))]
    junk2<-junk1[,-which(is.na(junk1[1,,1])),]
  }
  psi.sigma.tt<-junk2[,,1]
  for (j in 2:length(p.cat)){
    psi.sigma.tt<-cbind(psi.sigma.tt,junk2[,,j])
  }

  psi.mu.z<-matrix(NA,N,dim.z)
  junk<-matrix(NA,nrow=N,ncol=(dim.z^2))
  psi.sigma.zystar<-matrix(NA,nrow=N,ncol=dim.z)
  for (j in 1:dim.z) {
    psi.mu.z[,j]<-z[,j]-mu.z[j]
    psi.sigma.zystar[,j]<-psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
  }
  for (j in 1:dim.z) {
    for (k in j:dim.z) {
      junk[,k+dim.z*(j-1)]<-psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
    }
  }
  psi.sigma.zz<-junk
  if (dim.z>=2) {
    psi.sigma.zz<-junk[,-which(is.na(junk[1,]))]
  }

  psi.sigma.wz<-matrix(NA,nrow=N,ncol=dim.w*dim.z)
  for (j in 1:dim.w) {
    for (k in 1:dim.z) {
      psi.sigma.wz[,k+dim.z*(j-1)]<-psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
    }
  }
  junk<-array(0,dim=c(N,dim.w,length(p.cat)))
  for (j in 1:dim.w) {
    for (k in 1:length(p.cat)) {
      junk[,j,k]<-ifelse(v==1&cat==poss.cat[k],t[,j]*tstar-sigma.ttstar[j,1,k],0)
    }
  }
  psi.sigma.ttstar<-junk[,,1]
  for (j in 2:length(p.cat)){
    psi.sigma.ttstar<-cbind(psi.sigma.ttstar,junk[,,j])
  }

  psi<-cbind( psi.mu.w,
             psi.sigma.ww,
             psi.mu.z,
             psi.sigma.zz,
             psi.sigma.wz,
             psi.sigma.tt,
             psi.mu.ystar,
             psi.sigma.wystar,
             psi.sigma.zystar,
             psi.sigma.ttstar)

  B<-t(psi)%*%psi/N

  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)*length(p.cat)+1+dim.w+dim.z+dim.w*length(p.cat)
  
  A<-matrix(0,dim.A,dim.A)
  for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z) ) {
    A[j,j]<-1
  }
  tracker<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z
  for (k in 1:length(p.cat)) {
    for (j in 1:sum(1:dim.w)) {
      A[tracker+j+(k-1)*sum(1:dim.w),tracker+j+(k-1)*sum(1:dim.w)]<-sum(cat==poss.cat[k]&v==1)/N
    }
  }
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)*length(p.cat)+1):(dim.A-dim.w*length(p.cat))) {
    A[j,j]<-1
  }
  for (k in 1:length(p.cat)) {
    for (j in 1:dim.w) {
      A[dim.A-dim.w*length(p.cat)+j+(k-1)*dim.w, dim.A-dim.w*length(p.cat)+j+(k-1)*dim.w]<-sum(cat==poss.cat[k]&v==1)/N
    }
  }

  var.theta<-solve(A)%*%B%*%solve(A)/N

  var.theta.diag<-diag(var.theta)

  Sigma.tt<-sigma.tt[,,1]*p.cat[1]
  Sigma.ttstar<-sigma.ttstar[,,1]*p.cat[1]
  for (j in 2:length(p.cat)) {
    Sigma.tt<-Sigma.tt+sigma.tt[,,j]*p.cat[j]
    Sigma.ttstar<-Sigma.ttstar+sigma.ttstar[,,j]*p.cat[j]
  }
  
  C<-matrix(NA,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  C[1:dim.w,1:dim.w]<-sigma.ww-Sigma.tt             ####  Only works with 1 variable in X
  C[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-sigma.wz
  C[(dim.w+1):(dim.w+dim.z),1:dim.w]<-t(sigma.wz)
  C[(dim.w+1):(dim.w+dim.z),(dim.w+1):(dim.w+dim.z)]<-sigma.zz

  D<-c(sigma.wystar-Sigma.ttstar,sigma.zystar)

  C.inv<-solve(C)

  est<-C.inv%*%D
  C.inv%*%D

  dbeta.dmu.w<- matrix(0,nrow=dim.w+dim.z,ncol=dim.w)

  dbeta.dsigma.ww<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.ww)[2])
  for (j in 1:dim(psi.sigma.ww)[2]) {
    junk<-rep(0,dim(psi.sigma.ww)[2])
    junk[j]<-1
    Dmat<-Dmat1<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1:dim.w]<-junk[1:dim.w]
    if (dim.w>=2){
      Dmat[2,2:dim.w]<-junk[(dim.w+1):(2*dim.w-1)]
      if (dim.w>=3) {
        Dmat[3,3:dim.w]<-junk[(2*dim.w):(3*dim.w-3)]
        if (dim.w>=4) {
          Dmat[4,4:dim.w]<-junk[(3*dim.w-2):(4*dim.w-6)]
          if (dim.w>=5) {
            Dmat[5,5:dim.w]<-junk[(4*dim.w-5):(5*dim.w-10)]
            if (dim.w==6) {
              Dmat[6,6:dim.w]<-junk[(5*dim.w-9):(6*dim.w-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.ww[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }
  dbeta.dsigma.tt<- -dbeta.dsigma.ww*p.cat[1]
  for (j in 2:length(p.cat)) {
    dbeta.dsigma.tt<- -cbind(dbeta.dsigma.tt,dbeta.dsigma.ww*p.cat[j])
  }
  
  dbeta.dmu.z<- matrix(0,nrow=dim.w+dim.z,ncol=dim.z)

  dbeta.dsigma.zz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.zz)[2])
  for (j in 1:dim(psi.sigma.zz)[2]) {
    junk<-rep(0,dim(psi.sigma.zz)[2])
    junk[j]<-1
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)]<-junk[1:dim.z]
    if (dim.z>=2){
      Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)]<-junk[(dim.z+1):(2*dim.z-1)]
      if (dim.z>=3) {
        Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)]<-junk[(2*dim.z):(3*dim.z-3)]
        if (dim.z>=4) {
          Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)]<-junk[(3*dim.z-2):(4*dim.z-6)]
          if (dim.z>=5) {
            Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)]<-junk[(4*dim.z-5):(5*dim.z-10)]
            if (dim.z==6) {
              Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)]<-junk[(5*dim.z-9):(6*dim.z-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.zz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dsigma.wz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.wz)[2])
  for (j in 1:dim(psi.sigma.wz)[2]) {
    junk<-rep(0,dim(psi.sigma.wz)[2])
    junk[j]<-1
    Dmat12<-matrix(junk,nrow=dim.w,ncol=dim.z,byrow=TRUE)
    Dmat21<-t(Dmat12)
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-Dmat12
    Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w]<-Dmat21
    dbeta.dsigma.wz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dmu.ystar<- matrix(0,nrow=dim.w+dim.z,ncol=1)

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  diag(stuff)<-1
  dbeta.dsigma.wzystar<- C.inv%*%stuff

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w)
  diag(stuff)<- -p.cat[1]
  for (j in 2:length(p.cat)){
    junk<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w)
    diag(junk)<- -p.cat[j]
    stuff<- cbind(stuff, junk)
  }
  dbeta.dsigma.ttstar<- C.inv%*%stuff


  dg.theta<-cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt,
                  dbeta.dmu.ystar, dbeta.dsigma.wzystar, dbeta.dsigma.ttstar)

  var.est<-dg.theta%*%var.theta%*%t(dg.theta)
  
  return(list("CorrectEst"=est,"VarCorrectEst"=var.est))

}












#######  This is the same as before except now I assume there are no errors in Y (that ystar is correct)

#  p.cat = Probability of being in a particular category (if there are 3 categories, this is a vector of length 3)
#  cat = Category an observation is classified into
#  poss.cat = unique categories (same length and order as p.cat)

audit.correct.cov.vdependent.norho<-function(ystar,w,z,v,x,y,p.cat,cat,poss.cat) {

  N<-length(ystar)
  nv<-sum(v)

  if(!is.matrix(z)) z = matrix(z,ncol=1)
  if(!is.matrix(w)) w = matrix(w,ncol=1)
  if(!is.matrix(x)) x = matrix(x,ncol=1)
    
  dim.z<-ncol(z)
  dim.w<-ncol(w)
  
  t<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:ncol(w)) { 
    t[,j]<-ifelse(v==1,w[,j]-x[,j],NA)
  }
  tstar<-ifelse(v==1,ystar-y,NA)         ### In practice we can't differentiate between tstar1 and tstar2, but that doesn't matter.

  mu.w<-apply(w,2,mean)
  sigma.ww<-cov(w,w)
  mu.z<-apply(z,2,mean)
  sigma.zz<-cov(z,z) 
  sigma.wz<-cov(w,z)
  sigma.tt<-NULL                                  ####  Right now this only works when X is composed of 1 variable
  for (j in 1:length(p.cat)) {
    sigma.tt[j]<-cov(t[cat==poss.cat[j]],t[cat==poss.cat[j]],use="complete.obs")
  }
  mu.ystar<-mean(ystar)
  sigma.wystar<-cov(w,ystar)
  sigma.zystar<-cov(z,ystar)

  psi.mu.ystar<-ystar-mu.ystar
  psi.mu.w<-matrix(NA,N,dim.w)
  junk<-matrix(NA,nrow=N,ncol=(dim.w^2))
#  junk1<-matrix(NA,nrow=N,ncol=dim.w^2)
  psi.sigma.wystar<-matrix(NA,nrow=N,ncol=dim.w)
  for (j in 1:dim.w) {
    psi.mu.w[,j]<-w[,j]-mu.w[j]
    psi.sigma.wystar[,j]<-psi.mu.ystar*psi.mu.w[,j]-sigma.wystar[j,]
  }
  for (j in 1:dim.w) {
    for (k in j:dim.w) {
      junk[,k+dim.w*(j-1)]<-psi.mu.w[,j]*psi.mu.w[,k]-sigma.ww[j,k]   #  Would sum to 0 if multiply by (N-1)/N.  Similar elsewhere
#      junk1[,k+dim.w*(j-1)]<-ifelse(v==1,t[,j]*t[,k]-sigma.tt[j,k],0)
    }
  }
  psi.sigma.ww<-junk
#  psi.sigma.tt<-junk1                                       #### Currently only works for 1 variable in X
  if (dim.w>=2) {
    psi.sigma.ww<-junk[,-which(is.na(junk[1,]))]
#    psi.sigma.tt<-junk1[,-which(is.na(junk[1,]))]
  }
  junk1<-matrix(NA,nrow=N,ncol=length(p.cat))
  for (j in 1:length(p.cat)) {
    junk1[,j]<-ifelse(v==1&cat==poss.cat[j],t*t-sigma.tt[j],0)
  }
  psi.sigma.tt<-junk1

  psi.mu.z<-matrix(NA,N,dim.z)
  junk<-matrix(NA,nrow=N,ncol=(dim.z^2))
  psi.sigma.zystar<-matrix(NA,nrow=N,ncol=dim.z)
  for (j in 1:dim.z) {
    psi.mu.z[,j]<-z[,j]-mu.z[j]
    psi.sigma.zystar[,j]<-psi.mu.ystar*psi.mu.z[,j]-sigma.zystar[j,]
  }
  for (j in 1:dim.z) {
    for (k in j:dim.z) {
      junk[,k+dim.z*(j-1)]<-psi.mu.z[,j]*psi.mu.z[,k]-sigma.zz[j,k] 
    }
  }
  psi.sigma.zz<-junk
  if (dim.z>=2) {
    psi.sigma.zz<-junk[,-which(is.na(junk[1,]))]
  }

  psi.sigma.wz<-matrix(NA,nrow=N,ncol=dim.w*dim.z)
  for (j in 1:dim.w) {
    for (k in 1:dim.z) {
      psi.sigma.wz[,k+dim.z*(j-1)]<-psi.mu.w[,j]*psi.mu.z[,k]-sigma.wz[j,k]
    }
  }

  psi<-cbind( psi.mu.w,
             psi.sigma.ww,
             psi.mu.z,
             psi.sigma.zz,
             psi.sigma.wz,
             psi.sigma.tt,
             psi.mu.ystar,
             psi.sigma.wystar,
             psi.sigma.zystar)

  B<-t(psi)%*%psi/N

#  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w)+1+dim.w+dim.z+dim.w    
  dim.A<-dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1+dim.w+dim.z  ### only works for single X
  
  A<-matrix(0,dim.A,dim.A)
  for (j in 1:(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z) ) {
    A[j,j]<-1
  }
#  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+sum(1:dim.w))) {
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+1):(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat))) {
#    A[j,j]<-nv/N
    A[j,j]<-sum(cat==poss.cat[j-(dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z)]&v==1)/N
  }
#  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1):(dim.A-dim.w)) {
  for (j in (dim.w+sum(1:dim.w)+dim.z+sum(1:dim.z)+dim.w*dim.z+length(p.cat)+1):dim.A) {
    A[j,j]<-1
  }

  var.theta<-solve(A)%*%B%*%solve(A)/N

  var.theta.diag<-diag(var.theta)

  C<-matrix(NA,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  C[1:dim.w,1:dim.w]<-sigma.ww-sigma.tt%*%p.cat             ####  Only works with 1 variable in X
  C[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-sigma.wz
  C[(dim.w+1):(dim.w+dim.z),1:dim.w]<-t(sigma.wz)
  C[(dim.w+1):(dim.w+dim.z),(dim.w+1):(dim.w+dim.z)]<-sigma.zz

  D<-c(sigma.wystar,sigma.zystar)

  C.inv<-solve(C)

  est<-C.inv%*%D
  C.inv%*%D

  dbeta.dmu.w<- matrix(0,nrow=dim.w+dim.z,ncol=dim.w)

  dbeta.dsigma.ww<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.ww)[2])
  for (j in 1:dim(psi.sigma.ww)[2]) {
    junk<-rep(0,dim(psi.sigma.ww)[2])
    junk[j]<-1
    Dmat<-Dmat1<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1:dim.w]<-junk[1:dim.w]
    if (dim.w>=2){
      Dmat[2,2:dim.w]<-junk[(dim.w+1):(2*dim.w-1)]
      if (dim.w>=3) {
        Dmat[3,3:dim.w]<-junk[(2*dim.w):(3*dim.w-3)]
        if (dim.w>=4) {
          Dmat[4,4:dim.w]<-junk[(3*dim.w-2):(4*dim.w-6)]
          if (dim.w>=5) {
            Dmat[5,5:dim.w]<-junk[(4*dim.w-5):(5*dim.w-10)]
            if (dim.w==6) {
              Dmat[6,6:dim.w]<-junk[(5*dim.w-9):(6*dim.w-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.ww[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
#    dbeta.dsigma.tt[,j]<- - dbeta.dsigma.ww[,j]
  }
  dbeta.dsigma.tt<-matrix(NA,nrow=dim.w+dim.z,ncol=length(p.cat))       #### Doesn't work if more than one variable in X
  for (j in 1:length(p.cat)) {
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1,1]<- -p.cat[j]
    dbeta.dsigma.tt[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }
  
  dbeta.dmu.z<- matrix(0,nrow=dim.w+dim.z,ncol=dim.z)

  dbeta.dsigma.zz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.zz)[2])
  for (j in 1:dim(psi.sigma.zz)[2]) {
    junk<-rep(0,dim(psi.sigma.zz)[2])
    junk[j]<-1
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[dim.w+1,(dim.w+1):(dim.w+dim.z)]<-junk[1:dim.z]
    if (dim.z>=2){
      Dmat[dim.w+2,(dim.w+2):(dim.w+dim.z)]<-junk[(dim.z+1):(2*dim.z-1)]
      if (dim.z>=3) {
        Dmat[dim.w+3,(dim.w+3):(dim.w+dim.z)]<-junk[(2*dim.z):(3*dim.z-3)]
        if (dim.z>=4) {
          Dmat[dim.w+4,(dim.w+4):(dim.w+dim.z)]<-junk[(3*dim.z-2):(4*dim.z-6)]
          if (dim.z>=5) {
            Dmat[dim.w+5,(dim.w+5):(dim.w+dim.z)]<-junk[(4*dim.z-5):(5*dim.z-10)]
            if (dim.z==6) {
              Dmat[dim.w+6,(dim.w+6):(dim.w+dim.z)]<-junk[(5*dim.z-9):(6*dim.z-15)]
            }}}}}
    if (sum(diag(Dmat))==0)  {
      Dmat<-Dmat+t(Dmat)
    }
    dbeta.dsigma.zz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dsigma.wz<-matrix(NA,nrow=dim.w+dim.z,ncol=dim(psi.sigma.wz)[2])
  for (j in 1:dim(psi.sigma.wz)[2]) {
    junk<-rep(0,dim(psi.sigma.wz)[2])
    junk[j]<-1
    Dmat12<-matrix(junk,nrow=dim.w,ncol=dim.z,byrow=TRUE)
    Dmat21<-t(Dmat12)
    Dmat<-matrix(0,dim.w+dim.z,dim.w+dim.z)
    Dmat[1:dim.w,(dim.w+1):(dim.w+dim.z)]<-Dmat12
    Dmat[(dim.w+1):(dim.w+dim.z),1:dim.w]<-Dmat21
    dbeta.dsigma.wz[,j]<- -C.inv%*%Dmat%*%C.inv%*%D
  }

  dbeta.dmu.ystar<- matrix(0,nrow=dim.w+dim.z,ncol=1)

  stuff<-matrix(0,nrow=dim.w+dim.z,ncol=dim.w+dim.z)
  diag(stuff)<-1
  dbeta.dsigma.wzystar<- C.inv%*%stuff

  dg.theta<-cbind(dbeta.dmu.w, dbeta.dsigma.ww, dbeta.dmu.z, dbeta.dsigma.zz, dbeta.dsigma.wz, dbeta.dsigma.tt,
                  dbeta.dmu.ystar, dbeta.dsigma.wzystar)

  var.est<-dg.theta%*%var.theta%*%t(dg.theta)
  
  return(list("CorrectEst"=est,"VarCorrectEst"=var.est))

}

