#### Chen and Chen (2000), A Unified Approach to Regression Analysis Under Double-Sampling Designs ###
chen2 = function (Y, X, Y_tilde, X_tilde) {
    V = as.numeric(!is.na(Y))
    n = sum(V)
    N = length(V)
    
    X = as.matrix(X)
    X_tilde = as.matrix(X_tilde)
    
    Y2 = Y[V==1]
    X2 = X[V==1,]
    Y_tilde2 = Y_tilde[V==1]
    X_tilde2 = X_tilde[V==1,]
    
    model.true.2 = lm(Y2~X2)
    model.approx.2 = lm(Y_tilde2~X_tilde2)
    model.approx.1 = lm(Y_tilde~X_tilde)
    
    beta_hat = coef(model.true.2)
    gamma_hat = coef(model.approx.2)
    gamma_bar = coef(model.approx.1)
    
    X2_int = cbind(1, X2)
    X_tilde2_int = cbind(1, X_tilde2)
    
    D1 = -crossprod(X2_int, X2_int)/n
    D2 = -crossprod(X_tilde2_int, X_tilde2_int)/n
    
    S2 = X2_int*residuals(model.true.2)
    S_tilde2 = X_tilde2_int*residuals(model.approx.2)
    C11 = crossprod(S2, S2)/n
    C12 = crossprod(S2, S_tilde2)/n
    C22 = crossprod(S_tilde2, S_tilde2)/n
    
    D1_inv = solve(D1)
    C22_inv = solve(C22)
    D1C12 = D1_inv%*%C12
    D1C12C22 = D1C12%*%C22_inv
    
    beta_bar = as.vector(beta_hat-D1C12C22%*%D2%*%(gamma_hat-gamma_bar))
    Omega = (D1_inv%*%C11%*%D1_inv-(1-n/N)*D1C12C22%*%t(D1C12))/n
    se_beta_bar = sqrt(diag(Omega))
    
    return(list(est=beta_bar, se=se_beta_bar))
}
#### Chen and Chen (2000), A Unified Approach to Regression Analysis Under Double-Sampling Designs ###
