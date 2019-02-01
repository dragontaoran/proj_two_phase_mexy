p_set = c(0.6, 1)
rho_set = c(0.3, 0.5)
sigma_set = c(0.5, 1)
hn_set = c(0.1)
nsieve_set = c(10)
design_set = c("srs", "ssrs")
beta = 0.4
gamma = 0.5

z975 = qnorm(0.975)
nrow = 2*length(sigma_set)*length(p_set)*length(rho_set)

for (nsieve in nsieve_set) {
    for (hn in hn_set) {
        for (design in design_set) {
            fn_out = paste0("sum2_nsieve", nsieve, "_hn", hn, "_", design, ".tab")
            
            sink(fn_out)
            cat("\t\t\t\tMoment Estimator\t\t\t\t\tSMLE\t\t\t\t\n")
            cat("$\\tau$\t$r$\t$p$\tCovariate\tBias\tSE\tSEE\tCP\t\tBias\tSE\tSEE\tCP\tRE\n")
            sink()
            
            res = matrix(NA, nrow=nrow, ncol=14)
            
            i = 1
            for (sigma in sigma_set) {
                for (rho in rho_set) {
                    for (p in p_set) {
                        res[i,1] = res[i+1,1] = sigma
                        res[i,2] = res[i+1,2] = rho
                        res[i,3] = res[i+1,3] = p
                        res[i,4] = "$X_a$"
                        res[i+1,4] = "$X_b$"
                        
                        prefix = paste0("SY2011_", design, "_rho", rho, "_p", p, "_sigma", sigma)
                        load(paste0("results/", prefix, ".RData"))
                        
                        res[i,5] = mean(results[,1])-beta
                        res[i,6] = sd(results[,1])
                        res[i,7] = mean(results[,2])
                        res[i,8] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                        
                        res[i+1,5] = mean(results[,3])-gamma
                        res[i+1,6] = sd(results[,3])
                        res[i+1,7] = mean(results[,4])
                        res[i+1,8] = mean((results[,3]-z975*results[,4] <= gamma) & (results[,3]+z975*results[,4] >= gamma))
                        
                        prefix = paste0("nsieve", nsieve, "_hn", hn, "_", design, "_sigma", sigma, "_rho", rho, "_p", p)
                        load(paste0("results/", prefix, ".RData"))

                        res[i,10] = mean(results[,1])-beta
                        res[i,11] = sd(results[,1])
                        res[i,12] = mean(results[,2])
                        res[i,13] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                        res[i,14] = (as.numeric(res[i,6])/as.numeric(res[i,11]))^2
                        
                        res[i+1,10] = mean(results[,3])-gamma
                        res[i+1,11] = sd(results[,3])
                        res[i+1,12] = mean(results[,4])
                        res[i+1,13] = mean((results[,3]-z975*results[,4] <= gamma) & (results[,3]+z975*results[,4] >= gamma))
                        res[i+1,14] = (as.numeric(res[i+1,6])/as.numeric(res[i+1,11]))^2
                        
                        i = i+2
                    }
                }               
            }
            write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")  
        }
    }    
}
