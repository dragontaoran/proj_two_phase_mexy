p_set = c(0.1, 0.3, 0.6, 1)
rho_set = c(0, 0.3, 0.5)
hn_set = c(1)
nsieve_set = c(10)
beta = 0.4

z975 = qnorm(0.975)
nrow = length(p_set)*length(rho_set)

for (nsieve in nsieve_set) {
    for (hn in hn_set) {
        
        fn_out = paste0("sum2_nsieve", nsieve, "_hn", hn, ".tab")
        
        sink(fn_out)
        cat("\t\t$X_1$\t\t\t\t\t$X_2$\t\t\t\n")
        cat("$r$\t$p$\tBias\tSE\tSEE\tCP\t\tBias\tSE\tSEE\tCP\n")
        sink()
        
        res = matrix(NA, nrow=nrow, ncol=11)
        
        i = 1
        for (rho in rho_set) {
            for (p in p_set) {
                res[i,1] = rho
                res[i,2] = p
                
                prefix = paste0("nsieve", nsieve, "_hn", hn, "_rho", rho, "_p", p)
                load(paste0("results/", prefix, ".RData"))

                res[i,3] = mean(results[,1])-beta
                res[i,4] = sd(results[,1])
                res[i,5] = mean(results[,2])
                res[i,6] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                res[i,8] = mean(results[,3])-beta
                res[i,9] = sd(results[,3])
                res[i,10] = mean(results[,4])
                res[i,11] = mean((results[,3]-z975*results[,4] <= beta) & (results[,3]+z975*results[,4] >= beta))
                
                i = i+1
            }
        }
        
        write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")    
    }    
}
