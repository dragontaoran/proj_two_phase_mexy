p_set = c(0.1, 0.3, 0.6, 1)
rho_set = c(-0.5, -0.3, 0, 0.3, 0.5)
hn_set = c(1)
nsieve_set = c(10, 15, 20, 25, 30)
beta = 0.4

z975 = qnorm(0.975)
nrow = length(p_set)*length(rho_set)

for (nsieve in nsieve_set) {
    for (hn in hn_set) {
        
        fn_out = paste0("sum2_nsieve", nsieve, "_hn", hn, "_20190927.tab")
        
        sink(fn_out)
        cat("\t\tSY\t\t\t\tCC\t\t\t\tMLE\t\t\t\n")
        cat("rho\tp\tBias\tSE\tRE\t\tBias\tSE\tRE\t\tBias\tSE\tSEE\tCP\n")
        sink()
        
        res = matrix(NA, nrow=nrow, ncol=14)
        
        i = 1
        for (rho in rho_set) {
            for (p in p_set) {
                res[i,1] = rho
                res[i,2] = p
                
                prefix = paste0("SY2011_rho", rho, "_p", p)
                load(paste0("results/", prefix, ".Rdata"))

                res[i,3] = mean(results[,1])-beta
                res[i,4] = sd(results[,1])

                prefix = paste0("CC2000_rho", rho, "_p", p)
                load(paste0("results/", prefix, ".RData"))

                res[i,7] = mean(results[,1])-beta
                res[i,8] = sd(results[,1])

                prefix = paste0("nsieve", nsieve, "_hn", hn, "_rho", rho, "_p", p)
                load(paste0("results/", prefix, ".Rdata"))

                res[i,11] = mean(results[,1])-beta
                res[i,12] = sd(results[,1])
                res[i,13] = mean(results[,2])
                res[i,14] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                res[i,5] = (res[i,12]/res[i,4])^2
				res[i,9] = (res[i,12]/res[i,8])^2
                
                i = i+1
            }
        }
        
        write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")    
    }    
}
