muu_set = c(0, 0.5)
muw_set = c(0, 0.5)
rho_set = c(-0.5, 0, 0.5)
hn_set = c(0.1)
nsieve_set = c(20)
beta = 0.4

z975 = qnorm(0.975)
nrow = length(muu_set)*length(rho_set)*length(muw_set)

for (nsieve in nsieve_set) {
    for (hn in hn_set) {
        
        fn_out = paste0("sum2_nsieve", nsieve, "_hn", hn, "_20191003.tab")
        
        sink(fn_out)
        cat("\t\t\tMBE\t\t\t\t\tCC\t\t\t\t\t\tSMLE\t\t\t\n")
        cat("rho\t$\\mu_U$\t$\\mu_W$\tBias\tSE\tSEE\tCP\t\tBias\tSE\tSEE\tCP\tRE\t\tBias\tSE\tSEE\tCP\n")
        sink()
        
        res = matrix(NA, nrow=nrow, ncol=18)
        
        i = 1
        for (rho in rho_set) {
            for (muu in muu_set) {
                for (muw in muw_set) {
                    res[i,1] = rho
                    res[i,2] = muu
                    res[i,3] = muw
                    
                    prefix = paste0("SY2011_rho", rho, "_muu", muu, "_muw", muw)
                    load(paste0("results/", prefix, ".RData"))
                    
                    res[i,4] = mean(results[,1])-beta
                    res[i,5] = sd(results[,1])
                    res[i,6] = mean(results[,2])
                    res[i,7] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                    
                    prefix = paste0("CC2000_rho", rho, "_muu", muu, "_muw", muw)
                    load(paste0("results/", prefix, ".RData"))
                    
                    res[i,9] = mean(results[,1])-beta
                    res[i,10] = sd(results[,1])
                    res[i,11] = mean(results[,2])
                    res[i,12] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
                    
					prefix = paste0("nsieve", nsieve, "_hn", hn, "_rho", rho, "_muu", muu, "_muw", muw)
                    load(paste0("results/", prefix, ".RData"))

                    res[i,15] = mean(results[,1])-beta
                    res[i,16] = sd(results[,1])
                    res[i,17] = mean(results[,2])
                    res[i,18] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
					
					res[i,13] = (res[i,16]/res[i,10])^2

                    i = i+1                    
                }
            }
        }
        
        write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")    
    }    
}
