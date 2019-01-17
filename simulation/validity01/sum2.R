n2_set = c(300)
p_s_set = c(0.05, 0.1, 0.2, 0.3)
rho_u_ustar_set = c(-0.5, 0, 0.5)
beta = -0.01

z975 = qnorm(0.975)
fn_out = "sum2.tab"
sink(fn_out)
cat("\t\t\tSY\t\t\t\t\t\tMLE\t\t\t\t\t\n")
cat("rho_u_ustar\tp_s\tn2\tBias_persent\tSE_1E2\tSEE_1E2\tCP_percent\tMSE_1E7\t\tBias_persent\tSE_1E2\tSEE_1E2\tCP_percent\tMSE_1E7\tRE\n")
sink()

nrow = length(n2_set)*length(p_s_set)*length(rho_u_ustar_set)

res = matrix(NA, nrow=nrow, ncol=15)

i = 1
for (rho_u_ustar in rho_u_ustar_set) {
    for (p_s in p_s_set) {
        for (n2 in n2_set) {
            res[i,1] = rho_u_ustar
            res[i,2] = p_s
            res[i,3] = n2

            prefix = paste("SY2011_n2", n2, "p_s", p_s, "rho_u_ustar", rho_u_ustar, sep="_")
            load(paste0("results/", prefix, ".Rdata"))
            
            res[i,4] = (mean(results[,1])-beta)/beta*100
            res[i,5] = sd(results[,1])*100
            res[i,6] = mean(results[,2])*100
            res[i,7] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))*100
            res[i,8] = mean((results[,1]-beta)^2+results[,2]^2)*1E7
            
			prefix = paste("n2", n2, "p_s", p_s, "rho_u_ustar", rho_u_ustar, sep="_")
            load(paste0("results/", prefix, ".Rdata"))
            
            res[i,10] = (mean(results[,1])-beta)/beta*100
            res[i,11] = sd(results[,1])*100
            res[i,12] = mean(results[,2])*100
            res[i,13] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))*100
            res[i,14] = mean((results[,1]-beta)^2+results[,2]^2)*1E7
			
			res[i,15] = (res[i,5]/res[i,11])^2

            i = i+1
        }
    }
}

write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")

