dft_set = c(3, 5, 10, 15, 20, 30)
boundary_set = c(1, 2)
beta = 0.4

z975 = qnorm(0.975)
nrow = length(dft_set)+length(boundary_set)

fn_out = paste0("sum2_v2.tab")

sink(fn_out)
cat("\tMBE\t\t\t\t\tSMLE\t\t\t\n")
cat("Distribution\tBias\tSE\tSEE\tCP\t\tBias\tSE\tSEE\tCP\n")
sink()

res = matrix(NA, nrow=nrow, ncol=9)
distn = rep(NA, nrow)

i = 1
for (dft in dft_set) {
    distn[i] = paste0("t", dft)

    prefix = paste0("SY2011_rho0.3_p0.6_dft", dft)
    load(paste0("results/", prefix, ".RData"))

    res[i,1] = mean(results[,1])-beta
    res[i,2] = sd(results[,1])
    res[i,3] = mean(results[,2])
    res[i,4] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
    
    prefix = paste0("dft", dft)
    load(paste0("results/", prefix, ".RData"))

    res[i,6] = mean(results[,1])-beta
    res[i,7] = sd(results[,1])
    res[i,8] = mean(results[,2])
    res[i,9] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
    
    i = i+1
}

for (boundary in boundary_set) {
	distn[i] = paste0("Uniform(-", boundary, ", ", boundary, ")")
	
	prefix = paste0("SY2011_unif_boundary", boundary)
	load(paste0("results/", prefix, ".RData"))
	
	res[i,1] = mean(results[,1])-beta
	res[i,2] = sd(results[,1])
	res[i,3] = mean(results[,2])
	res[i,4] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
	
	prefix = paste0("unif_boundary", boundary)
	load(paste0("results/", prefix, ".RData"))
	
	res[i,6] = mean(results[,1])-beta
	res[i,7] = sd(results[,1])
	res[i,8] = mean(results[,2])
	res[i,9] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
	
	i = i+1
}
write.table(data.frame(distn, res), file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")

