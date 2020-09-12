n2 = c(25, 50, 100, 200, 300)
hn = c(40, 40, 30, 10, 1)
beta = 0.4

z975 = qnorm(0.975)
nrow = length(n2)

fn_out = paste0("sum2_20200702.tab")

sink(fn_out)
cat("\tMBE\t\t\t\t\t\tUAEE\t\t\t\t\t\tSMLE\t\t\t\n")
cat("n2\tBias\tSE\tSEE\tCP\tRE\t\tBias\tSE\tSEE\tCP\tRE\t\tBias\tSE\tSEE\tCP\n")
sink()

res = matrix(NA, nrow=nrow, ncol=17)

i = 1
for (k in 1:4) {
    res[i,1] = n2[k]
    
    prefix = paste0("SY2011_n2_", n2[k])
    load(paste0("results/", prefix, ".RData"))
    
	print(nrow(results))
    res[i,2] = mean(results[,1])-beta
    res[i,3] = sd(results[,1])
	res[i,4] = mean(results[,2])
	res[i,5] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))

    prefix = paste0("CC2000_n2_", n2[k])
    load(paste0("results/", prefix, ".RData"))
    
	print(nrow(results))
    res[i,8] = mean(results[,1])-beta
    res[i,9] = sd(results[,1])
	res[i,10] = mean(results[,2])
	res[i,11] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
    
	prefix = paste0("n2_", n2[k], "_hn", hn[k])
    load(paste0("results/", prefix, ".RData"))

	print(nrow(results))
    res[i,14] = mean(results[,1])-beta
    res[i,15] = sd(results[,1])
    res[i,16] = mean(results[,2])
    res[i,17] = mean((results[,1]-z975*results[,2] <= beta) & (results[,1]+z975*results[,2] >= beta))
    
	res[i,6] = (res[i,15]/res[i,3])^2
	res[i,12] = (res[i,15]/res[i,9])^2
    i = i+1
}

write.table(res, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t", na="")
