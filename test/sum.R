NJOB = 100
true_values = c(6, -0.01)
z975 = qnorm(0.975)
fn_out = "sum.tab"
sink(fn_out)
cat("r\tCovariate\tBias\tSE\tSEE\tCP\n")
sink()

out_covar = c("intercept", "X")
p = length(out_covar)

load("res/test_1.Rdata")
res_est = results_est
res_se = results_se

for (i in 2:100) {
    load(paste0("res/test_", i, ".Rdata"))
    res_est = rbind(res_est, results_est)
    res_se = rbind(res_se, results_se)
}
print(dim(res_est))
print(dim(res_se))

out_bias = colMeans(res_est)-true_values
out_se = apply(res_est, 2, sd)
out_see = colMeans(res_se)
out_cp = rep(NA, p)
for (i in 1:p) {
    out_cp[i] = mean(res_est[,i]-z975*res_se[,i] <= true_values[i] & res_est[,i]+z975*res_se[,i] >= true_values[i])
}

out = data.frame(out_covar, out_bias, out_se, out_see, out_cp)
write.table(out, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
