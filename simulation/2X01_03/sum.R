p_set = c(0.6, 1)
rho_set = c(0.3, 0.5)
sigma_set = c(0.5, 1)
design_set = c("srs", "ssrs")
hn_set = c(0.1)
nsieve_set = c(10)
NJOB = 10
NSIM = 1000

dir.create("results", showWarnings=FALSE)

for(nsieve in nsieve_set) {
    for (hn in hn_set) {
        for (design in design_set) {
            for (sigma in sigma_set) {
                for (p in p_set) {
                    for (rho in rho_set) {
                        prefix = paste0("nsieve", nsieve, "_hn", hn, "_", design, "_sigma", sigma, "_rho", rho, "_p", p)
                        
                        load(paste0("res/", prefix, "/1.RData"))
                        tmp = results[1:NSIM,]
                        
                        for (njob in 2:NJOB) {
                            load(paste0("res/", prefix, "/", njob, ".RData"))
                            tmp = rbind(tmp, results[1:NSIM,])
                        }
                        
                        results = tmp
                        
                        print(dim(results))
                        
                        save(list="results", file=paste0("results/", prefix, ".RData"))
                    }
                }                  
            }
        }
    }
}
