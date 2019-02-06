p_set = c(0.1, 0.3, 0.6, 1)
rho_set = c(-0.5, -0.3, 0, 0.3, 0.5)
hn_set = c(0.1)
nsieve_set = c(20, 30)
NJOB = 10

dir.create("results", showWarnings=FALSE)

for(nsieve in nsieve_set) {
    for (hn in hn_set) {
        for (p in p_set) {
            for (rho in rho_set) {
                prefix = paste0("nsieve", nsieve, "_hn", hn, "_rho", rho, "_p", p)
                
                load(paste0("res/", prefix, "/1.RData"))
                tmp = results
                
                for (njob in 2:NJOB) {
                    load(paste0("res/", prefix, "/", njob, ".RData"))
                    tmp = rbind(tmp, results)
                }
                
                results = tmp
                
                print(dim(results))
                
                save(list="results", file=paste0("results/", prefix, ".RData"))
            }
        }
    }
}
