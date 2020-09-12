p_set = c(0.1, 0.3, 0.6, 1)
rho_set = c(0, 0.3, 0.5)
hn_set = c(1)
nsieve_set = c(10)
NJOB = 250

dir.create("results", showWarnings=FALSE)

for(nsieve in nsieve_set) {
    for (hn in hn_set) {
        for (p in p_set) {
            for (rho in rho_set) {
                prefix = paste0("nsieve", nsieve, "_hn", hn, "_rho", rho, "_p", p)
                
				njob = 1
				fn = paste0("res/", prefix, "/", njob, ".RData")
				while (!file.exists(fn)) {
					njob = njob + 1
					fn = paste0("res/", prefix, "/", njob, ".RData")
				}
				load(fn)
				tmp = results
                
                while (njob < NJOB) {
					njob = njob + 1
					fn = paste0("res/", prefix, "/", njob, ".RData")
					if (file.exists(fn)) {
						load(fn)
                    	tmp = rbind(tmp, results)
					}
                }
                
                results = tmp
                
                print(paste("nsieve:", nsieve, "hn:", hn, "p:", p, "rho:", rho, "NSIM:", nrow(results)))
                
                save(list="results", file=paste0("results/", prefix, ".RData"))
            }
        }
    }
}
