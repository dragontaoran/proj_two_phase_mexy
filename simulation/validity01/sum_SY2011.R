n2_set = c(300)
p_s_set = c(0.05, 0.1, 0.2, 0.3)
rho_u_ustar_set = c(-0.5, 0, 0.5)
NJOB = 100

dir.create("results", showWarnings=FALSE)

for (n2 in n2_set) {
    for (p_s in p_s_set) {
        for (rho_u_ustar in rho_u_ustar_set) {
            prefix = paste("SY2011_n2", n2, "p_s", p_s, "rho_u_ustar", rho_u_ustar, sep="_")
            
            load(paste0("res/", prefix, "/1.Rdata"))
            tmp = results
            
            for (njob in 2:NJOB) {
                load(paste0("res/", prefix, "/", njob, ".Rdata"))
                tmp = rbind(tmp, results)
            }
        
            results = tmp
            
            print(dim(results))
            
            save(list="results", file=paste0("results/", prefix, ".Rdata"))
        }
    }
}
