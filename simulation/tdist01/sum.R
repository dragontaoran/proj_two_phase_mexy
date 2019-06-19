dft_set = c(3, 5, 10, 15, 20, 30)
NJOB = 10

dir.create("results", showWarnings=FALSE)

for(dft in dft_set) {
    prefix = paste0("dft", dft)
    
    load(paste0("res/", prefix, "/1.Rdata"))
    tmp = results
    
    for (njob in 2:NJOB) {
        load(paste0("res/", prefix, "/", njob, ".Rdata"))
        tmp = rbind(tmp, results)
    }
    
    results = tmp
    
    print(dim(results))
    
    save(list="results", file=paste0("results/", prefix, ".RData"))
}
