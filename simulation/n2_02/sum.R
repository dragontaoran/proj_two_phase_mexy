p = 0.6
rho = 0.3
hn_set = c(30, 40, 60)
nsieve = 20
NJOB = 100
n2_set = c(25)

dir.create("results", showWarnings=FALSE)

for (hn in hn_set) {
	for(n2 in n2_set) {
	    prefix = paste0("n2_", n2, "_hn", hn)
	    
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
