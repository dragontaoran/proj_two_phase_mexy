NJOB = 10
boundary_set = c(0.5, 1, 2)

dir.create("results", showWarnings=FALSE)

for (boundary in boundary_set) {
	prefix = paste0("unif_boundary", boundary)
	
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

