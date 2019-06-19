NJOB = 10

dir.create("results", showWarnings=FALSE)

prefix = "unif"

load(paste0("res/", prefix, "/1.RData"))
tmp = results

for (njob in 2:NJOB) {
    load(paste0("res/", prefix, "/", njob, ".RData"))
    tmp = rbind(tmp, results)
}

results = tmp

print(dim(results))

save(list="results", file=paste0("results/", prefix, ".RData"))

