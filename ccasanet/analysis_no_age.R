###################################################################################
######   Analyses that will be in the paper (exclude age)
###################################################################################

rm(list=ls())
gc()
load("analysis-data-CCASAnet-audit.rda")

z975 = qnorm(0.975)

#### START: Moment Estimator ######################################################
source("audit-correct-cov.R")

z1 = ifelse(site=="argentina", 1, 0)
z2 = ifelse(site=="brazil", 1, 0)
z3 = ifelse(site=="chile", 1, 0)
z4 = ifelse(site=="honduras", 1, 0)
z5 = ifelse(site=="mexico", 1, 0)
z = cbind(z1, z2, z3, z4, z5)

cat = ifelse(site=="argentina", "a",
             ifelse(site=="brazil", "b",
                    ifelse(site=="chile", "c",
                           ifelse(site=="honduras", "h",
                                  ifelse(site=="mexico", "m",
                                        ifelse(site=="peru", "p", NA)
                                        )
                                  )
                           )
                    )
             )
poss.cat = c("a", "b", "c", "h", "m", "p")
p.cat = table(cat)/length(cat)

res_naive = summary(lm(ystar~w+z+male))
res_naive

res_sy = audit.correct.cov.vdependent.2plus(ystar, w, cbind(z,male), v, x, y, p.cat, cat, poss.cat)
res_sy
#### END: Moment Estimator ########################################################

#### START: SMLE ##################################################################
library(TwoPhaseReg)
library(splines)
dat = data.frame(ystar, w, z, male, v, x, y, cat)
dat$y[v == 0] = NA
dat$x[v == 0] = NA
dat$bs_a = as.numeric(cat == "a")
dat$bs_b = as.numeric(cat == "b")
dat$bs_c = as.numeric(cat == "c")
dat$bs_h = as.numeric(cat == "h")
dat$bs_m = as.numeric(cat == "m")
dat$bs_p = as.numeric(cat == "p")

id_n2a = which(dat$v == 1 & dat$cat == "a")
id_n2b = which(dat$v == 1 & dat$cat == "b")
id_n2c = which(dat$v == 1 & dat$cat == "c")
id_n2h = which(dat$v == 1 & dat$cat == "h")
id_n2m = which(dat$v == 1 & dat$cat == "m")
id_n2p = which(dat$v == 1 & dat$cat == "p")

tab1 = data.frame(site=names(table(dat$cat)), n=as.vector(table(dat$cat)))
tab1$n2 = aggregate(dat$v, list(dat$cat), FUN=sum)$x
tab1$n2_error_art = c(sum(dat$w[id_n2a] != dat$x[id_n2a]),
                      sum(dat$w[id_n2b] != dat$x[id_n2b]),
                      sum(dat$w[id_n2c] != dat$x[id_n2c]),
                      sum(dat$w[id_n2h] != dat$x[id_n2h]),
                      sum(dat$w[id_n2m] != dat$x[id_n2m]),
                      sum(dat$w[id_n2p] != dat$x[id_n2p]))
tab1$cor_xstar_u_art = c(cor(dat$w[id_n2a], dat$w[id_n2a]-dat$x[id_n2a]),
                         cor(dat$w[id_n2b], dat$w[id_n2b]-dat$x[id_n2b]),
                         cor(dat$w[id_n2c], dat$w[id_n2c]-dat$x[id_n2c]),
                         cor(dat$w[id_n2h], dat$w[id_n2h]-dat$x[id_n2h]),
                         cor(dat$w[id_n2m], dat$w[id_n2m]-dat$x[id_n2m]),
                         0)

### check correlation between errors and sex
dat.m = dat[which(dat$male == 1),]
dat.w = dat[which(dat$male == 0),]

id_n2a_m = which(dat.m$v == 1 & dat.m$cat == "a")
id_n2b_m = which(dat.m$v == 1 & dat.m$cat == "b")
id_n2c_m = which(dat.m$v == 1 & dat.m$cat == "c")
id_n2h_m = which(dat.m$v == 1 & dat.m$cat == "h")
id_n2m_m = which(dat.m$v == 1 & dat.m$cat == "m")
id_n2p_m = which(dat.m$v == 1 & dat.m$cat == "p")

id_n2a_w = which(dat.w$v == 1 & dat.w$cat == "a")
id_n2b_w = which(dat.w$v == 1 & dat.w$cat == "b")
id_n2c_w = which(dat.w$v == 1 & dat.w$cat == "c")
id_n2h_w = which(dat.w$v == 1 & dat.w$cat == "h")
id_n2m_w = which(dat.w$v == 1 & dat.w$cat == "m")
id_n2p_w = which(dat.w$v == 1 & dat.w$cat == "p")

tab1$n2_male = aggregate(dat.m$v, list(dat.m$cat), FUN=sum)$x
tab1$n2_error_art_male = c(sum(dat.m$w[id_n2a_m] != dat.m$x[id_n2a_m]),
                           sum(dat.m$w[id_n2b_m] != dat.m$x[id_n2b_m]),
                           sum(dat.m$w[id_n2c_m] != dat.m$x[id_n2c_m]),
                           sum(dat.m$w[id_n2h_m] != dat.m$x[id_n2h_m]),
                           sum(dat.m$w[id_n2m_m] != dat.m$x[id_n2m_m]),
                           sum(dat.m$w[id_n2p_m] != dat.m$x[id_n2p_m]))

tab1$n2_female = aggregate(dat.w$v, list(dat.w$cat), FUN=sum)$x
tab1$n2_error_art_female = c(sum(dat.w$w[id_n2a_w] != dat.w$x[id_n2a_w]),
                           sum(dat.w$w[id_n2b_w] != dat.w$x[id_n2b_w]),
                           sum(dat.w$w[id_n2c_w] != dat.w$x[id_n2c_w]),
                           sum(dat.w$w[id_n2h_w] != dat.w$x[id_n2h_w]),
                           sum(dat.w$w[id_n2m_w] != dat.w$x[id_n2m_w]),
                           sum(dat.w$w[id_n2p_w] != dat.w$x[id_n2p_w]))

tab1$site.new = c("Site C", "Site D", "Site B", "Site A", "Site E", "Site F")
tab1 = tab1[order(tab1$site.new),]


nsieve_a = 5
nsieve_c = 2
nsieve_h = 2
nsieve_m = 3

Bspline_a = bs(dat$w[dat$cat == "a"], df=nsieve_a, degree=1, Boundary.knots=range(dat$w[dat$cat == "a"]), intercept=TRUE)
Bspline_c = bs(dat$w[dat$cat == "c"], df=nsieve_c, degree=1, Boundary.knots=range(dat$w[dat$cat == "c"]), intercept=TRUE)
Bspline_h = bs(dat$w[dat$cat == "h"], df=nsieve_h, degree=1, Boundary.knots=range(dat$w[dat$cat == "h"]), intercept=TRUE)
Bspline_m = bs(dat$w[dat$cat == "m"], df=nsieve_m, degree=1, Boundary.knots=range(dat$w[dat$cat == "m"]), intercept=TRUE)
dat$bs_a1 = 0
dat$bs_a2 = 0
dat$bs_a3 = 0
dat$bs_a4 = 0
dat$bs_a5 = 0
dat$bs_a6 = 0
dat$bs_a7 = 0
dat$bs_a8 = 0
dat$bs_a9 = 0
dat$bs_a10 = 0
dat[dat$cat == "a", paste0("bs_a", 1:nsieve_a)] = Bspline_a
dat$bs_c1 = 0
dat$bs_c2 = 0
dat$bs_c3 = 0
dat$bs_c4 = 0
dat[dat$cat == "c", paste0("bs_c", 1:nsieve_c)] = Bspline_c
dat$bs_h1 = 0
dat$bs_h2 = 0
dat$bs_h3 = 0
dat[dat$cat == "h", paste0("bs_h", 1:nsieve_h)] = Bspline_h
dat$bs_m1 = 0
dat$bs_m2 = 0
dat$bs_m3 = 0
dat$bs_m4 = 0
dat$bs_m5 = 0
dat$bs_m6 = 0
dat$bs_m7 = 0
dat$bs_m8 = 0
dat$bs_m9 = 0
dat$bs_m10 = 0
dat[dat$cat == "m", paste0("bs_m", 1:nsieve_m)] = Bspline_m

res_smle = smle_MEXY(Y="y", X="x", Y_tilde="ystar", X_tilde="w", Z=c("z1", "z2", "z3", "z4", "z5", "male"),
                Bspline=paste0("bs_", c(paste0("a", 1:nsieve_a), "b", paste0("c", 1:nsieve_c), paste0("h", 1:nsieve_h), paste0("m", 1:nsieve_m), "p")), data=dat, hn_scale=0.05)
res_smle
#### END: SMLE ####################################################################

naive_est = res_naive$coef[c(2, 8, 6, 5, 3, 4, 7),1]
naive_se = res_naive$coef[c(2, 8, 6, 5, 3, 4, 7),2]
sy_est = res_sy$CorrectEst[c(1, 7, 5, 4, 2, 3, 6),1]
sy_se = sqrt(diag(res_sy$VarCorrectEst)[c(1, 7, 5, 4, 2, 3, 6)])
smle_est = res_smle$coef[c(2, 8, 6, 5, 3, 4, 7),1]
smle_se = res_smle$coef[c(2, 8, 6, 5, 3, 4, 7),2]

tab2 = data.frame(cov=c("Date of ART initiation (per year)", 
                        "Male", 
                        paste("Site", c("A", "B", "C", "D", "E"))),
                  naive_est=round(naive_est, 3),
                  naive_ci=paste0("(", round(naive_est-z975*naive_se, 3), ", ", round(naive_est+z975*naive_se, 3), ")"), 
                  sy_est=round(sy_est, 3),
                  sy_ci=paste0("(", round(sy_est-z975*sy_se, 3), ", ", round(sy_est+z975*sy_se, 3), ")"),
                  smle_est=round(smle_est, 3),
                  smle_ci=paste0("(", round(smle_est-z975*smle_se, 3), ", ", round(smle_est+z975*smle_se, 3), ")"))
write.table(tab2, file="analysis_no_age.tab", quote=FALSE, row.names=FALSE, sep="\t")


