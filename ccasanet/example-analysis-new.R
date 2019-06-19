

rm(list=ls())

dm<-read.csv("mexico_selfaudit_results_0.5.csv")

start.date<-as.Date(dm$Start.date..1st.,"%m/%d/%Y")
start.date.code<-dm$Start.date..1st._site_code
start.date.correction<-as.Date(dm$Start.date..1st._site_val,"%m/%d/%Y")

cd4<-dm$CD4..base.
cd4.code<-dm$CD4..base._site_code
cd4.correction<-dm$CD4..base._site_val

weight<-dm$Weight
weight.code<-dm$Weight_site_code
weight.correction<-dm$Weight_site_val

age<-dm$Age
age.code<-dm$Age_site_code
age.correction<-dm$Age_site_val

male<-dm$Male
male.code<-dm$Male_site_code
male.correction<-dm$Male_site_val

mex.audit<-data.frame(id=dm$Record.ID,site="mexico",
                      start.date,start.date.code,start.date.correction,
                      cd4,cd4.code,cd4.correction,
                      weight,weight.code,weight.correction,
                      age,age.code,age.correction,
                      male,male.code,male.correction)

mex.audit$weight.correction[mex.audit$id=="MX186"]<-54  ## Per Steph's instructions

start.date.correction-start.date
mex.audit[start.date.code==3,c(1,2,4:5,7)]




dc<-read.csv("chile_selfaudit_results_0.5.csv")

start.date<-as.Date(dc$Start.date..1st.,"%m/%d/%Y")
start.date.code<-dc$Start.date..1st._site_code
start.date.correction<-as.Date(dc$Start.date..1st._site_val,"%m/%d/%Y")

cd4<-dc$CD4..base.
cd4.code<-dc$CD4..base._site_code
cd4.correction<-dc$CD4..base._site_val

weight<-dc$Weight
weight.code<-dc$Weight_site_code
weight.correction<-dc$Weight_site_val

age<-floor(dc$Age)
age.code<-dc$Age_site_code
age.correction<-dc$Age_site_val

male<-dc$Male
male.code<-dc$Male_site_code
male.correction<-dc$Male_site_val

chile.audit<-data.frame(id=dc$Record.ID,site="chile",
                      start.date,start.date.code,start.date.correction,
                      cd4,cd4.code,cd4.correction,
                      weight,weight.code,weight.correction,
                      age,age.code,age.correction,
                      male,male.code,male.correction)





dho<-read.csv("honduras_selfaudit_results_1.0.csv")

start.date<-as.Date(dho$init.date,"%m/%d/%Y")
start.date.code<-dho$init.date_site_code
start.date.correction<-as.Date(dho$init.date_site_val,"%m/%d/%Y")
start.date.code.cc<-dho$init.date_cc_code
start.date.correction.cc<-as.Date(dho$init.date_cc_val,"%m/%d/%Y")

cd4<-dho$cd4baseline
cd4.code<-dho$cd4baseline_site_code
cd4.correction<-dho$cd4baseline_site_val
cd4.code.cc<-dho$cd4baseline_cc_code
cd4.correction.cc<-dho$cd4baseline_cc_val

weight<-dho$weight
weight.code<-dho$weight_site_code
weight.correction<-dho$weight_site_val
weight.code.cc<-dho$weight_cc_code
weight.correction.cc<-dho$weight_cc_val

age<-dho$age
age.code<-dho$age_site_code
age.correction<-dho$age_site_val
age.code.cc<-dho$age_cc_code
age.correction.cc<-dho$age_cc_val

male<-dho$male
male.code<-dho$male_site_code
male.correction<-dho$male_site_val
male.code.cc<-dho$male_cc_code
male.correction.cc<-dho$male_cc_val

hon.audit1<-data.frame(id=dho$id,site="honduras",
                      start.date,start.date.code,start.date.code.cc,start.date.correction,start.date.correction.cc,
                      cd4,cd4.code,cd4.code.cc,cd4.correction,cd4.correction.cc,
                      weight,weight.code,weight.code.cc,weight.correction,weight.correction.cc,
                      age,age.code,age.code.cc,age.correction,age.correction.cc,
                      male,male.code,male.code.cc,male.correction,male.correction.cc)

hon.audit1$weight.code.cc[hon.audit1$id==24]<-5



da1<-read.csv("arg_hiv_reaudit_data_both.csv")
stuff<-read.csv("arg_hiv_reaudit_data.csv")

start.date<-as.Date(stuff$init.date,"%m/%d/%Y")
start.date.code<-da1$init.date_site_code
junk1<-da1$init.date_site_val
junk0<-da1$init.date
start.date.correction<-start.date+(junk1-junk0)
start.date.code.cc<-stuff$init.date_cc_code
start.date.correction.cc<-as.Date(stuff$init.date_cc_val,"%m/%d/%Y")

cd4<-da1$cd4baseline
cd4.code<-da1$cd4baseline_site_code
cd4.correction<-da1$cd4baseline_site_val
cd4.code.cc<-da1$cd4baseline_cc_code
cd4.correction.cc<-da1$cd4baseline_cc_val

weight<-da1$weight
weight.code<-da1$weight_site_code
weight.correction<-da1$weight_site_val
weight.code.cc<-da1$weight_cc_code
weight.correction.cc<-da1$weight_cc_val

age<-da1$age
age.code<-da1$age_site_code
age.correction<-da1$age_site_val
age.code.cc<-da1$age_cc_code
age.correction.cc<-da1$age_cc_val

male<-da1$male
male.code<-da1$male_site_code
male.correction<-da1$male_site_val
male.code.cc<-da1$male_cc_code
male.correction.cc<-da1$male_cc_val

arg1.audit<-data.frame(id=da1$ID,site="argentina",
                      start.date,start.date.code,start.date.code.cc,start.date.correction,start.date.correction.cc,
                      cd4,cd4.code,cd4.code.cc,cd4.correction,cd4.correction.cc,
                      weight,weight.code,weight.code.cc,weight.correction,weight.correction.cc,
                      age,age.code,age.code.cc,age.correction,age.correction.cc,
                      male,male.code,male.code.cc,male.correction,male.correction.cc)




da2<-read.csv("argentina_selfaudit2_results_0.5.csv")

start.date<-as.Date(da2$Start.date..1st.,"%m/%d/%Y")
start.date.code<-da2$Start.date..1st._site_code
start.date.correction<-as.Date(da2$Start.date..1st._site_val,"%m/%d/%Y")

cd4<-da2$CD4..base.
cd4.code<-da2$CD4..base._site_code
cd4.correction<-da2$CD4..base._site_val

weight<-da2$Weight
weight.code<-da2$Weight_site_code
weight.correction<-da2$Weight_site_val

age<-da2$Age
age.code<-da2$Age_site_code
age.correction<-da2$Age_site_val

male<-da2$Male
male.code<-da2$Male_site_code
male.correction<-da2$Male_site_val

arg2.audit<-data.frame(id=da2$Record.ID,site="argentina",
                      start.date,start.date.code,start.date.correction,
                      cd4,cd4.code,cd4.correction,
                      weight,weight.code,weight.correction,
                      age,age.code,age.correction,
                      male,male.code,male.correction)




###  Here I make decisions for each of the conflicts between the DCC and self-audits

## patient 295 needs to be excluded because started non-HAART ART

hon1<-hon.audit1

hon1$start.date.correction[hon1$id==170]<-hon1$start.date.correction.cc[hon1$id==170]
hon1$start.date.code[hon1$id==170]<-hon1$start.date.code.cc[hon1$id==170]

####  Mauricio verified that the true start date is the one recorded in the database, not what the DCC found.
#hon1$start.date.correction[hon1$id==410]<-hon1$start.date.correction.cc[hon1$id==410]
#hon1$start.date.code[hon1$id==410]<-hon1$start.date.code.cc[hon1$id==410]

hon1$age.correction<-ifelse(hon1$age.code.cc==2&hon1$age.code==1,hon1$age.correction.cc,hon1$age.correction)
hon1$age.code<-ifelse(hon1$age.code.cc==2&hon1$age.code==1,hon1$age.code.cc,hon1$age.code)
hon1$age.code[hon1$id==9]<-hon1$weight.code[hon1$id==9]<-1

library(Hmisc)
hon.audit<-upData(hon1, drop=c("start.date.code.cc","start.date.correction.cc","cd4.code.cc","cd4.correction.cc",
                     "weight.code.cc","weight.correction.cc","age.code.cc","age.correction.cc","male.code.cc","male.correction.cc"))



arg1<-arg1.audit

changestuff<-with(arg1, which(start.date.correction.cc != start.date.correction | !is.na(start.date.correction.cc)&is.na(start.date.correction)))
arg1$start.date.correction[changestuff]<-arg1$start.date.correction.cc[changestuff]
arg1$start.date.code[changestuff]<-3

arg1$cd4.correction[arg1$id=="MARGA22121973"]<-arg1$cd4.correction.cc[arg1$id=="MARGA22121973"]

arg1$age.correction[arg1$id=="FSUFR17121968"]<-arg1$age.correction.cc[arg1$id=="FSUFR17121968"]
arg1$age.code[arg1$id=="FSUFR17121968"]<-3

arg1$start.date.correction[arg1$id=="MLAAL06031950"]<-as.Date("2004-03-09")    ## Carina's instructions (8/13/2010)

arg.audit1<-upData(arg1, drop=c("start.date.code.cc","start.date.correction.cc","cd4.code.cc","cd4.correction.cc",
                     "weight.code.cc","weight.correction.cc","age.code.cc","age.correction.cc","male.code.cc","male.correction.cc"))

## Carina's instructions (8/13/2010):
arg2.audit$cd4.correction[arg2.audit$id=="MANGA20111976"]<-302
arg2.audit$cd4.code[arg2.audit$id=="MJORO23041981"]<-1
arg2.audit$cd4.correction[arg2.audit$id=="MJORO23041981"]<-NA
arg2.audit$cd4.code[arg2.audit$id=="FMAPE16041973"]<-1

## My judgement based on Carina's notes.  This id started HAART 2 months earlier than recorded in the database, and Carina was unable to find
## any CD4 measurements.  I am treating this as if the CD4 measurement has a not audited error code (5), not that the true value of CD4 is missing.
## In the analysis, this will default to the original baseline CD4 measurement.
arg2.audit$cd4.code[arg2.audit$id=="MPACA14071970"]<-5
arg2.audit$cd4.correction[arg2.audit$id=="MPACA14071970"]

arg.audit<-rbind(arg.audit1,arg2.audit)

arg.audit$cd4.correction[arg.audit$id=="MMAFE06081967"]<-45  ## Per Steph's instructions
arg.audit$cd4.code[arg.audit$id=="FSUFR17121968"]<-3  ## Per Carina's instruction (8/13/2010)



mex.audit$weight.correction[mex.audit$id=="MX186"]<-NA
mex.audit$weight.code[mex.audit$id=="MX186"]<-1

mex.audit$cd4.code[mex.audit$id=="MX337"]<-2
mex.audit$cd4.correction[mex.audit$id=="MX337"]<-280



########  Now combining all into one dataset

mex.audit$id<-as.character(mex.audit$id)
chile.audit$id<-as.character(chile.audit$id)
arg.audit$id<-as.character(arg.audit$id)
hon.audit$id<-as.character(hon.audit$id)

audit.second<-rbind(mex.audit,chile.audit,hon.audit,arg.audit)


save(audit.second,file="second-audit-results.rda")



####  Some quick diagnostics

with(audit.second,sum(start.date.code==2|start.date.code==3))
with(audit.second,sum(start.date.code!=4|start.date.code!=5))
with(audit.second, sum(start.date.code==2|start.date.code==3)/sum(start.date.code!=4|start.date.code!=5))

with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="argentina"&!is.na(cd4)))
with(audit.second, sum((start.date.code!=4|start.date.code!=5)&site=="argentina"&!is.na(cd4)))
with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="argentina"&!is.na(cd4))/sum((start.date.code!=4|start.date.code!=5)&site=="argentina"&!is.na(cd4)))

with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="chile"&!is.na(cd4)))
with(audit.second, sum((start.date.code!=4|start.date.code!=5)&site=="chile"&!is.na(cd4)))
with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="chile"&!is.na(cd4))/sum((start.date.code!=4|start.date.code!=5)&site=="chile"&!is.na(cd4)))

with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="honduras"&!is.na(cd4)))
with(audit.second, sum((start.date.code!=4|start.date.code!=5)&site=="honduras"&!is.na(cd4)))
with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="honduras"&!is.na(cd4))/sum((start.date.code!=4|start.date.code!=5)&site=="honduras"&!is.na(cd4)))

with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="mexico"&!is.na(cd4)))
with(audit.second, sum((start.date.code!=4|start.date.code!=5)&site=="mexico"&!is.na(cd4)))
with(audit.second, sum((start.date.code==2|start.date.code==3)&site=="mexico"&!is.na(cd4))/sum((start.date.code!=4|start.date.code!=5)&site=="mexico"&!is.na(cd4)))




####################################################################################
#################  Now loading in the old data

rm(list=ls())

load("original-audit-data-ages.rda")
load("second-audit-results.rda")
d<-merge(audit.orig,audit.second,by=c("id","site"),all=TRUE)

#####  Checking that we have the same values for variables
with(d, summary(age.x-age.y))                                                    #yes 
with(d, summary(cd4baseline-cd4))                                                #yes
with(d, summary(weight.x-weight.y))                                              #yes (only difference is Honduras is lbs)
with(d, sum(male.x!=male.y,na.rm=TRUE))                                          #yes 
with(d, sum(as.character(init.date)!=as.character(start.date),na.rm=TRUE))       #yes

d$site[!is.na(d$weight.y) & d$weight.x!=d$weight.y]
summary(d$weight.y[d$site=="honduras"]/2.204623 -d$weight.x[d$site=="honduras"])
d$weight.correction[d$site=="honduras"]<-d$weight.correction[d$site=="honduras"]/2.204623

d$age<-d$age.x
d$male<-d$male.x
d$weight<-d$weight.x

library(Hmisc)
d<-upData(d, drop=c("male.x","male.y","age.x","age.x","age.y","weight.x","weight.y","cd4","start.date",
                    "regimen","stage","aids","risk","route.category","logvl","hemoglobin","init.reg","last.visit","death",
                    "date.death","event","followup","lfup","male.code","male.correction"))



####  In the previous code I had 3 variables:  cd4baseline (database), cd4.correct (charts), and cd4baseline.new (incorporated
#### changes due to an error in regimen start date.  In the latest rounds of audits, we didn't distinguish between which ones
#### were errors because of regimen date change.  Because of this, I am reducing it to two variables:  cd4baseline and
#### cd4.correct, the latter of which designates errors due to the regimen date change.  This doesn't hurt the CD4 by HAART
#### date analysis, but it does mean that the weight by CD4 analysis won't be exactly right because we didn't alter baseline
#### dates when there was an error in date of HAART initiation.

d$s.cd4<-ifelse(d$cd4.correct!=d$cd4baseline,1,d$s.cd4) ## 2 ids with corrected CD4 value weren't marked as s.cd4=1.  Now fixed.
d$cd4.correct<-ifelse(d$cd4.correct!=d$cd4baseline.new & d$s.reg==1 & !is.na(d$s.reg), d$cd4baseline.new, d$cd4.correct)
d$s.cd4<-ifelse(d$cd4.correct!=d$cd4baseline,1,d$s.cd4) ## 6 ids whose CD4 changed because of ART date change now get s.cd4=1

d$s.cd4<-ifelse(is.na(d$cd4.code),d$s.cd4,
                ifelse(d$cd4.code==1&is.na(d$cd4baseline),d$s.cd4,
                ifelse(d$cd4.code==1&!is.na(d$cd4baseline),0,
                ifelse(d$cd4.code==2|d$cd4.code==3,1,d$s.cd4))))
d$v.cd4<-ifelse(!is.na(d$s.cd4),1,0)
d$cd4.correct<-ifelse(d$s.cd4==1&is.na(d$cd4.correct),d$cd4.correction,
                      ifelse(d$s.cd4==0&is.na(d$cd4.correct),d$cd4baseline,
                             d$cd4.correct))


d$s.age<-ifelse(d$age.correct!=d$age,1,d$s.age)
d$age.correct<-ifelse(d$age.correct!=d$age.new & d$s.reg==1 & !is.na(d$s.reg), d$age.new, d$age.correct)
d$s.age<-ifelse(d$age.correct!=d$age,1,d$s.age)

d$s.age<-ifelse(is.na(d$age.code),d$s.age,
                ifelse(d$age.code==1&is.na(d$age),d$s.age,
                ifelse(d$age.code==1&!is.na(d$age),0,
                ifelse(d$age.code==2|d$age.code==3,1,d$s.age))))
d$v.age<-ifelse(!is.na(d$s.age),1,0)
d$age.correct<-ifelse(d$s.age==1&is.na(d$age.correct),d$age.correction,
                      ifelse(d$s.age==0&is.na(d$age.correct),d$age,
                             d$age.correct))


d$v.orig<-d$v.reg

d$s.reg<-ifelse(is.na(d$start.date.code),d$s.reg,
                ifelse(d$start.date.code==1,0,
                ifelse(d$start.date.code==2|d$start.date.code==3,1,d$s.reg)))
d$v.reg<-ifelse(!is.na(d$s.reg),1,0)
d$init.date.correct<-as.Date(ifelse(d$s.reg==1&is.na(d$init.date.correct),as.character(d$start.date.correction),
                             ifelse(d$s.reg==0&is.na(d$init.date.correct),as.character(d$init.date),
                             ifelse(d$s.reg==1&!is.na(d$init.date.correct)&!is.na(d$start.date.correction),
                                    as.character(d$start.date.correction),
                                    as.character(d$init.date.correct)))))


d$cd4.correct<-ifelse(d$v.reg==1 & is.na(d$s.cd4),d$cd4baseline,d$cd4.correct)    ## People with v.reg=1 and s.cd4=NA were being excluded
d$age.correct<-ifelse(d$v.reg==1 & is.na(d$s.age),d$age,d$age.correct)            ## Same thing with age


d$s.wt<-ifelse(is.na(d$weight.code),d$s.wt,
               ifelse(d$weight.code==1&is.na(d$weight),d$s.wt,
               ifelse(d$weight.code==1&!is.na(d$weight),0,
               ifelse(d$weight.code==2|d$weight.code==3,1,d$s.wt))))
d$v.wt<-ifelse(!is.na(d$s.wt),1,0)
d$weight.correct<-ifelse(d$s.wt==1&is.na(d$weight.correct),d$weight.correction,
                  ifelse(d$s.wt==0&is.na(d$weight.correct),d$weight,
                                    d$weight.correct))



#################################################################################

ystar1<-d$cd4baseline
y1=ifelse(d$v.reg==1,d$cd4.correct,ystar1)     ## ignoring whether v.cd4=1 (may need to revisit this)

w1=as.numeric(as.Date(with(d,init.date)))/365.25
w1[d$id=="MX401"]<-NA                          ### started non-HAART regimen so excluded
w1[d$id=="MX4"]<-NA                            ### started non-HAART regimen so excluded
w1[d$id=="295"&d$site=="honduras"]<-NA         ### started non-HAART regimen so excluded

v1=d$v.reg
x1=as.numeric(as.Date(d$init.date.correct))/365.25
s1=d$s.reg

w2<-d$age
x2<-d$age.correct
s2<-d$s.age

#v=ifelse(is.na(s)&v==1,0,v)

### removing those with missing values
remove1<-ifelse(is.na(ystar1)|is.na(y1)|is.na(w1),1,0)
# remove<-ifelse(is.na(ystar1)|is.na(y1)|is.na(w1)|d$id=="FOLPE13061974",1,0)   ## Removing the one outlier
remove<-ifelse(d$site=="haiti",1,remove1)
#remove<-ifelse(d$site!="argentina",1,remove1)

ystar<-sqrt(ystar1[remove==0])
y<-sqrt(y1[remove==0])
#ystar<-ystar1[remove==0]
#y<-y1[remove==0]
w<-w1[remove==0]
v<-v1[remove==0]
x<-x1[remove==0]
s<-s1[remove==0]
site<-d$site[remove==0]
id<-d$id[remove==0]
male<-d$male[remove==0]
w.age<-w2[remove==0]
s.age<-s2[remove==0]
x.age<-x2[remove==0]
v.orig<-d$v.orig[remove==0]

x<-ifelse(v==0,0,x)    ### to prevent errors caused by NA
s<-ifelse(v==0,0,s)    ### to prevent errors caused by NA
x.age<-ifelse(v==0,0,x.age)
s.age<-ifelse(v==0,0,s.age)

save(v,ystar,y,w,x,s,w.age,x.age,s.age,male,site,id,v.orig, file="analysis-data-CCASAnet-audit.rda")

## Data checking/cleaning stuff
#idnew<-id
#sitenew<-site
#load("idstuff.rda")
#idsite<-paste(id,site,sep=",")
#idsitenew<-paste(idnew,sitenew,sep=",")
#setdiff(idsitenew,idsite)
#setdiff(idsite,idsitenew)


N<-length(s)

mod.atten<-lm(ystar~w)

p.est<-mean(s[v==1])
u.est<-w[s*v==1]-x[s*v==1]
var.u<-var(u.est,na.rm=TRUE)#*(sum(s*v,na.rm=TRUE)-1)/sum(s*v,na.rm=TRUE)
var.x<-var(x[v==1],na.rm=TRUE)#*((sum(v)-1)/sum(v))
var.w<-var(w,na.rm=TRUE)
resid.y<-y[v*s==1]-ystar[v*s==1]
resid.x<-x[v*s==1]-w[v*s==1]
cov.est<-cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)])*
         sqrt(var(resid.y,na.rm=TRUE)*var(resid.x,na.rm=TRUE))

sum(resid.y[!is.na(resid.y)&!is.na(resid.x)]*resid.x[!is.na(resid.y)&!is.na(resid.x)])/
  (length(resid.y[!is.na(resid.y)&!is.na(resid.x)])-1)     ## covariance estimated the way we mention in paper.

cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)],method="spearman")*
         sqrt(var(resid.y,na.rm=TRUE)*var(resid.x,na.rm=TRUE))



N
sum(v)
sum(s*v)
p.est
sqrt(var.u)
sqrt(var.x)
cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)])



gamma1<-mod.atten$coeff[2]

fixed<-(gamma1*(var.x+var.u*p.est) - p.est*cov.est)/var.x

  mu.x<-mean(x[v==1])
  mu.w<-mean(w)
  mu.star<-mean(resid.y,na.rm=TRUE)
  
  var.betaN<-summary(mod.atten)$coeff[2,2]^2

    A<-matrix(0,8,8)
    A[1,1]<-1
    A[1,2]<-mean(w)
    A[2,1]<-mean(w)
    A[2,2]<-sum(w^2)/N
    A[3,3]<-sum(v)/N
    A[4,3]<-2*sum(v*(x-mu.x))/N
    A[4,4]<-sum(v)/N
    A[5,5]<-sum(v)/N
    A[6,6]<-sum(s*v)/N
    A[7,7]<-sum(s*v)/N
    A[8,7]<-sum(s*v*(w-x))/N
    A[8,8]<-sum(s*v)/N

    psi<-cbind((ystar-mod.atten$coeff[1]-gamma1*w),
               (ystar-mod.atten$coeff[1]-gamma1*w)*w,
               (x-mu.x)*v,
               ((x-mu.x)^2-var.x)*v,
               (s-p.est)*v,
               ((w-x)^2-var.u)*s*v,
               (ystar-y-mu.star)*s*v,
               ((ystar-y-mu.star)*(w-x)-cov.est)*s*v 
              )
    B<-t(psi)%*%psi/N
    var.theta<-solve(A)%*%B%*%solve(A)/N
    
    dg<-c(0, (var.x+p.est*var.u)/var.x, 0, -(gamma1*var.u*p.est-p.est*cov.est)/var.x^2,
          (gamma1*var.u-cov.est)/var.x, gamma1*p.est/var.x, 0, -p.est/var.x )

    var.fixed<-dg%*%var.theta%*%dg      
  
  lower.fixed<-fixed-1.96*sqrt(var.fixed)
  upper.fixed<-fixed+1.96*sqrt(var.fixed)

  lower.atten<-gamma1-1.96*sqrt(var.betaN)
  upper.atten<-gamma1+1.96*sqrt(var.betaN)

fixed
lower.fixed
upper.fixed

gamma1
lower.atten
upper.atten





par(mar=c(5,5,4,2))
plot(w,ystar,xlab="Date of ART initiation", ylab="CD4 Count",axes=FALSE,cex.lab=2)
box()
axis(2,cex.axis=1.5)
axis(1,as.numeric(as.Date(c("1997-01-01","1999-01-01","2001-01-01","2003-01-01","2005-01-01","2007-01-01")))/365.25,
     c("1997","1999","2001","2003","2005","2007"),cex.axis=1.5)
abline(mod.atten$coeff,col=gray(.5),lwd=3)


summary(u.est)

par(mar=c(5,5,4,2))
hist(u.est,main="Magnitude of ART date Errors (W-X given S=1)",xlab="Difference in Days",
     cex.main=2,cex.lab=2,cex.axis=2,nclass=10)


par(mar=c(5,5,4,2))
plot(-resid.x,-resid.y,cex.axis=2,cex.lab=2,xlab="Date Change (W-X given S=1)",ylab="CD4 Change (Y*-Y given S=1)")





########  Now doing the analysis including site as a covariate


z1<-ifelse(site=="argentina",1,0)
z2<-ifelse(site=="brazil",1,0)
z3<-ifelse(site=="chile",1,0)
z4<-ifelse(site=="honduras",1,0)
z5<-ifelse(site=="mexico",1,0)
z<-cbind(z1,z2,z3,z4,z5)

source("audit-correct-cov.R")

p.cat<-c(.244,.366,.140,.250)
cat<-ifelse(site=="argentina","a",
            ifelse(site=="brazil"|site=="chile"|site=="honduras","bch",
            ifelse(site=="mexico","m",
            ifelse(site=="peru","p",NA))))
poss.cat<-c("a","bch","m","p")
p.cat<-table(cat)/length(cat)

ans<-audit.correct.cov(ystar,w,z,v,x,y)   ### ignoring the fact that we over-sampled some sites
ans1<-audit.correct.cov.vdependent(ystar,w,z,v,x,y,p.cat,cat,poss.cat)    ### accounting for the over-sampling of some sites
naive<-lm(ystar~w+z)

cbind(round(ans$CorrectEst,2),round(ans$CorrectEst-1.96*sqrt(diag(ans$VarCorrectEst)),2),round(ans$CorrectEst+1.96*sqrt(diag(ans$VarCorrectEst)),2))
cbind(round(ans1$CorrectEst,2),round(ans1$CorrectEst-1.96*sqrt(diag(ans1$VarCorrectEst)),2),round(ans1$CorrectEst+1.96*sqrt(diag(ans1$VarCorrectEst)),2))
cbind(round(naive$coeff[-1],2),round(naive$coeff[-1]-1.96*summary(naive)$coeff[-1,2],2),round(naive$coeff[-1]+1.96*summary(naive)$coeff[-1,2],2))


naive.interact<-lm(ystar~w*z)    ## No evidence of interaction between year of HAART initiation and site on baseline CD4
summary(naive.interact)

naive.unadjusted<-lm(ystar~w)

plot(w,ystar,type="n")
points(w[site=="argentina"],ystar[site=="argentina"],pch=1)
points(w[site=="brazil"],ystar[site=="brazil"],pch=2)
points(w[site=="chile"],ystar[site=="chile"],pch=3)
points(w[site=="honduras"],ystar[site=="honduras"],pch=4)
points(w[site=="mexico"],ystar[site=="mexico"],pch=5)
points(w[site=="peru"],ystar[site=="peru"],pch=6)
abline(naive.unadjusted$coeff,col=2)
abline(naive$coeff[c(1,2)],lwd=3, col=2)
       
plot(w,ystar)       
naive.unadjusted<-lm(ystar~w)
abline(naive.unadjusted$coeff,col=2)


#### Results are the same if we consider each site separately for the different categories

cat<-ifelse(site=="argentina","a",
            ifelse(site=="brazil","b",
            ifelse(site=="chile","c",
            ifelse(site=="honduras","h",
            ifelse(site=="mexico","m",
            ifelse(site=="peru","p",NA))))))
poss.cat<-c("a","b","c","h","m","p")
p.cat<-table(cat)/length(cat)

ans2<-audit.correct.cov.vdependent(ystar,w,z,v,x,y,p.cat,cat,poss.cat)  

cbind(round(ans2$CorrectEst,2),round(ans2$CorrectEst-1.96*sqrt(diag(ans2$VarCorrectEst)),2),round(ans2$CorrectEst+1.96*sqrt(diag(ans2$VarCorrectEst)),2))
cbind(round(ans1$CorrectEst,2),round(ans1$CorrectEst-1.96*sqrt(diag(ans1$VarCorrectEst)),2),round(ans1$CorrectEst+1.96*sqrt(diag(ans1$VarCorrectEst)),2))


#####  Adjusting for gender

z.male<-cbind(z,male)

naive.male<-lm(ystar~w+z.male)
ans2.male<-audit.correct.cov.vdependent(ystar,w,z.male,v,x,y,p.cat,cat,poss.cat)  

cbind(round(ans2$CorrectEst,2),round(ans2$CorrectEst-1.96*sqrt(diag(ans2$VarCorrectEst)),2),round(ans2$CorrectEst+1.96*sqrt(diag(ans2$VarCorrectEst)),2))
cbind(round(ans2.male$CorrectEst,2),round(ans2.male$CorrectEst-1.96*sqrt(diag(ans2.male$VarCorrectEst)),2),round(ans2.male$CorrectEst+1.96*sqrt(diag(ans2.male$VarCorrectEst)),2))


##### Sqrt-transformed CD4

naive.s<-lm(sqrt(ystar)~w+z)
ans2.s<-audit.correct.cov.vdependent(sqrt(ystar),w,z,v,x,sqrt(y),p.cat,cat,poss.cat)  

cbind(round(naive.s$coeff[-1],2),round(naive.s$coeff[-1]-1.96*summary(naive.s)$coeff[-1,2],2),round(naive.s$coeff[-1]+1.96*summary(naive.s)$coeff[-1,2],2))
cbind(round(ans2.s$CorrectEst,2),round(ans2.s$CorrectEst-1.96*sqrt(diag(ans2.s$VarCorrectEst)),2),round(ans2.s$CorrectEst+1.96*sqrt(diag(ans2.s$VarCorrectEst)),2))


naive.male.s<-lm(sqrt(ystar)~w+z.male)
ans2.male.s<-audit.correct.cov.vdependent(sqrt(ystar),w,z.male,v,x,sqrt(y),p.cat,cat,poss.cat)  

cbind(round(naive.male.s$coeff[-1],2),round(naive.male.s$coeff[-1]-1.96*summary(naive.male.s)$coeff[-1,2],2),round(naive.male.s$coeff[-1]+1.96*summary(naive.male.s)$coeff[-1,2],2))
cbind(round(ans2.male.s$CorrectEst,2),round(ans2.male.s$CorrectEst-1.96*sqrt(diag(ans2.male.s$VarCorrectEst)),2),round(ans2.male.s$CorrectEst+1.96*sqrt(diag(ans2.male.s$VarCorrectEst)),2))


plot(w,sqrt(ystar))
abline(naive.male.s$coeff[c(1,2)],lwd=3, col=2)
abline(naive.male.s$coeff[1],.18,lwd=2,col=3)
       








###################################################################################
######   Analyses that will be in the paper
###################################################################################

rm(list=ls())
load("analysis-data-CCASAnet-audit.rda")
source("audit-correct-cov.R")


z1<-ifelse(site=="argentina",1,0)
z2<-ifelse(site=="brazil",1,0)
z3<-ifelse(site=="chile",1,0)
z4<-ifelse(site=="honduras",1,0)
z5<-ifelse(site=="mexico",1,0)
z<-cbind(z1,z2,z3,z4,z5)

cat<-ifelse(site=="argentina","a",
            ifelse(site=="brazil","b",
            ifelse(site=="chile","c",
            ifelse(site=="honduras","h",
            ifelse(site=="mexico","m",
            ifelse(site=="peru","p",NA))))))
poss.cat<-c("a","b","c","h","m","p")
p.cat<-table(cat)/length(cat)

mod.naive.full<-lm(ystar~w+w.age+z+male)
summary(mod.naive.full)


ans.full<-audit.correct.cov.vdependent.2plus(ystar,cbind(w,w.age),cbind(z,male),v,cbind(x,x.age),y,p.cat,cat,poss.cat)

cbind(round(mod.naive.full$coeff[-1],2),
      round(mod.naive.full$coeff[-1]-1.96*summary(mod.naive.full)$coeff[-1,2],2),
      round(mod.naive.full$coeff[-1]+1.96*summary(mod.naive.full)$coeff[-1,2],2))

cbind(round(ans.full$CorrectEst,2),
      round(ans.full$CorrectEst-1.96*sqrt(diag(ans.full$VarCorrectEst)),2),
      round(ans.full$CorrectEst+1.96*sqrt(diag(ans.full$VarCorrectEst)),2))


mod.naive.site<-lm(ystar~w+z)
summary(mod.naive.site)

ans.site<-audit.correct.cov.vdependent.2plus(ystar,w,z,v,x,y,p.cat,cat,poss.cat)    ### These aren't exactly the same, even though they should be.
ans.site1<-audit.correct.cov.vdependent(ystar,w,z,v,x,y,p.cat,cat,poss.cat)         ###  However, they are very close to the same.
ans.site.ignore<-audit.correct.cov(ystar,w,z,v,x,y)                  ### Not correct because it ignores the audit-specific sampling,
                                                                     ### although results are similar

cbind(round(mod.naive.site$coeff[-1],2),
      round(mod.naive.site$coeff[-1]-1.96*summary(mod.naive.site)$coeff[-1,2],2),
      round(mod.naive.site$coeff[-1]+1.96*summary(mod.naive.site)$coeff[-1,2],2))

cbind(round(ans.site$CorrectEst,2),
      round(ans.site$CorrectEst-1.96*sqrt(diag(ans.site$VarCorrectEst)),2),
      round(ans.site$CorrectEst+1.96*sqrt(diag(ans.site$VarCorrectEst)),2))

cbind(round(ans.site1$CorrectEst,2),
      round(ans.site1$CorrectEst-1.96*sqrt(diag(ans.site1$VarCorrectEst)),2),
      round(ans.site1$CorrectEst+1.96*sqrt(diag(ans.site1$VarCorrectEst)),2))

cbind(round(ans.site.ignore$CorrectEst,2),
      round(ans.site.ignore$CorrectEst-1.96*sqrt(diag(ans.site.ignore$VarCorrectEst)),2),
      round(ans.site.ignore$CorrectEst+1.96*sqrt(diag(ans.site.ignore$VarCorrectEst)),2))


##### Without covariates
N<-length(s)
mod.atten<-lm(ystar~w)
p.est<-mean(s[v==1])
u.est<-w[s*v==1]-x[s*v==1]
var.u<-var(u.est,na.rm=TRUE)#*(sum(s*v,na.rm=TRUE)-1)/sum(s*v,na.rm=TRUE)
var.x<-var(x[v==1],na.rm=TRUE)#*((sum(v)-1)/sum(v))
var.w<-var(w,na.rm=TRUE)
resid.y<-y[v*s==1]-ystar[v*s==1]
resid.x<-x[v*s==1]-w[v*s==1]
cov.est<-cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)])*
         sqrt(var(resid.y,na.rm=TRUE)*var(resid.x,na.rm=TRUE))
gamma1<-mod.atten$coeff[2]
fixed<-(gamma1*(var.x+var.u*p.est) - p.est*cov.est)/var.x
  mu.x<-mean(x[v==1])
  mu.w<-mean(w)
  mu.star<-mean(resid.y,na.rm=TRUE)
  var.betaN<-summary(mod.atten)$coeff[2,2]^2
    A<-matrix(0,8,8)
    A[1,1]<-1
    A[1,2]<-mean(w)
    A[2,1]<-mean(w)
    A[2,2]<-sum(w^2)/N
    A[3,3]<-sum(v)/N
    A[4,3]<-2*sum(v*(x-mu.x))/N
    A[4,4]<-sum(v)/N
    A[5,5]<-sum(v)/N
    A[6,6]<-sum(s*v)/N
    A[7,7]<-sum(s*v)/N
    A[8,7]<-sum(s*v*(w-x))/N
    A[8,8]<-sum(s*v)/N
    psi<-cbind((ystar-mod.atten$coeff[1]-gamma1*w),
               (ystar-mod.atten$coeff[1]-gamma1*w)*w,
               (x-mu.x)*v,
               ((x-mu.x)^2-var.x)*v,
               (s-p.est)*v,
               ((w-x)^2-var.u)*s*v,
               (ystar-y-mu.star)*s*v,
               ((ystar-y-mu.star)*(w-x)-cov.est)*s*v 
              )
    B<-t(psi)%*%psi/N
    var.theta<-solve(A)%*%B%*%solve(A)/N
    dg<-c(0, (var.x+p.est*var.u)/var.x, 0, -(gamma1*var.u*p.est-p.est*cov.est)/var.x^2,
          (gamma1*var.u-cov.est)/var.x, gamma1*p.est/var.x, 0, -p.est/var.x )
    var.fixed<-dg%*%var.theta%*%dg      
  lower.fixed<-fixed-1.96*sqrt(var.fixed)
  upper.fixed<-fixed+1.96*sqrt(var.fixed)
  lower.atten<-gamma1-1.96*sqrt(var.betaN)
  upper.atten<-gamma1+1.96*sqrt(var.betaN)

fixed
lower.fixed
upper.fixed

gamma1
lower.atten
upper.atten


par(mar=c(5,5,4,2))
plot(w,ystar,xlab="Date of ART initiation", ylab=quote(sqrt(CD4)),axes=FALSE,cex.lab=1.5)
box()
axis(2,cex.axis=1.5)
axis(1,as.numeric(as.Date(c("1997-01-01","1999-01-01","2001-01-01","2003-01-01","2005-01-01","2007-01-01")))/365.25,
     c("1997","1999","2001","2003","2005","2007"),cex.axis=1.5)
abline(mod.atten$coeff,col=gray(.5),lwd=3)


summary(u.est)

postscript("audit-me-pres-corr.eps",width=5,height=5)
par(mar=c(5,5,4,2))
plot(-resid.x,-resid.y,cex.axis=1.5,cex.lab=1.5,xlab="W-X | S=1  (years)",ylab=expression(paste("Y*-Y | S=1",(sqrt(CD4)),sep="  ")),xlim=c(-4,4),ylim=c(-12,12))
dev.off()




















###############################################################################################################################
#################  The original analysis in the paper

rm(list=ls())

load("original-audit-data.rda")
d<-audit.orig

d$cd4baseline<-with(d,ifelse(d$s.cd4==1&!is.na(d$s.cd4),cd4.correct,d$cd4baseline))

ystar1<-with(d,ifelse(v.cd4==1,cd4.correct,cd4baseline)) ### Replacing incorrect CD4 with correct CD4 for those audited.
                                                         ### These errors are assumed independent and CD4 will be the
                                                         ### outcome variable, so the errors have no impact on bias, but it
                                                         ### is better still to include the correct values.
#ystar=with(d,ifelse(is.na(s.reg)|is.na(s.cd4),cd4baseline,
#                    ifelse(s.reg==1|s.cd4==1,cd4.correct,cd4baseline)))
y1=ifelse(d$v.reg==1,d$cd4baseline.new,ystar1)
w1=as.numeric(as.Date(with(d,init.date)))/365.25
w1[d$id=="MX401"]<-NA                          ### started non-HAART regimen so excluded
w1[d$id=="MX4"]<-NA                            ### started non-HAART regimen so excluded
#w1[d3$id=="MX4"]<-NA    ### Previous code used for first manuscript submission which I believe was incorrect, but with minimal impact
v1=d$v.reg
x1=as.numeric(as.Date(d$init.date.correct))/365.25
s1=d$s.reg

#v=ifelse(is.na(s)&v==1,0,v)

### removing those with missing values
remove<-ifelse(is.na(ystar1)|is.na(y1)|is.na(w1),1,0)
# remove<-ifelse(is.na(ystar1)|is.na(y1)|is.na(w1)|d$id=="FOLPE13061974",1,0)   ## Removing the one outlier

ystar<-ystar1[remove==0]
y<-y1[remove==0]
w<-w1[remove==0]
v<-v1[remove==0]
x<-x1[remove==0]
s<-s1[remove==0]

x<-ifelse(v==0,0,x)
s<-ifelse(v==0,0,s)

N<-length(s)

mod.atten<-lm(ystar~w)

p.est<-mean(s[v==1])
u.est<-w[s*v==1]-x[s*v==1]
var.u<-var(u.est,na.rm=TRUE)#*(sum(s*v,na.rm=TRUE)-1)/sum(s*v,na.rm=TRUE)
var.x<-var(x[v==1],na.rm=TRUE)#*((sum(v)-1)/sum(v))
var.w<-var(w,na.rm=TRUE)
resid.y<-y[v*s==1]-ystar[v*s==1]
resid.x<-x[v*s==1]-w[v*s==1]
cov.est<-cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)])*
         sqrt(var(resid.y,na.rm=TRUE)*var(resid.x,na.rm=TRUE))

sum(resid.y[!is.na(resid.y)&!is.na(resid.x)]*resid.x[!is.na(resid.y)&!is.na(resid.x)])/
  (length(resid.y[!is.na(resid.y)&!is.na(resid.x)])-1)     ## covariance estimated the way we mention in paper.

cor(resid.y[!is.na(resid.y)&!is.na(resid.x)],resid.x[!is.na(resid.y)&!is.na(resid.x)],method="spearman")*
         sqrt(var(resid.y,na.rm=TRUE)*var(resid.x,na.rm=TRUE))

gamma1<-mod.atten$coeff[2]

fixed<-(gamma1*(var.x+var.u*p.est) - p.est*cov.est)/var.x

  mu.x<-mean(x[v==1])
  mu.w<-mean(w)
  mu.star<-mean(resid.y,na.rm=TRUE)
  
  var.betaN<-summary(mod.atten)$coeff[2,2]^2

    A<-matrix(0,8,8)
    A[1,1]<-1
    A[1,2]<-mean(w)
    A[2,1]<-mean(w)
    A[2,2]<-sum(w^2)/N
    A[3,3]<-sum(v)/N
    A[4,3]<-2*sum(v*(x-mu.x))/N
    A[4,4]<-sum(v)/N
    A[5,5]<-sum(v)/N
    A[6,6]<-sum(s*v)/N
    A[7,7]<-sum(s*v)/N
    A[8,7]<-sum(s*v*(w-x))/N
    A[8,8]<-sum(s*v)/N

    psi<-cbind((ystar-mod.atten$coeff[1]-gamma1*w),
               (ystar-mod.atten$coeff[1]-gamma1*w)*w,
               (x-mu.x)*v,
               ((x-mu.x)^2-var.x)*v,
               (s-p.est)*v,
               ((w-x)^2-var.u)*s*v,
               (ystar-y-mu.star)*s*v,
               ((ystar-y-mu.star)*(w-x)-cov.est)*s*v 
              )
    B<-t(psi)%*%psi/N
    var.theta<-solve(A)%*%B%*%solve(A)/N
    
    dg<-c(0, (var.x+p.est*var.u)/var.x, 0, -(gamma1*var.u*p.est-p.est*cov.est)/var.x^2,
          (gamma1*var.u-cov.est)/var.x, gamma1*p.est/var.x, 0, -p.est/var.x )

    var.fixed<-dg%*%var.theta%*%dg      
  
  lower.fixed<-fixed-1.96*sqrt(var.fixed)
  upper.fixed<-fixed+1.96*sqrt(var.fixed)

  lower.atten<-gamma1-1.96*sqrt(var.betaN)
  upper.atten<-gamma1+1.96*sqrt(var.betaN)

fixed
lower.fixed
upper.fixed



##############
