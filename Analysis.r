##R 4.0.3
library(MendelianRandomization)
library(TwoSampleMR)
library(dplyr)
library(mr.raps)
library(MRPRESSO)
library(readxl)

outcome<-outcome[outcome$pval.outcome>0.00000005,]
dat <- harmonise_data(e1, outcome, action=2)
result<-data.frame(t(rep(NA,30)))


result[1,1]<-"MR-PRESSO"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
##error: Not enough intrumental variables
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.05){
  result[1,2]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,3]<-exp(presso$`Main MR results`$Sd[2])
  result[1,4]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,5]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,6]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.05){
  result[1,2]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,3]<-exp(presso$`Main MR results`$Sd[1])
  result[1,4]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,5]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,6]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,7]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,8]<-exp(presso$`Main MR results`$Sd[2])
  result[1,9]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,10]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,11]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,7]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,8]<-exp(presso$`Main MR results`$Sd[1])
  result[1,9]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,10]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,11]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)

result[1,1]<-"MR-PRESSO"
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,12]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,13]<-exp(presso$`Main MR results`$Sd[2])
  result[1,14]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,15]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,16]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,12]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,13]<-exp(presso$`Main MR results`$Sd[1])
  result[1,14]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,15]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,16]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
##error: Not enough intrumental variables
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,17]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,18]<-exp(presso$`Main MR results`$Sd[2])
  result[1,19]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,20]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,21]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,17]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,18]<-exp(presso$`Main MR results`$Sd[1])
  result[1,19]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,20]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,21]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,22]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,23]<-exp(presso$`Main MR results`$Sd[2])
  result[1,24]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,25]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,26]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,22]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,23]<-exp(presso$`Main MR results`$Sd[1])
  result[1,24]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,25]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,26]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,27]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,28]<-exp(presso$`Main MR results`$Sd[2])
  result[1,29]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,30]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,31]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,27]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,28]<-exp(presso$`Main MR results`$Sd[1])
  result[1,29]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,30]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,31]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
presso
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue <=0.01){
  result[1,32]<-exp(presso$`Main MR results`$`Causal Estimate`[2])
  result[1,33]<-exp(presso$`Main MR results`$Sd[2])
  result[1,34]<-exp(presso$`Main MR results`$`Causal Estimate`[2]-1.96*presso$`Main MR results`$Sd[2])
  result[1,35]<-exp(presso$`Main MR results`$`Causal Estimate`[2]+1.96*presso$`Main MR results`$Sd[2])
  result[1,36]<-round(presso$`Main MR results`$`P-value`[2],3)
}
if(presso$`MR-PRESSO results`$`Global Test`$Pvalue >0.01){
  result[1,32]<-exp(presso$`Main MR results`$`Causal Estimate`[1])
  result[1,33]<-exp(presso$`Main MR results`$Sd[1])
  result[1,34]<-exp(presso$`Main MR results`$`Causal Estimate`[1]-1.96*presso$`Main MR results`$Sd[1])
  result[1,35]<-exp(presso$`Main MR results`$`Causal Estimate`[1]+1.96*presso$`Main MR results`$Sd[1])
  result[1,36]<-round(presso$`Main MR results`$`P-value`[1],3)
}
rm(presso)


result[2,1]<-"MR-IVW"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,2]<-exp(res$Estimate)
result[2,3]<-res$StdError
result[2,4]<-exp(res$Estimate-1.96*res$StdError)
result[2,5]<-exp(res$Estimate+1.96*res$StdError)
result[2,6]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,7]<-exp(res$Estimate)
result[2,8]<-res$StdError
result[2,9]<-exp(res$Estimate-1.96*res$StdError)
result[2,10]<-exp(res$Estimate+1.96*res$StdError)
result[2,11]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,12]<-exp(res$Estimate)
result[2,13]<-res$StdError
result[2,14]<-exp(res$Estimate-1.96*res$StdError)
result[2,15]<-exp(res$Estimate+1.96*res$StdError)
result[2,16]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,17]<-exp(res$Estimate)
result[2,18]<-res$StdError
result[2,19]<-exp(res$Estimate-1.96*res$StdError)
result[2,20]<-exp(res$Estimate+1.96*res$StdError)
result[2,21]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,22]<-exp(res$Estimate)
result[2,23]<-res$StdError
result[2,24]<-exp(res$Estimate-1.96*res$StdError)
result[2,25]<-exp(res$Estimate+1.96*res$StdError)
result[2,26]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,27]<-exp(res$Estimate)
result[2,28]<-res$StdError
result[2,29]<-exp(res$Estimate-1.96*res$StdError)
result[2,30]<-exp(res$Estimate+1.96*res$StdError)
result[2,31]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[2,32]<-exp(res$Estimate)
result[2,33]<-res$StdError
result[2,34]<-exp(res$Estimate-1.96*res$StdError)
result[2,35]<-exp(res$Estimate+1.96*res$StdError)
result[2,36]<-round(res$Pvalue,3)
rm(res)


result[3,1]<-"MR-Egger"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
#method requires data on >2 variants.
res
result[3,2]<-exp(res$Estimate)
result[3,3]<-res$StdError.Est
result[3,4]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,5]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,6]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,7]<-exp(res$Estimate)
result[3,8]<-res$StdError.Est
result[3,9]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,10]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,11]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,12]<-exp(res$Estimate)
result[3,13]<-res$StdError.Est
result[3,14]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,15]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,16]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,17]<-exp(res$Estimate)
result[3,18]<-res$StdError.Est
result[3,19]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,20]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,21]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,22]<-exp(res$Estimate)
result[3,23]<-res$StdError.Est
result[3,24]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,25]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,26]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,27]<-exp(res$Estimate)
result[3,28]<-res$StdError.Est
result[3,29]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,30]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,31]<-round(res$Pvalue.Est,3)
rm(res)
dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_egger(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome))
res
result[3,32]<-exp(res$Estimate)
result[3,33]<-res$StdError.Est
result[3,34]<-exp(res$Estimate-1.96*res$StdError.Est)
result[3,35]<-exp(res$Estimate+1.96*res$StdError.Est)
result[3,36]<-round(res$Pvalue.Est,3)
rm(res)


result[4,1]<-"Weighted median"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
##Method requires data on >2 variants.
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,2]<-exp(res$Estimate)
result[4,3]<-res$StdError
result[4,4]<-exp(res$Estimate-1.96*res$StdError)
result[4,5]<-exp(res$Estimate+1.96*res$StdError)
result[4,6]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,7]<-exp(res$Estimate)
result[4,8]<-res$StdError
result[4,9]<-exp(res$Estimate-1.96*res$StdError)
result[4,10]<-exp(res$Estimate+1.96*res$StdError)
result[4,11]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,12]<-exp(res$Estimate)
result[4,13]<-res$StdError
result[4,14]<-exp(res$Estimate-1.96*res$StdError)
result[4,15]<-exp(res$Estimate+1.96*res$StdError)
result[4,16]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,17]<-exp(res$Estimate)
result[4,18]<-res$StdError
result[4,19]<-exp(res$Estimate-1.96*res$StdError)
result[4,20]<-exp(res$Estimate+1.96*res$StdError)
result[4,21]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,22]<-exp(res$Estimate)
result[4,23]<-res$StdError
result[4,24]<-exp(res$Estimate-1.96*res$StdError)
result[4,25]<-exp(res$Estimate+1.96*res$StdError)
result[4,26]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,27]<-exp(res$Estimate)
result[4,28]<-res$StdError
result[4,29]<-exp(res$Estimate-1.96*res$StdError)
result[4,30]<-exp(res$Estimate+1.96*res$StdError)
result[4,31]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_median(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"weighted")
res
result[4,32]<-exp(res$Estimate)
result[4,33]<-res$StdError
result[4,34]<-exp(res$Estimate-1.96*res$StdError)
result[4,35]<-exp(res$Estimate+1.96*res$StdError)
result[4,36]<-round(res$Pvalue,3)
rm(res)



result[5,1]<-"Remove lipid traits (IVW)"


dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,2]<-exp(res$Estimate)
result[5,3]<-res$StdError
result[5,4]<-exp(res$Estimate-1.96*res$StdError)
result[5,5]<-exp(res$Estimate+1.96*res$StdError)
result[5,6]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,7]<-exp(res$Estimate)
result[5,8]<-res$StdError
result[5,9]<-exp(res$Estimate-1.96*res$StdError)
result[5,10]<-exp(res$Estimate+1.96*res$StdError)
result[5,11]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,12]<-exp(res$Estimate)
result[5,13]<-res$StdError
result[5,14]<-exp(res$Estimate-1.96*res$StdError)
result[5,15]<-exp(res$Estimate+1.96*res$StdError)
result[5,16]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,17]<-exp(res$Estimate)
result[5,18]<-res$StdError
result[5,19]<-exp(res$Estimate-1.96*res$StdError)
result[5,20]<-exp(res$Estimate+1.96*res$StdError)
result[5,21]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,22]<-exp(res$Estimate)
result[5,23]<-res$StdError
result[5,24]<-exp(res$Estimate-1.96*res$StdError)
result[5,25]<-exp(res$Estimate+1.96*res$StdError)
result[5,26]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,27]<-exp(res$Estimate)
result[5,28]<-res$StdError
result[5,29]<-exp(res$Estimate-1.96*res$StdError)
result[5,30]<-exp(res$Estimate+1.96*res$StdError)
result[5,31]<-round(res$Pvalue,3)
rm(res)
dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
res <- MendelianRandomization::mr_ivw(mr_input(dat$beta.exposure,dat$se.exposure,dat$beta.outcome ,dat$se.outcome),"random")
res
result[5,32]<-exp(res$Estimate)
result[5,33]<-res$StdError
result[5,34]<-exp(res$Estimate-1.96*res$StdError)
result[5,35]<-exp(res$Estimate+1.96*res$StdError)
result[5,36]<-round(res$Pvalue,3)
rm(res)

exposure<-read_exposure_data("1_1.txt",sep="\t")
beta<-data.frame(exposure$SNP, exposure$beta.exposure)
colnames(beta)[1]<-"SNP"
se<-data.frame(exposure$SNP, exposure$se.exposure)
colnames(se)[1]<-"SNP"
for (i in 2:7){
   if(i==7){
     outcome<-read_outcome_data(paste0(i,"_1.txt"),sep="\t")
     outcome<-outcome[outcome$pval.outcome>0.00000005,]
   }
  else{
    outcome<-read_outcome_data(paste0(i,"_1.txt"),sep="\t")
  }
    dat <- harmonise_data(exposure, outcome, action=2)
    dat<-dat[dat$mr_keep==T,]
    beta2<-merge(beta,dat,by="SNP",sort = F)
    site1<-grep("^beta.outcome$", colnames(beta2))
    beta<-beta2[,c(c(1:i),site1)]
    colnames(beta)[i+1]<-i
    se2<-merge(se,dat,by="SNP",sort = F)
    site2<-grep("^se.outcome$", colnames(se2))
    se<-se2[c(c(1:i),site2)]
    colnames(se)[i+1]<-i
    
}
write.table(beta,"beta_mmv1.txt",sep="\t",quote = F,row.names = F)
write.table(se,"se_mmv1.txt",sep="\t",quote = F,row.names = F)
# column 1:acarot; column 2:bcarot; column 3:lutein;column 4:lycop;column 5:zeax; column 6:t2d

result[6,1]<-"multivariable MR-IVW"
mvb<-read.csv("beta_mmv1.txt",sep="\t")
mvbse<-read.csv("se_mmv1.txt",sep="\t")
mv<-mr_mvinput(as.matrix(mvb[,c(2:7)]),as.matrix(mvbse[,c(2:7)]),mvb[,c(8)],mvbse[,c(8)])
res<-mr_mvivw(mv)
res
result[6,2]<-exp(res$Estimate[1])
result[6,3]<-res$StdError[1]
result[6,4]<-exp(res$Estimate[1]-1.96*res$StdError[1])
result[6,5]<-exp(res$Estimate[1]+1.96*res$StdError[1])
result[6,6]<-round(res$Pvalue[1],3)
result[6,7]<-exp(res$Estimate[2])
result[6,8]<-res$StdError[2]
result[6,9]<-exp(res$Estimate[2]-1.96*res$StdError[2])
result[6,10]<-exp(res$Estimate[2]+1.96*res$StdError[2])
result[6,11]<-round(res$Pvalue[2],3)
result[6,12]<-exp(res$Estimate[3])
result[6,13]<-res$StdError[3]
result[6,14]<-exp(res$Estimate[3]-1.96*res$StdError[3])
result[6,15]<-exp(res$Estimate[3]+1.96*res$StdError[3])
result[6,16]<-round(res$Pvalue[3],3)
result[6,17]<-exp(res$Estimate[4])
result[6,18]<-res$StdError[4]
result[6,19]<-exp(res$Estimate[4]-1.96*res$StdError[4])
result[6,20]<-exp(res$Estimate[4]+1.96*res$StdError[4])
result[6,21]<-round(res$Pvalue[4],3)
result[6,22]<-exp(res$Estimate[5])
result[6,23]<-res$StdError[5]
result[6,24]<-exp(res$Estimate[5]-1.96*res$StdError[5])
result[6,25]<-exp(res$Estimate[5]+1.96*res$StdError[5])
result[6,26]<-round(res$Pvalue[5],3)
result[6,32]<-exp(res$Estimate[6])
result[6,33]<-res$StdError[6]
result[6,34]<-exp(res$Estimate[6]-1.96*res$StdError[6])
result[6,35]<-exp(res$Estimate[6]+1.96*res$StdError[6])
result[6,36]<-round(res$Pvalue[6],3)

exposure<-read_exposure_data("1_2.txt",sep="\t")

beta<-data.frame(exposure$SNP, exposure$beta.exposure)
colnames(beta)[1]<-"SNP"

se<-data.frame(exposure$SNP, exposure$se.exposure)
colnames(se)[1]<-"SNP"

for (i in 2:10 ){
  if(i==10){
    outcome<-read_outcome_data(paste0(i,"_2.txt"),sep="\t")
    outcome<-outcome[outcome$pval.outcome>0.00000005,]
  }
  else{
    outcome<-read_outcome_data(paste0(i,"_2.txt"),sep="\t")
  }
  dat <- harmonise_data(exposure, outcome, action=2)
    dat<-dat[dat$mr_keep==T,]
    beta2<-merge(beta,dat,by="SNP",sort = F)
    site1<-grep("^beta.outcome$", colnames(beta2))
    beta<-beta2[,c(c(1:i),site1)]
    colnames(beta)[i+1]<-i
    se2<-merge(se,dat,by="SNP",sort = F)
    site2<-grep("^se.outcome$", colnames(se2))
    se<-se2[c(c(1:i),site2)]
    colnames(se)[i+1]<-i
  
}



write.table(beta,"beta_mmv2.txt",sep="\t",quote = F,row.names = F)
write.table(se,"se_mmv2.txt",sep="\t",quote = F,row.names = F)
# column 1:acarot; column 2:bcarot; column 3:lutein;column 4:lycop;column 5:zeax;column 6: hdl;column 7:ldl;column 8: tg;colum 9:t2d
exposure<-read_exposure_data("1_3.txt",sep="\t")
beta<-data.frame(exposure$SNP, exposure$beta.exposure)
colnames(beta)[1]<-"SNP"
se<-data.frame(exposure$SNP, exposure$se.exposure)
colnames(se)[1]<-"SNP"
for (i in 2:5 ){
  if(i==5){
    outcome<-read_outcome_data(paste0(i,"_3.txt"),sep="\t")
    outcome<-outcome[outcome$pval.outcome>0.00000005,]
  }
  else{
    outcome<-read_outcome_data(paste0(i,"_3.txt"),sep="\t")
  }
  dat <- harmonise_data(exposure, outcome, action=2)
  dat<-dat[dat$mr_keep==T,]
  beta2<-merge(beta,dat,by="SNP",sort = F)
  site1<-grep("^beta.outcome$", colnames(beta2))
  beta<-beta2[,c(c(1:i),site1)]
  colnames(beta)[i+1]<-i
  se2<-merge(se,dat,by="SNP",sort = F)
  site2<-grep("^se.outcome$", colnames(se2))
  se<-se2[c(c(1:i),site2)]
  colnames(se)[i+1]<-i
  
}

result[7,1]<-"multivariable MR-IVW(adjust lipids)"
mvb<-read.csv("beta_mmv2.txt",sep="\t")
mvbse<-read.csv("se_mmv2.txt",sep="\t")
mv<-mr_mvinput(as.matrix(mvb[,c(2:10)]),as.matrix(mvbse[,c(2:10)]),mvb[,c(11)],mvbse[,c(11)])
res<-mr_mvivw(mv)
res
result[7,2]<-exp(res$Estimate[1])
result[7,3]<-res$StdError[1]
result[7,4]<-exp(res$Estimate[1]-1.96*res$StdError[1])
result[7,5]<-exp(res$Estimate[1]+1.96*res$StdError[1])
result[7,6]<-round(res$Pvalue[1],3)
result[7,7]<-exp(res$Estimate[2])
result[7,8]<-res$StdError[2]
result[7,9]<-exp(res$Estimate[2]-1.96*res$StdError[2])
result[7,10]<-exp(res$Estimate[2]+1.96*res$StdError[2])
result[7,11]<-round(res$Pvalue[2],3)
result[7,12]<-exp(res$Estimate[3])
result[7,13]<-res$StdError[3]
result[7,14]<-exp(res$Estimate[3]-1.96*res$StdError[3])
result[7,15]<-exp(res$Estimate[3]+1.96*res$StdError[3])
result[7,16]<-round(res$Pvalue[3],3)
result[7,17]<-exp(res$Estimate[4])
result[7,18]<-res$StdError[4]
result[7,19]<-exp(res$Estimate[4]-1.96*res$StdError[4])
result[7,20]<-exp(res$Estimate[4]+1.96*res$StdError[4])
result[7,21]<-round(res$Pvalue[4],3)
result[7,22]<-exp(res$Estimate[5])
result[7,23]<-res$StdError[5]
result[7,24]<-exp(res$Estimate[5]-1.96*res$StdError[5])
result[7,25]<-exp(res$Estimate[5]+1.96*res$StdError[5])
result[7,26]<-round(res$Pvalue[5],3)
result[7,32]<-exp(res$Estimate[6])
result[7,33]<-res$StdError[6]
result[7,34]<-exp(res$Estimate[6]-1.96*res$StdError[6])
result[7,35]<-exp(res$Estimate[6]+1.96*res$StdError[6])
result[7,36]<-round(res$Pvalue[6],3)
mvb<-read.csv("beta_mmv3.txt",sep="\t")
mvbse<-read.csv("se_mmv3.txt",sep="\t")
mv<-mr_mvinput(as.matrix(mvb[,c(2:5)]),as.matrix(mvbse[,c(2:5)]),mvb[,c(6)],mvbse[,c(6)])
res<-mr_mvivw(mv)
res
result[7,27]<-exp(res$Estimate[1])
result[7,28]<-res$StdError[1]
result[7,29]<-exp(res$Estimate[1]-1.96*res$StdError[1])
result[7,30]<-exp(res$Estimate[1]+1.96*res$StdError[1])
result[7,31]<-round(res$Pvalue[1],3)
write.csv(result,"result20220422.csv",quote = F)



##pleiotropy test
result<-data.frame()
result[1,1]<-"MR-Egger regression test"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,2]<-egger$b_i
result[1,3]<-egger$se_i
result[1,4]<-egger$pval_i

dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,5]<-egger$b_i
result[1,6]<-egger$se_i
result[1,7]<-egger$pval_i

dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,8]<-egger$b_i
result[1,9]<-egger$se_i
result[1,10]<-egger$pval_i

dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,11]<-egger$b_i
result[1,12]<-egger$se_i
result[1,13]<-egger$pval_i

dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,14]<-egger$b_i
result[1,15]<-egger$se_i
result[1,16]<-egger$pval_i


dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,17]<-egger$b_i
result[1,18]<-egger$se_i
result[1,19]<-egger$pval_i

dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
egger<- mr_egger_regression(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome,T)
egger
result[1,20]<-egger$b_i
result[1,21]<-egger$se_i
result[1,22]<-egger$pval_i

##Q test

result[2,1]<-"MR-PRESSO Global Test"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,4]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue
dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,7]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue
dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,10]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue
dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,13]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue

dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,16]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue
dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,19]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue
dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
presso<-mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure ="se.exposure", data= dat,OUTLIERtest = T, DISTORTIONtest =T,NbDistribution = 1000,  SignifThreshold = 0.05)
##Not enough intrumental variables
presso
result[2,22]<-presso$`MR-PRESSO results`$`Global Test`$Pvalue

result[3,1]<-"Cochran??s Q test"
dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,4]<-IVW$Q_pval

dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,7]<-IVW$Q_pval

dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,10]<-IVW$Q_pval


dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,13]<-IVW$Q_pval

dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,16]<-IVW$Q_pval


dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,19]<-IVW$Q_pval

dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
IVW <- TwoSampleMR::mr_ivw(dat$beta.exposure,dat$beta.outcome,dat$se.exposure ,dat$se.outcome)
result[3,22]<-IVW$Q_pval

write.csv(result,"pleiotropy_test_1114.csv",quote = F)

dat <- harmonise_data(e1, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"??-carotene"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e2, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"??-carotene"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e3, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"??-cryptoxanthin"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e4, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"lutein"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e5, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"lycopene"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e7, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"zeaxanthin"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 

dat <- harmonise_data(e6, outcome, action=2)
dat<-dat[dat$mr_keep==T,]
input<-mr_input(dat$beta.exposure,dat$se.exposure ,dat$beta.outcome,dat$se.outcome)
input@exposure<-"total carotenoids"
input@outcome<-"type 2 diabetes"
mr_plot(input, line="ivw",interactive=FALSE,labels = F) 


test<-data.frame(test$MRCid_IAgen4,test$age_censor)
colnames(test)<-c("MRCid_IAgen4","age_censor")
library(SUMnlmr)
library(readstata13)
library(readxl)
trait<-unique(effect_size$trait)
trait
variables<-colnames(mydata)
phenotypes<-c("carot_alpha_carotene_c","carot_beta_carotene_c","carot_beta_cryptoxanthin_c","carot_lutein_c","carot_lycopene_c","carot_total_carotenoids_c","carot_zeaxanthin_c"     )
mydata$diabetes<-NA
mydata$diabetes[mydata$dmstatus_ver=="Incident case"]<-1
table(mydata$diabetes)
mydata$diabetes[mydata$dmstatus_ver=="Prevalent case"]<-NA
table(mydata$diabetes)
mydata$diabetes[mydata$dmstatus_ver=="Incident post-censor"]<-1
mydata$diabetes[mydata$dmstatus_ver=="Non-case"]<-0
table(mydata$diabetes)
coln<-colnames(mydata)
s1<-grep("^gwas_c1$",coln)
e1<-grep("gwas_c10",coln)
s2<-grep("^corex_combined_c1$",coln)
e2<-grep("corex_combined_c10",coln)

sum(is.na(mydata[,s1]))
for (i in 1:nrow(mydata)) {
  if(is.na(mydata[i,s1])) mydata[i,s1:e1]<-mydata[i,s2:e2]
}


covariates<-as.data.frame(cbind(mydata$age_censor,mydata$centre,mydata$sex, mydata$gwas_c1,mydata$gwas_c2,mydata$gwas_c3,mydata$gwas_c4,mydata$gwas_c5,mydata$gwas_c6,mydata$gwas_c7,mydata$gwas_c8,mydata$gwas_c9,mydata$gwas_c10))


result<-data.frame()
result2<-data.frame()
m<-1
for (i in 1:length(trait)) {
  trait_beta<-effect_size[effect_size$trait==trait[i],]
  trait_beta2<-merge(trait_beta,snp_info,by="SNP")
  count<-c(rep(0,length(trait)))
  ## GRS calculation  
  mydata[as.character(trait[i])] <- NA
  mydata2<-data.frame(rep(NA,nrow(mydata)))
  ## A1:0, A2:1, effect allele:A2
  for (j in 1:nrow(trait_beta2)) {
    ## Keep the direction of beta 
    if( toupper(trait_beta2$effect_allele[j])== trait_beta2$A2[j] &&toupper(trait_beta2$other_allele[j])== trait_beta2$A1[j]  && abs(trait_beta2$eaf[j]-trait_beta2$FreqA2_corex[j])<0.2 ){
      trait_beta2$beta_correct[j]<- trait_beta2$beta[j]
    }
    ## reverse the direction of beta 
    else if( toupper(trait_beta2$effect_allele[j])== trait_beta2$A1[j] &&toupper(trait_beta2$other_allele[j])== trait_beta2$A2[j]  && abs(1-trait_beta2$eaf[j]-trait_beta2$FreqA2_corex[j])<0.2 ){
      trait_beta2$beta_correct[j]<- -trait_beta2$beta[j]
    }
    ## Fail to merge
    else{
      count[i]<-count[i]+1
    }
    site<-grep(paste0("^",trait_beta2$SNP[j],"$"),variables)
    mydata2[,j]<-mydata[,site]*trait_beta2$beta_correct[j]
  }
  mydata[as.character(trait[i])] <- rowMeans(mydata2,na.rm = T)
  mydata3<-cbind(mydata$diabetes,scale(log(mydata[as.character(phenotypes[i])]),T,T),mydata[as.character(trait[i])],covariates)
  mydata3<-mydata3[complete.cases(mydata3),]
  ## y,x,g,c
  summ_bin<-create_nlmr_summary(y = mydata3[,1],
                                x = mydata3[,2],
                                g = mydata3[,3],
                                covar = as.matrix(mydata3[,4:16]) ,
                                family = "binomial",
                                q = 10)
  plm<-with( summ_bin$summary, piecewise_summ_mr(by, bx, byse, bxse, xmean, xmin,xmax, 
                                           ci="bootstrap_se",
                                           nboot=1000, 
                                           fig=TRUE,
                                           family="binomial",
                                           ci_fig="ribbon")
  )
  
  result[i,1]<-as.character(phenotypes[i])
  result[i,2]<-as.character(trait[i])
  result[i,3]<-nrow(mydata3)

  for (j in 1:10) {
    result[i,j+3]<-paste0(round(exp(plm$lace[j,1]),2),"(",round(exp(plm$lace[j,3]),2),"-",round(exp(plm$lace[j,4]),2),")")
    #result[i,j*3+2]<-plm$lace[j,2]
    #result[i,j*3+3]<-plm$lace[j,5]

    result2[m,1]<-as.character(phenotypes[i])
    result2[m,2]<-as.character(trait[i])
    result2[m,3]<-nrow(mydata3)
    result2[m,4]<-j
    result2[m,5]<-exp(plm$lace[j,1])
    result2[m,6]<-exp(plm$lace[j,3])
    result2[m,7]<-exp(plm$lace[j,4])
    result2[m,8]<-plm$lace[j,5]
    m<-m+1
  }
  result[i,j+4]<-plm$p_tests[2]
  rm(plm, summ_bin)
}
write.csv(result,"nlmr_result.csv",quote = F,row.names = F)
write.csv(result2,"nlmr_result_plot.csv",quote = F,row.names = F)
