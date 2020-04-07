##Produce Table
library(xtable)
helper<-function(results,BIAS,MC_SD,EST_SD,CRATE,sample_index)
{
  results=rbind(results,c(
    MC_SD[sample_index,1],
    (MC_SD[sample_index,2]/MC_SD[sample_index,c(3,5,4)])^2,
    (EST_SD[sample_index,c(3,5,4)]/MC_SD[sample_index,c(3,5,4)])^2,
    CRATE[sample_index,c(3,5,4)]))
  return(results)
}
sample_index=c(1)

#########Table 1
results_one=NULL
#setwd("cont/")
h.degree=0
pt=0.5
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))

results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)

h.degree=0.25
pt=0.5
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)


h.degree=0.5
pt=0.5
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)

h.degree=0.75
pt=0.5
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)

h.degree=0
pt=0.6
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)


h.degree=0
pt=0.7
mis.specified=0
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)

h.degree=0
pt=0.5
mis.specified=1
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)


h.degree=0
pt=0.7
mis.specified=1
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
results_one=helper(results_one,BIAS,MC_SD,EST_SD,CRATE,sample_index)


print(xtable(results_one,digits=c(0,0,rep(3,9))),include.rownames = F)

#########Table 2
results_two=NULL
setwd("..")
setwd("binary/")
rare.degree=0
h.degree=0
pt=0.5
mis.specified=0

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)

rare.degree=0
h.degree=0
pt=0.5
mis.specified=1

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)

rare.degree=-0.85
h.degree=0
pt=0.5
mis.specified=0

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)


rare.degree=-0.85
h.degree=0
pt=0.5
mis.specified=1

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)

rare.degree=0
h.degree=0.75
pt=0.5
mis.specified=0

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)

rare.degree=0
h.degree=0
pt=0.7
mis.specified=0

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)


rare.degree=-1.386
h.degree=0
pt=0.5
mis.specified=0

load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)



rare.degree=-2.197
h.degree=0
pt=0.5
mis.specified=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

results_two=helper(results_two,BIAS_DIFF,MC_SD_DIFF,EST_SD_DIFF,CRATE_DIFF,sample_index)
results_two=helper(results_two,BIAS_RISK,MC_SD_RISK,EST_SD_RISK,CRATE_RISK,sample_index)
results_two=helper(results_two,BIAS_ODDS,MC_SD_ODDS,EST_SD_ODDS,CRATE_ODDS,sample_index)

print(xtable(results_two,digits=c(0,0,rep(3,9))),include.rownames = F)

######################Table 3
sample_index=1:16
Failure_times=NULL
rare.degree=0
h.degree=0
pt=0.5
mis.specified=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
Failure_times=cbind(Failure_times,NON_CONVER[sample_index,])

rare.degree=-0.85
h.degree=0
pt=0.5
mis.specified=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
Failure_times=cbind(Failure_times,NON_CONVER[sample_index,2])

rare.degree=-1.386
h.degree=0
pt=0.5
mis.specified=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
Failure_times=cbind(Failure_times,NON_CONVER[sample_index,2])


rare.degree=-2.197
h.degree=0
pt=0.5
mis.specified=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
Failure_times=cbind(Failure_times,NON_CONVER[sample_index,2])
#Failure_times[,2:5]=Failure_times[,2:5]/20
print(xtable(Failure_times,digits=rep(0,6)),include.rownames = F)
      