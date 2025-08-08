#******************************************************************************#
#******************************************************************************#
#** Randomization inference for before-and-after studies with multiple units **# 
#********** An application to a criminal procedure reform in Uruguay **********#
#******************************************************************************#
#******************************************************************************#

#* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */

#* Replication Code */
rm(list = ls())
library(readstata13)
library(dplyr)
library(xtable)

source("CDT_functions.R")

#******************************************************************************#
#*********************************** TABLE 1 **********************************#
#******************************************************************************#
dd =  read.dta13("Data_daily.dta") 
t.test(dd$theft[dd$new_code==1]   ,dd$theft[dd$new_code==0])
t.test(dd$robbery[dd$new_code==1] ,dd$robbery[dd$new_code==0])
t.test(dd$dv[dd$new_code==1]      ,dd$dv[dd$new_code==0])
t.test(dd$other[dd$new_code==1]   ,dd$other[dd$new_code==0])
t.test(dd$crime[dd$new_code==1]   ,dd$crime[dd$new_code==0])


#******************************************************************************#
#*********************************** SET UP **********************************#
#******************************************************************************#

# Setup 
dataALL = read.dta13("Data_daily_barrio.dta")
dim(dataALL)

# Keep smaller dataset for the runs that only use observations within windows
data = dataALL[abs(dataALL$score) <=300,]
dim(data)

path = "./tables-and-plots/"

seed = 1345
N = 10000
N_CI = 10000
Wlist = c(1,7,14)

#******************************************************************************#
#*********************************** TABLE 2 **********************************# 
#******************************************************************************#

thetas = seq(-2,2,by=0.01)
rr = data.frame(W = rep(NA,length(Wlist)), Sobs = rep(NA,length(Wlist)), MeanCo =  rep(NA,length(Wlist)), 
                     pval =rep(NA,length(Wlist)), CI_lo = rep(NA,length(Wlist)), CI_up = rep(NA,length(Wlist)))

#------------------------#
# Assignment mechanism 1
#------------------------#
results = rr
for(j in 1:length(Wlist)) {
  out = BArandinf_fast( Y=data$crime, time = data$score, unit= data$barrio , w = Wlist[j], method = "TR", seed=seed, statistic="diffmeans",
                   ConfInt = TRUE, thetas = thetas, N=N, N_CI=N_CI)  
  results[j,"pval"] = out$pval
  results[j,"W"] = Wlist[j]
  results[j,"Sobs"] = out$Sobs
  results[j,"MeanCo"] = out$meanCo
  results[j,"CI_lo"] = out$CI[1]
  results[j,"CI_up"] = out$CI[2]
}
results_AM1 = results
print(results_AM1)

#------------------------#
# Assignment mechanism 2: AT 
#------------------------#
results = rr
for(j in 1:length(Wlist)) {
  if(Wlist[j]==1) next
  a = c(-(Wlist[j]-1):0)
  print(a)
  cat("Window: ", Wlist[j], "--A set: ",a, "\n")
  out = BArandinf_fast( Y=data$crime, time = data$score, unit= data$barrio , w = Wlist[j], method = "AT", seed=seed, statistic="diffmeans",
                   a=a, ConfInt = TRUE, thetas = thetas, N=N, N_CI=N_CI)  
  results[j,"pval"] = out$pval
  results[j,"W"] = Wlist[j]
  results[j,"Sobs"] = out$Sobs
  results[j,"MeanCo"] = out$meanCo
  results[j,"CI_lo"] = out$CI[1]
  results[j,"CI_up"] = out$CI[2]
}
results_AM2_101= results
print(results_AM2_101)

#------------------------#
# Combine in single table
#------------------------#
#load(file=paste(path,"results_Table2.RData",sep=""))
resultsTab2= cbind(results_AM1,results_AM2_101[,4:6])

resultsTab2 = cbind(results_AM1[,1:4],CI=paste("[",results_AM1[,"CI_lo"], ",", results_AM1[,"CI_up"], "]", sep=""),
                          results_AM2_101[,4],CI=paste("[",results_AM2_101[,"CI_lo"], ",", results_AM2_101[,"CI_up"], "]", sep="")
)
save(resultsTab2,file=paste(path,"results_Table2.RData",sep=""))
print(resultsTab2)
resultsTab2[,1]
resultsTab2[1,1] ="1 day"
resultsTab2[2,1] ="7 days"
resultsTab2[3,1] ="14 days"
print(xtable(resultsTab2, digits=3), include.rownames =FALSE)

#******************************************************************************#
#*********************************** TABLE 3 **********************************# 
#******************************************************************************#
thetas = seq(-2,2,by=0.01)
Jlist = c(7,14)
rr = data.frame(W1obs = rep(NA,length(Jlist)), pval1 =rep(NA,length(Jlist)) , 
                W2obs =  rep(NA,length(Jlist)), pval2 =rep(NA,length(Jlist)) ,
                W3obs =  rep(NA,length(Jlist)), pval3 =rep(NA,length(Jlist)))
#------------------------#
# Assignment mechanism 1
#------------------------#
results = rr
for(j in 1:length(Jlist)) {
  out = BArandinf_Joint(Y=data$crime, time = data$score, unit= data$barrio, J=Jlist[j], method = "TR", a = NULL, N=N, seed=seed)
  
  results[j,"W1obs"] =  out$W1obs
  results[j,"W2obs"] =  out$W2obs
  results[j,"W3obs"] =  out$W3obs
  results[j,"pval1"] =  out$pval1
  results[j,"pval2"] =  out$pval2
  results[j,"pval3"] =  out$pval3
}

names = c("First 7 windows TR", "First 14 windows TR")
results_AM1 = cbind(names,results)
print(results_AM1)

#------------------------#
# Assignment mechanism 2: persistent, cutoffs -1,0,1
#------------------------#
results = rr

for(j in 1:length(Jlist)) {
  
    a = c(-(Jlist[j]-1):0)
    print(a)
    cat("Window: ", Jlist[j], "--A set: ",a, "\n")
  
    out = BArandinf_Joint(Y=data$crime, time = data$score, unit= data$barrio, J=Jlist[j], method = "AT", a = a, N=N, seed=seed) 
    
 
    results[j,"W1obs"] =  out$W1obs
    results[j,"W2obs"] =  out$W2obs
    results[j,"W3obs"] =  out$W3obs
    results[j,"pval1"] =  out$pval1
    results[j,"pval2"] =  out$pval2
    results[j,"pval3"] =  out$pval3
}

names = c("First 7 windows AT", "First 14 windows AT")
results_AM2_101= cbind(names,results)
print(results_AM2_101)

#------------------------#
# Combine in single table
#------------------------#
#load(file=paste(path,"results_Table3.RData",sep=""))
resultsTab3= rbind(results_AM1,results_AM2_101)

save(resultsTab3,file=paste(path,"results_Table3.RData",sep=""))
print(resultsTab3)
print(xtable(resultsTab3, digits=3), include.rownames =FALSE)

#******************************************************************************#
#*********************************** TABLE 4 **********************************# 
#* Placebo tests for Nov 1 in 2015, 2016 and 2018
#******************************************************************************#

dates = c("2015-11-01","2016-11-01","2018-11-01")
rr = data.frame(W = rep(NA,length(Wlist)), Sobs = rep(NA,length(Wlist)), pval =rep(NA,length(Wlist)))

#------------------------#
# Assignment mechanism 1
#------------------------#
results = cbind(rr,rr,rr)
for(i in 1:length(dates)) {
  # Create placebo score and placebo treatment assignment
  placebo_date = dates[i]
  cutoff = dataALL$score[dataALL$date == placebo_date] 
  score_new = dataALL$score - cutoff
  score_new[score_new >=0] =score_new [score_new >=0] + 1  # take to -2,-1,1,2
  new_code_new = as.numeric(score_new >= 0 )
  
  for(j in 1:length(Wlist)) {
    k= 1 + (i-1) * 3
    out = BArandinf_fast( Y=dataALL$crime, time = score_new, unit= dataALL$barrio , w = Wlist[j], method = "TR", seed=seed, statistic="diffmeans",
                     ConfInt = FALSE, N=N)  
    results[j,k] = Wlist[j]
    results[j,k+1] = out$Sobs
    results[j,k+2] = out$pval
  }
}
results_AM1 = results[,-c(4,7)]
print(results_AM1)
#------------------------#
# Assignment mechanism 2
#------------------------#
results = cbind(rr,rr,rr)
for(i in 1:length(dates)) {
  # Create placebo score and placebo treatment assignment
  placebo_date = dates[i]
  cutoff = dataALL$score[dataALL$date == placebo_date] 
  score_new = dataALL$score - cutoff
  score_new[score_new >=0] =score_new [score_new >=0] + 1  # take to -2,-1,1,2
  new_code_new = as.numeric(score_new >= 0 )
  
  for(j in 1:length(Wlist)) {
    if(Wlist[j] == 1) next
    a = c(-(Wlist[j]-1):0)
    print(a)
    cat("Window: ", Wlist[j], "--A set: ",a, "\n")
    k= 1 + (i-1) * 3
    out = BArandinf_fast( Y=dataALL$crime, time = score_new, unit= dataALL$barrio , w = Wlist[j], method = "AT", a = a,
                     seed=seed, statistic="diffmeans", ConfInt = FALSE,N=N)  
    results[j,k] = Wlist[j]
    results[j,k+1] = out$Sobs
    results[j,k+2] = out$pval
  }
}
results_AM2 = results[,-c(4,7)]
print(results_AM2)


#------------------------#
# Combine in single table
#------------------------#
results= rbind(results_AM1,results_AM2)
names(results) = c("W","2015_Sobs", "2015_pval","2016_Sobs", "2016_pval","2018_Sobs", "2018_pval")

# Keeping only Sobs from AM1, since they are the same 
resultsTab4= cbind(
               results_AM1[,1:3], results_AM2[,3],
               results_AM1[,4:5], results_AM2[,5],
               results_AM1[,6:7], results_AM2[,7]
)
names(resultsTab4) = c("W","2015_Sobs", "2015_pval1", "2015_pval2", "2016_Sobs", "2016_pval1","2016_pval2", "2018_Sobs", "2018_pval1","2018_pval2")
print(resultsTab4)
resultsTab4[,1]
resultsTab4[1,1] ="1 day"
resultsTab4[2,1] ="7 days"
resultsTab4[3,1] ="14 days"
save(resultsTab4, file=paste(path,"results_Table4_placNov1.RData",sep=""))
#load(file=paste(path,"results_Table4_placNov1.RData",sep=""))
print(xtable(resultsTab4, digits=3), include.rownames =FALSE)
