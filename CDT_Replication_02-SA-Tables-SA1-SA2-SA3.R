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

#******************************************************************************/
#*********************************** TABLE SA1**********************************/ 
# TR Assignment mechanism, using time-adjusted statistic
#******************************************************************************/

##################
# Run results 
###############
thetas = seq(-2,2,by=0.01)
thetas = c(-2, seq(-1, 2,by=0.01))

rr = data.frame(W = rep(NA,length(Wlist)), Sobs = rep(NA,length(Wlist)), MeanCo =  rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)), CI_lo = rep(NA,length(Wlist)), CI_up = rep(NA,length(Wlist)))

#------------------------#
# Assignment mechanism 1
#------------------------#
results = rr
for(j in 1:length(Wlist)) {
  fixed_w = TRUE
  out = BArandinf_fast( Y=data$crime, time = data$score, unit= data$barrio , w = Wlist[j], method = "TR", seed=seed, statistic="timeadj",
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
# Save
#------------------------#
resultsTab = cbind(results_AM1[,1:4],CI=paste("[",results_AM1[,"CI_lo"], ",", results_AM1[,"CI_up"], "]", sep=""))
print(resultsTab)
resultsTab[,1]
resultsTab[1,1] ="1 day"
resultsTab[2,1] ="7 days"
resultsTab[3,1] ="14 days"
print(xtable(resultsTab, digits=3), include.rownames =FALSE)

save(resultsTab, file=paste(path,"results_timeadj-TR.RData",sep=""))
print(xtable(resultsTab, digits=3), include.rownames =FALSE)



#******************************************************************************#
#*********************************** TABLE SA-2 **********************************# 
# Placebo tests for first Wednesday in November in 2015, 2016 and 2018
#******************************************************************************#

dates = c("2015-11-04","2016-11-02","2018-11-07")
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
                          ConfInt = FALSE,N=N)  
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
    out = BArandinf( Y=dataALL$crime, time = score_new, unit= dataALL$barrio , w = Wlist[j], method = "AT", a = a,
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
resultsTab5= cbind(results_AM1[,1:3], results_AM2[,3],
                   results_AM1[,4:5], results_AM2[,5],
                   results_AM1[,6:7], results_AM2[,7]
)
names(resultsTab5) = c("W","2015_Sobs", "2015_pval1", "2015_pval2", "2016_Sobs", "2016_pval1","2016_pval2", "2018_Sobs", "2018_pval1","2018_pval2")
print(resultsTab5)
resultsTab5[,1]
resultsTab5[1,1] ="1 day"
resultsTab5[2,1] ="7 days"
resultsTab5[3,1] ="14 days"
print(resultsTab5)
save(resultsTab5, file=paste(path,"results_Table5_placWed1.RData",sep=""))
#load(file=paste(path,"results_Table5_placWed1.RData",sep=""))
print(xtable(resultsTab5, digits=3), include.rownames =FALSE)


#******************************************************************************#
#*********************************** TABLE SA-3 **********************************# 
# Tests for different types of crime
#******************************************************************************#


crimes = c("theft", "robbery","dv")
thetas = seq(-2,2,by=0.01)
rr = data.frame(W = rep(NA,length(Wlist)), Sobs = rep(NA,length(Wlist)), MeanCo =  rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)), CI= rep(NA,length(Wlist)))
#------------------------#
# Assignment mechanism 1
#------------------------#
results = rbind(rr,rr,rr)
for(i in 1:length(crimes)) {
  varname = crimes[i]
  outcome = dataALL[[varname]]
  k= 1 + (i-1) * 3
  
  for(j in 1:length(Wlist)) {
    out = BArandinf_fast( Y=outcome, time = dataALL$score, unit= dataALL$barrio , w = Wlist[j], method = "TR", 
                          seed=seed, statistic="diffmeans", 
                          ConfInt = FALSE, thetas = thetas, N=N, N_CI=N_CI)  
    results[k +(j-1),1] = Wlist[j]
    results[k +(j-1),2] = out$Sobs
    results[k +(j-1),3] = out$meanCo
    results[k +(j-1),4] = out$pval
    results[k +(j-1),5] = paste("[", out$CI[1], ",", out$CI[2], "]", sep="")
  }
}
results_AM1 = results
print(results_AM1)

#------------------------#
# Assignment mechanism 2
#------------------------#
results = rbind(rr,rr,rr)
for(i in 1:length(crimes)) {
  varname = crimes[i]
  outcome = dataALL[[varname]]
  k= 1 + (i-1) * 3
  
  for(j in 1:length(Wlist)) {
    if(Wlist[j] == 1) next
    a = c(-(Wlist[j]-1):0)
    print(a)
    cat("Window: ", Wlist[j], "--A set: ",a, "\n")
    
    out = BArandinf_fast( Y=outcome, time = dataALL$score, unit= dataALL$barrio , w = Wlist[j], method = "AT", a = a,
                          seed=seed, statistic="diffmeans", 
                          ConfInt = FALSE, thetas = thetas, N=N, N_CI=N_CI)  
    results[k +(j-1),1] = Wlist[j]
    results[k +(j-1),2] = out$Sobs
    results[k +(j-1),3] = out$meanCo
    results[k +(j-1),4] = out$pval
    results[k +(j-1),5] = paste("[", out$CI[1], ",", out$CI[2], "]", sep="")
  }
}
results_AM2 = results
print(results_AM2)

#------------------------#
# Combine in single table
#------------------------#
# two panels top and bottom
resultsTab6= cbind(results_AM1,results_AM2[,4:5])
resultsTab6[,1]
resultsTab6[c(1,4,7),1] ="1 day"
resultsTab6[c(2,5,8),1] ="7 days"
resultsTab6[c(3,6,9),1] ="14 days"
print(resultsTab6)
save(resultsTab6, file=paste(path,"results_Table6.RData",sep=""))
#load(file=paste(path,"results_Table6.RData",sep=""))
print(xtable(resultsTab6, digits=3), include.rownames =FALSE)

