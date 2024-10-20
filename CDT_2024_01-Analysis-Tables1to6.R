#******************************************************************************#
#******************************************************************************#
#** Randomization inference for before-and-after studies with multiple units **# 
#********** An application to a criminal procedure reform in Uruguay **********#
#******************************************************************************#
#******************************************************************************#

#* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */

#* Replication Code */
library(readstata13)
library(dplyr)
 
#******************************************************************************#
#*********************************** TABLE 1 **********************************#
#******************************************************************************#
dd =  read.dta13("Data_daily.dta") 

t.test(dd$theft[dd$new_code==1]   ,dd$theft[dd$new_code==0])
t.test(dd$robbery[dd$new_code==1] ,dd$robbery[dd$new_code==0])
t.test(dd$dv[dd$new_code==1]      ,dd$dv[dd$new_code==0])
t.test(dd$other[dd$new_code==1]   ,dd$other[dd$new_code==0])
t.test(dd$crime[dd$new_code==1]   ,dd$crime[dd$new_code==0])


#******************************************************************************/
#*********************************** TABLE 2 **********************************/ 
#******************************************************************************/

panel = read.dta13("Data_panel_before_and_after.dta")
dim(panel)
# Divide every column by window (half) length to go from total crime in each window to average per-day crime in each window
for(i in 1:20) {
  var = paste0("crime_",i)
  panel[[var]] = panel[[var]]/i
}


set.seed(1345)
N = 10000
Ys = cbind(panel$crime_1, panel$crime_7, panel$crime_14)
length = c(1,7,14)
panel = arrange(panel,barrio,treated)
for(k in 1:ncol(Ys)) {
  Sobs = mean(Ys[,k][panel$treated==1]) -  mean(Ys[,k][panel$treated==0])
  S = numeric(N)
  for(i in 1:N) {
    #cat("Iteration ",i,"\n")
    Tr = NULL
    for(j in 1:62) {
      tr = sample(c(0,1), replace=FALSE)
      Tr = c(Tr,tr)
    }
    S[i] = mean(Ys[,k][Tr==1]) -  mean(Ys[,k][Tr==0])
  }
 cat("Fisherian p-value for crime in window of +/- ",length[k], " days is", sum(abs(S) >= abs(Sobs))/N, "\n")
}


#******************************************************************************
#*********************************** TABLE 3 ********************************** 
#******************************************************************************
library(matlib)
set.seed(9345)
N = 10000
n = 62
klist = c(1,7,14)
nms = c("K", "W","Wpval","W2", "W2pval", "W3", "W3pval")
results = matrix(data=NA, nrow=length(klist),ncol=length(nms))
colnames(results) = nms
round=0
for (k in klist) {
  round = round+1
  cat("Round", round, " for k=",k,"\n")
  J = k
  Diff = numeric(J)
  D1    = matrix(data=NA, ncol= J, nrow = n*2)
  D0 = D1
  for(j in 1:J) {
    varname     = paste0("crime_", j)
    D1[,j]      = panel[[varname]][panel$treated==1]
    D0[,j]      = panel[[varname]][panel$treated==0]
    Diff[j]     = mean(D1[,j]) -  mean(D0[,j])
  }
  SigmaTr = cov(D1) 
  SigmaCo = cov(D0)
  Sigma = ((n-1)*SigmaTr + (n-1)*SigmaCo)/(n+n-2)
  Wobs  = max(Diff)
  
  if(k==1) {
    W2obs =  ((n*n)/(n+n)) * (Diff^2)/as.numeric(Sigma)
  } else {
    W2obs =  ((n*n)/(n+n)) * as.numeric(t(Diff)%*%inv(Sigma)%*%(Diff))
  }
  W3obs = mean(Diff)
  cat("Diff is", Diff,"\n")
  cat("Wobs is", Wobs,"\n")
  cat("W2obs is", W2obs,"\n")
  cat("W3obs is", W3obs,"\n")
  
  W  = numeric(N)
  W2 = numeric(N)
  W3 = numeric(N)
  Diffsim = numeric(J)
  for(i in 1:N) {
    if(i%%2500 == 0) cat("Iteration ",i,"\n")
    Tr = NULL
    for(j in 1:62) {
      tr = sample(c(0,1), replace=FALSE)
      Tr = c(Tr,tr)
    }
    D1sim    = matrix(data=NA, ncol= J, nrow = n*2)
    D0sim = D1sim
    for(j in 1:J) {
      varname     = paste0("crime_", j)
      D1sim[,j]      = panel[[varname]][Tr==1] 
      D0sim[,j]      = panel[[varname]][Tr==0]
      Diffsim[j] =     mean(D1sim[,j] ) -  mean(D0sim[,j])
    }
    SigmasimTr = cov(D1sim) 
    SigmasimCo = cov(D0sim)
    Sigmasim = ((n-1)*SigmaTr + (n-1)*SigmaCo)/(n+n-2)
    W[i] = max(Diffsim)
    
    if(k==1) {
      W2[i] =  ((n*n)/(n+n)) * (Diffsim^2)/as.numeric(Sigmasim)
    } else {
      W2[i] = ((n*n)/(n+n)) * as.numeric(t(Diffsim)%*%inv(Sigmasim)%*%(Diffsim))
    }
    W3[i] = mean(Diffsim)
  }
  cat("Summary of W  in this round is", summary(W), "\n")
  cat("Summary of W2 in this round is", summary(W2), "\n")
  # Fisherian p-value is
  results[round,"K"]      = k
  results[round,"W"]      = Wobs
  results[round,"W2"]     = W2obs
  results[round,"W3"]     = W3obs
  results[round,"Wpval"]  = sum(abs(W) >= abs(Wobs))/N
  results[round,"W2pval"] = sum(abs(W2) >= abs(W2obs))/N
  results[round,"W3pval"] = sum(abs(W3) >= abs(W3obs))/N
}

results

#/******************************************************************************
#/******************************* TABLE 4: Falsification analyses ************* 
#/******************************************************************************

set.seed(1345)
for(y in c(2015,2016,2018)) {
  panelFa = read.dta13(paste0("Data_panel_before_and_after_",y,".dta"))
  # Divide every column by window (half) length to go from total crime in each window to average per-day crime in each window
  for(l in 1:20) {
    var = paste0("crime_",l)
    panelFa[[var]] = panelFa[[var]]/l
  }
  N = 10000
  Ws = c(1,7,14)
  panelFa = arrange(panelFa,barrio,treated)
  for(k in Ws) {
    varname = paste0("crime_", k)
    Sobs    = mean(panelFa[[varname]][panelFa$treated==1], na.rm=TRUE) -  mean(panelFa[[varname]][panelFa$treated==0], na.rm=TRUE)
    S = numeric(N)
    for(i in 1:N) {
      #cat("Iteration ",i,"\n")
      Tr = NULL
      for(j in 1:62) {
        tr = sample(c(0,1), replace=FALSE)
        Tr = c(Tr,tr)
      }
      S[i] = mean(panelFa[[varname]][Tr==1],na.rm=TRUE) -  mean(panelFa[[varname]][Tr==0],na.rm=TRUE)
    }
    cat("-------------------------\n")
    cat("Year ", y, "\n")
    cat("Diff in means of crime in window of +/- ",k, " days is", Sobs, "\n")
    cat("Fisherian p-value for crime in window of +/- ",k, " days is", sum(abs(S) >= abs(Sobs))/N, "\n")
    cat("-------------------------\n")
  }
}

#/******************************************************************************
#/******************************* TABLE 5: Falsification analyses ************* 
#/******************************************************************************

set.seed(1345)
for(y in c(2015,2016,2018)) {
  panelFa = read.dta13(paste0("Data_panel_before_and_after_",y,"_w.dta"))
  # Divide every column by window (half) length to go from total crime in each window to average per-day crime in each window
  for(l in 1:20) {
    var = paste0("crime_",l)
    panelFa[[var]] = panelFa[[var]]/l
  }
  N = 10000
  Ws = c(1,7,14)
  panelFa = arrange(panelFa,barrio,treated)
  for(k in Ws) {
    varname = paste0("crime_", k)
    Sobs    = mean(panelFa[[varname]][panelFa$treated==1], na.rm=TRUE) -  mean(panelFa[[varname]][panelFa$treated==0], na.rm=TRUE)
    S = numeric(N)
    for(i in 1:N) {
      #cat("Iteration ",i,"\n")
      Tr = NULL
      for(j in 1:62) {
        tr = sample(c(0,1), replace=FALSE)
        Tr = c(Tr,tr)
      }
      S[i] = mean(panelFa[[varname]][Tr==1],na.rm=TRUE) -  mean(panelFa[[varname]][Tr==0],na.rm=TRUE)
    }
    cat("-------------------------\n")
    cat("Year ", y, "\n")
    cat("Diff in means of crime in window of +/- ",k, " days is", Sobs, "\n")
    cat("Fisherian p-value for crime in window of +/- ",k, " days is", sum(abs(S) >= abs(Sobs))/N, "\n")
    cat("-------------------------\n")
  }
}

#/******************************************************************************
#/******************************* TABLE 6 ************* 
#/******************************************************************************
set.seed(6547)
for(var in c("theft", "robbery","dv")) {
  dd = read.dta13(paste0("Data_panel_before_and_after_",var,".dta"))
  # Divide every column by window (half) length to go from total crime in each window to average per-day crime in each window
  for(l in 1:20) {
    varname = paste0(var,"_",l)
    dd[[varname]] = dd[[varname]]/l
  }
  N = 10000
  Ws = c(1,7,14)
  dd = arrange(dd,barrio,treated)
  for(k in Ws) {
    varname = paste0(var,"_", k)
    Sobs    = mean(dd[[varname]][dd$treated==1], na.rm=TRUE) -  mean(dd[[varname]][dd$treated==0], na.rm=TRUE)
    S = numeric(N)
    for(i in 1:N) {
      #cat("Iteration ",i,"\n")
      Tr = NULL
      for(j in 1:62) {
        tr = sample(c(0,1), replace=FALSE)
        Tr = c(Tr,tr)
      }
      S[i] = mean(dd[[varname]][Tr==1],na.rm=TRUE) -  mean(dd[[varname]][Tr==0],na.rm=TRUE)
    }
    cat("-------------------------\n")
    cat("Variable ", var, "\n")
    cat("Diff in means of crime in window of +/- ",k, " days is", Sobs, "\n")
    cat("Fisherian p-value for crime in window of +/- ",k, " days is", sum(abs(S) >= abs(Sobs))/N, "\n")
    cat("-------------------------\n")
  }
}

###################################################
