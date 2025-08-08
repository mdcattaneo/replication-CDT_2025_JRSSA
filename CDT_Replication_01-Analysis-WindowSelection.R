#* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */

#* Replication Code */
rm(list = ls())
library(readstata13)
library(dplyr)
library(xtable)

source("CDT_functions.R")

#******************************************************************************/
# WINDOW SELECTOR
#******************************************************************************/

# -------------- Read Data ------------------------#
dataALL = read.dta13("Data_daily_barrio.dta")
dim(dataALL)
names(dataALL)

# Save real score and real treatment assignment
dataALL$score_orig    = dataALL$score
dataALL$new_code_orig = dataALL$new_code

Wlist = 1:50
N = 10000
path = "./tables-and-plots/"
nms = c("W", "Sobs", "MeanCo", "cutoff", "pval")
results = data.frame(matrix(NA,ncol=length(nms), nrow=length(Wlist)*length(C), dimnames = list(NULL,nms)))

C = c(-35,
      -28, # October 4th, first Wednesday in October
      -21)

seed = 1345

#------------------------#
# TR Assignment
#------------------------#
for(i in 1:length(C)){
  cat("Cutoff ",C[i],"\n")
  for(j in 1:length(Wlist)) {
    cat("Window ",Wlist[j],"\n")
    # Create placebo score and placebo treatment assignment
    cutoff = C[i]
    dataALL$score = dataALL$score_orig - cutoff
    dataALL$score[dataALL$score >=0] =dataALL$score[dataALL$score >=0] + 1  # take to -2,-1,1,2
    dataALL$new_code = as.numeric(dataALL$score >= 0 )
    
    out = BArandinf_fast( Y=dataALL$crime, time = dataALL$score, unit= dataALL$barrio , w = Wlist[j], method = "TR", seed=seed, statistic="diffmeans",
                     ConfInt = FALSE,N=N)  
    
    results[(i-1) * length(Wlist) + j,"pval"] = out$pval
    results[(i-1) * length(Wlist) + j,"W"] = Wlist[j]
    results[(i-1) * length(Wlist) + j,"cutoff"] = C[i]
    results[(i-1) * length(Wlist) + j,"Sobs"] = out$Sobs
    results[(i-1) * length(Wlist) + j,"MeanCo"] = out$meanCo
      
  }
}
results_AM1 = results
print(results_AM1)

path = "./tables-and-plots/"
save(results_AM1, file=paste(path,"Window-selector-TR.RData",sep=""))

# Window selector plot, only include  windows that occur in actual pre-intervention period
c = C[1]
pdf(paste(path,"CDT-plot-Wsel-C",abs(c),"-TR.pdf", sep=""))
dplot = results %>% filter(cutoff==c & W <=abs(c))
plot(y=dplot$pval, x=dplot$W, xlab="Window half-length", ylab="P-value", ylim=c(0,1))  # main=paste("Cutoff at ",c,sep="")
abline(h= 0.15, col="red", lty="dashed")
dev.off()

c = C[2]
pdf(paste(path,"CDT-plot-Wsel-C",abs(c),"-TR.pdf", sep=""))
dplot = results %>% filter(cutoff==c & W <=abs(c))
plot(y=dplot$pval, x=dplot$W, xlab="Window half-length", ylab="P-value", ylim=c(0,1)) #main=paste("Cutoff at ",c,sep="")
abline(h= 0.15, col="red", lty="dashed")
dev.off()

c = C[3]
pdf(paste(path,"CDT-plot-Wsel-C",abs(c),"-TR.pdf", sep=""))
dplot = results %>% filter(cutoff==c & W <=abs(c))
plot(y=dplot$pval, x=dplot$W, xlab="Window half-length", ylab="P-value", ylim=c(0,1)) # main=paste("Cutoff at ",c,sep="")
abline(h= 0.15, col="red", lty="dashed")
dev.off()


