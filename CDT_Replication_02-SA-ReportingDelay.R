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
library(ggplot2)
library(data.table)

source("CDT_functions.R")

seed = 1345
N = 10000
N_CI = 10000
Wlist = 1:21

#******************************************************************************#
#****************** Reporting: Less than 15/30/45 min Delay *******************# 
#******************************************************************************#

dataALL <- fread("crime2017_15delay.csv")
dim(dataALL)

dates = c("2017-11-01")
rr = data.frame(W = rep(NA,length(Wlist)), 
                Sobs = rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)))

#------------------------#
# TR Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM1 = results
print(results_AM1)

colnames(results_AM1) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM1_15 = results_AM1

#------------------------#
# Plot
#------------------------#

max <- 0.9

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 15 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM1) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 3, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(0, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_15_", year,"_1"), p)
  print(p)
}

#####################################

#------------------------#
# AT Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM2 = results
print(results_AM2)

colnames(results_AM2) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM2_15 = results_AM2

#------------------------#
# Plot
#------------------------#


for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 15 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM2) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 5, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(-1, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_15_", year,"_2"), p)
  print(p)
}

################################################################################

dataALL <- fread("crime2017_30delay.csv")
dim(dataALL)

dates = c("2017-11-01")
rr = data.frame(W = rep(NA,length(Wlist)), 
                Sobs = rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)))

#------------------------#
# TR Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM1 = results
print(results_AM1)

colnames(results_AM1) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM1_30 = results_AM1

#------------------------#
# Plot 
#------------------------#

max <- 0.9

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 30 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM1) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 3, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(0, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_30_", year,"_1"), p)
  print(p)
}

#####################################

#------------------------#
# AT Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM2 = results
print(results_AM2)

colnames(results_AM2) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM2_30 = results_AM2

#------------------------#
# Plot
#------------------------#

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 30 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM2) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 5, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(-1, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_30_", year,"_2"), p)
  print(p)
}


################################################################################

dataALL <- fread("crime2017_45delay.csv")
dim(dataALL)

dates = c("2017-11-01")
rr = data.frame(W = rep(NA,length(Wlist)), 
                Sobs = rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)))

#------------------------#
# TR Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM1 = results
print(results_AM1)

colnames(results_AM1) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM1_45 = results_AM1

#------------------------#
# Plot 
#------------------------#

max <- 0.9

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 45 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM1) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 3, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(0, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_45_", year,"_1"), p)
  print(p)
}

#####################################

#------------------------#
# AT Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM2 = results
print(results_AM2)

colnames(results_AM2) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

results_AM2_45 = results_AM2

#------------------------#
# Plot
#------------------------#

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "Reporting Delay < 45 min"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM2) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 5, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(-1, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_45_", year,"_2"), p)
  print(p)
}

################################################################################

dataALL = read.dta13("Data_daily_barrio.dta")
dim(dataALL)

dates = c("2017-11-01")
rr = data.frame(W = rep(NA,length(Wlist)), 
                Sobs = rep(NA,length(Wlist)), 
                pval =rep(NA,length(Wlist)))

#------------------------#
# TR Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM1 = results
print(results_AM1)

colnames(results_AM1) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

#------------------------#
# Plot 
#------------------------#

max <- 0.9

for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "All Reports"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM1) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 15, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(0, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_", year,"_1"), p)
  print(p)
}

#####################################

#------------------------#
# AT Mechanism
#------------------------#
results = cbind(rr)
for(i in 1:length(dates)) {
  if (dates[i] == "2017-11-01") {
    score_new <- dataALL$score
  } else {
    placebo_date = dates[i]
    cutoff = dataALL$score[dataALL$date == placebo_date] 
    score_new = dataALL$score - cutoff
    score_new[score_new >=0] =score_new [score_new >=0] + 1  
  }
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

results_AM2 = results
print(results_AM2)

colnames(results_AM2) <- c(
  "window",
  "delta_crime.17", "pval_crime.17"
)

#------------------------#
# Plots 
#------------------------#


for (year in 17:17) {
  pval_col <- paste0("pval_crime.", year)
  delta_col <- paste0("delta_crime.", year)
  
  plot_title <- if (year == 17) {
    "All Reports"
  } else {
    paste0("Placebo: Nov. 1st, 20", year)
  }
  
  p <- ggplot(results_AM2) +  
    geom_bar(aes(x = window, y = .data[[pval_col]]), 
             stat = "identity", 
             fill = "grey", 
             colour = "black") + 
    geom_point(aes(x = window, 
                   y = (.data[[delta_col]] / max)), 
               stat = "identity", 
               color = "red") + 
    geom_hline(yintercept = 0.1,
               color = "black", 
               linetype = "dashed",
               size = 1) +
    geom_label(aes(x = 15, 
                   y = 0.1, 
                   label = "P-Value = 0.1"), 
               fill = "white") +
    labs(
      title = plot_title, 
      x = "Window Size (Days)", 
      y = "P-Value (Grey Bars)"
    ) + 
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(~.*max, 
                          name = "Change in Number of Police Reports (Red Dots)",
                          breaks = seq(0, 1, 0.1))
    ) + 
    theme_linedraw() +
    theme(
      plot.title = element_text(color = "black", size = 12, hjust = 0.5),
      axis.text.y.right = element_text(color = "red"),   
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.title.y.left = element_text(size = 10),
      axis.title.x = element_text(size = 10)
    )
  
  assign(paste0("plot_", year,"_2"), p)
  print(p)
}

################################################################################

library(gridExtra)
library(grid)

all <- grid.arrange(
  arrangeGrob(
    grobs = list(plot_15_17_1,plot_30_17_1,plot_45_17_1,plot_17_1),
    nrow = 1,
    top = textGrob("Treatment Reversal Assignment Mechanism", gp = gpar(fontsize = 14, fontface = "bold"))
  ),
  arrangeGrob(
    grobs = list(plot_15_17_2,plot_30_17_2,plot_45_17_2,plot_17_2),
    nrow = 1,
    top = textGrob("Adoption Timing Assignment Mechanism", gp = gpar(fontsize = 14, fontface = "bold"))
  ),
  nrow = 2
)

ggsave("tables-and-plots/CDT_Figure_SA_ReportingDelay.png", plot = all, width = 18, height = 9, dpi = 600)
