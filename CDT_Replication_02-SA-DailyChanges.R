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

source("CDT_functions.R")

seed = 1345
N = 10000
N_CI = 10000
Clist = 1:365

#******************************************************************************#
#******************************* Daily Changes ********************************#
#******************************************************************************#

dataALL = read.dta13("Data_daily_barrio.dta")
dim(dataALL)

dates <- seq(from = as.Date("2016-11-02"), 
             to = as.Date("2017-11-01"), 
             by = "day")

rr = data.frame(Score = rep(NA,length(Clist)), 
                Sobs = rep(NA,length(Clist)), 
                pval =rep(NA,length(Clist)))

w_list <- c(1, 10, 20)

#------------------------#
# TR Assignment Mechanism
#------------------------#

results = cbind(rr,rr,rr)

for (w in w_list) {
  for(i in 1:length(dates)) {
    if (dates[i] == "2017-11-01") {
      score_new <- dataALL$score
    } else {
      placebo_date <- dates[i]
      cutoff <- dataALL$score[dataALL$date == placebo_date] 
      score_new <- dataALL$score - cutoff
      score_new[score_new >= 0] <- score_new[score_new >= 0] + 1  
    } 
    new_code_new = as.numeric(score_new >= 0 )
    out = BArandinf_fast(Y = dataALL$crime, 
                           time = score_new, 
                           unit= dataALL$barrio,
                           w = w, 
                           method = "TR", 
                           seed = seed, 
                           statistic = "diffmeans",
                           ConfInt = FALSE, 
                           N = N
                           )  
      k = 1 + (which(w_list == w) - 1) * 3
      results[i, k] = dates[i]
      results[i, k + 1] = out$Sobs
      results[i, k + 2] = out$pval
  }
}

results_AM1 = results[,-c(4,7)]
  
colnames(results_AM1) <- c(
  "cutoff",
  "delta_crime.1", "pval_crime.1",
  "delta_crime.10", "pval_crime.10",
  "delta_crime.20", "pval_crime.20"
)

results_AM1$cutoff <- as.Date(results_AM1$cutoff)


#------------------------#
# Plot
#------------------------#

ref_date <- as.Date("2017-11-01")
windows <- c(1, 10, 20)
ref_values <- list()

for (w in windows) {
  delta_var <- paste0("delta_crime.", w)
  pval_var  <- paste0("pval_crime.", w)
  
  ref_values[[as.character(w)]] <- results_AM1 %>%
    filter(cutoff == ref_date) %>%
    pull(!!sym(delta_var))
  
  full_hist <- hist(results_AM1[[delta_var]], breaks = 30, plot = FALSE)
    }

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = "black", size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    panel.grid.major = element_line(color = "grey70", size = 0.3),
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )

for (w in windows) {
  delta_var <- paste0("delta_crime.", w)
  ref_value <- ref_values[[as.character(w)]]
  
  title_full <- paste0(w, "-Day Window (Nov. 2nd 2016 to Nov. 1st 2017)")
  
  p <- ggplot(results_AM1, aes_string(x = delta_var)) +
    geom_histogram(fill = "grey", color = "black", bins = 30) +
    geom_vline(xintercept = ref_value, color = "red", linetype = "dashed", size = 0.4) +
    annotate("text", x = ref_value, y = 25, label = "Nov. 1st, 2017",
             color = "red", vjust = -0.5, angle = 90, size = 3) +
    labs(
      title = title_full,
      x = "Change in Number of Police Reports",
      y = "Frequency"
    ) +
    scale_y_continuous(limits = c(0, 35), 
                       breaks = seq(0, 35, by = 5),
                       minor_breaks = seq(0, 35, by = 1)) +
    base_theme
  
  print(p)
  
  assign(paste0("p_1_", w), p)
}

#############################

library(grid)
library(gridExtra)
all <- grid.arrange(
  p_1_1,
  p_1_10,
  p_1_20,
  nrow = 1)


ggsave("tables-and-plots/CDT_Figure_SA_DailyChanges.png", plot = all, width = 18, height = 9, dpi = 600)
