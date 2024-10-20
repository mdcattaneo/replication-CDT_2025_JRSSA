rm(list = ls(all.names = TRUE))
library(tidyverse)
library(ggplot2) 
library(plyr)
library(dplyr)

################################################
data <- read.csv("fisherian_before_and_after.csv")
max <- 0.9

#Nov. 1st, 2017
################################################

plot_17 <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Intervention: Nov. 1st, 2017", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_17)

################################################
data <- read.csv("fisherian_before_and_after_2015.csv")

#Nov. 1st, 2015
################################################

plot_15 <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: Nov. 1st, 2015", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_15)
################################################
data <- read.csv("fisherian_before_and_after_2016.csv")

#Nov. 1st, 2016
################################################

plot_16 <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: Nov. 1st, 2016", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_16)

################################################
data <- read.csv("fisherian_before_and_after_2018.csv")

#Nov. 1st, 2018
################################################

plot_18 <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: Nov. 1st, 2018", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_18)

################################################
data <- read.csv("fisherian_before_and_after_2015_w.csv")

#1st November Wednesday of 2015
################################################

plot_15_w <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: 1st November Wednesday of 2015", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_15_w)

################################################
data <- read.csv("fisherian_before_and_after_2016_w.csv")

#1st November Wednesday of 2016
################################################

plot_16_w <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: 1st November Wednesday of 2016", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_16_w)

################################################
data <- read.csv("fisherian_before_and_after_2018_w.csv")

#1st November Wednesday of 2018
################################################

plot_18_w <- ggplot(data) +  
  geom_bar(aes(x = window, y = pval_crime), 
           stat = "identity", 
           fill = "grey", 
           colour = "black") + 
  geom_point(aes(x = window, 
                 y = (delta_crime/max)
  ), 
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
    title = "Placebo: 1st November Wednesday of 2018", 
    x = "Window Size (Days)", 
    y = "P-Value (Grey Bars)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.1),
    sec.axis = sec_axis(~.*max, 
                        name = "Change in Number of Police Reports (Red Dots)",
                        breaks = seq(0, 1, 0.1))
  ) + theme_linedraw()+
  theme(plot.title = element_text(color = "black",
                                  size = 12,
                                  hjust = 0.5),
        axis.text.y.right = element_text(color = "red"),   
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red", size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.x = element_text(size=10)) 

print(plot_18_w)

################################################

library(grid)
library(gridExtra)
all <- grid.arrange(
  plot_15,
  plot_16,
  plot_17,
  plot_18,
  plot_15_w,
  plot_16_w,
  plot_17,
  plot_18_w,
  nrow = 2)


ggsave("CDT_Figure2.png", plot = all, width = 18, height = 9, dpi = 600)
