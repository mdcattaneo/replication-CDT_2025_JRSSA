rm(list = ls(all.names = TRUE))
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(ggforce)
library(plyr)
library(readstata13)

################################################
data =  read.dta13("Data_daily.dta") 

scatter_total <- ggplot(data) +
  geom_point(aes(x = score, 
                 y = crime),
             stat = "identity", 
             color = "black",
             size = 3)  +
  labs(x = "Number of days before/after the new CCP entered into force", 
       y = "Number of police reports") + 
  scale_y_continuous(limits = c(200, 480),
                     breaks = seq(200, 480, 20)) +
  scale_x_continuous(limits = c(-650, 400),
                     breaks = seq(-650, 400, 50)) +
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18)) +
  geom_vline(xintercept = 0,
             color = "black", 
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = -21,
             color = "red", 
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = 21,
             color = "red", 
             linetype = "dashed",
             size = 0.75) 

print(scatter_total)

ggsave("tables-and-plots/CDT_Figure_TotalReports1618.png", 
       plot = scatter_total, 
       width = 18, height = 9, dpi = 600)

################################################


