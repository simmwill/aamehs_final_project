# Quantile Regression Session


library(readr)
library(dplyr) 
library(quantreg)
library(ggplot2)

#0c Declare directories

DataFolder   <- "~/Desktop/CUMC-MPH/Spring 2020/ADVANCED METHODS/aamehs_final_project/data/"
OutputFolder <- "~/Desktop/CUMC-MPH/Spring 2020/ADVANCED METHODS/aamehs_final_project/data/"

#data import 
# Maybe we should think about creating and exporting a dataset with all of our variables of iterest.

df = readRDS("./data/analytic_dataset.RDS")



#50% percentile -mean
Mod50 <- rq(thyroid_outcome ~ bpa_u + bpf_u + bps_u + creatinine_u + sex + 
            age + race, 
            data = df, 
            tau = 0.5)
summary(Mod50, alpha = 0.05)
