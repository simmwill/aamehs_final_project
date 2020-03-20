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
demo_11 = haven::read_xpt('./data/DEMO_11_12.xpt')
demo_13 = haven::read_xpt('./data/DEMO_13_14.xpt')
demo_15 = haven::read_xpt('./data/DEMO_15_16.xpt')


#Self-reported Thyroid function
med_11 = haven::read_xpt('./data/MCQ_11_12.xpt')
med_13 = haven::read_xpt('./data/MCQ_13_14.xpt')
med_15 = haven::read_xpt('./data/MCQ_15_16.xpt')

#Merging by year
nh1112<-merge(demo_11,med_11,by="SEQN",all=T)
nh1314<-merge(demo_13,med_13,by="SEQN",all=T)
nh1516<-merge(demo_15,med_15,by="SEQN",all=T)

df <- read_csv(paste0(DataFolder, "2010_County_Data.csv"))