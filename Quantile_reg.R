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

#Bisphenols
pc_11 = haven::read_xpt('./data/EPH_11_12.xpt')           ## different dataset for bisphenols 11-12
pc_13 = haven::read_xpt('./data/EPHPP_13_14.xpt')
pc_15 = haven::read_xpt('./data/EPHPP_15_16.xpt')


#Self-reported Thyroid function
med_11 = haven::read_xpt('./data/MCQ_11_12.xpt')
med_13 = haven::read_xpt('./data/MCQ_13_14.xpt')
med_15 = haven::read_xpt('./data/MCQ_15_16.xpt')

#Merging by year
nh1112<-merge(demo_11,med_11,by="SEQN",all=T)
nh1112<-merge(nh1112, pc_11, by="SEQN", all=T)

nh1314<-merge(demo_13,med_13,by="SEQN",all=T)
nh1112<-merge(nh1314, pc_13, by="SEQN", all=T)

nh1516<-merge(demo_15,med_15,by="SEQN",all=T)
nh1112<-merge(nh1516, pc_15, by="SEQN", all=T)

#?rq 

nh1112 %>% 
  mutate(
    id = SEQN,  
    bpa_u = URXBPH,   
    bpa_u_det = URDBPHLC,   
    bpf_u = URXBPF,
    bpf_u_det = URDBPFLC,
    bps_u = URXBPS,
    bps_u_det = URDBPSLC,
    thyroid_problem = MCQ160M,  ## ever told you had thyroid problem
    thyroid_current = MCQ170M)

#Did we get any feedback about our binary outcome for quantile regression?

#50% percentile -mean
Mod50 <- rq(thyroid_problem ~ bpa_u + bpf_u + PerLatinx + PerAsianAm + 
              MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
            data = df, 
            tau = 0.5)