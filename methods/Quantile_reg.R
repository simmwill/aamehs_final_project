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

df =readRDS('./data/new_analytic_dataset.RDS')
head(df)



#BMI categories
df =df %>% 
  mutate(cdc_bmi = if_else(bmi <18.50, "0", 
                    if_else(bmi > 18.50 & bmi <=24.9, "1",
                    if_else(bmi > 24.9 & bmi <=29.9, "2", 
                    if_else(bmi > 29.9, "3", "NA")))))
df$cdc_bmi = as.factor(df$cdc_bmi)

head(df)

#50% percentile -mean
Mod50 <- rq(tsh ~ bpa + bmi + hh_income  + sex + 
            age + race + bpa_creatinine, 
            data = df, 
            tau = 0.5)
summary(Mod50, alpha = 0.05)


plot(Mod50$residuals)

