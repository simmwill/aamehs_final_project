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

# a bit of data wrangling

df$hh_income <- as.factor(df$hh_income)
df$sex <- as.factor(df$sex)
df$race <- as.factor(df$race)

#BMI categories
df =df %>% 
  mutate(cdc_bmi = if_else(bmi <18.5, "underwt", 
                    if_else(bmi > 18.5 & bmi <=24.9, "normal",
                    if_else(bmi > 24.9 & bmi <=29.9, "overwt", 
                    if_else(bmi > 29.9, "obese", "NA")))))


#50% percentile -mean
Mod50 <- rq(tsh ~ bpa + cdc_bmi + hh_income  + sex + 
            age + race + bpa_creatinine, 
            data = df, 
            tau = 0.5)
summary(Mod50, alpha = 0.05)


# 2c.i Fit mean model
ModMean <- lm(tsh ~ bpa + cdc_bmi + hh_income + sex + 
                age + race + bpa_creatinine, 
              data = df)

CoeffMod50   <- summary(Mod50, alpha = 0.05)$coefficients[,1]
CoeffModMean <- summary(ModMean)$coefficients[,1]
coeff.table0  <- data.frame(CoeffMod50, CoeffModMean)
coeff.table <- coeff.table0 %>% 

  #compute percentage difference
  mutate(PercentDiff = 100*(CoeffMod50 - CoeffModMean)/CoeffModMean) %>% 
  # round numbers 
  mutate(CoeffMod50   = round(CoeffMod50, 3), 
         CoeffModMean = round(CoeffModMean, 3),
         PercentDiff   = round(PercentDiff, 1))
coeff.table

# 2d Compare models with plots 
# for these plots, using ggplot2 
MedianModel <- c(summary(Mod50, alpha = 0.05)$coefficients[2,1:3])

coeff.lm    <- summary(ModMean)$coefficients
MeanModel <- c(coeff.lm[2,1], 
               coeff.lm[2,1] - 1.96 * coeff.lm[2,2], 
               coeff.lm[2,1] + 1.96 * coeff.lm[2,2])
# create dataframe 

coeff.table <- rbind(MedianModel, MeanModel)
coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# set names for dataframe

names(coeff.table) <- c("coeff", "lci", "uci")
coeff.table        <- coeff.table %>% 
  mutate(ModelName = c("Median Model", "Mean Model"))


ForestPlotMeanMedian <- ggplot(data = coeff.table, 
                               # defines what dataset ggplot will use    
                               # aes() defines which variables the geoms will use   
                               aes( # defines variable for the x axis
                                 x = ModelName,  
                                 # defines the variable for the point along the y axis
                                 y = coeff,      
                                 # defines the lower bound of the confidence interval
                                 ymin = lci,     
                                 # define the upper bound of the confidence interval 
                                 ymax = uci)) +  
  # creates a point (y) with line defined by ymin and ymax
  geom_pointrange() +   
  # creates lines with bars, i.e. here the CIs
  geom_errorbar()+      
  # add a dashed line at y=0
  geom_hline(aes(yintercept = 0.0), lty = 2) +
  # labels for axes
  xlab("Model Name") +    
  ylab(expression("Coefficient for PM"[2.5]~" (95% CI)"))

ForestPlotMeanMedian # produces the plot in the plots panel