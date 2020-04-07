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



## **quant regs*####

#50% percentile -mean
Mod50 <- rq(tsh ~ bpa + bmi + hh_income  + sex + 
            age + race + bpa_creatinine, 
            data = df, 
            tau = 0.5)
summary(Mod50, alpha = 0.05)

#can i use bmi as a continuous var?

Mods25.50 <- rq(tsh ~ bpa + bmi+ hh_income + sex + age + 
                  race + bpa_creatinine, 
                  data = df, 
                  tau= c(0.25, 0.50)) # c() creates a vector

summary(Mods25.50)

# Plot two quantile models 


# 3b.i Combine estimates from each model into a single dataframe 
# assemble estimates from each model
# we will save the summary as an object 
# so we do not have to keep re-summarizing the models. 

summary25.50 <- summary(Mods25.50, alpha = 0.05)

# we use the brackets to specify which model we are extracting

Model25th   <- c(summary25.50[[1]]$coefficients[2,1:3])
Model50th   <- c(summary25.50[[2]]$coefficients[2,1:3])


# create dataframe 

coeff.table <- rbind(Model25th, Model50th)
coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# set names for dataframe

names(coeff.table) <- c("coeff", "lci", "uci")
coeff.table        <- coeff.table %>% 
  mutate(ModelName = c("Model 25th", "Model 50th")) 

# 3b.ii Plot

ForestPlot.25.50 <- ggplot(data=coeff.table, # defines what dataset we are using
                           aes(x=ModelName,  # defines variable for the x axis
                               y=coeff,      # defines the variable for the point along the y axis
                               ymin=lci,     # defines the lower bound of the confidence interval
                               ymax=uci)) +  # define the upper bound of the confidence interval   
  geom_pointrange() +          # creates a point (y) with line defined by ymin and ymax        
  geom_errorbar()+             # creates lines with bars
  geom_hline(aes(yintercept=0.0), lty=2) + # add a dashed line at y=0 
  xlab("Model Name") +         # labels for axes
  ylab(expression("Coefficient for BPA (95% CI)"))

ForestPlot.25.50


# 3c Plot Multiple Quantiles
# we can just use the same procedure as before, 
# but with additional models for every quantile of interest

# 3c.i Create the models
# seq() creates a sequence with intervals set by the by arguement 

TauList <- seq(0.1, 0.9, by = 0.1)
TauList

qr.Mods  <- rq(tsh ~ bpa + bmi + hh_income + sex + age + 
                race + bpa_creatinine, 
                data = df, 
                tau = TauList)

# 3c.ii Assemble estimates from each model

summary.qr.Mods <- summary(qr.Mods, alpha = 0.05)

Model10th   <- c(summary.qr.Mods[[1]]$coefficients[2,1:3])
Model20th   <- c(summary.qr.Mods[[2]]$coefficients[2,1:3])
Model30th   <- c(summary.qr.Mods[[3]]$coefficients[2,1:3])
Model40th   <- c(summary.qr.Mods[[4]]$coefficients[2,1:3])
Model50th   <- c(summary.qr.Mods[[5]]$coefficients[2,1:3])
Model60th   <- c(summary.qr.Mods[[6]]$coefficients[2,1:3])
Model70th   <- c(summary.qr.Mods[[7]]$coefficients[2,1:3])
Model80th   <- c(summary.qr.Mods[[8]]$coefficients[2,1:3])
Model90th   <- c(summary.qr.Mods[[9]]$coefficients[2,1:3])

# create dataframe 

coeff.table <- rbind(Model10th, Model20th, Model30th, Model40th, 
                     Model50th, Model60th, Model70th, Model80th, 
                     Model90th)

coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)
# set names for dataframe

names(coeff.table) <- c("coeff", "lci", "uci")
coeff.table        <- coeff.table %>% 
  mutate(ModelName = c("Model 10th", "Model 20th", "Model 30th", "Model 40th",
                       "Model 50th", "Model 60th", "Model 70th", "Model 80th", 
                       "Model 90th"))
#plot
ForestPlot.Mods <- ggplot(data=coeff.table, # defines what dataset we are using
                          aes(x=ModelName,  # defines variable for the x axis
                              y=coeff,      # defines the variable for the point along the y axis
                              ymin=lci,     # defines the lower bound of the confidence interval
                              ymax=uci)) +  # define the upper bound of the confidence interval   
  geom_pointrange() +               # creates a point (y) with line defined by ymin and ymax        
  geom_errorbar()+                  # creates lines with bars
  geom_hline(aes(yintercept=0.0), lty=2) + # add a dashed line at y=0 
  xlab("Model Name") +              # labels for axes
  ylab(expression("Coefficient for BPA (95% CI)"))

ForestPlot.Mods

##*EXTRA**##########
####

ForestPlot.fancy <- ggplot(data=coeff.table, 
                           # tell geom_functions which variables to use
                           aes(x=ModelName, y=coeff, ymin=lci, ymax=uci)) + 
  # point is a diamond , increase size 
  geom_pointrange(shape = 18, size = 1.5) +       
  # increase the size of the error bars
  geom_errorbar(size = 1.5)+ 
  # changes the color of the line
  geom_hline(aes(yintercept=0.0), lty=2, color ="grey") + 
  # flip coordinates (puts labels on y axis)
  coord_flip() +                                     
  xlab("Model\nName") + ylab(expression("Coefficient for BPA (95% CI)")) +
  # change the angle of the title of the y-axis
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +  
  # change the size of the axis titles
  theme(axis.title = element_text(size = 28)) +                 
  # change the size of the axis text
  theme(axis.text = element_text(size = 20)) +      
  # use a white background without gridlines
  theme(panel.background = element_rect(fill = 'white', color = "black")) 

ForestPlot.fancy



####****************************************************
#### Footnote 3: Interactions in Linear Regression  ####
####****************************************************

# Does the association between county-level annual PM2.5 and county average BMI 
# vary by climate region? 
# Let's add an interaction term 

# F3a Fit regression model 

mod.interaction <- lm(AveBMI ~ AvePM*ClimateRegion + PerBlack + PerLatinx + PerAsianAm + 
                        MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp, 
                      data = df, na.action = na.omit)
# F3b Review Model  

summary(mod.interaction)

# We see that the term for pm*upper_midwest is almost significant 
# We can use the values from the model to estimate 
# the PM-AveBMI association within the  Upper Midwest
# and to estimate the confidence intervals of the association 

# F3c Compute the within-region association and its uncertainty 

# F3.i Extract coefficents and covariances
# here we create tables of coefficients and covariance
coef.mat <- summary(mod.interaction)$coefficients
var.mat  <- vcov(mod.interaction)

# F3.ii Compute total term for within-region association
# sum of the term in the reference region plus the term for Upper Midwest

beta.PM_upper_midwest <- coef.mat["AvePM",1] + 
  coef.mat["AvePM:ClimateRegionupper_midwest",1]

# F3.iii Compute variance of within-region association 
# in order to compute standard error
# We must compute the variance for the total term 
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + CoVar(Beta1, Beta3) + CoVar(Beta3, Beta1)
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + 2*CoVar(Beta1, Beta3) 

var.PM_upper_midwest  <- var.mat["AvePM", "AvePM"] + 
  var.mat["AvePM:ClimateRegionupper_midwest", "AvePM:ClimateRegionupper_midwest"] +
  2*var.mat["AvePM", "AvePM:ClimateRegionupper_midwest"]

se.PM_upper_midwest  <- sqrt(abs(var.PM_upper_midwest))

# F3.iv Compute confidence intervals 
# using ste for within-region association

lci.PM_upper_midwest <- beta.PM_upper_midwest - 1.96*se.PM_upper_midwest
uci.PM_upper_midwest <- beta.PM_upper_midwest + 1.96*se.PM_upper_midwest

UMWval <- paste(round(beta.PM_upper_midwest, 3), " (95% CI: ", round(lci.PM_upper_midwest, 3), ", ", 
                round(uci.PM_upper_midwest, 3), ")", sep = "")
UMWval

# F3.v Assess model fit
# we can ask whether including the interaction terms improved model fit 
# anova() provides a nice test comparing model fit

mod.noInteraction <- lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
                          MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
                        data = df, 
                        na.action = na.omit)

anova(mod.noInteraction, mod.interaction)

# the results of the anova are not statistically significant, 
# indicating that as a whole interaction between pm and region does not improve model fit
# i.e. no evidence of effect modification by region




#EXTRA STUFF