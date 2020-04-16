##########################
### *  QUANTILE REG NEW *
#########################

####************************************
#### Table of Contents ####

####********************
#### 0: Preparation + data prep ####
####********************

library(readr)
library(dplyr) 
library(quantreg)
library(ggplot2)

DataFolder   <- "~/Desktop/CUMC-MPH/Spring 2020/ADVANCED METHODS/aamehs_final_project/data/"
OutputFolder <- "~/Desktop/CUMC-MPH/Spring 2020/ADVANCED METHODS/aamehs_final_project/data/"

####******************
#### 1: Load Data + see dist ####
####******************
# 1a Load data 

df =readRDS('./data/new_analytic_dataset.RDS')
head(df)

plot(df$log_bpa_creatinine)
plot(df$log_tsh)

####*********************
#### 2: Median Model ####
####*********************

Mod50 <- rq(log_tsh ~ log_bpa_creatinine + hh_income + sex + age + 
              race, 
            data = df, 
            tau = 0.5)
summary(Mod50, alpha = 0.05, se ="boot")

coeff.qr50    <- summary(Mod50)$coefficients

Mod50.qr <- c(coeff.qr50[2,1], 
               coeff.qr50[2,1] - 1.96 * coeff.qr50[2,2], 
               coeff.qr50[2,1] + 1.96 * coeff.qr50[2,2])



####*******************************************
#### 2: Compare QR at Different Quantiles  ####
####*******************************************

Mods25.50 <- rq(log_tsh ~ log_bpa_creatinine + hh_income + sex + age + 
                  race, 
                data = df, tau= c(0.25, 0.50)) # c() creates a vector

summary(Mods25.50)

summary25.50 <- summary(Mods25.50, alpha = 0.05, se = "boot")

Model25th   <- c(summary25.50[[1]]$coefficients[2,1:3])
Model50th   <- c(summary25.50[[2]]$coefficients[2,1:3])


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

TauList <- seq(0.1, 0.9, by = 0.1)
TauList

qr.Mods  <- rq(log_tsh ~ log_bpa_creatinine + hh_income + age + sex + 
                 race, 
               data = df, 
               tau = TauList)

# 3c.ii Assemble estimates from each model
summary(df$tsh)

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
names(coeff.table) <- c("coeff", "Std.Error", " t.value")



coeff.table$lci = coeff.table$coeff - 1.96 * coeff.table$Std.Error
coeff.table$uci = coeff.table$coeff + 1.96 * coeff.table$Std.Error



# set names for dataframe


coeff.table        <- coeff.table %>% as.data.frame () %>% 
  mutate(ModelName = c("Model 10th", "Model 20th", "Model 30th", "Model 40th",
                       "Model 50th", "Model 60th", "Model 70th", "Model 80th", 
                       "Model 90th"))

# 3b.ii Plot

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

#------------------------------------------------------------------------------------------------------------
##*********
## log_tert_octylphenol_creatinine

TauList <- seq(0.1, 0.9, by = 0.1)
TauList

qr.Mods  <- rq(log_tsh ~ log_tert_octylphenol_creatinine + hh_income + age + sex + 
                 race, 
               data = df, 
               tau = TauList)

# 3c.ii Assemble estimates from each model

summary.qr.Mods <- summary(qr.Mods, alpha = 0.05, se= "boot")

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

# 3b.ii Plot

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


##### log_benzophenone_3_creatinine
## log_benzophenone_3_creatinine##
####

TauList <- seq(0.1, 0.9, by = 0.1)
TauList

qr.Mods  <- rq(log_tsh ~ log_propyl_paraben_creatinine + hh_income + age + sex + 
                 race, 
               data = df, 
               tau = TauList)

# 3c.ii Assemble estimates from each model

summary.qr.Mods <- summary(qr.Mods, alpha = 0.05, se= "boot")

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

# 3b.ii Plot

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

