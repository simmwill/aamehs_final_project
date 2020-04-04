# PCA+FA Lab Session
# Marianthi-Anna Kioumourtzoglou
# Sebastian Rowland
# Advanced Analytic Methods for Environmental Epidemiology
# Session 8: PCA+FA
# Special thanks to 
# Yanelli Nunez, Lizzy Gibson, Ahlam Abuawad, and Marianthi-Anna Kioumourtzoglou 
# For example code from the 2018 Mixtures Workshop

####***********************
#### Table of Contents ####
####***********************
# 0:  Preparation 
# 1:  Prepare Exposure Data

# A: Principal Component Analysis 
# 2: Run Principal Component Analysis 
# 3: Visualize PCA
# 4: Create Exposure Matrix of PCA Scores

# B: Exploratory Factor Analysis
# 5: Choose Appropriate EFA Model
# 5A: Othogonal Factors 
# 5B: Oblique Factors 
# 5C: Compare Number of Factors
# 6: Visualize EFA 
# 7: Create Exposure Matrix of EFA Scores

####********************
#### 0: Preparation ####
####********************

# 0a Install packages 

# 0a.i Data management packages

install.packages("tidyverse")
install.packages("janitor")

# 0a.ii Visualization packages

install.packages("ggcorrplot")
install.packages("ggfortify")
install.packages("gridExtra")
install.packages("factoextra")
install.packages("ggrepel")
install.packages("GPArotation")
install.packages("pals")
install.packages(GGally)

# 0a.iii Factor Analysis packages 

install.packages("psych")

# 0b Load packages

library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(ggfortify)  
library(gridExtra)
library(factoextra)
library(GPArotation)
library(ggrepel)
library(psych)
library(pals)
library(GGally)

# 0c Declare folder paths

DataFolder   <- "~/Dropbox/AAMEHS/Spring2020/A2_Labs/Lab08_PCA_FA/2020 Lab/"

####*******************************
#### 1: Prepare Exposure Data #####
####*******************************

# 1a Readin Data 

df0 <- read_csv(paste0(DataFolder, "Boston_PM_constituents.csv"))

# 1b Convert date variable to datetime format 

df0$Date <- parse_date_time(df0$Date, "mdy")

# 1c View data 

head(df0)

# 1d Plot potassium concentrations over time

plot(df0$Date, df0$K, type="l")

# 1e Remove days affected by 4th of July 
# fireworks actually emit a significant amount of some constituents, 
# especially potassium 
# so typically air pollution analysts will exclude 4th of July, 
# and the days preceeding it. 

# 1e.i Create a list of days to remove

DaysAffected <- c("7_2", "7_3", "7_4", "7_5")

# 1e.ii Remove those days, for each year 

df0 <- df0 %>% mutate(MonthDay = paste0(month(Date), "_", day(Date))) %>% 
  filter(!(MonthDay %in% DaysAffected )) %>% 
  dplyr::select(-MonthDay)

# 1f Remove unneccesary variables
# we  remove PM2.5, since PM2.5 is total mass of particles 
# and represents the sum of mass of the constituent elements
# we will also remove Sodium
# because XRF readings for Na are not so accurate 

df0 <- df0 %>% dplyr::select(-pm25, -Sodium)

# 1g Remove days with missing data 

df0 <- df0 %>% filter(complete.cases(df0))

# 1h Remove non-exposure variables 

Constituents <- df0 %>% dplyr::select(-Date, -ent)

# 1i Check dimensions of dataset

dim(Constituents)

# Class Question **********************
# How many days of data do we have in our dataset? 
# How many constituents do we have in our dataset? 
#**********************

# 1j Summary statistics on dataset

summary(Constituents)

# 1k Correlation of constituents 

ggcorr(Constituents)

# Class Question **********************
# What does it mean that Al and Si are highly correlated? 
# What does it mean that Cl and S are slightly negatively correlated? 
#**********************


####************************************************
#### A: Principal Components Analysis #####*********
####************************************************

####*******************************************
#### 2: Run Principal Components Analysis #####
####*******************************************

# 2a Run PCA 
# scale.: a logical value indicating whether the variables should be scaled
# prcomp requires the columns to have a variance of 1 before doing analyis 
# if scale = TRUE, prcomp will scale our data for us. 

pca.constituent  <- prcomp(Constituents, scale. = TRUE) 

# 2b Structure of PCA output

ls(pca.constituent)

# center is the mean of each original variable
# sdev is the scaled singular values
# rotation is the loadings of each component onto each constituent
# x is the matrix of the scores of each pca component on each observation

# Class Question **********************
# How many principal components will we have?
# What will be the correlation between these components? 
#**********************

ncol(pca.constituent$x)
ggcorr(pca.constituent$x)

# 2c Summary of PCA results
# note that the proportion of variance explained is based on the eigenvalues 

summary(pca.constituent)

# Class Question **********************
# What percentage of the variance is explained by Principal Component 1? 
# What about Principal Component 15? 
# Which Principal Component explains the most variance? 
# Could we reasonably describe the data with just 1 principal component? 
#**********************

# 2c Calculate the eigenvalues 
# the eigenvalues are the square of the singular values

eigenvalues.v <- pca.constituent$sdev^2

# 2d Create table of eigenvalues and percent variance explained
# We will calculate the percent variance explained 
# using the eigenvalues

# 2d.i Compute percent of variance explained by each principal component

perc_variance.v <- eigenvalues.v/sum(eigenvalues.v)

# 2d.ii Put percent variance in percentage format 

perc_variance.v <- round(100 * perc_variance.v, 1)

# 2d.iii Compute cumulative percent variance explained

cumulative_perc_var.v <- cumsum(perc_variance.v)

# 2d.iv Create dataframe 

eigenvalues.constituent <- data.frame(Principal_Component = c(1:length(eigenvalues.v)), 
                             Eigenvalues = eigenvalues.v, 
                             Proportion_Var_Explained = perc_variance.v, 
                             Cumulative_Proportion = cumulative_perc_var.v)

# 2d.v View eignvalues 

eigenvalues.constituent

# Class Question **********************
# What does the Cumulative Proportion column tell us? 
# How many Principal Components do we need in order to 
# explain 50% of the variance of the data? 
#**********************

# 2e Assess number of principal components with an eigenvalue > 1
# Since we scaled the exposure data, the variables have a variance of 1 
# so any PC with an eigenvalue > 1 is explaining more of the variability 
# of the data than a column of the original constituent data 

# Class Question **********************
# How many Principal Components have a eigenvalue greater than 1? 
#**********************

###************************
#### 3: Visualize PCA #####
####***********************

# 3a Scree Plot
# Plots the proportion of variance explained by each component 

# fviz is a specially written function 
# that takes in pca solution objects 
# and then visualizes proportion variance from each PC
# One way to determine the number of factors or components in 
# a data or correlation matrix is to examine
# the “scree" plot of the successive eigenvalues. 
# Sharp breaks in the plot suggest the appropriate number of components
# or factors to extract. 
# The number of components to use depends on the research question 
# and the data itself. 

fviz_eig(pca.constituent, main = "Percent Variance Explained \n by Principal Component",
         xlab = "Principal Component",
         ylim = c(0,70)) 

# Class Question **********************
# Where do we see a steep drop in the curve? 
# This indicates a drop in the marginal increase in information provided by the PC
#**********************

# 3b Visualization of the loadings 
# Loadings are the weights that each original variable contributes to a PC

# 3b.i Extract the rotation or loadings matrix 

loadings.constituent <- as.data.frame.matrix(pca.constituent$rotation) 

# 3b.ii Create column with names of constituents

loadings.constituent$Constituent <- row.names(loadings.constituent)

# 3b.iii Put loading data in long format 

loadings.long <- loadings.constituent %>% 
  gather(key = "PC", value = "Loading", -Constituent) 

# 3b.iv Plot just 2 Principal Components

loadings.long.2PC <- loadings.long %>% 
  filter(PC %in% c("PC1", "PC2")) 

# choose color palette
Color.list <- stepped(20)

# make plot 
ggplot(loadings.long.2PC, aes(x = Constituent, y = Loading)) + 
  geom_col(aes(color = Constituent, fill= Constituent)) +  # creates a column for each loading
  geom_hline(yintercept = 0, size = 0.2) + # creates a line at 0
  facet_wrap(. ~ PC) +                       # creates a distinct box for each PC 
  theme_bw() +                             # sets theme options
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = expression("PM"[2.5]~"Constituents"),
       y = "Loadings") + 
  scale_color_manual(values = Color.list) +
  scale_fill_manual(values = Color.list) 

# Class Question **********************
# Which elements have the biggest loadings for PC2? 
# What does it mean that elements X and Y have positive loadings 
# Whereas elements P and Q have negative loadings? 
# What would it mean if a day had a score of 5 for PC1? (all other socres= 0)
# What would it mean if a day had a score of -5 for PC1? (all other scores = 0)
#**********************

# 3b.v Choose just the first 7 principal components 

loadings.long.7PC <- loadings.long %>% 
  filter(PC %in% c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")) 

# 3b.vi Plot 7 principal components 

Color.list <- stepped(20) 

ggplot(loadings.long.7PC, aes(x = Constituent, y = Loading)) + 
  geom_col(aes(color = Constituent, fill= Constituent)) +   # creates a column for each loading
  geom_hline(yintercept = 0, size = 0.2) + # creates a line at 0
  facet_wrap(~ PC) +                       # creates a distinct box for each PC 
  theme_bw() +                             # sets theme options
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = expression("PM"[2.5]~"Constituents"),
       y = "Loadings") + 
  scale_color_manual(values = Color.list) +
  scale_fill_manual(values = Color.list)

# Class Question **********************
# What are the most important elements for PC7?  
# Which PC most explain the variability of Se?
#**********************

# 3c.i Plot bivariate loadings
# Creates a plot showing the loadings for principal component 1 and 2. 

autoplot(pca.constituent,    # name the pca solution
         data = Constituents, # name the original data
         size = 0.8, colour = 'blue', alpha = 0.5,    
         loadings = FALSE, loadings.colour = 'orange',  
         loadings.label = TRUE, loadings.label.repel = T, 
         loadings.label.size = 2.5, loadings.label.colour = 'black',
         main = "Principal Component Analysis Loading Plot")

# 3c.ii ggplot version 

# ggplot(loadings, aes(x = PC1, y = PC2)) + 
#   geom_point() +
#   geom_label_repel(aes(label = Constituent),
#                    box.padding   = 0.35,
#                    point.padding = 0.5,
#                    segment.color = 'grey50') + 
#   theme_bw() + 
#   theme(legend.position = "bottom") +
#   labs(title = "Variable Loadings on First and Second Factors")

###***********************************************
#### 4: Create Exposure Matrix of PCA Scores #####
####**********************************************

# Each day has a score for each principal component, 
# which is computed from the concentrations of constituents
# the score can be thought of as how much of that component
# was observed on that day 
# x is a matrix of the scores in the pca solution from prcomp

# we can combine the date variable and
# the scores of the first 7 principal components 
# to create a new exposure matrix 
# that we could then use in an epidemiological analysis 
# where the principal components are continuous variables

# 4a Extract pca scores 

scores.constituent <- as.data.frame(pca.constituent$x)

# 4b Choose just the first 7 principal components 

scores.constituent <- scores.constituent %>% 
  select(PC1, PC2, PC3, PC4, PC5, PC6, PC7)

# 4c Add the dates from the original data 

scores.constituent$Date <- df0$Date

# 4d View dataframe 

head(scores.constituent)

# Class Question **********************
# What does the column for PC1 mean?
# What do we expect to be the correlation between PC1 and PC2? 
#**********************

# 4e Compute correlations

cor(scores.constituent$PC1, scores.constituent$PC2)
cor(scores.constituent$PC1, scores.constituent$PC3)
cor(scores.constituent$PC1, scores.constituent$PC5)

# 4f Visualize correlations
scores.constituent1 <- scores.constituent %>% select(-Date)

ggcorr(scores.constituent1)


####**********************************************
#### B: Exploratory Factor Analysis #####*********
####**********************************************

# clean the environment 
rm(eigenvalues.constituent, eigenvalues.v, loadings.constituent, loadings.long,
   loadings.long.2PC, DaysAffected, cumulative_perc_var.v,
   loadings.long.7PC, perc_variance.v, pca.constituent,  scores.constituent, scores.constituent1)

# In factor analysis, we need to pre-specify the number of factors in the solution 

# If we have expert knowledge about the variables / exposures /sources
# we can use that knowledge to inform the number of factors 

# If we do not have expert knowledge, 
# We can use the data to suggest a range of number of factors 
# we can perform a PCA and then either
# use the elbows of the scree plot 
# or the number of components with eigenvalues >1

# In this case, there have been a number of studies of air pollution 
# in Boston, 
# and based on those studies, we expect 5-7 sources. 

####*************************
#### 5: Orthogonal EFA  #####
####*************************

# Orthogonal rotation is used if it is desirable to identify factors 
# that are as independent from one another as possible.
# In Factor Analysis you get values for uniqueness and comparativeness. 

# 5a Create orthogonal EFA solution with 5 factors 

fa_5.ortho <- fa(Constituents,
                 nfactors = 5,            # number of factors in the solution 
                 rotate   = "varimax",    # rotation
                 scores   = "regression", # method to estimate scores
                 fm       = "ml")         # estimation method

# 5b View EFA solution with 5 factors
# the EFA is a list with many parts 
# similar to a regression model 
# we can use $ to call specific elements 
ls(fa_5.ortho)

fa_5.ortho$factors
fa_5.ortho

# view loadings
# when we call for the loadings object, 
# R will not present loadings lower than 0.1
# but they exist
fa_5.ortho$loadings

# view scores
head(fa_5.ortho$scores) 

# Visualize correlations of scores
ggcorr(fa_5.ortho$scores)

# Class Question **********************
# What does the column for ML4 mean?
# What does the red color in the ML3-ML5 box tell us? 
#**********************

####***********************
#### 6: Oblique EFA   #####
####***********************

# Oblique rotation (i.e. correlated factors) are commonly used 
# since we often hypothesize our latent variables of interest
# to be correlated with one another.
# Rotated ortogonal solution to get correlated results.
# In the orthogonal solution we only get uncorrelated results. 

# 6a Create oblique solution with 5 factors

fa_5.oblique <- fa(Constituents, 
                 nfactors = 5,             
                 rotate   = "promax",     # promax is a popular oblique rotation
                 scores   = "regression", 
                 fm       = "ml") 

# 6b View oblique solution with 5 factors

fa_5.oblique

# here ivsualize correlations,
# and first ask the students if they think that the PC will be correlated
# use ggcorr
ggcorr(fa_5.oblique$scores)

# Class Question **********************
# What is the major difference between the
# oblique rotation and the orthogonal rotation?
#**********************

# 6c Create oblique solution with 6 factors 

fa_6.oblique <- fa(Constituents, nfactors = 6, rotate = "promax", scores = "regression", fm = "ml") 

# 6d View oblique solution with 6 factors 

fa_6.oblique

# 6e Create oblique EFA solutions with 7 factors  

fa_7.oblique <- fa(Constituents, nfactors = 7, rotate = "promax", scores = "regression", fm = "ml") 

#####*****************************
#### 7: Compare EFA Model Fit  #### 
#####*****************************
# If we do not have expert knowledge to guide us in choosing the best factor solution 
# we can used eBIC to compare how well models fit the data 
# A lower eBIC score corresponds to a better fit 

# from the documentation:
# "eBIC -- When normal theory fails (e.g., in the case of non-positive definite matrices), 
# it useful to examine the empirically derived eBIC
# based upon the empirical chi^2 - 2 df."

# 7a Create table of EFA models and their eBIC scores

# 7a.i Construct Table

fit.efa <- as.data.frame(rbind(cbind("5 Factor", "Promax", round(fa_5.oblique$EBIC)),
                               cbind("6 Factor", "Promax", round(fa_6.oblique$EBIC)),
                               cbind("7 Factor", "Promax", round(fa_7.oblique$EBIC))))

# 7a.ii Rename columns

names(fit.efa) <- c("Model", "Rotation", "EBIC")

# 7b Examine fit table 

fit.efa

# Class Question **********************
# Which EFA model has the best fit, according to eBIC?
#**********************

#####**********************
#### 8: Visualize EFA  #### 
#####**********************

# 8a Extract loadings

# 8a.i Create table of loadings 

loadings <- data.frame(fa_7.oblique$loadings[])

# 8a.ii Create column with names of constituents

loadings$Constituent <- rownames(fa_7.oblique$loadings)

# 8b Identify the factor with the largest contribution to each constituent

loadings$Max <- colnames(loadings[,1:7])[max.col(loadings[,1:7], ties.method = "first")]

# 8c Plot Loadings 

# 8c.i Rename factor columns 
loadings <- loadings %>% 
  rename(Factor1 = ML1, 
         Factor2 = ML2, 
         Factor3 = ML3, 
         Factor4 = ML4, 
         Factor5 = ML5, 
         Factor6 = ML6, 
         Factor7 = ML7)

# 8c.ii Put loading data in long format 

loadings.long <- loadings %>% 
  dplyr::select(-Max) %>% 
  gather(key = "Factor", value = "Loading", -Constituent)

# 8c.iii Choose just the first 7 factors
# here we will plot all 7 factors, but you can use this line 
# to choose to plot fewer factors

loadings.long.2F <- loadings.long %>% 
  filter(Factor %in% c("Factor1", "Factor2")) 

loadings.long.6F <- loadings.long %>% 
  filter(Factor %in% c("Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6")) 

# 8c.iv Loadings plot 

Color.list <- stepped(20) 

ggplot(loadings.long.2F, aes(x = Constituent, y = Loading)) + 
  geom_col(aes(color = Constituent, fill= Constituent)) +   # creates a column for each loading
  geom_hline(yintercept = 0, size = 0.2) + # creates a line at 0
  facet_wrap(~ Factor) +                       # creates a distinct box for each PC 
  theme_bw() +                             # sets theme options
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = expression("PM"[2.5]~"Constituents"),
       y = "Loadings") + 
  scale_color_manual(values = Color.list) +
  scale_fill_manual(values = Color.list)

# Class Question **********************
# Which elements have the biggest loadings for Factor 1?
# What about Factor 2? 
#**********************

ggplot(loadings.long.6F, aes(x = Constituent, y = Loading)) + 
  geom_col(aes(color = Constituent, fill= Constituent)) +   # creates a column for each loading
  geom_hline(yintercept = 0, size = 0.2) + # creates a line at 0
  facet_wrap(~ Factor) +                       # creates a distinct box for each PC 
  theme_bw() +                             # sets theme options
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = expression("PM"[2.5]~"Constituents"),
       y = "Loadings") + 
  scale_color_manual(values = Color.list) +
  scale_fill_manual(values = Color.list)

# Class Question **********************
# If one day had very high Br, which factor would likely be high? 
# If one day had low Al, Ca, and Si, which factor would likely be low?
# If a day had high Factor 5, which constituents would likely be high? 
#**********************

# 8d Plot Bivariate loadings
ggplot(loadings, aes(x = Factor1, y = Factor2)) + 
geom_point() +
geom_label_repel(aes(label = Constituent),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
theme_bw() + 
theme(legend.position = "bottom") +
labs(title = "Variable Loadings on First and Second Factors")

#####********************************************
#### 9: Create Exposure Matrix of EFA Scores #### 
#####********************************************

# Each day has a score for each factor 
# The score is the degree to which that factor contributed to 
# that day's PM

# 9a Extract Scores

# 9a.i Create table of scores

scores <- data.frame(fa_7.oblique$scores[])

# 9a.ii Create column with names of constituents

scores$Constituent <- rownames(fa_7.oblique$scores)

# 9b For each day, identify the factor that contributed the most to the PM2.5

scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]

# 9c Add day column 

scores$Date <- df0$Date

# 9c View scores table 
head(scores)

# Class Question **********************
# What is the score of Factor 1 (ML1) on 1/1/2003? 
# Do we expect Al to be high or low? 
#**********************

Constituents$Al[1]
mean(Constituents$Al)

# Class Question **********************
# What is the score of Factor 2 (ML2) on 1/2/2003? 
# Do we expect Ni to be high or low? 
#**********************
Constituents$Ni[2]
mean(Constituents$Ni)
