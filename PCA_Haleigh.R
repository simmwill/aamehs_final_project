#load packages
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

#select exposure vars since this is unsupervised method

expo <- new_analytic_dataset %>% select(tert_octylphenol_creatinine, bpa_creatinine, benzophenone_3_creatinine, triclosan_creatinine, butyl_paraben_creatinine,
    ethyl_paraben_creatinine, methyl_paraben_creatinine, propyl_paraben_creatinine)
saveRDS(expo, "exposures")

#Summary statistics on dataset
summary(expo)
# 1k Correlation of constituents 
ggcorr(expo)
          #not a ton of correlation surprisingly 


##########################################PCA######################################
#scale = TRUE to scale values 
pca.expo  <- prcomp(expo, scale. = TRUE) 

# Summary of PCA results
        # note that the proportion of variance explained is based on the eigenvalues 
summary(pca.expo)

# 2c Calculate the eigenvalues 
        # the eigenvalues are the square of the singular values
eigenvalues.v <- pca.expo$sdev^2

#We will calculate the percent variance explained using the eigenvalues

# 2d.i Compute percent of variance explained by each principal component

perc_variance.v <- eigenvalues.v/sum(eigenvalues.v)

# 2d.ii Put percent variance in percentage format 

perc_variance.v <- round(100 * perc_variance.v, 1)

# 2d.iii Compute cumulative percent variance explained

cumulative_perc_var.v <- cumsum(perc_variance.v)

# 2d.iv Create dataframe 

eigenvalues.expo <- data.frame(Principal_Component = c(1:length(eigenvalues.v)), 
                                      Eigenvalues = eigenvalues.v, 
                                      Proportion_Var_Explained = perc_variance.v, 
                                      Cumulative_Proportion = cumulative_perc_var.v)
#View eignvalues 
eigenvalues.expo

##screeplot 
fviz_eig(pca.expo, main = "Percent Variance Explained \n by Principal Component",
         xlab = "Principal Component",
         ylim = c(0,70))

#Extract the rotation or loadings matrix 
loadings.expo <- as.data.frame.matrix(pca.expo$rotation) 

#Create column with names of expos
loadings.expo$expo <- row.names(loadings.expo)

# Put loading data in long format 
loadings.long <- loadings.expo %>% 
  gather(key = "PC", value = "Loading", -expo) 

# Plot Loadings
loadings.long.8PC <- loadings.long %>% 
  filter(PC %in% c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")) 

# choose color palette
Color.list <- stepped(20)

# make plot 
ggplot(loadings.long.8PC, aes(x = expo, y = Loading)) + 
  geom_col(aes(color = expo, fill= expo)) +  # creates a column for each loading
  geom_hline(yintercept = 0, size = 0.2) + # creates a line at 0
  facet_wrap(. ~ PC) +                       # creates a distinct box for each PC 
  theme_bw() +                             # sets theme options
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = expression("PM"[2.5]~"expos"),
       y = "Loadings") + 
  scale_color_manual(values = Color.list) +
  scale_fill_manual(values = Color.list) 
