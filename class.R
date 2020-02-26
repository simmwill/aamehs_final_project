########################################
######### Sample Code ##################
########################################

#Sample code from Annie Nigra`s class for Environmental Health Data Analysis using NHANES Survey Weights 


Packagaes 
library(survey) #This package handles survey weights/designs easily
library(Hmisc) #This is a package (H misc.) for basic desciptive functions. I like it for the "describe" function.
library(foreign) #This package allows us to open .XPT files
library(tidyverse)
library(labelled)

#Data import usign read.xport

demo1112 <- read.xport("DEMO_G.XPT")
#Laboratory urinary arsenic files
as1112 <- read.xport("UAS_G.XPT")
#Blood pressure examination files
bp1112 <- read.xport("BPX_G.XPT")