
##########For analysis using PCA with all of these categories

###### Load Data#####

pc_11 = haven::read_xpt('./data/EPH_11_12.xpt')           ## different dataset for bisphenols 11-12
pc_13 = haven::read_xpt('./data/EPHPP_13_14.xpt')
pc_15 = haven::read_xpt('./data/EPHPP_15_16.xpt')

# demographics
demo_11 = haven::read_xpt('./data/DEMO_11_12.xpt')
demo_13 = haven::read_xpt('./data/DEMO_13_14.xpt')
demo_15 = haven::read_xpt('./data/DEMO_15_16.xpt')

# medical conditions
med_11 = haven::read_xpt('./data/MCQ_11_12.xpt')
med_13 = haven::read_xpt('./data/MCQ_13_14.xpt')
med_15 = haven::read_xpt('./data/MCQ_15_16.xpt')

#################for each year
data_11 =
  list(pc_11,    ## exposures
       med_11,    ## outcomes
       demo_11) %>%      ## covariates - removed BMI
  reduce(full_join, by = "SEQN") %>% 
  mutate(year = "11_12")

data_13 =
  list(pc_13,    ## exposures
       med_13,    ## outcomes
       demo_13) %>%      ## covariates - removed BMI
  reduce(full_join, by = "SEQN") %>% 
  mutate(year = "13_14")

data_15 =
  list(pc_15,    ## exposures
       med_15,    ## outcomes
       demo_15) %>%      ## covariates - removed bmi
  reduce(full_join, by = "SEQN") %>% 
  mutate(year = "15_16")


#################joining data/cleaning
## exposures

join_13 =
  data_13 %>% 
  mutate(year = "13_14") %>% 
  select(
    year,
    id = SEQN,    ## unique ID
    
    ## EXPOSURES
    
    bpa_u = URXBPH,   ## urinary BPA
    bpa_u_det = URDBPHLC,   ## 1 = below lower detection limit
    bpf_u = URXBPF,
    bpf_u_det = URDBPFLC,
    bps_u = URXBPS,
    bps_u_det = URDBPSLC,
    
    triclocarban_u = URXTLC,  ## Urinary Triclocarban (ng/mL)
    triclocarban_u_det = URDTLCLC,  ## Urinary Triclocarban comment
    triclosan_u = URXTRS,  ## Urinary Triclosan (ng/mL)
    triclosan_u_det = URDTRSLC,  ## Urinary Triclosan comment
    butyl_paraben_u = URXBUP,  ## Butyl paraben (ng/ml)
    butyl_paraben_u_det = URDBUPLC,  ## Butyl paraben comment
    ethyl_paraben_u = URXEPB,  ## Ethyl paraben (ng/ml)
    ethyl_paraben_u_det = URDEPBLC,  ## Ethyl paraben comment
    methyl_paraben_u = URXMPB,  ## Methyl paraben (ng/ml)
    methyl_paraben_u_det = URDMPBLC,  ## Methyl paraben comment
    propyl_paraben_u = URXPPB,  ## Propyl paraben (ng/ml)
    propyl_paraben_u_det = URDPPBLC,  ## Propyl paraben comment
    dichlorophenol_2_5_u = URX14D,  ## 2,5-dichlorophenol (ug/L)
    dichlorophenol_2_5_u_det = URD14DLC,  ## 2,5-dichlorophenol comment
    dichlorophenol_2_4_u = URXDCB,  ## 2,4-dichlorophenol (ug/L)
    dichlorophenol_2_4_u_det = URDDCBLC,  ## 2,4-dichlorophenol comment
    creatinine_u = URXUCR,  ## Urinary creatinine (no comment for detection limit) -- two measures of this, ignoring for now
    
    ## OUTCOMES
    
    thyroid_problem = MCQ160M,  ## ever told you had thyroid problem
    thyroid_current = MCQ170M,  ## still have thyroid problem
    #thyroid_age = MCQ180M       ## age at which told you had thyroid problem - shouldn't be relevant since cross-sectional?
    
    ## COVARIATES
    
    #bmi = BMXBMI,
    sex = RIAGENDR,
    age = RIDAGEYR,
    race = RIDRETH3     ## 1 = mex am, 2 = other hisp, 3 = non-hisp white, 4 = nh black, [no 5], 6 = nh asian, 7 = other/multi
    
  )

join_15 =
  data_15 %>% 
  mutate(year = "15_16") %>% 
  select(
    year,
    id = SEQN,    ## unique ID
    
    ## EXPOSURES
    
    bpa_u = URXBPH,   ## urinary BPA
    bpa_u_det = URDBPHLC,   ## 1 = below lower detection limit
    bpf_u = URXBPF,
    bpf_u_det = URDBPFLC,
    bps_u = URXBPS,
    bps_u_det = URDBPSLC,
    
    triclocarban_u = URXTLC,  ## Urinary Triclocarban (ng/mL)
    triclocarban_u_det = URDTLCLC,  ## Urinary Triclocarban comment
    triclosan_u = URXTRS,  ## Urinary Triclosan (ng/mL)
    triclosan_u_det = URDTRSLC,  ## Urinary Triclosan comment
    butyl_paraben_u = URXBUP,  ## Butyl paraben (ng/ml)
    butyl_paraben_u_det = URDBUPLC,  ## Butyl paraben comment
    ethyl_paraben_u = URXEPB,  ## Ethyl paraben (ng/ml)
    ethyl_paraben_u_det = URDEPBLC,  ## Ethyl paraben comment
    methyl_paraben_u = URXMPB,  ## Methyl paraben (ng/ml)
    methyl_paraben_u_det = URDMPBLC,  ## Methyl paraben comment
    propyl_paraben_u = URXPPB,  ## Propyl paraben (ng/ml)
    propyl_paraben_u_det = URDPPBLC,  ## Propyl paraben comment
    dichlorophenol_2_5_u = URX14D,  ## 2,5-dichlorophenol (ug/L)
    dichlorophenol_2_5_u_det = URD14DLC,  ## 2,5-dichlorophenol comment
    dichlorophenol_2_4_u = URXDCB,  ## 2,4-dichlorophenol (ug/L)
    dichlorophenol_2_4_u_det = URDDCBLC,  ## 2,4-dichlorophenol comment
    #creatinine_u = URXUCR,  ## Urinary creatinine (no comment for detection limit) -- two measures of this, ignoring for now (for 2015 is in separate data file)
    
    
    ## OUTCOMES
    
    thyroid_problem = MCQ160M,  ## ever told you had thyroid problem
    thyroid_current = MCQ170M,  ## still have thyroid problem
    #thyroid_age = MCQ180M       ## age at which told you had thyroid problem - shouldn't be relevant since cross-sectional?
    
    ## COVARIATES
    
    # bmi = BMXBMI,
    sex = RIAGENDR,
    age = RIDAGEYR,
    race = RIDRETH3     ## 1 = mex am, 2 = other hisp, 3 = non-hisp white, 4 = nh black, [no 5], 6 = nh asian, 7 = other/multi
    
  )

joined = 
  full_join(
    join_13,
    join_15
  )
