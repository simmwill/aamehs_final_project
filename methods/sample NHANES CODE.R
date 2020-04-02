##################################33
######sample NHANES code###########


## Load data and clean it up
r data
# Load data

#Demographic files
demo1112 <- read.xport("DEMO_G.XPT")
#Laboratory urinary arsenic files
as1112 <- read.xport("UAS_G.XPT")
#Blood pressure examination files
bp1112 <- read.xport("BPX_G.XPT")

#Merge data to create one dataframe per year; then keep only the necessary variables:
nh1112<-merge(demo1112,bp1112,by="SEQN",all=T)
nh1112<-merge(nh1112, as1112, by="SEQN", all=T)

#Here is a list of variables we will need for our analysis:
# SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, WTMEC2YR, SDMVPSU, SDMVSTRA, WTSA2YR, URXUAS, BPXSY1, BPXSY2, BPXSY3

# Remove unnecessary variables and rename (My preference is to keep the weighting variable names as they are)
nh1112_sub <- nh1112 %>%
  select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, WTMEC2YR, SDMVPSU, SDMVSTRA, WTSA2YR, URXUAS, BPXSY1, BPXSY2, BPXSY3)
head(nh1112_sub)

## NOTE: the correct weights to use for this analysis of urinary arsenic data are WTSA2YR

nh1112_sub <- nh1112_sub%>%
  rename(gender = RIAGENDR, age = RIDAGEYR, race=RIDRETH1, totalarsenic = URXUAS, sbp1 = BPXSY1, sbp2 = BPXSY2, sbp3 = BPXSY3)  

glimpse(nh1112_sub)

# Label variable values
nh1112_sub <- nh1112_sub %>%
  set_value_labels(
    gender = c("Male" = 1, "Female" = 2), 
    race = c("Mexican American" = 1, "Other Hispanic" = 2, "NH White" = 3, "NH Black" = 4, "Other including multi" = 5))

glimpse(nh1112_sub)

# Treat any labelled variables as factors
nh1112_sub <- nh1112_sub %>%
  mutate_if(is.labelled, to_factor)

```



# Part A.
**Let's evaluate the missing data. Urinary arsenic is only measured in a 1/3 subset of participants 6yrs and older. Let's find how many people are missing our main variables of interest: urinary arsenic and blood pressure readings **
  ```{r A}
describe(nh1112_sub$totalarsenic) # details about totalarsenic
describe(nh1112_sub$sbp1) 
describe(nh1112_sub$sbp2) 
describe(nh1112_sub$sbp3) 

nh1112_sub<-nh1112_sub[which(!is.na(nh1112_sub$totalarsenic)),]
nh1112_sub<-nh1112_sub[which(!is.na(nh1112_sub$sbp1)),]
nh1112_sub<-nh1112_sub[which(!is.na(nh1112_sub$sbp2)),]
nh1112_sub<-nh1112_sub[which(!is.na(nh1112_sub$sbp3)),]

dim(nh1112_sub) #dimentions

```
**7,252 are missing urinary total arsenic; 3,000 are missing SB1; 2,848 are missing SB2;  2,839 are missing SB3. After removing these participants 2,124 remain. ** 
  
  # Part B.
  **Now I want to explore if acconting for survey weights really matters. Let's calculate the percentage of the US population in 2011-2012 who are Non-Hispanic Black, with and without accounting for the complex survey design**
```{r B}
#Set the svydesign!
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub)

#If I ask svymean to give me the mean of a binary or categorical variable, it will give me a proportion:
svymean(~race, nhanes.svy)
#To automatically round your estimates up to percentages:
100*(svymean(~race, nhanes.svy))

#Another option: using "wtd.table" and specifying your survey weights as "nh1112_sub$WTSA2YR"
round(100*wtd.table(nh1112_sub$race,weights=nh1112_sub$WTSA2YR,type="table")/sum(nh1112_sub$WTSA2YR),1)

table(nh1112_sub$race) #Incorrect; this is unweighted
574/( 254+219+708+574+369) #[1] 0.2702448

```
**Accounting for weights: 12.2%.  Not accounting for weights: 27.0%. ** 

# Part C
**Let's create a new variable that is the average of the three systolic blood pressure readings and call it "avbp" (stands for average blood pressure). To find the mean and SE of avbp in the US population (accounting for survey weights and design) we have to set the svydesign. NOTE: We will always have to re-set the survey design after we create any new variable.**
  
  ```{r C}
nh1112_sub$avbp<- (nh1112_sub$sbp1 +nh1112_sub$sbp2 +nh1112_sub$sbp3)/3
describe(nh1112_sub$avbp)
# OR #
nh1112_sub <- nh1112_sub %>%
  mutate(avbp = (sbp1+sbp2+sbp3)/3)

#Setting the survey design, using WTSA2YR as the individual weights
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub)

#svymean is the function that will compute means and SEs of continuous variables within your "svydesign"
svymean(~avbp, nhanes.svy)
```
**Mean = 118.91; SE = 0.91 ** 
  
  # Part D
  **Okay, now let's determine if we should log-transform averaage blood pressure for statistical tests.**
```{r D}

# "svyhist" creates histograms accounting for survey weights
svyhist(~avbp, nhanes.svy, main="Survey weighted average BP",col="blue")
svyhist(~log(avbp), nhanes.svy, main="Survey weighted log(average BP)",col="blue")
# Not bad, but looks better log transformed!

#Okay, let's say we want to now compare log(avp) across race groups.
# If we are comparing a continuous variable across a categorical variable, what are our options? 
# Option 1: Linear regression
# Currently, Mexican-Americans are coded as our reference group:
model1 <- svyglm(log(avbp)~race,nhanes.svy)
summary(model1)
# think about the intercept (alpha) as a reference group

# Option 2: Use "svyby", which allows us to look at means across levels of a factor (like an ANOVA).
#svyby is the function that will compute summary statistics of continuous variables within your "svydesign", across a cateogrical variable. In this case, we want average log transformed average blood pressure separately for males and females. We specify "svymean" at the end of the command line to tell the command that we want the average means.
svyby(~log(avbp), by=~race, nhanes.svy,  svymean, ci=TRUE)
```
**With log transformation: NH Black highest (GM 120.342) Mexican-American lowest(GM 112.621)** 
  
  # Part E
  **I may know from background knowledge that males have higher BP on average than women. I should check my new variable to see if it matches my background knowledge. Let's find the mean or geometric mean of average blood pressure in males vs. females, and determine if this difference is statistically significant**
```{r E}
## 
#svyby is the function that will compute summary statistics of continuous variables within your "svydesign", across a cateogrical variable. In this case, we want average log transformed average blood pressure separately for males and females. We specify "svymean" at the end of the command line to tell the command that we want the average means.
svyby(~log(avbp), by=~gender, nhanes.svy,  svymean)
exp(4.789061) #120.1885
exp(4.749571) #115.5347

#To determine if this difference is statistically significnat, I do a t-test because I am comparing the mean of a continuous variable across two groups 
tt<-svyttest(log(avbp)~gender, nhanes.svy)
tt
#What if I want a 90% CI instead of a 95% CI? 
confint(tt, level=0.9)

```
**Males = 120.1885; Females = 115.5347; yes significantly different (p<0.05) ** 

# Part F
**Let's use our new average systolic blood pressure variable to define hypertension as an average systolic blood pressure greater than or equal to 120 mm Hg). One way to check our confidence in our new variable is to determine how many participants in this dataset have hypertension. Accounting for survey design, what is the proportion of the 2011/2012 US population with hypertension? NOTE: We will always have to re-set the survey design after we create any new variable.**
  ```{r F}

nh1112_sub$htn <-0
nh1112_sub$htn[nh1112_sub$avbp>=120]<-1
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub) #Re-setting the svydesign!
svymean(~htn, nhanes.svy) #41.0%
# OR #
nh1112_sub <- nh1112_sub %>%
  mutate(htn = if_else(avbp>=120, "1", if_else(avbp<120,"0", NA_character_)))

nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub)
table(nh1112_sub$htn)
svymean(~htn, nhanes.svy) #41.0%

#What type of variable is my "htn"?
class(nh1112_sub$htn)
#Let's change it to a numeric.
nh1112_sub$htn<-as.numeric(nh1112_sub$htn)

table(nh1112_sub$htn)
857/(1267+857)# 40.3% This tells us the proprotion of hypertension in our database but does not account for survey weights!

```
**857 individuals (41.0% of US population in 2011/2012) are hypertensive ** 
  
  # Part G
  **Let's explore if hypertension status is associated with race, and if the difference in hypertension status across racial groups is statistically significant?**
```{r G}
#We are comparing a binary variable (hypertension status) across a categorical variable, so let's use a chi-squared test. We can get the raw numbers from a simple table.
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub) #Re-setting the svydesign!

#Contingency table and chi-squared tests for survey data using svychisq
table(nh1112_sub$htn, nh1112_sub$race)
svychisq(~htn+race, nhanes.svy, statistic="adjWald")

```
**Yes, the difference is significant** 
  
  # Part H
  **Now let's create our new variable for tertiles of urinary total arsenic concentrations. We use "svyquantile" to determine  cut-points for tertiles. Then we will want to know how many participants are in each tertile of urinary total arsenic. Remember we will have to re-set the survey design after creating a new variable.**
```{r H}

#"svyquantile" will give us quantiles, but weighted by survey weights
#We tell it what percentile cut-points we want using this piece of code, with "c" standing for "combine"  -->    c(X,X,X)
#If we want the 10th and 80th percentile, we code it like this:
svyquantile(~totalarsenic, nhanes.svy, c( 0.10, 0.80),ci=F)

#If we want tertiles (33%, 66%), we code it like this:
svyquantile(~totalarsenic, nhanes.svy, c( 0.33, 0.66),ci=F)
#             0.33     0.66
#totalarsenic 3.96 9.499501

# Now that we know where to make the cut-points, we create a new variable for tertiles:
## re-do this with mutate
nh1112_sub$astertiles<-NA
nh1112_sub$astertiles[which(nh1112_sub$totalarsenic<=3.96 )]<-1
nh1112_sub$astertiles[which(nh1112_sub$totalarsenic>3.96&nh1112_sub$totalarsenic<=9.499501 )]<-2
nh1112_sub$astertiles[which(nh1112_sub$totalarsenic>9.499501)]<-3
# OR #
nh1112_sub <- nh1112_sub %>%
  mutate(astertiles = if_else(totalarsenic<=3.96, "1", if_else(totalarsenic>3.96 & totalarsenic<=9.499501,"2", if_else(totalarsenic>9.499501, "3", NA_character_))))

#Now that I have made 1,2,3 characters using "". I can now change them to numerics using:
nh1112_sub$astertiles<-as.numeric(nh1112_sub$astertiles)
class(nh1112_sub$astertiles)

table(nh1112_sub$astertiles) 
#   1   2   3 
# 581 667 876 

#Re-set the svydesign because we have made a new variable:
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh1112_sub)

```
**581, 667, and 876** 


# Part J.
**Determine if you should log transform urinary arsenic concentrations for statistical tests. Which race group has the highest urinary total arsenic? Which group has the lowest urinary total arsenic?**
```{r J}
svyhist(~totalarsenic, nhanes.svy, main="Survey weighted total arsenic",col="blue")
svyhist(~log(totalarsenic), nhanes.svy, main="Survey weighted log(total arsenic)",col="blue")
# Wow, looks much better log transformed!

summary(svyglm(log(totalarsenic)~race,nhanes.svy))
svyby(~log(totalarsenic), by=~race, nhanes.svy,  svymean)
exp(2.439959) #11.47
exp(1.767377) #5.86


# if you find a different variable that has a lower level maybe think about making them a reference group
##Note the same value for the intercept from the glm as for MA group in the svyby
```
**Answer: The Other, including multiple race group has the highest urinary arsenic (GM 11.47ug/L). Non-Hispanic Whites have the lowest (GM 5.86 ug/L)** 

# Part K.
**Compared to Mexican-American participants, which race groups have statistically different urinary arsenic? In other words, set Mexican-American group as the reference in your model**
```{r K}
summary(svyglm(log(totalarsenic)~race,nhanes.svy))
exp(0.2882)

```
**Answer: Compared to Mexican-Americans, "Other including multiple" race groups has average total urinary arsenic that is statistically significantly different. Compared to Mexican-Americans, Non-Hispanic Black participants have a geometric mean of urinary arsenic that is 1.33 ug/L higher (but this difference is not significant).** 


## You now know all the commands that you need to make a Table 1, Table 2, and Table 3 for a paper for publication. 

# Filling in Table 1:
**Overall and across tertiles of urinary total arsenic, compare: **
**A: The number and weighted percentage of individuals by race/ethnic group**
```{r T1 race}
table(nh1112_sub$astertiles)

#The total N can come from a simple table:
table(nh1112_sub$race)

#Weighted proportions:
#Overall:
round(100*svymean(~race, nhanes.svy, na.rm=T),1)

#Separate for each arsenic tertile:
table(nh1112_sub$race, nh1112_sub$astertiles)
round(100*svyby(~race, ~astertiles, nhanes.svy, svymean, na.rm=T),1)

# Is this difference significant? Note: Here we are comparing one categorical varible (arsenic tertiles) to another categorical variable (race) - what test is appropriate?
svychisq(~race+astertiles, nhanes.svy, statistic="adjWald") #Yes different; note R can read your numeric "astertiles" as a group for a chisquared test

nh1112_sub$new<-as.factor(nh1112_sub$astertiles)
nhanes.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=nh1112_sub)
svychisq(~race+new, nhanes.svy, statistic="adjWald") #Same result- note R can read your numeric "astertiles" as a group for a chisquared test; doesn't matter if your tertiles are numeric or factor
```

**B: The number and weighted percentage of individuals by sex**
  ```{r T1 gender}
table(nh1112_sub$gender)
#  Male Female 
#  1092   1032 

100*(svymean(~gender, nhanes.svy))
#               mean     SE
#genderMale   49.488 0.0105
#genderFemale 50.512 0.0105

table(nh1112_sub$gender, nh1112_sub$astertiles)
#          1   2   3
#  Male   267 351 474
#  Female 314 316 402

100*(round(svyby(~gender, ~astertiles, nhanes.svy,  svymean),10))
#  astertiles genderMale genderFemale se.genderMale se.genderFemale
#1        100   42.93879     57.06121      3.228538        3.228538
#2        200   52.48584     47.51416      2.117381        2.117381
#3        300   53.01603     46.98397      2.634025        2.634025

# Is this difference significant? 
# Again we compare two categorical variables-- what test is appropriate?
svychisq(~gender+astertiles, nhanes.svy, statistic="adjWald")  #No
```

**C: The mean and standard error for age (you can treat tertiles as continuous for significance test)**
  ```{r T1 age}
svymean(~age, nhanes.svy)
#        mean     SE
#  age 40.621 0.8572
svyby(~age, ~astertiles, nhanes.svy,  svymean)
#  astertiles      age       se
#1          1 39.18031 1.337795
#2          2 39.83722 1.432049
#3          3 42.77386 1.021377

#Now we are comparing means across 3 groups (comparing a continuous to a categorical variable)- what test is appropraite?
#One option is regression, treating the tertiles as continuous/numeric
modelT1age <- svyglm(astertiles~age, family=gaussian, design=nhanes.svy)
summary(modelT1age) #Yes significant
## You can treat tertiles as continuous for your HW when comparing mean levels of a continuous variable across tertiles

```

**D: The association between urinary total arsenic and average blood pressure (you can treat tertiles as continuous for significance test)**
  ```{r T1 bp}
svymean(~avbp, nhanes.svy)
#       mean     SE
# avbp 118.81 0.9311
round(svyby(~avbp, ~astertiles, nhanes.svy,  svymean),10)
#  astertiles     avbp        se
#1          1 118.0898 1.3497719
#2          2 117.9677 0.9243835
#3          3 120.3113 1.4328200

modelT3 <- svyglm(log(avbp)~log(totalarsenic), family=gaussian, design=nhanes.svy)
summary(modelT3) #Not significant
## You can treat tertiles as continuous for your HW when comparing mean levels of a continuous variable across tertiles

```


# Filling in Table 2:
**Overall and across hypertension status (Y/N), compare: **
  **A: The number and weighted percentage of individuals by race/ethnic group**
  ```{r T2 race}
#The total N can come from a simple table:
table(nh1112_sub$race)

#Weighted proportions:
#Overall:
round(100*svymean(~as.factor(race), nhanes.svy, na.rm=T),1)

#Separate for HTN status:
table(nh1112_sub$race, nh1112_sub$htn)
#                        No Yes
#  Mexican American      187  67
#  Other Hispanic        147  72
#  NH White              410 298
#  NH Black              287 287
#  Other including multi 236 133

#We need to let R read "htn" as numeric to get a proportion across race groups
round(100*svyby(~race, ~htn, nhanes.svy, svymean, na.rm=T),1)

# Is this difference significant? 
svychisq(~race+htn, nhanes.svy, statistic="adjWald") #Yes different

```

**A: The number and weighted percentage of individuals by sex**
  ```{r T2 gender}
table(nh1112_sub$gender)
100*(svymean(~gender, nhanes.svy))

table(nh1112_sub$gender, nh1112_sub$htn)
100*(round(svyby(~gender, ~htn, nhanes.svy,  svymean),10))

svychisq(~gender+htn, nhanes.svy, statistic="adjWald") #Yes different

```

**A: The mean and standard error for age**
  ```{r T2 age}
svymean(~age, nhanes.svy)

round(svyby(~age, ~htn, nhanes.svy,  svymean),10)

#Significant? T-test
tt<-svyttest(age~htn, nhanes.svy)
tt
```

**A: The mean and standard error for urinary arsenic**
  ```{r T2}
svymean(~totalarsenic, nhanes.svy)
#                mean     SE
# totalarsenic 14.657 1.1708

round(svyby(~totalarsenic, ~htn, nhanes.svy,  svymean),10)

svymean(~log(totalarsenic), nhanes.svy)
exp(1.2159)
round(svyby(~log(totalarsenic), ~htn, nhanes.svy,  svymean),10)
exp(1.323323)
exp(1.074877)

tt<-svyttest(log(totalarsenic)~htn, nhanes.svy)
tt #No, not significant
```


# Filling in Table 3:
**Build a linear regression model, regressing urinary arsenic on average systolic blood pressure. Interpret the coefficient.**
  
  ```{r T3}
modelT3<-svyglm(log(avbp)~log(totalarsenic),nhanes.svy)
summary(modelT3)
exp(0.007298 ) #1.007325

```
**Answer: For every one-unit increase in log-arsenic, log-average systolic blood pressure increases by 1.007325 mmHg. This is statistically significant.**  
  
  
  # Here are our tables:
  
  ```{r png}
knitr::include_graphics("myTable1.png")
knitr::include_graphics("myTable2.png")
knitr::include_graphics("myTable3.png")


```

