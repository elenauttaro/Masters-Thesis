knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(haven)
library(tidyverse)
#install.packages("survey")
library(survey)
#install.packages("srvyr")
library(srvyr)


#Reading data files

data19 <- read_csv("adult19.csv")
data20 <- read_csv("adult20.csv")
data21 <- read_csv("adult21.csv")
data22 <- read_csv("adult22.csv")


#grabbing select values from each dataset

new19 <- na.omit(data19[,c("PPSU","PSTRAT","WTFA_A","MARITAL_A","HISPALLP_A","AGEP_A","SEX_A","SMKCIGST_A","FAMINCTC_A","BMICAT_A","BREASCAN_A")])

new20<- na.omit(data20[,c("PPSU","PSTRAT","WTFA_A","MARITAL_A","HISPALLP_A","AGEP_A","SMKCIGST_A","BMICAT_A","BREASCAN_A","SEX_A")])

new21 <- na.omit(data21[,c("PPSU","PSTRAT","WTFA_A","MARITAL_A","HISPALLP_A","AGEP_A","SMKCIGST_A","BMICAT_A","BREASCAN_A","SEX_A")])

new22 <- na.omit(data22[,c("PPSU","PSTRAT","WTFA_A","MARITAL_A","HISPALLP_A","AGEP_A","SMKCIGST_A","BMICAT_A","SEX_A","BREASCAN_A")])

merg1<- bind_rows(new19,new20)
merg2 <- bind_rows(merg1,new21)
data <- bind_rows(merg2,new22)



#cleaning dataset

#deleting males from dataset
df <- subset(data, SEX_A!=1)
#View(df)

#combining 4,5 6 hispallpa
df$HISPALLP_A <- ifelse(df$HISPALLP_A %in% c(5, 6), 5, df$HISPALLP_A)
table(df$HISPALLP_A)

#combining underweight and normal weight 
df$BMICAT_A <- ifelse(df$BMICAT_A %in% c(1,2),1, ifelse (df$BMICAT_A %in% c(3,4),2, df$BMICAT_A))
df <- subset(df, !BREASCAN_A %in% c(7,9))
#deleting unknown BMI
df <- subset(df, !(BMICAT_A %in%9))
table(df$BMICAT_A)

#deleting refused marital status
df <- subset(df, !(MARITAL_A %in%c(7, 8, 9)))

#deleting unknown smoking
df <- subset(df, !(SMKCIGST_A %in%9))

#deleting NA from family income
df <- df[complete.cases(df$FAMINCTC_A), ]

#making breast cancer binary
df$BREASCAN_A <- ifelse(df$BREASCAN_A == 1, 1, ifelse(df$BREASCAN_A == 2, 0, df$BREASCAN_A))

#table(df$BMICAT_A,df$HISPALLP_A)
#table(df$BMICAT_A)


#as.factoring 

#normal weight reference category
df$BMICAT_A <- factor(df$BMICAT_A, levels = c(1,2))

#married reference category
df$MARITAL_A <- factor(df$MARITAL_A, levels = c(1,2,3))

#non hispanic asian reference category
df$HISPALLP_A <- factor(df$HISPALLP_A, levels = c(2,1,3,4,5,7))

#Smoking status
df$SMKCIGST_A <- factor(df$SMKCIGST_A, levels = c(1,2,3,4))



#weighting
df$WT_ADJ <- df$WTFA_A/4
nhis <- svydesign(id = ~PPSU, strata = ~PSTRAT, weights = ~WT_ADJ,nest = TRUE, data = df)


#bivariate weighted outcome
#subsetting for no breascan and yes breascan
subset_data_no <- subset(df, BREASCAN_A == 0)
nhis_sub_no <- svydesign(ids = ~PPSU, data = subset_data_no,strata = ~PSTRAT, weights = ~WT_ADJ, nest = TRUE)
svytable(~BREASCAN_A, design =nhis_sub_no)

subset_data_yes <- subset(df, BREASCAN_A == 1)
nhis_sub_yes <- svydesign(ids = ~PPSU, data = subset_data_yes,strata = ~PSTRAT, weights = ~WT_ADJ, nest = TRUE)
svytable(~BREASCAN_A, design =nhis_sub_yes)

#marital:3093643
svytable(~MARITAL_A,design=nhis)
svytable(~ BREASCAN_A + MARITAL_A, design = nhis)
svychisq(~BREASCAN_A +MARITAL_A,design = nhis)
prop.table(svytable(~MARITAL_A,design=nhis_sub_no))
prop.table(svytable(~MARITAL_A,design=nhis_sub_yes))

#race/ethnicity
svytable(~HISPALLP_A,design=nhis)
prop.table(svytable(~HISPALLP_A, design=nhis))*100
svytable(~ BREASCAN_A + HISPALLP_A, design = nhis)
svychisq(~BREASCAN_A +HISPALLP_A,design = nhis)
prop.table(svytable(~HISPALLP_A,design=nhis_sub_no))*100
prop.table(svytable(~HISPALLP_A,design=nhis_sub_yes))*100

#smoking status
svytable(~SMKCIGST_A,design=nhis)
prop.table(svytable(~SMKCIGST_A, design=nhis))*100
svytable(~ BREASCAN_A + SMKCIGST_A, design = nhis)
svychisq(~BREASCAN_A +SMKCIGST_A,design = nhis)
prop.table(svytable(~SMKCIGST_A,design=nhis_sub_no))*100
prop.table(svytable(~SMKCIGST_A,design=nhis_sub_yes))*100


#BMI
prop.table(svytable(~BMICAT_A, design=nhis))*100

svytable(~BMICAT_A,design=nhis)
svytable(~ BREASCAN_A + BMICAT_A, design = nhis)
svychisq(~BREASCAN_A +BMICAT_A,design = nhis)
prop.table(svytable(~BMICAT_A,design=nhis_sub_no))*100
prop.table(svytable(~BMICAT_A,design=nhis_sub_yes))*100


#age
svyttest(AGEP_A ~ BREASCAN_A, design=nhis )
svymean(~AGEP_A, design = nhis)
svyby(~AGEP_A, ~BREASCAN_A, design = nhis, subset = BREASCAN_A == 0, FUN = svymean)
svyquantile(~AGEP_A, design =nhis, quantiles = 0.5)
svyquantile(~AGEP_A, design=nhis_sub_no, quantiles = 0.5)
svyquantile(~AGEP_A, design=nhis_sub_yes, quantiles = 0.5)

#family income
svyttest(FAMINCTC_A ~ BREASCAN_A, design=nhis )
svymean(~FAMINCTC_A, design = nhis)
svyby(~FAMINCTC_A, ~BREASCAN_A, design = nhis, subset = BREASCAN_A == 0, FUN = svymean)
svyquantile(~FAMINCTC_A, design=nhis,quantiles = 0.5)
svyquantile(~FAMINCTC_A, design=nhis_sub_no, quantiles = 0.5)
svyquantile(~FAMINCTC_A, design=nhis_sub_yes, quantiles = 0.5)


#bivariate weighted EXPOSURE    

#subsetting for no breascan and yes breascan
subset_nw_BMI <- subset(df, BMICAT_A == 1)
nhis_BMI_norm <- svydesign(ids = ~PPSU, data = subset_nw_BMI,strata = ~PSTRAT, weights = ~WT_ADJ, nest = TRUE)
svytable(~BMICAT_A, design =nhis_BMI_norm)

subset_above_BMI <- subset(df, BMICAT_A == 2)
nhis_BMI_above <- svydesign(ids = ~PPSU, data = subset_above_BMI,strata = ~PSTRAT, weights = ~WT_ADJ, nest = TRUE)
svytable(~BMICAT_A, design =nhis_BMI_above)

#marital:3093643
svytable(~MARITAL_A,design=nhis)
svytable(~ BMICAT_A + MARITAL_A, design = nhis)
svychisq(~BMICAT_A +MARITAL_A,design = nhis)
prop.table(svytable(~MARITAL_A,design=nhis_BMI_norm))*100
prop.table(svytable(~MARITAL_A,design=nhis_BMI_above))*100

#race/ethnicity
svytable(~HISPALLP_A,design=nhis)
svytable(~ BMICAT_A + HISPALLP_A, design = nhis)
svychisq(~BMICAT_A +HISPALLP_A,design = nhis)
prop.table(svytable(~HISPALLP_A,design=nhis_BMI_norm))*100
prop.table(svytable(~HISPALLP_A,design=nhis_BMI_above))*100

#smoking status
svytable(~SMKCIGST_A,design=nhis)
svytable(~ BMICAT_A + SMKCIGST_A, design = nhis)
svychisq(~BMICAT_A +SMKCIGST_A,design = nhis)
prop.table(svytable(~SMKCIGST_A,design=nhis_BMI_norm))*100
prop.table(svytable(~SMKCIGST_A,design=nhis_BMI_above))*100


#BREAST CANCER
svytable(~BREASCAN_A,design=nhis)
svytable(~ BMICAT_A + BREASCAN_A, design = nhis)
svychisq(~BMICAT_A +BREASCAN_A,design = nhis)
prop.table(svytable(~BREASCAN_A,design=nhis_BMI_norm))*100
prop.table(svytable(~BREASCAN_A,design=nhis_BMI_above))*100


#age
svyttest(AGEP_A ~ BMICAT_A, design=nhis )
svymean(~AGEP_A, design = nhis)
svyby(~AGEP_A, ~BMICAT_A, design = nhis, FUN = svymean)
svyquantile(~AGEP_A, design =nhis, quantiles = 0.5)
svyquantile(~AGEP_A, design=nhis_BMI_norm, quantiles = 0.5)
svyquantile(~AGEP_A, design=nhis_BMI_above, quantiles = 0.5)

#family income
svyttest(FAMINCTC_A ~ BMICAT_A, design=nhis )
svymean(~FAMINCTC_A, design = nhis)
svyby(~FAMINCTC_A, ~BMICAT_A, design = nhis, FUN = svymean)
svyquantile(~FAMINCTC_A, design=nhis,quantiles = 0.5)
svyquantile(~FAMINCTC_A, design=nhis_BMI_norm, quantiles = 0.5)
svyquantile(~FAMINCTC_A, design=nhis_BMI_above, quantiles = 0.5)


#models weighted

#crude model weighted
model1 <- svyglm(BREASCAN_A ~ BMICAT_A, design = nhis, family = binomial)
exp(cbind(OR = coef(model1), confint(model1)))
summary(model1)

#full model weighted
model2 <- svyglm(BREASCAN_A ~ AGEP_A + MARITAL_A + HISPALLP_A + SMKCIGST_A + FAMINCTC_A + BMICAT_A, design = nhis, family = binomial)
exp(cbind(OR = coef(model2), confint(model2)))
summary(model2)

#interaction model weighted
model3 <- svyglm(BREASCAN_A ~ AGEP_A + MARITAL_A + HISPALLP_A + SMKCIGST_A + FAMINCTC_A + BMICAT_A + BMICAT_A*HISPALLP_A, design = nhis, family = binomial)
summary(model3)
OR <-exp(cbind(OR = coef(model3), confint(model3)))



library(ggeffects)
pred <- ggpredict(model3, c("BMICAT_A","HISPALLP_A")) 
plot(pred)
