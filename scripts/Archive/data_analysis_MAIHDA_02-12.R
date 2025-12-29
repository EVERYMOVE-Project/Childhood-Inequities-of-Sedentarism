## Data Analysis - MAIHDA

## Load libraries----
library(haven)
library(tidyverse)
library(ggeffects)
library(lme4)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)
library(glmmTMB)
library(sandwich)

library(dplyr)
library(broom)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)

library(nlme)
library(lme4)
library(ggplot2)
library(clipr)

library(lmtest)
library(tibble)
library(ggrepel)

## Load data ----
dt <- get(load("maihda.RData"))

## Generate stratum ID
levels(dt$sexo)
levels(dt$edad_cat3)
levels(dt$clase_3)
dt$clase_3 <- factor(dt$clase_3,
                   levels = c("Class III", "Class II", "Class I"))
levels(dt$clase)
dt$clase <- factor(dt$clase,
                    levels = c("Class VI", "Class V", "Class IV", "Class III", "Class II", "Class I"))
levels(dt$urb_rur)
levels(dt$survey)

dt$urb_rur <- factor(dt$urb_rur,
                     levels = c("Rural", "Semi-urban", "Urban"),
                     ordered = FALSE)

## sexo:     1 = Male, 2 = Female
## edad_cat3: 1 = 6-9, 2 = 10-12, 3 = 13-15
## clase_3:   1 = Class III, 2 = Class II, 3 = Class I
## clase:    1 = Class VI, 2 = Class V, 3 = Class IV, 4 = Class III, 5 = Class II, 6 = Class I
## urb_rur:  1 = Rural, 2 = Semi-Urban, 3 = Urban
## survey:   1 = 2003, 2 = 2006, 3 = 2011, 4 = 2017, 5 = 2023

## Need numeric type to create stratum ID
dt <- dt %>%
  mutate(
    sexo_num = as.numeric(sexo),
    edad_cat3_num = as.numeric(edad_cat3),
    clase_3_num = as.numeric(clase_3),
    clase_num = as.numeric(clase),
    urb_rur_num = as.numeric(urb_rur),
    survey_num = as.numeric(survey),
    sedentarismo2 = as.numeric(sedentarismo), # 1 and 2, not 0 and 1
    nacionalidad_num = as.numeric(nacionalidad)
    )

# Convert 1/2 to 0/1
dt$sedentarismo2 <- dt$sedentarismo2 - 1
table(dt$sedentarismo2)

## dt for sex, age 3 groups, social class 3 groups, urban/rural/semi
dt1 <- dt %>% 
  mutate(
   stratum = 1000*sexo_num + 100*edad_cat3_num + 10*clase_3_num + urb_rur_num # 54 groups 
  )
dt1$stratum <- as.factor(dt1$stratum)

## dt for sex, age 3 groups, social class 3 groups, urban/rural/semi, survey year    
dt2 <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat3_num + 100*clase_3_num + 10*urb_rur_num + survey_num # 270 groups
    ) 
dt2$stratum <- as.factor(dt2$stratum)

## dt for sex, age 3 groups, social class 3 groups, urban/rural/semi, nationality
dt3 <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat3_num + 100*clase_3_num + 10*urb_rur_num + nacionalidad_num # 108 groups
  )
dt3$stratum <- as.factor(dt3$stratum)

## dt for sex, age 3 groups, social class 6 groups, urban/rural/semi
dt4 <- dt %>% 
  mutate(
    stratum = 1000*sexo_num + 100*edad_cat3_num + 10*clase_num + 1*urb_rur_num # 108 groups
  )
dt4$stratum <- as.factor(dt4$stratum)

test <- table(dt1$stratum, dt1$sedentarismo2)

## Sort data by stratum
dt1 <- dt1[order(dt1$stratum),]
dt2 <- dt2[order(dt2$stratum),]
dt3 <- dt3[order(dt3$stratum),]
dt4 <- dt4[order(dt4$stratum),]

## Generate a new variable which records stratum size
dt1 <- dt1 %>%
  group_by(stratum) %>%
  mutate(strataN = n())
  ## stratas all greater than 100 individuals

dt2 <- dt2 %>% 
  group_by(stratum) %>%
  mutate(strataN = n()) 
  ## stratas with less than 7 individuals

dt3 <- dt3 %>%
  group_by(stratum) %>%
  mutate(strataN = n())
  ## stratas with 1 or 2 individuals per group

dt4 <- dt4 %>%
  group_by(stratum) %>%
  mutate(strataN = n())
  ## stratas with groups with minimum 45 individuals

table(dt1$stratum)
table(dt2$stratum)
table(dt3$stratum)
table(dt4$stratum)

## ORs can substantially overestimate the risk ratio RR when the outcome is common
## prevalence > 10% therefore a modified Poisson regression with robust sandwich
## estimation is used, which allows to directly estimate prevalence ratios PR 
## or risk ratios RR which are easier to interpret in cross-sectional data

## Fit a two-level modified poisson regression with no covariates
modelA1 <- glmmTMB(sedentarismo2 ~ (1|stratum), data = dt1, family = "poisson")
modelA1_log <- glmmTMB(sedentarismo ~ (1|stratum), data = dt1, family = "binomial")

modelA2 <- glmmTMB(sedentarismo2 ~ (1|stratum), data = dt2, family = "poisson")
modelA3 <- glmmTMB(sedentarismo2 ~ (1|stratum), data = dt3, family = "poisson")

modelA4 <- glmmTMB(sedentarismo2 ~ (1|stratum), data = dt4, family = "poisson")
modelA4_log <- glmer(sedentarismo ~ (1|stratum), data = dt4, family = "binomial")

## Modeling the log of the expected prevalence as a function of a fixed intercept
## which is the overall log mean prevalence and a random intercept for each stratum
## capturing between-stratum variability in sedentarism
## This is the null model use to estimate VPC (how much variance is between vs
## within strata) and serves as baseline for later main-effects models

## sedentarismo outcome needs to be numeric
summary(modelA1)
exp(-2.07750)
## the baseline incidence rate of sedentarism is 0.125 (12.5%) and there is moderate
## variation between social strata, indicating some strata have higher baselines 
## sedentarism rates and some have lower but the variance is not extremely large
tab_model(modelA1)
exp(-1.93217)
summary(modelA1_log)
## On average, the odds of being sedentary are about 0.145 with a variance of 0.235 
## some clustering of sedentarism by stratum but not huge
tab_model(modelA1_log)

summary(modelA2)
summary(modelA3)

summary(modelA4)
tab_model(modelA4)

summary(modelA4_log)
tab_model(modelA4_log)

## VPC calculation for poisson
var1_stratum <- as.numeric(VarCorr(modelA1)$cond$stratum[1,1])
var1_stratum ## same as t00 stratum in summary()

var4_stratum <- as.numeric(VarCorr(modelA4)$cond$stratum[1,1])
var4_stratum

beta01 <- fixef(modelA1)$cond["(Intercept)"]
beta01 ## same as beta intercept in summary()

beta04 <- fixef(modelA4)$cond["(Intercept)"]
beta04

var1_within <- log(1+1/exp(beta01)) # approximating the within-stratum variance using Leckie-style
var1_within 

var4_within <- log(1 + 1/exp(beta04)) ## in a poisson model, the residual variance
## depends on the mean... because there is no canonical latent variable variance 
## like in the logit model
var4_within

VPC1A <- var1_stratum / (var1_stratum + var1_within)
VPC1A
VPC1A_percent <- VPC1A*100
VPC1A_percent ## about 7.1% of the total variance in sedentarism is attributable to differences 
## between intersectional strata, while the remaining ~93% is at the individual level
.
tau2 <- as.numeric(VarCorr(modelA1_log)$cond$stratum[1,1])
VPC_logit <- tau2 / (tau2 + (pi^2 / 3))
VPC_logit
VPC_logit_percent <- VPC_logit*100
VPC_logit_percent

VPC4A <- var4_stratum / (var4_stratum + var4_within)
VPC4A
VPC4A_percent <- VPC4A*100 ## about 7.6% of the total variance in sedentarism is attributable
## to differences between intersectional strata, while the remaining ~ 92.5% is at the indiv
VPC4A_percent

data.frame(
  var1_stratum = var1_stratum,
  beta01 = beta01,
  exp_beta0 = exp(beta01),
  var1_within = var1_within,
  VPC = VPC1A,
  VPC_percent = 100 * VPC1A
 )

## Fit two-level poisson regression with covariates
modelB1 <- glmmTMB(sedentarismo2 ~ sexo + edad_cat3 + clase_3 + urb_rur +
                     (1|stratum), data = dt1, family = "poisson")
summary(modelB1)

modelB4 <- glmmTMB(sedentarismo2 ~ sexo + edad_cat3 + clase + urb_rur +
                     (1|stratum), data = dt4, family = "poisson")
summary(modelB4)

## VPC calculation for poisson
varB1_stratum <- as.numeric(VarCorr(modelB1)$cond$stratum[1,1])
varB1_stratum

beta0B1 <- fixef(modelB1)$cond["(Intercept)"]
beta0B1

varB1_within <- log(1 + 1/exp(beta0B1))
varB1_within

VPC1B <- varB1_stratum / (varB1_stratum + varB1_within)
VPC1B
VPC1B_percent <- VPC1B*100
VPC1B_percent

tab_modelB1 <- tab_model(modelB1, show.se = T)
tab_modelB1

tab_modelB4 <- tab_model(modelB4, show.se=T)
tab_modelB4

varB4_stratum <- as.numeric(VarCorr(modelB4)$cond$stratum[1,1])
varB4_stratum

beta0B4 <- fixef(modelB4)$cond["(Intercept)"]
beta0B4

varB4_within <- log(1 + 1/exp(beta0B4))
varB4_within

VPC4B <- varB4_stratum / (varB4_stratum + varB4_within)
VPC4B
VPC4B_percent <- VPC4B*100
VPC4B_percent ## SO SMALL 

tab_modelB4 <- tab_model(modelB4, show.se=T)
tab_modelB4

## PCV Calculation Poisson models
var_null <- as.numeric(VarCorr(modelA1)$cond$stratum[1,1])
var_adj  <- as.numeric(VarCorr(modelB1)$cond$stratum[1,1])

PCV_poisson <- (var_null - var_adj) / var_null * 100
PCV_poisson

var_null <- as.numeric(VarCorr(modelA4)$cond$stratum[1,1])
var_adj  <- as.numeric(VarCorr(modelB4)$cond$stratum[1,1])

PCV_poisson <- (var_null - var_adj) / var_null * 100
PCV_poisson

# tidy output
data.frame(
  var_null = var_null,
  var_adj  = var_adj,
  PCV_pct  = PCV_poisson
) ## The included sociodemographic predictors explain approximately 98.89% of the between
## stratum heterogeneity in sedentarism (modelA1 and B1) and 99.99% for modelA4 and B4

# tidy output
data.frame(
  var_null = var_null,
  var_adj  = var_adj,
  PCV_pct  = PCV_poisson
) ## The included sociodemographic predictors explain approximately 99.99% of the between
## stratum heterogeneity in sedentarism (modelA4 and B4)

### Logistic regressions
## Fit two-level logistic regression with covariates
modelB1_log <- glmer(sedentarismo ~ sexo + edad_cat3 + clase_3 + urb_rur + 
                       (1|stratum), data = dt1, family = "binomial")
summary(modelB1_log)
tab_modelB1_log <- tab_model(modelB1_log, show.se=T)
tab_modelB1_log

tau00 <- as.numeric(VarCorr(modelB1_log)$stratum[1,1])
tau00
VPC1B_log <- tau00 / (tau00 + (pi^2 / 3))
VPC1B_log
VPC1B_log_percent <- VPC1B_log*100
VPC1B_log_percent ## about 0.24% of the total variance in sedentarism is attributable
## to differences between intersectional strata, while the remaining ~ 99.8% is at the indiv

var_null <- as.numeric(VarCorr(modelA1_log)$cond$stratum[1,1])
var_adj  <- as.numeric(VarCorr(modelB1_log)$cond$stratum[1,1])

PCV_log <- (var_null - var_adj) / var_null * 100
PCV_log

modelB4_log <- glmer(sedentarismo ~ sexo + edad_cat3 + clase + urb_rur + 
                       (1|stratum), data = dt4, family = "binomial")
summary(modelB4_log)
tab_modelB4_log <- tab_model(modelB4_log, show.se=T)
tab_modelB4_log

## VPC modelA4_log
var4_stratum_log <- as.numeric(VarCorr(modelA4_log)$stratum[1,1])
var4_stratum_log

VPC4A_log <- var4_stratum_log / (var4_stratum_log + (pi^2/3))
VPC4A_log
VPC4A_log_percent <- VPC4A_log*100
VPC4A_log_percent

## VPC modelA4_log
varB4_stratum_log <- as.numeric(VarCorr(modelB4_log)$stratum[1,1])
varB4_stratum_log

VPCB4_log <- varB4_stratum_log / (varB4_stratum_log + (pi^2/3))
VPCB4_log
VPCB4_log_percent <- VPCB4_log*100
VPCB4_log_percent

## PCV modelA4_log and modelB4_log
var_null <- as.numeric(VarCorr(modelA4_log)$cond$stratum[1,1])
var_adj  <- as.numeric(VarCorr(modelB4_log)$cond$stratum[1,1])

PCV_log <- (var_null - var_adj) / var_null * 100
PCV_log
