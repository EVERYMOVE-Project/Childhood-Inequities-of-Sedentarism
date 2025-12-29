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

## organise variables
dt <- dt %>%
  mutate(
    clase3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Upper class",
      clase %in% c("Class III", "Class IV") ~ "Middle class",
      clase %in% c("Class V", "Class VI") ~ "Lower class"
    )
  )

dt$clase3 <- factor(dt$clase3, 
                        levels = c("Lower class", "Middle class", "Upper class"))

# not ordered
dt$urb_rur <- factor(as.character(dt$urb_rur), 
                      levels = c("Rural", "Semi-urban", "Urban"))

## Generate stratum ID
levels(dt$sexo)
levels(dt$edad_cat)
levels(dt$clase3)
levels(dt$clase)
dt$clase <- factor(dt$clase,
                    levels = c("Class VI", "Class V", "Class IV", "Class III", "Class II", "Class I"))
levels(dt$urb_rur)
levels(dt$survey)

## sexo:     1 = Male, 2 = Female
## edad_cat: 1 = 0-5, 2 = 6-11, 3 = 12-15
## clase3:   1 = Lower Class, 2 = Middle Class, 3 = Upper Class
## clase:    1 = Clase VI, 2 = Clase V, 3 = Clase IV, 4 = Clase III, 5 = Clase II, 6 = Clase I
## urb_rur:  1 = Rural, 2 = Semi-Urban, 3 = Urban
## survey:   1 = 2003, 2 = 2006, 3 = 2011, 4 = 2017, 5 = 2023

## Need numeric type to create stratum ID
dt <- dt %>%
  mutate(
    sexo_num = as.numeric(sexo),
    edad_cat_num = as.numeric(edad_cat),
    clase3_num = as.numeric(clase3),
    clase_num = as.numeric(clase),
    urb_rur_num = as.numeric(urb_rur),
    survey_num = as.numeric(survey),
    sedentarismo2 = as.numeric(sedentarismo),
    nacionalidad_num = as.numeric(nacionalidad)
    )

dt1 <- dt %>% 
  mutate(
   stratum = 1000*sexo_num + 100*edad_cat_num + 10*clase3_num + urb_rur_num # 54 groups 
  )
dt1$stratum <- as.factor(dt1$stratum)
    
dt2 <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat_num + 100*clase3_num + 10*urb_rur_num + survey_num # 270 groups
    ) 
dt2$stratum <- as.factor(dt2$stratum)

dt3 <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat_num + 100*clase3_num + 10*urb_rur_num + nacionalidad_num # 108 groups
  )
dt3$stratum <- as.factor(dt3$stratum)

dt4 <- dt %>% 
  mutate(
    stratum = 1000*sexo_num + 100*edad_cat_num + 10*clase_num + 1*urb_rur_num # 108 groups
  )
dt4$stratum <- as.factor(dt4$stratum)

## Sort data by stratum
dt1 <- dt1[order(dt1$stratum),]
dt2 <- dt2[order(dt2$stratum),]
dt3 <- dt3[order(dt3$stratum),]
dt4 <- dt4[order(dt4$stratum),]

## Generate a new variable which records stratum size
dt1 <- dt1 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

dt2 <- dt2 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

dt3 <- dt3 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

dt4 <- dt4 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

table(dt1$stratum)
table(dt2$stratum)
table(dt3$stratum)
table(dt4$stratum)

## ORs can substantially overestimate the risk ratio RR when the outcome is common
## prevalence > 10% thereforea modified Poisson regression with robust sandwich
## estimation is used, which allows to directly estimate prevalence ratios PR 
## or risk ratios RR which are easier to interpret in cross-sectional data

## Fit a two-level modified poisson regression with no covariates
modelA1 <- glmmTMB(sedentarismo2 ~ (1|stratum), data = dt1, family = "poisson")
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
exp(0.21388) 
## the average probability of being sedentary across all strata is approximately 24%
## average prevalence ratio or overall predicted mean prevalence

summary(modelA2)
exp(0.216435)c # 1.24

summary(modelA3)
summary(modelA4)
tab_model(modelA4)
summary(modelA4_log)
tab_model(modelA4_log)

## VPC calculation for poisson
var4_stratum <- as.numeric(VarCorr(modelA4)$cond$stratum[1,1])
var4_stratum

beta04 <- fixef(modelA4)$cond["(Intercept)"]
beta04

var4_within <- log(1 + 1/exp(beta04)) ## in a poisson model, the residual variance
## depends on the mean... because there is no canonical latent variable variance 
## like in the logit model
var4_within

VPC4A <- var4_stratum / (var4_stratum + var4_within)
VPC4A
VPC4A_percent <- VPC4A*100
VPC4A_percent

## VPC modelA4_log
var4_stratum_log <- as.numeric(VarCorr(modelA4_log)$stratum[1,1])
var4_stratum_log

VPC4A_log <- var4_stratum_log / (var4_stratum_log + (pi^2/3))
VPC4A_log
VPC4A_log_percent <- VPC4A_log*100
VPC4A_log_percent

## estimates as ORs 
tab_model(modelA4_log, show.se=T)
tab_model(modelA4, show.se = T) # can I do this?

## Fit two-level poisson regression with covariates
modelB4 <- glmmTMB(sedentarismo2 ~ sexo + edad_cat + clase + urb_rur +
                     (1|stratum), data = dt4, family = "poisson")
summary(modelB4)

## VPC calculation for poisson
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

### going to move forward with just logistic regression for testing purposes

## Fit two-level logistic regression with covariates
modelB4_log <- glmer(sedentarismo ~ sexo + edad_cat + clase + urb_rur + 
                       (1|stratum), data = dt4, family = "binomial")
summary(modelB4_log)

## VPC
tau00 <- as.numeric(VarCorr(modelB4_log)$stratum[1,1])
tau00
VPC4B_log <- tau00 / (tau00 + (pi^2 / 3))
VPC4B_log
VPC4B_log_percent <- VPC4B_log*100
VPC4B_log_percent ## 0.69% # confirms that the intersectional strata explain 
## very little additional variance once individual predictors are included

## In the multilevel logistic regression model, females showed 55% higher 
# odds of being sedentary compared to males, while older children and adolescents 
# had markedly lower odds relative to those aged 0–5. A clear social gradient was 
# evident, with progressively lower odds of sedentarism among higher social classes. 
# Children living in more urban settings also had modestly higher odds of sedentarism. 
# The between-stratum variance (τ₀₀ = 0.023) and corresponding VPC (≈0.7%) indicate 
# that, after accounting for individual-level predictors, very little variability 
# in sedentarism remains attributable to differences between intersectional strata.

tab_modelB4_log <- tab_model(modelB4_log, show.se=T)
tab_modelB4_log

