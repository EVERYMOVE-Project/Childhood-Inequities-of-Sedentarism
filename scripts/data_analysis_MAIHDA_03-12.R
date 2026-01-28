## Data Analysis - MAIHDA

## Load libraries----
library(haven)
library(tidyverse)
library(ggeffects)
library(lme4)
library(broom.mixed)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)
library(glmmTMB)
library(sandwich)
library(emmeans)

library(dplyr)
library(broom)
library(gtsummary)
library(janitor)
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
library(segmented)

## Load data ----
joined_clean <- get(load("joined_clean_6.RData"))

rii_sedentarism_overall_clase <- get(load("~/UAH/PhD Documents/INEdatos/Analysis/Datasets/clase_tr/rii_sedentarism_overall_clase.RData"))

## Inflection Point - Edit Data ----
## use segmented package to decide the inflection point in the inequalities of sedentarism

## linear model fitted with survey year as a predictor
rii_sedentarism_overall_clase$encuesta <- as.numeric(rii_sedentarism_overall_clase$encuesta)

m0 <- lm(rii ~ encuesta, data = rii_sedentarism_overall_clase)
summary(m0)

## fit segmented model
seg_m <- segmented(m0, seg.Z = ~encuesta, psi = 2011)
seg_m

## plot inflection point
plot(rii ~ encuesta, data = rii_sedentarism_overall_clase)
plot(seg_m, add = TRUE, col = "red")
ggsave("Figures/inflection.png", width = 4000, height = 2200, dpi=300, units = "px")

joined_clean <- joined_clean %>% 
  mutate(survey2 = case_when(
    survey %in% c("2003", "2006", "2011") ~ "Pre",
    survey %in% c("2017", "2023") ~ "Post",
    TRUE ~ NA_character_
    ),
    survey2 = factor(survey2, levels = c("Pre", "Post")),
    urb_rur = as.character(urb_rur),
    urb_rur = factor(urb_rur)
  )

# Database MAIHDA ####
maihda <- joined_clean %>% 
  select(factor2, sexo, edad, edad_cat, edad_cat3, clase, clase_2, clase_3, survey, survey2, sedentarismo, nacionalidad, urb_rur, ccaa)
maihda$survey <- factor(maihda$survey)
save(maihda, file = "maihda.RData")

View(maihda)

table(maihda$survey2, maihda$sedentarismo)

## Load data ----
dt <- get(load("maihda.RData"))
summary(dt)

## Generate stratum ID
levels(dt$sexo)
levels(dt$edad_cat3)
levels(dt$clase_2)
dt$clase_2 <- factor(dt$clase_2,
                     levels = c("Non-Manual Workers", "Manual Workers"))
levels(dt$clase_3)
dt$clase_3 <- factor(dt$clase_3,
                   levels = c("Class III", "Class II", "Class I"))
levels(dt$clase)
dt$clase <- factor(dt$clase,
                    levels = c("Class VI", "Class V", "Class IV", "Class III", "Class II", "Class I"))
levels(dt$urb_rur)
levels(dt$survey2)

## sexo:     1 = Male, 2 = Female
## edad_cat3:1 = 6-9, 2 = 10-12, 3 = 13-15
## clase_2:  1 = Non-manual workers, Manual workers 
## clase_3:  1 = Class III, 2 = Class II, 3 = Class I
## clase:    1 = Class VI, 2 = Class V, 3 = Class IV, 4 = Class III, 5 = Class II, 6 = Class I
## urb_rur:  1 = Rural, 2 = Semi-Urban, 3 = Urban
## survey:   1 = 2003, 2 = 2006, 3 = 2011, 4 = 2017, 5 = 2023
## survey:   1 = Pre, 2 = Post

## NEW MAIHDA DATASET
save(maihda, file = "maihda.RData")

## Need numeric type to create stratum ID
dt <- dt %>%
  mutate(
    sexo_num = as.numeric(sexo),
    edad_cat3_num = as.numeric(edad_cat3),
    clase_2_num = as.numeric(clase_2),
    clase_3_num = as.numeric(clase_3),
    clase_num = as.numeric(clase),
    urb_rur_num = as.numeric(urb_rur),
    survey_num = as.numeric(survey),
    survey2_num = as.numeric(survey2),
    sedentarismo2 = as.numeric(sedentarismo), # 1 and 2, not 0 and 1
    nacionalidad_num = as.numeric(nacionalidad)
    )

# Convert 1/2 to 0/1
dt$sedentarismo2 <- dt$sedentarismo2 - 1

# percent of sedentarism overall and per sex
prop.table(table(dt$sedentarismo))*100
prop.table(table(dt$sedentarismo, dt$sexo), 1)*100 # where the 1 gives me row proportions

## create new database with variables needed to construct stratum
### sex, age group, social class, urban/semi/rural, survey year
dt0 <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat3_num + 100*clase_2_num + 10*urb_rur_num + survey2_num
  )
dt0$stratum <- as.factor(dt0$stratum)
summary(dt0$stratum)

# Prevalence of sedentarism per strata
tab <- prop.table(table(dt0$stratum, dt0$sedentarismo), 1)*100
tab_round <- round(tab, 1)
tab_round
clipr::write_clip(tab_round)

## Sort data by stratum
dt0 <- dt0[order(dt1$stratum),]

## Generate a new variable which records stratum size
dt0 <- dt0 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

summary(dt0$strataN) # minimum 62 counts in a group

## fit null model
model0 <- glmmTMB(sedentarismo ~ (1|stratum), data = dt0, family = "binomial")

## Modeling the log of the expected prevalence as a function of a fixed intercept
## which is the overall log mean prevalence and a random intercept for each stratum
## capturing between-stratum variability in sedentarism
## This is the null model use to estimate VPC (how much variance is between vs
## within strata) and serves as baseline for later main-effects models
summary(model0)
exp(-1.97733) 
## log odds converted to a probability
# The baseline risk of sedentarism is 12% averaged across all strata
tab_model(model0, show.se=T)
## at the residual level, the variance is not reported because in logistic models
## the residual variance at the individual-level is fixed at pi^2/3 = 3.29

## Calculate the VPC
## approximated as the variance(stratum)/variance(stratum + 3.29)
tau2 <- as.numeric(VarCorr(model0)$cond$stratum[1,1])
VPC0 <- tau2 / (tau2 + (pi^2 / 3))
VPC0
VPC0_percent <- VPC0*100
VPC0_percent
## about 7.6% of the variance in diabetes risk is attributable to differences
## between intersectional strata, while the remaining 92.4% is at the individual level
## this is the discriminatory accuracy of the strata predicting sedentarism

## Fit two-level logistic regression with covariates
model1 <- glmer(sedentarismo ~ sexo + edad_cat3 + clase_2 + urb_rur + survey2 +
                       (1|stratum), data = dt0, family = "binomial")
summary(model1)
tab_model(model1, show.se=T)

## Calculate the VPC
tau2 <- as.numeric(VarCorr(model1)$stratum[1,1])
tau2
VPC1 <- tau2 / (tau2 + (pi^2 / 3))
VPC1
VPC1_percent <- VPC1*100
VPC1_percent

## Calculate the PCV
var_null <- as.numeric(VarCorr(model0)$cond$stratum[1,1])
var_null
var_adj  <- as.numeric(VarCorr(model1)$stratum[1,1])
var_adj # after adjusting for predictors, there is very little variance remaining
# between strata - stratum level differences are not mostly explained by the additive
# effects of these predictors

PCV <- (var_null - var_adj) / var_null * 100
PCV

## Following Evans Tutorial Steps to create tables and figures
## Extract and prepare model predictions and random effects for interpretation
## and visualization 

# NULL MODEL (baseline inequality)
# --------------------------------
# • Stratum predicted probability (m0pb)
# • Grand mean probability (fixed only)
# 
# Purpose:
#   → Quantify total intersectional inequality
# 
# ADD. MODEL (adjusted inequality)
# -------------------------------
# • Stratum predicted log-odds + CI
# • Stratum predicted probabilities
# • Fixed-only predicted log-odds
# • Fixed-only predicted probabilities
# • Stratum random effects + SEs
# 
# Purpose:
# → Partition inequality:
#   - Additive part
#   - Intersectional excess part
# → Calculate PCV
# → Rank strata after adjustment

# predict the fitted linear predictor on the **probability scale**
dt0$m0pb <- predict(model0, type="response") ## m2Axbu
## type="response" tells R to convert the log-odds into probabilities using the logistic function
## using the logistic function, convert the linear predictor log-odds to predicted probability
## so each m0pb value is the predicted probability of being sedentary for that observation,
## taking into account the overall intercept (log-odds) and the random effect of that observation's 
## stratum - values between 0 and 1, all individuals of the same strata will have the same predicted
## probability

## for each person, R calculates the predicted probablity of being sedentary based on the overall 
## intercept (average log-odds of sedentarism) and the random effect for their stratum, aka their area's
## deviation from the average

# predict the linear predictor for the fixed portion of the model only on the **probability scale**
# (only the intercept, so this is just the weighted grand mean probability)
dt0$m0wgm <- predict(model0, type ="response", re.form=NA) ## m2Axb
## re.form=NA ignores the random effects, therefore predicting probability
## based only on the fixed portion of the model - meaning m0wgm is the overall
## probability of sedentarism across all observations, ignoring stratum differences

# predict the fitted linear predictor and confidence intervals on the **logit scale**
m1pb <- predictInterval(model1, level=0.95, include.resid.var=FALSE) ## m2Bm
## Simulates predicted values and uncertainty on the logit linear predictor scale
## the include.resid.var = FALSE means excluding individual-level residual
## variance, so the predictions represent the expected mean per stratum and NOT
## individual-level predictions creating a dataframe with fit: predicted
## log-odds, and lwr/upr 95% CI for log-odds

# create a new id variable for this new dataframe
m1pb <- mutate(m1pb, id=row_number())

# on the logit scale, predict the linear predictor for the fixed portion of the model only in the **log-scale**
dt0$m1wgm <- predict(model1, re.form=NA) ## m2BmF

# predict the fitted linear predictor, and confidence intervals, on the **probability scale**
m1pb_prob <- predictInterval(model1, level=0.95, include.resid.var=FALSE, type="probability") ## m2Bm_prob

# create a new id variable for this newly created dataframe
m1pb_prob <- mutate(m1pb_prob, id=row_number())

# predict the fitted linear predictor, on the **probability scale** for the fixed portion of the model only
dt0$m1wgm_prob <- predict(model1, type = "response", re.form=NA) ## m2Bxb
## Back-transforms the fixed-effects-only predictions to probability scale where
## each observation's value = predicted diabetes probability from the additive
## main effects ONLY (ignoring stratum residuals)

# predict the stratum random effects and associated standard errors
m1SE <- REsim(model1) ## m2BU
m1SE
## REsim simulates random intercepts (stratum effects) and associated uncertainty
## which represents residual intersectional effects, the part of stratum-specific
## risk not explained by additive main effects

## Create a dataframe at the strata level with the means of each variable ----
## setting up a stratum-level dataset that combines predictions, confidence
## intervals, and observed outcomes for both continuous and binary outcomes

## merge predictions with original data
# create an id variable for merging in the dt0 dataframe
dt0$id <- seq.int(nrow(dt0))

# create a new dataframe, dt2, that merges dt0 and m1pb
dt2 <- merge(dt0, m1pb, by = "id")
summary(dt2)

# rename the variables from m1pb
dt2 <- dt2 %>%
  rename(
    m1pbfit=fit,
    m1pbupr= upr,
    m1pblwr=lwr
  )

# merge in m1pb_prob
dt2 <- merge(dt2, m1pb_prob, by="id")

# rename the variables from m1pb_prob
dt2 <- dt2 %>%
  rename(
    m1pb_probfit=fit,
    m1pb_probupr= upr,
    m1pb_problwr=lwr
  )

# collapse the data down to a stratum-level dataset
stratum_level <- aggregate(
  x = dt2[c("sedentarismo2")],
  by = dt2[c("sexo", "edad_cat3", "clase_2", "urb_rur", "survey2",
             "stratum", "strataN", "m1pbfit", "m1pbupr", "m1pblwr",
             "m1pb_probfit", "m1pb_probupr", "m1pb_problwr", "m1wgm", "m1wgm_prob")],
  FUN = mean
) ## we are aggregating means and proportions to one row per stratum, so for each stratum,
## we compute the mean observed outcomes and average predictions/CI
## clean stratum-level dataset with: observed outcomes, predicted outcomes, confidence intervals,
## fixed-effect-only predictions
stratum_level

## convert the outcome from a proportion to a percentage
stratum_level$sedentarismo2_p <- stratum_level$sedentarismo2*100


## Table 1 ----
# Tabulate each individual characteristics
table(dt0$sexo)
table(dt0$edad_cat3)
table(dt0$urb_rur)
table(dt0$clase_2)
table(dt0$survey2)
table(dt0$sedentarismo)

# adapt category names before tables
dt <- dt %>%
  mutate(
    edad_cat3 = fct_recode(edad_cat3,
                           "6–9 years" = "6-9",
                           "10–12 years" = "10-12",
                           "13–15 years" = "13-15"),
    sexo = fct_recode(sexo,
                      "Boys" = "Male",
                      "Girls" = "Female")
  )

table1 <- dt %>%
  select(sexo, edad_cat3, urb_rur, clase_2, survey2, sedentarismo) %>%
  tbl_summary(
    label = list(
      sexo ~ "Sex",
      edad_cat3 ~ "Age Group",
      urb_rur ~ "Urbanicity",
      clase_2 ~ "Occupational Social Class",
      survey2 ~ "Pre-Post Inequality Inflection Point",
      sedentarismo ~ "Sedentarism"
    ),
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::tab_header(
    title = "Table 1. Descriptive Summary of Study Sample",
    subtitle = "Counts and percentages of variables used to define strata"
  )

table1

## Table 2 ----
# Generate binary indicators for whether each stratum has more than X 
# individuals
summary(stratum_level$strataN)
stratum_level$n100plus <- ifelse(stratum_level$strataN>=100, 1,0)
stratum_level$n50plus <- ifelse(stratum_level$strataN>=50, 1,0)
stratum_level$n30plus <- ifelse(stratum_level$strataN>=30, 1,0)
stratum_level$n20plus <- ifelse(stratum_level$strataN>=20, 1,0)
stratum_level$n10plus <- ifelse(stratum_level$strataN>=10, 1,0)
stratum_level$nlessthan10 <- ifelse(stratum_level$strataN<10, 1,0)

# tabulate the binary indicators
table(stratum_level$n100plus)
table(stratum_level$n50plus)
table(stratum_level$n30plus)
table(stratum_level$n20plus)
table(stratum_level$n10plus)
table(stratum_level$nlessthan10)
summary(dt0$stratum)
## all are greater than 100 counts except 6 strata

# summarise the observed stratum means
prop.table(table(dt0$stratum, dt0$sedentarismo), 1)*100

# Observed stratum-level means (prevalence)
observed_table <- dt0 %>%
  group_by(stratum) %>% 
  summarise(
    n = n(),
    cases = sum(sedentarismo == "Yes", na.rm = TRUE),
    prevalence = mean(sedentarismo == "Yes", na.rm = TRUE) 
  ) %>%
  mutate(prevalence_pct = prevalence * 100)

observed_table
clipr::write_clip(observed_table)

# Model-based predicted probabilities
model_basedsex <- emmeans(model1, ~ sexo, type = "response")
model_basedage <- emmeans(model1, ~ edad_cat3, type = "response")
model_basedclass <- emmeans(model1, ~ clase_2, type = "response")
model_basedurb <- emmeans(model1, ~ urb_rur, type = "response")
model_basedsurv <- emmeans(model1, ~ survey2, type = "response")

model_basedsex
model_basedage
model_basedclass
model_basedurb
model_basedsurv

stratum_level_predictedprob <- dt2 %>%
  group_by(
    sexo, edad_cat3, urb_rur, clase_2, survey2,
    stratum, strataN
  ) %>%
  summarise(
    observed_prevalence = mean(sedentarismo2, na.rm = TRUE),
    
    m1pb_probfit = mean(m1pb_probfit, na.rm = TRUE),
    m1pb_probupr = mean(m1pb_probupr, na.rm = TRUE),
    m1pb_problwr = mean(m1pb_problwr, na.rm = TRUE),
    
    m1pbfit      = mean(m1pbfit, na.rm = TRUE),
    m1pbupr      = mean(m1pbupr, na.rm = TRUE),
    m1pblwr      = mean(m1pblwr, na.rm = TRUE),
    
    m1wgm        = mean(m1wgm, na.rm = TRUE),
    m1wgm_prob     = mean(m1wgm_prob, na.rm = TRUE),
    
    .groups = "drop"
  )

stratum_level_predictedprob
clipr::write_clip(stratum_level_predictedprob)

## to calculate the confidence interval of the fixed-effect predicted probabilities
library(glmmTMB)
library(MuMIn)

# Model with fixed and random effects
model1 <- glmmTMB(sedentarismo ~ sexo + edad_cat3 + urb_rur + clase_2 + (1|stratum),
                  data = dt2, family = binomial)

# Fixed effects only predictions with CI
fx_pred <- predict(model1, 
                   dt2,
                   type = "response",
                   se.fit = TRUE,
                   re.form = NA)   # <--- this removes random effects

# Calculate CI on probability scale ### EXTRA
dt2$fixed_prob <- fx_pred$fit
dt2$fixed_low  <- plogis(qlogis(fx_pred$fit) - 1.96 * fx_pred$se.fit)
dt2$fixed_high <- plogis(qlogis(fx_pred$fit) + 1.96 * fx_pred$se.fit)

# Table 3. ----
# Create a table that includes all model estimates, including the Variance 
# Partitioning Coefficients (VPC)
tab_model(model0, model1, p.style="stars")

## combine VPCs of models and PCV all in one table
VPC0
VPC1
PCV

## How well does the model distinguish between cases and non-cases, where 0.5
## is no discrimination, random guessing, and 1 is perfect discrimination, so
## we check how much predictive ability comes from fixed effects vs. random 

# model0 based on the fixed and random effects
AUC0 <- auc(dt0$sedentarismo2, dt0$m0pb)
AUC0

# model0 based only on the fixed effects
AUC0f <- auc(dt0$sedentarismo2, dt0$m0wgm)
AUC0f

# model1 based on the fixed and random effects
AUC1 <- auc(dt0$sedentarismo2, m1pb_prob$fit)
AUC1

# model1 based only on the fixed effects
AUC1f <- auc(dt0$sedentarismo2, dt0$m1wgm_prob)
AUC1f

# Figures ----
## Figure 1. 
# Rank the predicted stratum probabilities
stratum_level <- stratum_level %>%
  mutate(rank2=rank(m1pb_probfit))

# convert probabilities to percentages
stratum_level$m1pb_probfit <- stratum_level$m1pb_probfit * 100
stratum_level$m1pb_probupr <- stratum_level$m1pb_probupr * 100
stratum_level$m1pb_problwr <- stratum_level$m1pb_problwr * 100

# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level, aes(y=m1pb_probfit, x=rank2)) +
  geom_point() +
  geom_pointrange(aes(ymin=m1pb_problwr, ymax=m1pb_probupr)) +
  ylab("Predicted Percent Sedentarism, Model 1") +
  xlab("Stratum Rank") + 
  theme_bw()

# Updated Figure
# model0 = null model
# model1 = additive model (fixed + random)

# Caterpillar plot
ggplot(stratum_level, aes(x = rank2, y = m1pb_probfit)) +
  geom_pointrange(aes(ymin = m1pb_problwr, ymax = m1pb_probupr), size = 0.5) +
  geom_text(
    aes(label = stratum),            
    hjust = -2,       
    vjust = 0,       
    size = 3,
    angle = 90,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "Predicted Probability (%) of Sedentarism per Strata",
    x = "Stratum Rank",
    y = "Predicted Percent of Sedentarism",
  ) +
  theme_bw()

ggplot(stratum_level, aes(x = rank2, y = m1pb_probfit)) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1pb_problwr, ymax = m1pb_probupr),
    color = "grey40",
    size = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2
  ) +
  
  geom_text(
    aes(label = stratum),            
    hjust = -1.8,       
    vjust = 0,       
    size = 3,
    angle = 90,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Predicted Probability of Sedentarism by Intersectional Stratum",
    x = "Intersectional Stratum (ranked from lowest to highest risk)",
    y = "Predicted probability of sedentarism",
    caption = "Points represent stratum-level predicted probabilities; bars indicate 95% confidence intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(size = 9)
  )
ggsave("Figures/MAIHDA/caterpillarplot_predictedprob.png", width = 4000, height = 2200, dpi=300, units = "px")

# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank2),]
lowest <- head(stratum_level)
clipr::write_clip(lowest)
highest <- tail(stratum_level)
clipr::write_clip(highest)

# Figure 3. on log-scale
plotREsim(m1SE)
## recall REsim estimates the stratum-level random effects and their uncertainty

# This is more difficult to do on the probability scale

# For this plot we follow a simulation-based approach to 
# calculating the limits of the approximate 95% confidence intervals of our 
# predictions. This involves simulating 1000 values for each predicted value.

# Approximate as the model assumes no sampling covariability between the 
# regression coefficients and the stratum random effect

# duplicate stratum data *1000 (creating a new dataframe, stratumsim)
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

## Creates 1000 simulated copies of each stratum (allows Monte Carlo simulation
## of prediction uncertainty)

# Generate the approximate standard error for the linear prediction on the
# logit scale. We do this based on the difference between one estimated
# confidence interval and the estimated fit, on the logit scale
stratumsim$m2Bmse <- (stratumsim$m2BmfitL - stratumsim$m2BmlwrL)/1.96

## Uses CI from model to estimate the SE on logit scale, capturing the uncertainty
## in predicted logit for each stratum

# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)

# Generate the predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect and factoring in 
# prediction uncertainty
stratumsim$m2Bpsim <- 100*invlogit(stratumsim$m2BmfitL + 
                                     rnorm(384000, mean=0, sd=stratumsim$m2Bmse))

## Simulate predicted probabilities with random noise, adds normal noise to each 
## linear predictor based on its SE, transforming it to probability scale using
## invlogit(), and multiplies by 100 to have predicted percent diabetic for each
## simulated stratum

# Generate the predicted stratum percentages ignoring the predicted stratum 
# effect
stratumsim$m2BpAsim <- 100*invlogit(stratumsim$m2BmF)

## here, uses fixed-effects-only prediction, which shows expected diabetes %
## based on main effects only, ignoring residual stratum deviation

# Generate the difference in the predicted stratum percentages due to the 
# predicted stratum effect
stratumsim$m2BpBsim <- stratumsim$m2Bpsim - stratumsim$m2BpAsim

## Calculate residual/intersectional effect, which is the difference between the 
## full prediction and fixed-effects-only prediction: how much the stratum's 
## risk deviates from what is predicted by additive effects: the residual 
## intersectional effect

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]

# collapse the data down to stratum level, generate mean and SE variables,
# then use these to generate rank and lower and upper limits of the approximate
# 95% confidence intervals of the difference in predicted stratum percentages 
# due to interaction variables.
stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m2BpBsim), std=sd(m2BpBsim)) %>%
  mutate(rank=rank(mean)) %>%
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std))

## compute mean residual effect and standard deviation for each stratum
## rank stata by mean residual effect - for ordered plotting

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent diabetic due to interactions") +
  theme_bw()

## x-axis is the stratum rank, ordered by residual effect
## y-axis is the predicted % deviation due to residual intersesctional effect
## red line at 0 means there is no residual effect, the stratum matches additive
## prediction

# Filter only significant strata
sig_strata <- stratumsim2 %>%
  filter(lo > 0 | hi < 0) %>%
  arrange(mean) %>%            # optional: order by mean
  mutate(rank = row_number())  # re-rank for plotting

# Caterpillar plot for significant strata only
ggplot(sig_strata, aes(x=rank, y=mean, label=stratum)) +
  geom_point(size=3, color="blue") +
  geom_pointrange(aes(ymin=lo, ymax=hi), color="blue") +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_text(hjust=0, vjust=1.5, size=3) +  # adjust label position
  xlab("Stratum Rank") +
  ylab("Difference in predicted % diabetic due to interactions") +
  theme_bw()
## just 22114