## Author: Diana Juanita Mora
## Project: Childhood Inequities of Sedentarism
## Script: MAIHDA Data analysis
## Edited: March 3rd 2026

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
library(readxl)

library(broom)
library(gtsummary)
library(janitor)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)

library(nlme)
library(ggplot2)
library(clipr)

library(lmtest)
library(tibble)
library(ggrepel)
library(segmented)
# library(extrafont)

# font_import(prompt = FALSE)   # run once (can take a few minutes)
# loadfonts(device = "win") 

## Load data ----
dt <- get(load("joined_clean_maihda.RData"))

## Inflection Point - Edit Data ----
rii_sedentarism_overall_clase <- get(load("~/UAH/PhD Documents/INEdatos/Analysis/Datasets/clase_tr/rii_sedentarism_overall_clase.RData"))

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

dt <- dt %>% 
  mutate(survey2 = case_when(
    survey %in% c("2003", "2006", "2011") ~ "Pre",
    survey %in% c("2017", "2023") ~ "Post",
    TRUE ~ NA_character_
    ),
    survey2 = factor(survey2, levels = c("Post", "Pre")),
    urb_rur = as.character(urb_rur),
    urb_rur = factor(urb_rur)
  )

# Database MAIHDA ####
maihda <- dt %>% 
  select(factor2, sexo, edad_cat3, clase_2, survey, survey2, sedentarismo, urb_rur)
maihda$survey <- factor(maihda$survey)
save(maihda, file = "maihda.RData")

## Load data ----
dt <- get(load("maihda.RData"))
summary(dt)

## Generate stratum ID
levels(dt$sexo)
levels(dt$edad_cat3)
levels(dt$clase_2)
levels(dt$urb_rur)
levels(dt$survey2)

## sexo:     1 = Male, 2 = Female
## edad_cat3:1 = 6-9, 2 = 10-12, 3 = 13-15
## clase_2:  1 = Non-manual workers, Manual workers 
## urb_rur:  1 = Rural, 2 = Semi-Urban, 3 = Urban
## survey2:   1 = Post, 2 = Pre

## Need numeric type to create stratum ID
dt <- dt %>%
  mutate(
    sexo_num = as.numeric(sexo),
    edad_cat3_num = as.numeric(edad_cat3),
    clase_2_num = as.numeric(clase_2),
    urb_rur_num = as.numeric(urb_rur),
    survey_num = as.numeric(survey),
    survey2_num = as.numeric(survey2),
    sedentarismo2 = as.numeric(sedentarismo) # 1 and 2, not 0 and 1
    )

# Convert 1/2 to 0/1
dt$sedentarismo2 <- dt$sedentarismo2 - 1

# percent of sedentarism overall and per sex
prop.table(table(dt$sedentarismo))*100
prop.table(table(dt$sedentarismo, dt$sexo), 1)*100 # where the 1 gives me row proportions

## create new database with variables needed to construct stratum
### sex, age group, social class, urban/semi/rural, survey year
dt <- dt %>% 
  mutate(
    stratum = 10000*sexo_num + 1000*edad_cat3_num + 100*clase_2_num + 10*urb_rur_num + survey2_num
  )
dt$stratum <- as.factor(dt$stratum)
summary(dt$stratum)

## maihda database with stratum variable
save(dt, file = "maihda_stratum.RData")

# Prevalence of sedentarism per strata
tab <- prop.table(table(dt$stratum, dt$sedentarismo), 1)*100
tab_round <- round(tab, 1)
tab_round
clipr::write_clip(tab_round)

## Sort data by stratum
dt <- dt[order(dt$stratum),]

## Generate a new variable which records stratum size
dt <- dt %>%
  group_by(stratum) %>%
  mutate(strataN = n())

summary(dt$strataN) # minimum 62 counts in a group

## Fit null model ----
model0 <- glmer(sedentarismo ~ (1|stratum), data = dt, family = "binomial")

## Modeling the log of the expected prevalence as a function of a fixed intercept
## which is the overall log mean prevalence and a random intercept for each stratum
## capturing between-stratum variability in sedentarism
## This is the null model use to estimate VPC (how much variance is between vs
## within strata) and serves as baseline for later main-effects models
summary(model0)
exp(-1.97749) 
## log odds converted to a probability
# The baseline risk of sedentarism is 12% averaged across all strata
tab_model(model0, show.se=T)
## at the residual level, the variance is not reported because in logistic models
## the residual variance at the individual-level is fixed at pi^2/3 = 3.29

## Calculate the VPC
## approximated as the variance(stratum)/variance(stratum + 3.29)
tau2 <- as.numeric(VarCorr(model0)$stratum[1,1]) ## the between-stratum variance
tau2 ## how much sedentarism risk differs across intersectional strata, and normal for
     ## for this to be much larger because model0 does not consider fixed effects therefore 
     ## appears to have greater intersectional inequalities
VPC0 <- tau2 / (tau2 + (pi^2 / 3)) ## in linear models, could do between-stratum variance/total variance
    ## but in logistic models, the individual-level residual variance is not estimated directly
     ## the work around is an assumption that the individual-level residual variance is pi^2/3

VPC0
VPC0_percent <- VPC0*100
VPC0_percent
## about 7.6% of the variance in diabetes risk is attributable to differences
## between intersectional strata, while the remaining 92.4% is at the individual level
## this is the discriminatory accuracy of the strata predicting sedentarism

## Fit two-level logistic regression with covariates ----
model1 <- glmer(sedentarismo ~ sexo + edad_cat3 + clase_2 + urb_rur + survey2 +
                       (1|stratum), data = dt, family = "binomial")
summary(model1)
tab_model(model1, show.se=T)

## Calculate the VPC
tau2 <- as.numeric(VarCorr(model1)$stratum[1,1]) ## the between-stratum variance
tau2 
VPC1 <- tau2 / (tau2 + (pi^2 / 3))
VPC1
VPC1_percent <- VPC1*100
VPC1_percent

## Calculate the PCV
var_null <- as.numeric(VarCorr(model0)$stratum[1,1])
var_null
var_adj  <- as.numeric(VarCorr(model1)$stratum[1,1])
var_adj # after adjusting for predictors, there is very little variance remaining
# between strata - stratum level differences are not mostly explained by the additive
# effects of these predictors

PCV <- ((var_null - var_adj) / var_null)*100
PCV ## answers how much of the between-stratum inequality is explained by additive effects
    ## HIGH PCV means inequalities seen are largely additive

## Following Evans Tutorial Steps to create tables and figures
## Extract and prepare model predictions and random effects for interpretation
## and visualization 

# NULL MODEL (baseline inequality)
# • Stratum predicted probability (m0pb)
# • Grand mean probability (fixed only) (m0wgm)
# 
# Purpose:
#   → Quantify total intersectional inequality
# 
# ADD. MODEL (adjusted inequality)
# • Stratum predicted log-odds + CI (m1pb)
# • Stratum predicted probabilities (m1pb_prob)
# • Fixed-only predicted log-odds (m1gwm)
# • Fixed-only predicted probabilities (m1wgm_prob)
# • Stratum random effects + SEs (m1SE)
# 
# Purpose:
# → Partition inequality:
#   - Additive part
#   - Intersectional excess part
# → Calculate PCV
# → Rank strata after adjustment

# predict the fitted linear predictor on the **probability scale**
dt$m0pb <- predict(model0, type="response") ## m2Axbu
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
dt$m0wgm <- predict(model0, type ="response", re.form=NA) ## m2Axb
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

# predict the fitted linear predictor, and confidence intervals, on the **probability scale**
m1pb_prob <- predictInterval(model1, which = "full", level=0.95, include.resid.var=FALSE, type="probability") ## m2Bm_prob

# create a new id variable for this newly created dataframe
m1pb_prob <- mutate(m1pb_prob, id=row_number())

# on the logit scale, predict the linear predictor for the fixed portion of the model only in the **log-scale**
dt$m1wgm <- predict(model1, re.form=NA) ## m2BmF
summary(dt$m1wgm)

# predict the fitted linear predictor, on the **probability scale** for the fixed portion of the model only
dt$m1wgm_prob <- predict(model1, type = "response", re.form=NA) ## m2Bxb
## Back-transforms the fixed-effects-only predictions to probability scale where
## each observation's value = predicted sedentarism probability from the additive
## main effects ONLY (ignoring stratum residuals)

# predict the stratum random effects and associated standard errors
m1SE <- REsim(model1) ## m2BU
m0SE <- REsim(model0)
m0SE

m1SE ## mean, median and standard deviation
## REsim simulates random intercepts (stratum effects) and associated uncertainty
## which represents residual intersectional effects, the part of stratum-specific
## risk not explained by additive main effects

## Create a dataframe at the strata level with the means of each variable
## setting up a stratum-level dataset that combines predictions, confidence
## intervals, and observed outcomes for both continuous and binary outcomes

## merge predictions with original data
# create an id variable for merging in the dt0 dataframe
dt$id <- seq.int(nrow(dt))

# create a new dataframe, dt2, that merges dt and m1pb
dt2 <- merge(dt, m1pb, by = "id")
summary(dt2)

# rename the variables from m1pb --- on a logit scale
dt2 <- dt2 %>%
  rename(
    m1pbfit=fit,
    m1pblwr=lwr,
    m1pbupr= upr,
  )

# merge in m1pb_prob
dt2 <- merge(dt2, m1pb_prob, by="id")

# rename the variables from m1pb_prob --- on a prob scale
dt2 <- dt2 %>%
  rename(
    m1pb_probfit=fit,
    m1pb_problwr=lwr,
    m1pb_probupr= upr
  )

# collapse the data down to a stratum-level dataset
stratum_level <- aggregate(
  x = dt2[c("sedentarismo2")],
  by = dt2[c("sexo", "edad_cat3", "clase_2", "urb_rur", "survey2",
             "stratum", "strataN", "m1pbfit", "m1pblwr", "m1pbupr", 
             "m1pb_probfit", "m1pb_problwr", "m1pb_probupr",  "m1wgm", "m1wgm_prob")],
  FUN = mean
) 

## we are aggregating means and proportions to one row per stratum, so for each stratum,
## we compute the mean observed outcomes and average predictions/CI
## clean stratum-level dataset with: observed outcomes, predicted outcomes, confidence intervals,
## fixed-effect-only predictions
stratum_level

## convert the outcome from a proportion to a percentage
stratum_level$sedentarismo2_p <- stratum_level$sedentarismo2*100

save(stratum_level, file = "stratum_level.RData")
stratum_level <- get(load("stratum_level.RData"))

## Table 1 ----
# Tabulate each individual characteristics
table(dt$sexo, dt$sedentarismo)
table(dt$edad_cat3, dt$sedentarismo)
table(dt$clase_2, dt$sedentarismo)
table(dt$urb_rur, dt$sedentarismo)
table(dt$survey2, dt$sedentarismo)
table(dt$sedentarismo)

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

table1 <- maihda %>%
  select(sexo, edad_cat3, urb_rur, clase_2, survey2, sedentarismo) %>%
  tbl_summary(
    label = list(
      sexo ~ "Sex",
      edad_cat3 ~ "Age Group",
      urb_rur ~ "Municipality Type",
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
stratum_level$n200plus <- ifelse(stratum_level$strataN>=200, 1,0)
stratum_level$n100plus <- ifelse(stratum_level$strataN>=100, 1,0)
stratum_level$n50plus <- ifelse(stratum_level$strataN>=50, 1,0)
stratum_level$n30plus <- ifelse(stratum_level$strataN>=30, 1,0)
stratum_level$n20plus <- ifelse(stratum_level$strataN>=20, 1,0)
stratum_level$n10plus <- ifelse(stratum_level$strataN>=10, 1,0)
stratum_level$nlessthan10 <- ifelse(stratum_level$strataN<10, 1,0)

# tabulate the binary indicators
table(stratum_level$n200plus)
table(stratum_level$n100plus)
table(stratum_level$n50plus)
table(stratum_level$n30plus)
table(stratum_level$n20plus)
table(stratum_level$n10plus)
table(stratum_level$nlessthan10)
summary(dt$stratum)
## all are greater than 100 counts except 6 strata

# summarise the observed stratum means/prevalence of sedentarism per group
proptable <- prop.table(table(dt$stratum, dt$sedentarismo), 1)*100
proptable
clipr::write_clip(proptable)

# Observed stratum-level means (prevalence)
observed_table <- dt %>%
  group_by(stratum) %>% 
  summarise(
    n = n(),
    cases = sum(sedentarismo == "Yes", na.rm = TRUE),
    prevalence = mean(sedentarismo == "Yes", na.rm = TRUE) 
  ) %>%
  mutate(prevalence_pct = prevalence * 100)

observed_table
clipr::write_clip(observed_table)

## *100
stratum_level_predictedprob_percent <- dt2 %>%
  group_by(
    sexo, edad_cat3, urb_rur, clase_2, survey2,
    stratum, strataN
  ) %>%
  summarise(
    observed_prevalence = mean(sedentarismo2, na.rm = TRUE)*100,
    
    m1pb_probfit = mean(m1pb_probfit, na.rm = TRUE)*100,
    m1pb_problwr = mean(m1pb_problwr, na.rm = TRUE)*100,
    m1pb_probupr = mean(m1pb_probupr, na.rm = TRUE)*100,
    
    
    m1pbfit      = mean(m1pbfit, na.rm = TRUE),
    m1pblwr      = mean(m1pblwr, na.rm = TRUE),
    m1pbupr      = mean(m1pbupr, na.rm = TRUE),
    
    m1wgm        = mean(m1wgm, na.rm = TRUE),
    m1wgm_prob     = mean(m1wgm_prob, na.rm = TRUE)*100,
    
    .groups = "drop"
  ) %>% 
  arrange(m1pb_probfit)

stratum_level_predictedprob_percent
head(stratum_level_predictedprob_percent)
clipr::write_clip(head(stratum_level_predictedprob_percent))

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

## Figure 2. ---- 
stratum_level <- stratum_level %>%
  mutate(rank2=rank(m1pb_probfit))

# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank2),]
lowest <- head(stratum_level)
lowest
clipr::write_clip(lowest)
highest <- tail(stratum_level)
clipr::write_clip(highest)
highest
clipr::write_clip(stratum_level)

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
top <- max(stratum_level$m1pb_probupr) + 1.5
ggplot(stratum_level, aes(x = rank2, y = m1pb_probfit)) +
  geom_pointrange(aes(ymin = m1pb_problwr, ymax = m1pb_probupr), size = 0.5) +
  geom_text(
    aes(x = rank2, y = top, label = stratum),            
    vjust = -0.15,
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

## new graph
top <- max(stratum_level$m1pb_probupr) + 1.5
ggplot(stratum_level, aes(x = rank2, y = m1pb_probfit)) +
  geom_vline(
    xintercept = seq(min(stratumsim2$rank),
                     max(stratumsim2$rank),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1pb_problwr, ymax = m1pb_probupr),
    color = "grey20",
    size = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2
  ) +
  
  geom_text(
    aes(x = rank2, y = top, label = stratum),            
    vjust = -0.15,
    size = 3,
    angle = 90,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Predicted Probability of Sedentarism by Intersectional Stratum",
    x = "Intersectional Stratum (lowest to highest risk)",
    y = "Predicted probability of sedentarism (%)",
    caption = "Stratum-level predicted probabilities; 95% confidence intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(size = 9),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_predictedprob.png", width = 4000, height = 2200, dpi=300, units = "px")

## Figure 3. Tutorial Residual Calculation 1 ----
## RECALL
# • Stratum predicted log-odds random and fixed (m1pb)
# • Stratum predicted probabilities random and fixed (m1pb_prob)
# • Fixed-only predicted log-odds (m1gwm)
# • Fixed-only predicted probabilities (m1wgm_prob)
# • Stratum random effects + SEs (m1SE)
m1SE <- REsim(model1) 
plotREsim(m1SE)

## recall REsim estimates the stratum-level random effects and their uncertainty

# For this plot we follow a simulation-based approach to 
# calculating the limits of the approximate 95% confidence intervals of our 
# predictions. This involves simulating 1000 values for each predicted value.
dt_new <- read_excel("Tables/MAIHDA residual effects_DJM.xlsx")
set.seed(1234)

# SE calculation for Excel
dt_new$SE_log <- (dt_new$log_total - dt_new$log_total_lower)/1.96

p <- dt_new$prob_total/100

dt_new$SE_prob <- 100*(dt_new$SE_log * p * (1 - p))

write_xlsx(dt_new, "MAIHDA residual effects SE_DJM.xlsx")

# duplicate stratum data *1000 (creating a new dataframe, stratumsim)
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

stratumsim_new <- rbind(dt_new, 
                    dt_new[rep(1:nrow(dt_new),999),])

# Generate the approximate standard error for the linear prediction on the
# logit scale. We do this based on the difference between one estimated
# confidence interval and the estimated fit, on the logit scale
stratumsim$m1pbSE <- (stratumsim$m1pbfit - stratumsim$m1pblwr)/1.96

stratumsim_new$m1pbSE <- (stratumsim_new$log_total - stratumsim_new$log_total_lower)/1.96


# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)
set.seed(354611)

## Convert everything to percentages: 
## from log-odds to percentages of sedentary children

# Generate the predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect and factoring in 
# prediction uncertainty
stratumsim$m1pbpred_total <- 100*invlogit(stratumsim$m1pbfit + 
                                      rnorm(nrow(stratumsim), mean=0, sd=stratumsim$m1pbSE))
stratumsim_new$prob_total2 <- 100*invlogit(stratumsim_new$log_total + 
                                            rnorm(nrow(stratumsim_new), mean=0, sd=stratumsim_new$m1pbSE))

# Simulate predicted probabilities with random noise, adds normal noise to each 
# linear predictor based on its SE, transforming it to probability scale using
# invlogit(), and multiplies by 100 to have predicted percent sedentarism for each
## simulated stratum

# Generate the predicted stratum percentages ignoring the predicted stratum 
# random effect
stratumsim$m1pbpred_fixed <- 100*invlogit(stratumsim$m1wgm)
stratumsim_new$prob_fixed2 <- 100*invlogit(stratumsim_new$log_fixed)

# Generate the difference in the predicted stratum percentages due to the 
# predicted stratum effect
stratumsim$m1pb_diff <- stratumsim$m1pbpred_total - stratumsim$m1pbpred_fixed
stratumsim_new$prob_diff2 <- stratumsim_new$prob_total2 - stratumsim_new$prob_fixed2

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]
stratumsim_new <- stratumsim_new[order(stratumsim_new$stratum),]

## Summarise this uncertainty per stratum:
## compute the average residual effect, the spread of that effect with the SD 
## and a 95% interval --> to tell if this stratum is consistently above or below what additive effects predict

# collapse the data down to stratum level, generate mean and SE variables,
# then use these to generate rank and lower and upper limits of the approximate
# 95% confidence intervals of the difference in predicted stratum percentages 
# due to interaction variables.
stratum_info <- stratumsim %>% 
  distinct(stratum, sexo, edad_cat3, urb_rur, survey2, strataN, m1pbfit, m1pbupr, 
           m1pb_probfit, m1pb_problwr, m1pb_probupr, m1wgm, m1wgm_prob, m1pbSE, 
           m1pbpred_total, m1pbpred_fixed, m1pb_diff)

stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m1pb_diff), std=sd(m1pb_diff)) %>%
  arrange(mean) %>%
  mutate(rank = row_number()) %>% 
  mutate(upper=(mean + 1.96*std)) %>%
  mutate(lower=(mean - 1.96*std)) %>% 
  left_join(stratum_info, by = "stratum")
clipr::write_clip(stratumsim2)

stratumsim2_new <- stratumsim_new %>%
  group_by(stratum) %>%
  summarise(mean=mean(prob_diff2), std=sd(prob_diff2)) %>%
  arrange(mean) %>%
  mutate(rank = row_number()) %>% 
  mutate(upper=(mean + 1.96*std)) %>%
  mutate(lower=(mean - 1.96*std))
clipr::write_clip(stratumsim2_new)

# compute mean residual effect and standard deviation for each stratum
# rank stata by mean residual effect - for ordered plotting

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent sedentarism due to interactions") +
  theme_bw()

ggplot(stratumsim2_new, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent sedentarism due to interactions") +
  theme_bw()

# significant strata
stratumsim2 %>% filter(lo>0)
stratumsim2 %>% filter(hi<0)
head(stratumsim2)
tail(stratumsim2)

stratumsim2_new %>% filter(lower>0)
stratumsim2_new %>% filter(upper<0)

# x-axis is the stratum rank, ordered by residual effect
# y-axis is the predicted % deviation due to residual intersectional effect
# red line at 0 means there is no residual effect, the stratum matches additive
# prediction

## THIS IS KEY: the filter selects strata whose entire uncertainty intervals lie above or below zero
## Have no significant strata because once additive effects are accounted for, no 
## stratum shows a statistically detectable residual deviation from the expected risk 
## thus the differences that exist between strata are fully explained by additive effects
## no evidence of strong, unique intersectional mechanisms over and above those factors

# Filter only significant strata
sig_strata <- stratumsim2 %>%
  filter(lo > 0 | hi < 0) %>% ## stratums significant ONLY if the zero is completely excluded from the uncertainty level
  arrange(mean) %>%            
  mutate(rank = row_number())  

# Caterpillar plot for significant strata only
ggplot(sig_strata, aes(x=rank, y=mean, label=stratum)) +
  geom_point(size=3, color="blue") +
  geom_pointrange(aes(ymin=lo, ymax=hi), color="blue") +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_text(hjust=0, vjust=1.5, size=3) +  # adjust label position
  xlab("Stratum Rank") +
  ylab("Difference in predicted % sedentarism due to interactions") +
  theme_bw()

## NO STRATUM WITHOUT 0 IN UNCERTAINTY LEVEL

### Updated Figure 3
top_y <- max(stratumsim2_new$upper) + 0.5

ggplot(stratumsim2_new, aes(x = rank, y = mean)) +
  geom_text(
    aes(
      x = rank, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratumsim2_new$rank),
                     max(stratumsim2_new$rank),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9,
    linetype = "solid"
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  
  labs(
    title = "Residual Intersectional Effects on Sedentarism",
    subtitle = "How much each stratum deviates from what would be expected based on additive effects alone.",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Difference in predicted percentage points",
    caption = "Stratum-level residual effects; 95% confidence intervals.\nRed line indicates no residual intersectional effect."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects.png", width = 4000, height = 2200, dpi=300, units = "px")

lowest <- head(stratumsim2)
lowest
clipr::write_clip(lowest)
highest <- tail(stratumsim2)
highest
clipr::write_clip(highest)
clipr::write_clip(stratumsim2)

## Excel database # values change ----
stratum_level <- get(load("stratum_level.RData"))
re_list <- ranef(model1, condVar = TRUE)$stratum %>%
  rownames_to_column(var = "stratum") %>%
  rename(random_effect = `(Intercept)`)

stratum_level2 <- merge(stratum_level, re_list, by="stratum")
stratum_level2 <- stratum_level2 %>% 
  mutate(prob_diff = m1pb_probfit - m1wgm_prob) %>% 
  mutate(across(
    c(m1pb_probfit, m1pb_problwr, m1pb_probupr, m1wgm_prob, prob_diff),
    ~ .x*100
  ))

stratum_level2
clipr::write_clip(stratum_level2)

## Luis Code without Simulation ----
# predict the fitted linear predictor on the **probability scale** model 0
dt$m0_prob_total <- predict(model0, type="response") 
dt$m0_prob_fixed <- predict(model0, type ="response", re.form=NA) 

# predict the linear predictor for the fixed portion of the model only in log-scale
dt$m1_log_fixed <- predict(model1, re.form=NA)

# predict the fitted linear predictor on the **probability scale** model 1
m1_prob_total <- predictInterval(model1, level=0.95, which = "full", include.resid.var=FALSE, type = "probability")
m1_prob_fixed <- predictInterval(model1, level=0.95, which = "fixed", include.resid.var=FALSE, type = "probability")

# create new id for this dataframe
m1_prob_total <- mutate(m1_prob_total, id=row_number())
m1_prob_fixed <- mutate(m1_prob_fixed, id=row_number())

# variable for merging
dt$id <- seq.int(nrow(dt))

# merge total effect
dt2 <- merge(dt, m1_prob_total, by = "id")

dt2 <- dt2 %>%
  rename(
    m1fit_total=fit,
    m1upr_total= upr,
    m1lwr_total=lwr
  )

# merge fixed effect
dt2 <- merge(dt2, m1_prob_fixed, by = "id")

dt2 <- dt2 %>%
  rename(
    m1fit_fixed=fit,
    m1upr_fixed= upr,
    m1lwr_fixed=lwr
  )

# collapse to stratum level
stratum_level2 <- aggregate(
  x = dt2[c("sedentarismo2", "strataN", "m0_prob_total", "m0_prob_fixed", "m1_log_fixed", 
            "m1fit_fixed", "m1upr_fixed", "m1lwr_fixed",  "m1fit_total", "m1upr_total", "m1lwr_total")],
  by = dt2[c("stratum")],
  FUN = mean
) 

# convert to percentage
stratum_level2 <- stratum_level2 %>%
  mutate(across(
    c(m0_prob_total, m0_prob_fixed, m1fit_fixed,
      m1upr_fixed, m1lwr_fixed, m1fit_total,
      m1upr_total, m1lwr_total),
    ~ .x * 100
  ))

# calculate standard errors for total and fixed effects
stratum_level2 <- stratum_level2 %>% 
  mutate(
    se_total = (m1upr_total - m1lwr_total) / (2*1.96), 
    se_fixed = (m1upr_fixed - m1lwr_fixed) / (2*1.96)
  )

# calculate the standard error of the difference (residual effects)
stratum_level2 <- stratum_level2 %>% 
  mutate(
    se_diff = sqrt(se_total^2 + se_fixed^2)
  )

# calculate the difference (residual effects)
stratum_level2 <- stratum_level2 %>% 
  mutate(
    dif_mean = m1fit_total - m1fit_fixed,
    dif_lwr = dif_mean - 1.96*se_diff,
    dif_upr = dif_mean + 1.96*se_diff
  )

# rank by total stratum probabilities
stratum_level2 <- stratum_level2 %>%
  mutate(rank1=rank(m1fit_total))

# rank by difference (residual effect on probability scale)
stratum_level2 <- stratum_level2 %>%
  mutate(rank2=rank(dif_mean))

# plot predicted stratum probabilities
ggplot(stratum_level2, aes(x = rank1, y = m1fit_total)) +
  geom_point() +
  geom_pointrange(aes(ymin = m1lwr_total, ymax = m1upr_total)) +
  geom_text(
    aes(
      x = rank2,
      y = m1fit_total + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Predicted Percent Sedentarism, Model 1") +
  xlab("Stratum Rank") +
  theme_bw()

# new plot
top_y <- max(stratum_level2$m1upr_total) + 1.5

ggplot(stratum_level2, aes(x = rank1, y = m1fit_total)) +
  geom_text(
    aes(
      x = rank1, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratum_level2$rank1),
                     max(stratum_level2$rank1),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1lwr_total, ymax = m1upr_total),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  labs(
    title = "Predicted Probability of Sedentarism by Intersectional Stratum",
    x = "Intersectional Stratum (lowest to highest risk)",
    y = "Predicted probability of sedentarism (%)",
    caption = "Stratum-level predicted probabilities; 95% confidence intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_predictedprob_new.png", width = 4000, height = 2200, dpi=300, units = "px")

# list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level2 <- stratum_level2[order(stratum_level2$rank1),]
head(stratum_level2)
tail(stratum_level2)

# residual effect plot
ggplot(stratum_level2, aes(x = rank2, y = dif_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = dif_lwr, ymax = dif_upr)) +
  geom_text(
    aes(
      x = rank2,
      y = dif_upr + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Probability difference in total and fixed effect for sedentarism, Model 1") +
  xlab("Stratum Rank") +
  theme_bw()

# new plot
top_y <- max(stratum_level2$dif_upr) + 1

ggplot(stratum_level2, aes(x = rank2, y = dif_mean)) +
  geom_text(
    aes(
      x = rank2, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratum_level2$rank2),
                     max(stratum_level2$rank2),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9,
    linetype = "solid"
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = dif_lwr, ymax = dif_upr),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  
  labs(
    title = "Residual Intersectional Effects on Sedentarism",
    subtitle = "How much each stratum deviates from what would be expected based on additive effects alone.",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Difference in predicted percentage points",
    caption = "Stratum-level residual effects; 95% confidence intervals.\nRed line indicates no residual intersectional effect."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_new_08-03-26.png", width = 4000, height = 2200, dpi=300, units = "px")

# list of 6 highest and 6 lowest stratum residual effects
stratum_level2 <- stratum_level2[order(stratum_level2$rank2),]
head(stratum_level2)
tail(stratum_level2)