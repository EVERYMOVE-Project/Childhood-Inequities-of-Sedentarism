## Author: Diana Juanita Mora
## Project: Childhood Inequities of Sedentarism
## Script: MAIHDA residual effects and bootstrapping
## Edited: March 3rd 2026

## Load libraries----
library(haven)
library(tidyverse)
library(lme4)
library(merTools)
library(labelled)
library(broom)

library(lme4)
library(ggplot2)
library(clipr)

library(ggrepel)
library(extrafont)

# font_import(prompt = FALSE)   # run once (can take a few minutes)
# loadfonts(device = "win") 

## Load data ----
maihda <- get(load("maihda_stratum.RData"))
stratum_level <- get(load("stratum_level.RData"))

# Figure 3. Tutorial Residual Calculation 1 ----
## RECALL
# • Stratum predicted log-odds random and fixed (m1pb)
# • Stratum predicted probabilities random and fixed (m1pb_prob)
# • Fixed-only predicted log-odds (m1gwm)
# • Fixed-only predicted probabilities (m1wgm_prob)
# • Stratum random effects + SEs (m1SE)
plotREsim(m1SE)

## recall REsim estimates the stratum-level random effects and their uncertainty

# For this plot we follow a simulation-based approach to 
# calculating the limits of the approximate 95% confidence intervals of our 
# predictions. This involves simulating 1000 values for each predicted value.

# duplicate stratum data *1000 (creating a new dataframe, stratumsim)
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

# Generate the approximate standard error for the linear prediction on the
# logit scale. We do this based on the difference between one estimated
# confidence interval and the estimated fit, on the logit scale
stratumsim$m1pbSE <- (stratumsim$m1pbfit - stratumsim$m1pblwr)/1.96

# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)
set.seed(354611)

## Convert everything to percentages: 
## from log-odds to percentages of sedentary children

# Generate the predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect and factoring in 
# prediction uncertainty
stratumsim$m1pbpred <- 100*invlogit(stratumsim$m1pbfit + 
                                      rnorm(nrow(stratumsim), mean=0, sd=stratumsim$m1pbSE))

# Simulate predicted probabilities with random noise, adds normal noise to each 
# linear predictor based on its SE, transforming it to probability scale using
# invlogit(), and multiplies by 100 to have predicted percent sedentarism for each
## simulated stratum

# Generate the predicted stratum percentages ignoring the predicted stratum 
# random effect
stratumsim$m1pbpred2 <- 100*invlogit(stratumsim$m1wgm)

# Generate the difference in the predicted stratum percentages due to the 
# predicted stratum effect
stratumsim$m1pb_diff <- stratumsim$m1pbpred - stratumsim$m1pbpred2
stratumsim$diff <- stratumsim$m1pbSE - stratumsim$m1wgm

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]

## Summarise this uncertainty per stratum:
## compute the average residual effect, the spread of that effect with the SD 
## and a 95% interval --> to tell if this stratum is consistently above or below what additive effects predict

# collapse the data down to stratum level, generate mean and SE variables,
# then use these to generate rank and lower and upper limits of the approximate
# 95% confidence intervals of the difference in predicted stratum percentages 
# due to interaction variables.
stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m1pb_diff), std=sd(m1pb_diff)) %>%
  arrange(mean) %>%
  mutate(rank = row_number()) %>% 
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std)) 

# compute mean residual effect and standard deviation for each stratum
# rank stata by mean residual effect - for ordered plotting

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent sedentarism due to interactions") +
  theme_bw()

# significant strata
stratumsim2 %>% filter(lo>0)
stratumsim2 %>% filter(hi<0)
head(stratumsim2)
tail(stratumsim2)

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
top_y <- max(stratumsim2$hi) + 0.5

ggplot(stratumsim2, aes(x = rank, y = mean)) +
  geom_text(
    aes(
      x = rank, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratumsim2$rank),
                     max(stratumsim2$rank),
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
    aes(ymin = lo, ymax = hi),
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

stratumsim2

lowest <- head(stratumsim2)
lowest
clipr::write_clip(lowest)
highest <- tail(stratumsim2)
highest
clipr::write_clip(highest)
clipr::write_clip(stratumsim2)

# Residual Calculation 2 Log-Odds ----
## unique stratum-specific effects
plotREsim(m0SE) # model0 - will have more strata significant
# predict the stratum random effects and associated standard errors
set.seed(123)
m1SE <- REsim(model1,n.sims = 10000)

plotREsim(m1SE) # model1 - just the random effect of model 1 on the log-scale

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_log-odds.png", width = 4000, height = 2200, dpi=300, units = "px")

## calculate the lower and upper CI
## where mean is the stratum-level random effect 
## uj is the stratum-specific deviation from the overall log-odds
## total log-odds for stratum j is:
## logit(pj) = betanot + uj 
## where betanote is the overall intercept (additive effects of variables in the model)
## uj is the excess or deficit log-odds due to the specific intersectional stratum
m1SE$lower<-m1SE$mean-qnorm(0.975)*m1SE$sd
m1SE$upper<-m1SE$mean+qnorm(0.975)*m1SE$sd
clipr::write_clip(m1SE$upper)
m1SE

## identify the strata that are either entirely above or below the null effect
m1SE %>% filter(lower>0) # 23112 a significantly higher log-odds of sedentarism than expected based on the fixed effects alone
m1SE %>% filter(upper<0) # 21132 a significantly lower log-odds of sedentarism than expected based on the fixed effects alone

## on a log-scale, these are the two strata that show statistically significant excess in log-odds of sedentarism
## beyond what would be expected from the additive effects of the constituent sociodemographic variables alone

# plot log-odds with strata labels
# Make a copy of your m1SE dataframe and add the stratum names
m1SE$group <- rownames(m1SE)

# Rank strata by the mean effect for plotting
m1SE <- m1SE[order(m1SE$mean), ]
m1SE$rank <- 1:nrow(m1SE)
m1SE

# Determine y position for labels (slightly above upper CI)
top_y <- max(m1SE$upper) + 0.02

# Base plotREsim
p <- plotREsim(m1SE, stat = "mean")  # this will plot your points and CIs as usual
p
p + 
  geom_text(
    data = m1SE,
    mapping = aes(x = rank, y = top_y, label = groupID),  # explicitly reference columns in m1SE
    inherit.aes = FALSE,  # VERY IMPORTANT: prevents ggplot from expecting y or ymax in transformed data
    angle = 90,
    vjust = -0.15,
    size = 3
  )

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_log-odds_new.png", width = 4000, height = 2200, dpi=300, units = "px")

## Residual Calculation 3 ----
# Calculate directly from random effects only log-scale
# Extract random effects with conditional variance
# conditional variance because want to quantify the uncertainty around each stratum's random effect estimate
# uj - point estimate of the intersectional residual effect
# need standard error to distinguish if deviation is statistically different from zero
# standard error comes from the conditional variance
re_list <- ranef(model1, condVar = TRUE) # random effects from model1
re <- re_list$stratum # random effect for each strata
postVar <- attr(re, "postVar") # array with dimensions 1,1,j, where j is the number of strata
re$se <- sqrt(postVar[1,1,]) # taking square root of each stratums conditional variance to my dt re
u <- re[,1]
se_u <- sqrt(postVar[1,1,])

# plot of random effect log-odds
# Create data frame
plot_df <- data.frame(
  stratum = rownames(re),
  u   = re[,1],
  se  = se_u
)

# 95% CI on logit scale
z <- qnorm(0.975)

plot_df$lower <- plot_df$u - z * plot_df$se
plot_df$upper <- plot_df$u + z * plot_df$se

# significant strata
plot_df %>% filter(lower>0)
plot_df %>% filter(upper<0)

# Order by magnitude
plot_df <- plot_df[order(plot_df$u), ]
plot_df$stratum <- factor(plot_df$stratum, levels = plot_df$stratum)

# create rank for plot
plot_df$rank <- rank(plot_df$u, ties.method = "first")
plot_df <- plot_df[order(plot_df$u), ]

top_y <- max(plot_df$upper) + 0.05  # adjust offset for readability

# new plot
ggplot(plot_df, aes(x = rank, y = u)) +
  
  # Vertical gridlines for each stratum
  geom_vline(
    xintercept = seq(min(plot_df$rank), max(plot_df$rank), by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Stratum labels above the points
  geom_text(
    aes(x = rank, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9
  ) +
  
  # Confidence intervals (log-odds scale)
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
    title = "Intersectional Residual Effects",
    subtitle = "Stratum-specific random effects (log-odds scale)",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Random effect (log-odds)",
    caption = "Random intercepts from MAIHDA model; 95% CI.\nRed line indicates no intersectional residual effect (u = 0)."
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
    axis.text.x = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_log-odds_4.png", width = 4000, height = 2200, dpi=300, units = "px")

## Residual Calculation 4 ----
## where calculate fixed-effect per strata and convert to probability step-by-step
re_list <- ranef(model1, condVar = TRUE) # random effects from model1
re <- re_list$stratum # random effect for each strata
postVar <- attr(re, "postVar") # array with dimensions 1,1,j, where j is the number of strata
re$se <- sqrt(postVar[1,1,]) # taking square root of each stratums conditional variance to my dt re

# Make a stratum-level dataset
dt_stratum <- dt %>%
  group_by(stratum) %>%
  summarise(
    sexo = first(sexo),
    edad_cat3 = first(edad_cat3),
    clase_2 = first(clase_2),
    urb_rur = first(urb_rur),
    survey2 = first(survey2)
  )

# Compute fixed-effect linear predictor for each stratum
# creating the matrix for the fixed effects
# for each stratum, the dummy-coded representation of its categories where each row 
# corresponds to one stratum's combination of the variables
X <- model.matrix(~ sexo + edad_cat3 + clase_2 + urb_rur + survey2, data = dt_stratum)
X
# computes the betas for each stratum
# linear fixed is the predicted log-odds for that stratum under the additive model only,
# assuming no intersectional deviation
linear_fixed <- X %*% fixef(model1)
linear_fixed

# Add the random effect for the stratum
# re[,1] is uj, the random intercept for each stratum (stratumj)
# computing betanot + betax + uj, meaning the full predicted log-odds for each stratum under the multilevel model
linear_total <- linear_fixed + re[,1]

# Transform to probabilities
prob_fixed <- plogis(linear_fixed)
prob_total <- plogis(linear_total)

# Residual / intersectional effect in probability scale
prob_diff <- prob_total - prob_fixed

# Compute 95% CI on logit scale and transform to probability
z <- qnorm(0.975)
# these CI reflect uncertainty in the random effect only 
# do not incorporate uncertainty in the fixed effects
# covariance between fixed and random effects
# uncertainty in variance components
lower <- plogis(linear_total - z * re$se) 
upper <- plogis(linear_total  + z * re$se)

# statistical inference is determined on the logit scale, not probability
lower_diff <- lower - prob_fixed
upper_diff <- upper - prob_fixed

# Combine into a plotting dataframe
plot_df <- dt_stratum %>%
  mutate(
    group = stratum,
    prob_total = prob_total*100,
    prob_fixed = prob_fixed*100,
    prob_diff = prob_diff*100,
    lower_diff = lower_diff*100,
    upper_diff = upper_diff*100
  )

# Plot intersectional residual effects
ggplot(plot_df, aes(x = reorder(group, prob_diff), y = prob_diff)) +
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin = lower_diff, ymax = upper_diff), width = 0) +
  labs(
    x = "Stratum",
    y = "Excess probability of sedentarism (intersectional effect)"
  ) + geom_hline(yintercept=0)+theme_bw()

## updated plot
# Rank strata by residual probability (prob_diff)
plot_df <- plot_df %>%
  arrange(prob_diff) %>%
  mutate(rank = row_number())

# significant strata
plot_df %>% filter(lower_diff>0)
plot_df %>% filter(upper_diff<0)

# Set label height above highest CI
top_y <- max(plot_df$upper_diff) + 1  # adjust offset for readability

ggplot(plot_df, aes(x = rank, y = prob_diff)) +
  
  # Stratum labels above the points
  geom_text(
    aes(x = rank, y = top_y, label = group),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  
  # Vertical gridlines for each stratum
  geom_vline(
    xintercept = seq(min(plot_df$rank), max(plot_df$rank), by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = lower_diff, ymax = upper_diff),
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
    subtitle = "Excess probability of sedentarism after accounting for additive fixed effects",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Excess probability (percentage points)",
    caption = "Stratum-level residual effects; 95% CI.\nRed line indicates no residual intersectional effect."
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

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_new.png", width = 4000, height = 2200, dpi=300, units = "px")

## Residual Calculation 5 ----
## only accounts for intercept fixed effects
# Extract random effects (on log-odds scale)
re <- ranef(model1)$stratum # random effects
re
postVar <- attr(re, "postVar")
re$se <- sqrt(postVar[1,1,]) 

fix_int <- fixef(model1)[1] # fixed effects, note how is just one value, the intercept of model1
fix_int

# Combine, transform to probability, and plot
re$probability <- plogis(fix_int + re[,1]) 

# to calculate probability, include both fixed effect and random effect of each strata
re$group <- rownames(re) # strata name
re$prob_base <-  plogis(fix_int) # probability of just the fixed effects *note how is same for all strata
re$prob_diff<-re$probability-re$prob_base # the difference between total and fixed

# Compute CI on logit scale, then transform
re$lower <- plogis(fix_int + re[,1] - qnorm(0.975) * re$se)
re$upper <- plogis(fix_int + re[,1] + qnorm(0.975) * re$se)

# Also compute CI for probability difference if desired
re$lower_diff <- re$lower - re$prob_base
re$upper_diff <- re$upper - re$prob_base

# Add group labels
re$group <- rownames(re)

# Plot with CI
ggplot(re, aes(x = reorder(group, prob_diff), y = prob_diff)) +
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin = lower_diff, ymax = upper_diff), width = 0) +
  labs(
    x = "Group",
    y = "Predicted probability difference vs overall mean"
  ) + geom_hline(yintercept=0)+theme_bw()


## Residual Calculation 6 ----
# extract random intercepts from the mixed model
## Code for mean estimates
re <- ranef(model1)$stratum

# extract the fixed effect linear predictor contribution and save as a dataset
# with a row for each stratum and its prediction
grid <- unique(dt[c("stratum", "sexo", "edad_cat3", "clase_2", "urb_rur", "survey2")])
grid$lp_fixed <- predict(model1, newdata = grid, re.form = NA)
grid

# get the full predictor for each stratum by adding the random effect term *logit-scale
grid$lp_full <- grid$lp_fixed + re$"(Intercept)"

# transform to probability scale to get a) probability of the outcome according to just the 
# fixed effects and b) probability of outcome according to both the fixed effects and the 
# random effect for each stratum
grid$p_fixed <- invlogit(grid$lp_fixed)
grid$p_full <- invlogit(grid$lp_full)

# take the difference between the two probabilities a) and b) to answer: how much 
# does belonging to a specific stratum combination shift the probability beyond
# what the fixed effects predict?
grid$diff_p <- grid$p_full - grid$p_fixed
grid

## Code for 95% CI for random effects on the probability scale
# following method of plotREsim()
# simulate random effects on the logit scale using REsim
re_sim <- REsim(model1, n.sims = 1000) # how getting random errors before
names(re_sim)
re_sim <- re_sim %>%
  rename(stratum = groupID)

# construct lower and upper bounds on the log-odds scale
re_sim$lower <- re_sim$mean - 1.96 * re_sim$sd
re_sim$upper <- re_sim$mean + 1.96 * re_sim$sd

# then, make predictions for each stratum using the fixed effects as above
grid <- unique(dt[c("stratum", "sexo", "edad_cat3", "clase_2", "urb_rur", "survey2")])
grid$lp_fixed <- predict(model1, newdata = grid, re.form = NA)
grid

# make a dataset which has a row per strata and has columns for the simulated random error summary
# and a column for the fixed effect contribution
re_sim <- merge(re_sim, grid[, c("stratum", "lp_fixed")], by = "stratum")
re_sim

# get mean probability for each stata and CI by doing inverse logit of the sum of the
# fixed and random part 
# fixed part alone does not have a CI as we are conditioning on the fixed part
# asking: how much does the stratum deviate from the fixed effect prediction?
# therefore, use the fixed effect prediction without any uncertainty around it
re_sim$prob_mean <- invlogit(re_sim$lp_fixed + re_sim$mean)
re_sim$prob_lower <- invlogit(re_sim$lp_fixed + re_sim$lower)
re_sim$prob_upper <- invlogit(re_sim$lp_fixed + re_sim$upper)
re_sim$p_fixed <- invlogit(re_sim$lp_fixed)

# have the mean and CI for the probability of sedentarism in each strata
# but must take away the probability of sedentarism in each strata if there were 
# no additive effect to obtain a CI for the difference
re_sim$delta_mean <- re_sim$prob_mean - re_sim$p_fixed
re_sim$delta_lower <- re_sim$prob_lower - re_sim$p_fixed
re_sim$delta_upper <- re_sim$prob_upper - re_sim$p_fixed

re_sim

# significant strata
re_sim %>% filter(delta_lower>0)
re_sim %>% filter(delta_upper<0)

# plot differences
re_sim <- re_sim %>%
  arrange(delta_mean) %>%
  mutate(stratum = factor(stratum, levels = stratum))
ggplot(re_sim, aes(x = stratum, y = delta_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = delta_lower, ymax = delta_upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Intersectional stratum",
    y = "Predicted difference (delta)",
    title = "Caterpillar plot of stratum-level effects"
  ) +
  theme_minimal()

# new plot
re_sim <- re_sim %>%
  arrange(delta_mean) %>%
  mutate(rank = row_number())

top_y <- max(re_sim$delta_upper, na.rm = TRUE) + 0.01

ggplot(re_sim, aes(x = rank, y = delta_mean)) +
  
  # Stratum labels above the points
  geom_text(
    aes(x = rank, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  
  # Vertical gridlines for each stratum
  geom_vline(
    xintercept = seq(min(re_sim$rank), max(re_sim$rank), by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = delta_lower, ymax = delta_upper),
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
    subtitle = "Excess probability after accounting for additive fixed effects",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Excess probability (percentage points)",
    caption = "Stratum-level residual effects; 95% CI.\nRed line indicates no residual intersectional effect."
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

ggsave("Figures/MAIHDA/caterpillarplot_residualeffects_6.png", width = 4000, height = 2200, dpi=300, units = "px")
