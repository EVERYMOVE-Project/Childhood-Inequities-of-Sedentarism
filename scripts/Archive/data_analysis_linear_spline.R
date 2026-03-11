## Author: Diana Juanita Mora
## Proyect: Childhood Inequities of Sedentarism
## Script: Data Analysis - RII and SII (clase_tr_2) spline modelling
## Edited: 6th of february 2026

## Load libraries
library(tidyverse)
library(MASS)
library(tidyr)
library(dplyr)
library(broom)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)
library(glmmTMB)
library(nlme)
library(lme4)
library(ggplot2)
library(clipr)
library(sandwich)
library(lmtest)
library(tibble)
library(ggrepel)
library(sf) #map
library(splines)
library(mgcv)

dt <- get(load("joined_clean_rii.RData"))

## RII assumes linearity where the risk difference or log-risk ratio changes linearly across the social rank. 
## To determine linearity, plot the proportion sedentary vs the midpoint rank.
## To determine linearity, fit a generalized additive model with a smooth term for x and test 
##  with a likelihood ratio test comparing GAM and a spline model, if there is significant difference
##  linearity is not plausible
dt <- dt %>%
  mutate(sedentarismo_num = ifelse(sedentarismo == "Yes", 1, 0))

dt_ranks <- dt %>%
  group_by(clase_tr_2) %>% ## 
  summarise(
    n = n(),  # total in class
    prop_sedentarismo = mean(sedentarismo_num, na.rm = TRUE)  # proportion "Yes"
  )

ggplot(dt_ranks, aes(x = clase_tr_2, y = prop_sedentarismo)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +   # linear fit
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "red") + # spline/GAM
  labs(
    x = "Midpoint rank (cumulative SEP)",
    y = "Proportion sedentary",
    title = "Linearity check of f(x) for RII/SII"
  ) +
  theme_minimal()

gam_fit <- gam(sedentarismo_num ~ s(clase_tr_2), family = poisson, data = dt)
summary(gam_fit) # EDF of 7.594 meaning effective degrees of freedom, greater than 1 is non-linear
plot(gam_fit)

rii_sedentarism_overall_spline <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson regression with cubic spline on clase_tr_2 (rank)
    fit <- glm(sedentarismo_num ~ ns(clase_tr_2, df = 4) + edad + sexo, 
               data = .x, family = poisson(link = "log"))
    
    # Predict log risk at bottom (0) and top (1) of social hierarchy
    pred_bottom <- predict(fit, 
                           newdata = data.frame(
                             clase_tr_2 = 0,
                             edad = mean(.x$edad, na.rm = TRUE),
                             sexo = "Male"
                           ), type = "link")
    
    pred_top <- predict(fit, 
                        newdata = data.frame(
                          clase_tr_2 = 1,
                          edad = mean(.x$edad, na.rm = TRUE),
                          sexo = "Male"
                        ), type = "link")
    
    # Calculate RII (relative risk)
    rii <- exp(pred_top - pred_bottom)
    
    # Approximate CI using delta method
    se_bottom <- predict(fit, 
                         newdata = data.frame(
                           clase_tr_2 = 0,
                           edad = mean(.x$edad, na.rm = TRUE),
                           sexo = "Male"
                         ), type = "link", se.fit = TRUE)$se.fit
    
    se_top <- predict(fit, 
                      newdata = data.frame(
                        clase_tr_2 = 1,
                        edad = mean(.x$edad, na.rm = TRUE),
                        sexo = "Male"
                      ), type = "link", se.fit = TRUE)$se.fit
    
    rii_log_se <- sqrt(se_bottom^2 + se_top^2)
    rii_infci <- exp(log(rii) - 1.96 * rii_log_se)
    rii_supci <- exp(log(rii) + 1.96 * rii_log_se)
    
    tibble(
      encuesta = unique(.x$survey),
      rii = rii,
      rii_infci = rii_infci,
      rii_supci = rii_supci,
      risk_factor = "Sedentarism",
      sexo = "Overall"
    )
  })

rii_sedentarism_overall_spline # new values of rii, different compared to not using spline functions

