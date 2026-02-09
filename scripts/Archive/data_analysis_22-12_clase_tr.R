## Author: Diana Juanita Mora
## Proyect: Childhood Inequities of Sedentarism
## Script: Data Analysis - RII and SII (clase_tr)
## Finalized: 28th of July 2025
## Edited: 29th of December 2025

## Load libraries
library(tidyverse)
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

## read joined data
dt <- get(load("joined_clean_6.RData"))

#### Regression Models for Inequality ####
#### 1. Relative Index of Inequality (RII) ####
# A summary measure of socioeconomic inequality in health which tells us how much
# more or less common sedentarism is across the whole social gradient - using class
# Rather than just comparing two groups, such as low and high class, the RII uses
# all levels of class ranked from most to least disadvantaged and takes into account
# the size of each group
# producing a single number that expresses the strength and direction of inequality
# while adjusting for other variables such as age and sex
#    RII = 1 : There is no inequality, where sedentarism is equally likely across all classes
#    RII > 1 : Sedentarism is more common in those of low social class
#    RII < 1 : Sedentarism is more common in those with high social class

## RIDIT Score 
# The cumulative proportion of the population below the midpoint of that category

dt <- dt %>%
  mutate(sedentarismo = ifelse(sedentarismo == "Yes", 1, 0))

# RII Overall
rii_sedentarism_overall_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    fit <- glm(sedentarismo ~ clase_tr + edad + sexo, data = .x, family = "poisson")
    coef <- tidy(fit) %>% filter(term == "clase_tr")
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate),
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      sexo = "Overall"
    )
  })
rii_sedentarism_overall_clase
clipr::write_clip(rii_sedentarism_overall_clase)
save(rii_sedentarism_overall_clase, file = "Datasets/rii_sedentarism_overall_clase.RData")

# RII Females
dt_females <- subset(dt, sexo == "Female")
rii_sedentarism_f_clase <- dt_females %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    fit <- glm(sedentarismo ~ clase_tr + edad, data = .x, family = "poisson")
    coef <- tidy(fit) %>% filter(term == "clase_tr")
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate),
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      sexo = "Girls"
    )
  })
rii_sedentarism_f_clase
clipr::write_clip(rii_sedentarism_f_clase)
save(rii_sedentarism_f_clase, file = "Datasets/rii_sedentarism_overall_clase_f.RData")

# RII Males
dt_males <- subset(dt, sexo == "Male")
rii_sedentarism_m_clase <- dt_males %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    fit <- glm(sedentarismo ~ clase_tr + edad, data = .x, family = "poisson")
    coef <- tidy(fit) %>% filter(term == "clase_tr")
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate),
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      sexo = "Boys"
    )
  })
clipr::write_clip(rii_sedentarism_m_clase)
save(rii_sedentarism_m_clase, file = "Datasets/rii_sedentarism_overall_clase_m.RData")

## Database for RII data by social class
rii_sedentarism_clase <- rii_sedentarism_overall_clase %>%
  rbind(rii_sedentarism_f_clase) %>%
  rbind(rii_sedentarism_m_clase) %>% 
  mutate(exp="RII Social Class") %>% 
  rename(est = rii, infci=rii_infci, supci=rii_supci, strata=sexo)
save(rii_sedentarism_clase, file = "Datasets/rii_sedentarism.RData")
clipr::write_clip(rii_sedentarism_clase)

#### 2. Slope Index of Inequality (SII) ####
# Tells us the absolute difference in prevalence of sedentarism between the
# highest and lowest levels of the social class ladder (higher SII higher inequality)
# Using a poisson with identity link: absolute risk difference, which tells us how 
# many percentage points difference there are across the social class spectrum
# i.e., how many more people per 100 are sedentary in the lowest social class 
# group compared to the highest?

sii_sedentarism_overall_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    # Fit model with identity link
    fit <- glm(
      sedentarismo ~ clase_tr + edad + sexo,
      data = .x,
      family = poisson(link = "identity"),
      start = c(1, 0, 0, 0)  # helps convergence
    )
    
    coef <- broom::tidy(fit) %>% filter(term == "clase_tr")
    
    tibble(
      encuesta = unique(.x$survey),
      sii = coef$estimate * 100,
      sii_infci = (coef$estimate - 1.96 * coef$std.error) * 100,
      sii_supci = (coef$estimate + 1.96 * coef$std.error) * 100,
      risk_factor = "Sedentarism",
      sexo = "Overall"
    )
  })
sii_sedentarism_overall_clase
save(sii_sedentarism_overall_clase, file = "Datasets/sii_sedentarism.RData")
clipr::write_clip(sii_sedentarism_overall_clase)

# SII Females
sii_sedentarism_f_clase <- dt_females %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    # Fit model with identity link
    fit <- glm(
      sedentarismo ~ clase_tr + edad,
      data = .x,
      family = poisson(link = "identity"),
      start = c(1, 0, 0) 
    )
    
    coef <- broom::tidy(fit) %>% filter(term == "clase_tr")
    
    tibble(
      encuesta = unique(.x$survey),
      sii = coef$estimate * 100,
      sii_infci = (coef$estimate - 1.96 * coef$std.error) * 100,
      sii_supci = (coef$estimate + 1.96 * coef$std.error) * 100,
      risk_factor = "Sedentarism",
      sexo = "Girls"
    )
  })
save(sii_sedentarism_f_clase, file = "Datasets/sii_sedentarism_f_clase.RData")
clipr::write_clip(sii_sedentarism_f_clase)

# SII Males
sii_sedentarism_m_clase <- dt_males %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    # Fit model with identity link
    fit <- glm(
      sedentarismo ~ clase_tr + edad,
      data = .x,
      family = poisson(link = "identity"),
      start = c(1, 0, 0) 
    )
    
    coef <- broom::tidy(fit) %>% filter(term == "clase_tr")
    
    tibble(
      encuesta = unique(.x$survey),
      sii = coef$estimate * 100,
      sii_infci = (coef$estimate - 1.96 * coef$std.error) * 100,
      sii_supci = (coef$estimate + 1.96 * coef$std.error) * 100,
      risk_factor = "Sedentarism",
      sexo = "Boys"
    )
  })
save(sii_sedentarism_m_clase, file = "Datasets/sii_sedentarism_m_clase.RData")
clipr::write_clip(sii_sedentarism_m_clase)

# Database for SII by social class
sii_sedentarism_clase <- sii_sedentarism_overall_clase %>%
  rbind(sii_sedentarism_f_clase) %>%
  rbind(sii_sedentarism_m_clase) %>%
  mutate(exp="SII Social Class") %>% 
  rename(est = sii, infci = sii_infci, supci = sii_supci, strata = sexo)
sii_sedentarism_clase
clipr::write_clip(sii_sedentarism_clase)
save(sii_sedentarism_clase, file = "Datasets/sii_sedentarism_clase.RData")

#### 3. Difference in sedentarism by sex, adjusted by age ####
sex_sedentarism_all <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    # Fit Poisson model with log link
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson # adjusted relative risk of sedentarism in females vs males
    ) # chat says if want proper CIs with robust standard errors have to use library(sandwich) coeftest(fit, vcov = sandwich)
      # this is because the model assumes the variance = mean, which is not true for the binary data... the variance is p(1-p)
      # if leave as is, the RR point estimate is fine but the SE will be underestimated therefore the CIs will be too narrow and the p-values too optimistic
      # means might conclude something is statistically significant when it isn't 
      # the sandwich package replaces the variance assumption with a robust estimate of the variance 
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), # keep same names for binding later, but is a risk ratio, not a relative index of inequality
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Overall"
    )
  })

sex_sedentarism_all <- sex_sedentarism_all %>% 
  mutate(exp = "Sex Inequality") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
save(sex_sedentarism_all, file = "Datasets/RR/sex_sedentarism_all.RData")
clipr::write_clip(sex_sedentarism_all)

#### 4. Difference in sedentarism by sex, by social class, adjusted by age ####
dt_clase_I <- subset(dt, clase == "Class I")

## Social Class I
sexo_sedentarism_clase_I <- dt_clase_I %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), # risk ratio of female vs male
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class I"
    )
  })
sexo_sedentarism_clase_I

# Social Class II
dt_clase_II <- subset(dt, clase == "Class II")

sexo_sedentarism_clase_II <- dt_clase_II %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class II"
    )
  })
sexo_sedentarism_clase_II

# Social Class III
dt_clase_III <- subset(dt, clase == "Class III")

sexo_sedentarism_clase_III <- dt_clase_III %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class III"
    )
  })
sexo_sedentarism_clase_III

# Social Class IV
dt_clase_IV <- subset(dt, clase == "Class IV")

sexo_sedentarism_clase_IV <- dt_clase_IV %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class IV"
    )
  })
sexo_sedentarism_clase_IV

# Social Class V
dt_clase_V <- subset(dt, clase == "Class V")

sexo_sedentarism_clase_V <- dt_clase_V %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class V"
    )
  })
sexo_sedentarism_clase_V

# Social Class VI
dt_clase_VI <- subset(dt, clase == "Class VI")

sexo_sedentarism_clase_VI <- dt_clase_VI %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Class VI"
    )
  })

sexo_sedentarism_clase_VI

## Database for sedentarism by sex and class
sex_sedentarism_class <- sexo_sedentarism_clase_I %>% 
  rbind(sexo_sedentarism_clase_II) %>% 
  rbind(sexo_sedentarism_clase_III) %>% 
  rbind(sexo_sedentarism_clase_IV) %>% 
  rbind(sexo_sedentarism_clase_V) %>% 
  rbind(sexo_sedentarism_clase_VI) %>% 
  mutate(exp = "Sex Inequality by Social Class") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
sex_sedentarism_class
clipr::write_clip(sex_sedentarism_class)
save(sex_sedentarism_class, file = "Datasets/RR/sex_sedentarism_class.RData")

#### 4B. Difference in sedentarism by sex, by Social Class 3 levels, adjusted by age ####
dt_clase_I <- subset(dt, clase_3 == "Class I")

## Social Class I
sexo_sedentarism_clase_I <- dt_clase_I %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), # risk ratio of female vs male
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "3-Class I"
    )
  })
sexo_sedentarism_clase_I

# Social Class II
dt_clase_II <- subset(dt, clase_3 == "Class II")

sexo_sedentarism_clase_II <- dt_clase_II %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "3-Class II"
    )
  })
sexo_sedentarism_clase_II

# Social Class III
dt_clase_III <- subset(dt, clase_3 == "Class III")

sexo_sedentarism_clase_III <- dt_clase_III %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0")))
    
    # Get tidy output and replace std.error with robust version
    coef_table <- tidy(fit)
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "3-Class III"
    )
  })
sexo_sedentarism_clase_III

## Database for sedentarism by sex and class
sex_sedentarism_class_3 <- sexo_sedentarism_clase_I %>% 
  rbind(sexo_sedentarism_clase_II) %>% 
  rbind(sexo_sedentarism_clase_III) %>% 
  mutate(exp = "Sex Inequality by Social Class") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
sex_sedentarism_class_3
clipr::write_clip(sex_sedentarism_class_3)
save(sex_sedentarism_class_3, file = "Datasets/RR/sex_sedentarism_class_3.RData")

#### 5. Difference in sedentarism by sex, by geographic region, adjusted by age ####
# Urban
dt_urb <- subset(dt, urb_rur == "Urban")

sexo_sedentarism_urb <- dt_urb %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors 
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0"))) 
    
    # Get tidy output and replace std.error with robust version 
    coef_table <- tidy(fit) 
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Urban"
    )
  })

sexo_sedentarism_urb

# Semi-urban
dt_semi <- subset(dt, urb_rur == "Semi-urban")

sexo_sedentarism_semi <- dt_semi %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors 
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0"))) 
    
    # Get tidy output and replace std.error with robust version 
    coef_table <- tidy(fit) 
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Semi-urban"
    )
  })

# Rural
dt_rur <- subset(dt, urb_rur == "Rural")

sexo_sedentarism_rur <- dt_rur %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors 
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0"))) 
    
    # Get tidy output and replace std.error with robust version 
    coef_table <- tidy(fit) 
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Rural"
    )
  })

sex_sedentarism_geo <- sexo_sedentarism_urb %>% 
  rbind(sexo_sedentarism_semi) %>% 
  rbind(sexo_sedentarism_rur) %>% 
  mutate(exp = "Sex Inequality by Geographic Region") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
sex_sedentarism_geo
save(sex_sedentarism_geo, file = "Datasets/RR/sex_sedentarism_geo.RData")
clipr::write_clip(sex_sedentarism_geo)

#### 6. Difference in sedentarism by sex, by nationality, adjusted by age ####
# Spanish
dt_spanish <- subset(dt, nacionalidad == "Spanish")

sexo_sedentarism_spanish <- dt_spanish %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors 
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0"))) 
    
    # Get tidy output and replace std.error with robust version 
    coef_table <- tidy(fit) 
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Spanish"
    )
  })
sexo_sedentarism_spanish

# Foreign
dt_foreign <- subset(dt, nacionalidad == "Foreign")

sexo_sedentarism_foreign <- dt_foreign %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~{
    fit <- glm(
      sedentarismo ~ sexo + edad,
      data = .x,
      family = poisson
    )
    
    # Robust (sandwich) standard errors 
    robust_se <- sqrt(diag(vcovHC(fit, type = "HC0"))) 
    
    # Get tidy output and replace std.error with robust version 
    coef_table <- tidy(fit) 
    coef_table$std.error <- robust_se
    
    # Extract coefficient for sex
    coef <- broom::tidy(fit) %>% filter(term == "sexoFemale")
    
    tibble(
      encuesta = unique(.x$survey),
      rii = exp(coef$estimate), 
      rii_infci = exp(coef$estimate - 1.96 * coef$std.error),
      rii_supci = exp(coef$estimate + 1.96 * coef$std.error),
      risk_factor = "Sedentarism",
      strata = "Foreign"
    )
  })
sexo_sedentarism_foreign

sex_sedentarism_nationality <- sexo_sedentarism_spanish %>% 
  rbind(sexo_sedentarism_foreign) %>% 
  mutate(exp = "Sex Inequality by Nationality") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
save(sex_sedentarism_nationality, file = "Datasets/RR/sex_sedentarism_nationality.RData")
clipr::write_clip(sex_sedentarism_nationality)

#### Databases of RII, SII, Sex Inequality ####
inequalities_sedentarism <- bind_rows(
  rii_sedentarism_clase %>% mutate(exp = "RII Social Class"),
  rbind(sii_sedentarism_clase) %>% mutate(exp = "SII Social Class"),
  rbind(sex_sedentarism_all) %>% mutate(exp = "Sex Inequality Adjusted by Age"),
  rbind(sex_sedentarism_class) %>% mutate(exp = "Sex Inequality stratified by Social Class Adjusted by Age"),
  rbind(sex_sedentarism_class_3) %>% mutate(exp = "Sex Inequality stratified by Social Class 3 Levels Adjusted by Age"),
  rbind(sex_sedentarism_geo) %>% mutate(exp = "Sex Inequality stratified by Region Adjusted by Age"),
  rbind(sex_sedentarism_nationality) %>% mutate(exp = "Sex Inequality stratified by Nationality Adjusted by Age"))
print(inequalities_sedentarism, n = 90)
  
## database of all regression
save(inequalities_sedentarism, file = "Datasets/inequalities_sedentarism.RData")

#### Visualization of RII in Sedentarism ####
load("Datasets/inequalities_sedentarism.RData")

# Define theme()
theme_inequalities <- function() {
  theme_minimal(base_size = 13) +
    theme(
      # 🔹 Force white background everywhere
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Gridlines
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      # Text
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      
      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 11),
      
      # Facets
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold")
    )
}

## Order variable levels for RII and SII
inequalities_sedentarism$strata <- factor(inequalities_sedentarism$strata, 
                                          levels = c("Overall", "Girls", "Boys",
                                                     "Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI",
                                                     "3-Class I", "3-Class II", "3-Class III", 
                                                     "Urban", "Semi-urban", "Rural",
                                                     "Spanish", "Foreign"))
inequalities_sedentarism$exp <- factor(inequalities_sedentarism$exp, 
                                         levels = c("RII Social Class", "SII Social Class", "Sex Inequality Adjusted by Age",
                                                    "Sex Inequality stratified by Social Class Adjusted by Age",
                                                    "Sex Inequality stratified by Social Class 3 Levels Adjusted by Age",
                                                    "Sex Inequality stratified by Region Adjusted by Age",
                                                    "Sex Inequality stratified by Nationality Adjusted by Age"))
# RII Social Class per Sex
inequalities_sedentarism$encuesta <- as.numeric(as.character(inequalities_sedentarism$encuesta))

fig_rii_class <- inequalities_sedentarism %>%
  filter(exp == "RII Social Class") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  # geom_text_repel(
  #   aes(label = round(est, 2)),
  #   size = 5,         # Adjust for readability
  #   nudge_y = 0.5,
  #   force = 2,
  #   max.overlaps = Inf,
  #   show.legend =  FALSE,
  #   fontface = "bold",
  #   color = "black"
  # ) +
  labs(
    title = "Relative Index of Inequality by Sex",
    x = NULL,
    y = "RII (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 2.5, 5.0, 10.0),
    limits = c(0.75, 12)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  theme_inequalities()
fig_rii_class
ggsave("Figures/17-12/fig_rii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

## new rii figure
fig_rii_class_sex <- inequalities_sedentarism %>%
  filter(exp == "RII Social Class") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    nudge_x = 1,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    title = "Relative Index of Inequality by Sex and Overall",
    x = NULL,
    y = "RII (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(1.0, 5.0, 10),
    limits = c(0.75, 11)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) + 
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_inequalities() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )
fig_rii_class_sex
ggsave("Figures/17-12/fig_rii_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization of SII in Sedentarism ####
# SII Social Class by Sex
fig_sii_class <- inequalities_sedentarism %>%
  filter(exp == "SII Social Class") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, lty=2, color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  labs(
    title = "Slope Index of Inequality by Sex",
    x = NULL,
    y = "SII (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    limits = c(-10, 30)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  theme_inequalities()
fig_sii_class
ggsave("Figures/17-12/fig_sii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

# SII Social Class by Sex
## new sii figure
fig_sii_class_sex <- inequalities_sedentarism %>%
  filter(exp == "SII Social Class") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    title = "Slope Index of Inequality by Sex and Overall",
    x = NULL,
    y = "RII (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    breaks = c(-5.0, 1.0, 5.0, 10, 15, 20, 25),
    limits = c(-0.75, 25)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) + 
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_inequalities() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )
fig_sii_class_sex
ggsave("Figures/17-12/fig_sii_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

# Original Figure - 3 Figures
fig_sii_separate_class <- inequalities_sedentarism %>% 
  filter(exp == "SII Social Class") %>% 
  ggplot(aes(x = encuesta, y = est, ymin = infci, ymax = supci, fill = strata, color = strata)) +
  geom_hline(yintercept = 1, lty=2, color = "gray40") +
  geom_ribbon(alpha = 0.25) +
  geom_line(linewidth = 1.2) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  scale_y_continuous(
    limits = c(-10, 30)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  labs(
    x = NULL,
    y = "SII (95% CI)",
    title = "Slope Index of Inequality by Sex") +
  scale_color_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "dimgray",
      "Boys" = "#f03b20",
      "Girls" = "#2c7fb8"
    )
  ) +
  theme_inequalities()
fig_sii_separate_class
ggsave("Figures/17-12/fig_sii_separate_class.png", width = 4000, height = 2200, dpi=300, units = "px")

#### RII and SII Figures Together ####
fig_sii_rii <- inequalities_sedentarism %>% 
  filter(exp == "SII Social Class"| exp == "RII Social Class") %>% 
  ggplot(aes(x = encuesta, y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, lty=2, color = "gray40") +
  geom_ribbon(alpha = 0.25, aes(fill=strata)) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(strata), rows = vars(exp), scales = "free_y") +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  labs(
    x = NULL,
    y = "Inequality (95% CI)",
  title = "RII and SII by Sex") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_inequalities()
fig_sii_rii
ggsave("Figures/17-12/fig_sii_rii.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization 3 + 4. ####
# Relative Risk Difference by Sex
fig_sex_class <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    title = "Sex Inequality Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 2.5),
    limits = c(0.75, 3)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "lightblue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "lightblue"
    )
  ) +
  theme_inequalities()
fig_sex_class
ggsave("Figures/17-12/RR/fig_sex_class.png", width = 4000, height = 2200, dpi=300, units = "px")

# Relative Risk Difference by Sex per Social Class
fig_rr_class <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Social Class Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1) +
  labs(
    title = "Sex Inequality Stratified by Social Class Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Household Social Class",
    fill = "Household Social Class"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2),
    limits = c(0.65, 3.5)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3",  # purple
      "Class IV" = "#e7298a",   # pink
      "Class V" = "#66a61e",    # lime green
      "Class VI" = "#e6ab02"   # mustard yellow
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3",  # purple
      "Class IV" = "#e7298a",   # pink
      "Class V" = "#66a61e",    # lime green
      "Class VI" = "#e6ab02"   # mustard yellow
    )
  ) +
  theme_inequalities()
fig_rr_class

# Relative Risk Difference by Sex per Social Class - Separate Figures
fig_rr_separate_class <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Social Class Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  labs(
    title = "Sex Inequality Stratified by Social Class Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Household Social Class",
    fill = "Household Social Class"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.35, 1, 1.5, 2),
    limits = c(0.35, 7)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3",  # purple
      "Class IV" = "#e7298a",   # pink
      "Class V" = "#66a61e",    # lime green
      "Class VI" = "#e6ab02"   # mustard yellow
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3",  # purple
      "Class IV" = "#e7298a",   # pink
      "Class V" = "#66a61e",    # lime green
      "Class VI" = "#e6ab02"   # mustard yellow
    )
  ) +
  theme_inequalities()
fig_rr_separate_class
ggsave("Figures/17-12/RR/fig_rr_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

# Relative Risk Difference by Sex per Social Class 3 Levels - Separate Figures
fig_rr_separate_class_3 <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Social Class 3 Levels Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  labs(
    title = "Sex Inequality Stratified by Social Class Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Household Social Class",
    fill = "Household Social Class"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 3),
    limits = c(0.7, 4)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3"  # purple
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",    # teal green
      "Class II" = "#d95f02",   # orange
      "Class III" = "#7570b3"  # purple
    )
  ) +
  theme_inequalities()
fig_rr_separate_class_3
ggsave("Figures/17-12/RR/fig_rr_class_sex_3.png", width = 4000, height = 2200, dpi=300, units = "px")

## Graph for Relative Rate of Change - Tasa Relativa de Variacion
## Overall, Girls, Boys
rii_data <- tribble(
  ~sex, ~year, ~RII,
  "Girls", 2003, 1.75,
  "Girls", 2006, 1.84,
  "Girls", 2011, 1.78,
  "Girls", 2017, 3.21,
  "Girls", 2023, 5.44,
  "Boys", 2003, 2.03,
  "Boys", 2006, 1.47,
  "Boys", 2011, 1.73,
  "Boys", 2017, 2.39,
  "Boys", 2023, 4.63,
  "Overall", 2003, 1.85,
  "Overall", 2006, 1.68,
  "Overall", 2011, 1.75,
  "Overall", 2017, 2.87,
  "Overall", 2023, 5.01
)

rrc_data <- rii_data %>%
  group_by(sex) %>%
  arrange(year) %>%
  mutate(
    RRC = (RII - dplyr::lag(RII)) / dplyr::lag(RII) * 100
  ) %>%
  filter(!is.na(RRC))

rrc_2003_2023 <- rii_data %>%
  filter(year %in% c(2003, 2023)) %>%
  group_by(sex) %>%
  arrange(year) %>%
  summarise(
    RII_2003 = first(RII),
    RII_2023 = last(RII),
    RRC_2003_2023 = (RII_2023 - RII_2003) / RII_2003 * 100,
    .groups = "drop"
    )
    
rrc_2003_2023 %>%
  mutate(
    RRC_label = paste0(round(RRC_2003_2023, 2), "%")
  )

## RRC graph
ggplot(rrc_data, aes(x = factor(year), y = RRC, color = sex, group = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Relative Rate Change (RRC) of RII in Sedentarism",
    subtitle = "By Sex and Survey Year",
    x = "Survey Year",
    y = "RRC (%)",
    color = "Sex"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_viridis_d(option = "Paired") +
  scale_fill_viridis_d(option = "Paired") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )
ggsave("Figures/fig_relative rate change.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization 5. ####
# Relative Risk Difference by Sex per Region
fig_rr_region <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Region Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  labs(
    title = "Sex Inequality Stratified by Region Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Geographic Region",
    fill = "Geographic Region"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2),
    limits = c(0.65, 3.5)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Urban" = "#1b9e77", 
      "Semi-urban" = "#d95f02", 
      "Rural" =  "#66a61e"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Urban" = "#1b9e77", 
      "Semi-urban" = "#d95f02", 
      "Rural" =  "#66a61e"
    )
  ) +
  theme_inequalities()
fig_rr_region
ggsave("Figures/fig_rr_region.png", width = 4000, height = 2200, dpi=300, units = "px")

# Relative Risk Difference by Sex per Social Class - Separate Figures
fig_rr_separate_region <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Region Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  labs(
    title = "Sex Inequality Stratified by Region Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Geographic Region",
    fill = "Geographic Region"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 4, 6, 8, 10),
    limits = c(0.65, 10)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Urban" = "#1b9e77", 
      "Semi-urban" = "#d95f02", 
      "Rural" =  "#66a61e"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Urban" = "#1b9e77", 
      "Semi-urban" = "#d95f02", 
      "Rural" =  "#66a61e"
    )
  ) +
  theme_inequalities()
fig_rr_separate_region
ggsave("Figures/17-12/RR/fig_rr_separate_region.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization 6. ####
# Relative Risk Difference by Sex per Nationality
fig_rr_nationality <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Nationality Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  labs(
    title = "Sex Inequality Stratified by Nationality Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Nationality",
    fill = "Nationality"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2),
    limits = c(0.75, 6)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  scale_color_manual(
    values = c(
      "Spanish" = "#1b9e77", 
      "Foreign" = "#d95f02"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Spanish" = "#1b9e77", 
      "Foreign" = "#d95f02"
    )
  ) +
  theme_inequalities()
fig_rr_nationality
ggsave("Figures/17-12/RR/fig_rr_nationality.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Relative Risk Visualization 3-6 ----
forest_data <- inequalities_sedentarism %>% 
  mutate(
    encuesta = as.factor(encuesta),
    strata = factor(strata, levels = unique(strata))
  )

forest_data

fp_filtered <- forest_data %>% 
  slice(31:n())
fp_filtered

# Forest plot
ggplot(fp_filtered, aes(x = est, y = strata)) +
  geom_point() +
  geom_errorbarh(aes(xmin = infci, xmax = supci), height = 0.2) +
  facet_wrap(~ encuesta, ncol = 5) +
  scale_x_log10() +  # optional: log scale for better visualization
  geom_vline(xintercept = 1, linetype = "dashed") +  # reference line for no difference
  labs(
    x = "Risk Ratio (Female vs Male)",
    y = "Category",
    title = "Sex Differences in Sedentarism by Category and Survey Year"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggplot(fp_filtered, aes(x = est, y = strata)) +
  geom_point(size = 3, aes(color = strata)) +  # Increased point size and color
  geom_errorbarh(aes(xmin = infci, xmax = supci, color = strata), height = 0.2) + 
  facet_wrap(~ encuesta, ncol = 5) +
  scale_x_log10() +  # Log-scale for clearer separation
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.7) +  
  labs(
    x = "Risk Ratio (Female vs Male)",
    y = "Category",
    title = "Sex Differences in Sedentarism by Category and Survey Year",
    subtitle = "Risk ratios adjusted for age"
  ) +
  geom_text(
    aes(label = round(est, 2)),      # Add RR label rounded to 2 decimals
    hjust = -0.2,                     # Slightly to the right of the point
    size = 3,
    nudge_y = 0.25,
    nudge_x = -0.05
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  theme(
    strip.background = element_rect(fill = "lightgray", colour = NA),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", colour = "black"),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"  # Hide legend if not needed
  )

ggsave(
  filename = paste0("Figures/17-12/RR/sex_differences_forest.png"),
  width = 4000, height = 2500, dpi = 300, units = "px"
)

#### RII in Sedentarism by CCAA, Survey and Sex with clase_tr (numeric based on 6 categories of class) ####
# This model estimates the association between social class and sedentarism for each year, while:
  # Adjusting for age (edad) and sex (sexo)
  # Accounting for random variation across regions (ccaa)
  # Allowing the effect of social class to vary by region (i.e., random slopes and intercepts)

## CCAA list
ccaas <- read_delim("Resources/ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

## New age variable
dt <- dt %>% 
  mutate(edad_original = edad)

## Center Age
  # So that the model's intercept represents the expected value of the outcome at the mean
  # age of the sample, rather than when age = 0, which also improves model fitting and convergence
dt <- dt %>% 
  mutate(edad = scale(edad, center = T, scale = F))

## Create function to extract values from model (RECALL CHANGE SEXO FOR EACH MODEL OUTPUT)
extract_rii_by_group_CCAA <- function(
    model,
    ccaa_ref,
    outcome_label = "Sedentarism",
    effect_name = "clase_tr_4",
    random_group = "survey:ccaa"
) {
  
  ## Extract fixed-effect estimate and SE
  fe <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  ## Extract random effects 
  re_list <- ranef(model)$cond
  
  if (!random_group %in% names(re_list)) {
    stop(paste("Random effect", random_group, "not found in model"))
  }
  
  re_df <- as.data.frame(re_list[[random_group]]) %>%
    tibble::rownames_to_column("group")
  
  ## Combine fixed + random effects
  re_df <- re_df %>%
    mutate(
      linear_pred = fe + .data[[effect_name]],
      rii = exp(linear_pred),
      rii_infci = exp(linear_pred - 1.96 * fe_se),
      rii_supci = exp(linear_pred + 1.96 * fe_se)
    )
  
  ## Split survey and NUTS1
  re_df <- re_df %>%
    tidyr::separate(
      group,
      into = c("survey", "ccaa"),
      sep = ":",
      remove = TRUE
    ) %>%
    mutate(
      survey = as.integer(survey),
      sex = "Overall",
      Outcome = outcome_label
    )
  
  ## Final tidy output
  out <- re_df %>%
    select(
      survey,
      ccaa,
      rii,
      rii_infci,
      rii_supci,
      sex,
      Outcome
    ) %>%
    arrange(survey, ccaa)
  
  return(out)
}


# Overall
table_ccaa <- dt %>%
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_4 = min(table(clase_tr_4)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_4 < 15)
table_ccaa
clipr::write_clip(table_ccaa)

rii_sedentarism_CCAA <- glmmTMB(sedentarismo~clase_tr_4+edad+sexo+
                                  (1+clase_tr_4|survey) + # allows the baseline level of sedentarism and the effect of social class to vary between survey years
                                  (1+clase_tr_4|survey:ccaa), # allows the same variations to differ between autonomous communities within each survey year
                                data = dt,
                                 family = "poisson", weights = factor2) # generalized linear mixed model
VarCorr(rii_sedentarism_CCAA)
# In (1 + clase_tr | survey) → the “1” means that each survey year (2003, 2006, 2011, 2017, 2023) gets its own baseline level of sedentarism.
# So, 2003 can start at a higher or lower average sedentarism than 2006, etc.
# This is the random intercept by survey.

# In (1 + clase_tr | survey:ccaa) → the “1” here means that each autonomous community (CCAA) within each survey year can also have its own baseline.
# For example, Madrid in 2003 may start with lower sedentarism than Andalucía in 2003.
# This is the random intercept by region within survey.

# The “+ clase_tr” part in both random-effect terms means that the effect (slope) of social class (clase_tr) is allowed to vary:
# Between survey years (so inequality may strengthen or weaken over time), and
# Between regions within each survey year (so some regions may have higher or lower inequality than others).

# The random slopes say that the relationship between social class and sedentarism is not fixed — it can change by time and place.

rii_sedentarism_CCAA <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_CCAA
clipr::write_clip(rii_sedentarism_CCAA)
save(rii_sedentarism_CCAA, file = "Datasets/rii_sedentarism_CCAA.RData")

## wide table overall
rii_sedentarism_CCAA_wide <- rii_sedentarism_CCAA %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

rii_sedentarism_CCAA_wide
clipr::write_clip(rii_sedentarism_CCAA_wide)
summary(dt$clase_tr)

## Females
table_ccaa_f <- dt %>%
  filter(sexo == "Female") %>% 
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_4 = min(table(clase_tr_4)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_4 < 15)
clipr::write_clip(table_ccaa_f)

rii_sedentarism_CCAA_females <- glmmTMB(sedentarismo~clase_tr_4+edad+(1+clase_tr_4|survey) 
                                        + (1+clase_tr_4|survey:ccaa),  data = subset(dt, sexo == "Female"),
                                family="poisson", weights = factor2)
VarCorr(rii_sedentarism_CCAA_females)

rii_sedentarism_CCAA_females_t <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA_females,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_females_t

## wide table females
rii_sedentarism_CCAA_females_wide <- rii_sedentarism_CCAA_females_t %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

rii_sedentarism_CCAA_females_wide

## NOTE THAT COLUMNS ARE NOT IN ORDER
clipr::write_clip(rii_sedentarism_CCAA_females_wide)

## Males
table_ccaa_m <- dt %>%
  filter(sexo == "Male") %>% 
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(clase_tr < 15)
clipr::write_clip(table_ccaa_m)

rii_sedentarism_CCAA_males <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) 
                                      + (1+clase_tr|survey:ccaa), data = subset(dt, sexo == "Male"),
                                        family="poisson", weights = factor2) # generalized linear mixed model
VarCorr(rii_sedentarism_CCAA_males)

rii_sedentarism_CCAA_males_t <- extract_rii_by_group(
  model = rii_sedentarism_CCAA_males,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_males_t
clipr::write_clip(rii_sedentarism_CCAA_males_t)

## wide table males
rii_sedentarism_CCAA_males_wide <- rii_sedentarism_CCAA_males_t %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

## NOTE TABLE COLUMNS NOT IN RIGHT ORDER
rii_sedentarism_CCAA_males_wide
clipr::write_clip(rii_sedentarism_CCAA_males_wide)

## combine results in one dataframe
rii_sedentarism_CCAA_combined <- rii_sedentarism_CCAA %>% 
  rbind(rii_sedentarism_CCAA_females_t) %>% 
  rbind(rii_sedentarism_CCAA_males_t)

rii_sedentarism_CCAA_combined

rii_sedentarism_CCAA_combined <- rii_sedentarism_CCAA_combined %>%
  mutate(sex = case_when(
    sex == "Females" ~ "Girls",
    sex == "Males" ~ "Boys",
    TRUE ~ sex  # keep any other values as they are
  ))
save(rii_sedentarism_CCAA_combined, file = "Datasets/rii_sedentarism_CCAA_combined.RData")

## Relative Rate of Change of sedentarism overtime per CCAA
rii_change <- rii_sedentarism_CCAA_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023 <- rii_change$change_2003_2023
rii_change_2003_2023
clipr::write_clip(rii_change_2003_2023)

rii_change <- rii_sedentarism_CCAA_females_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023_f <- rii_change$change_2003_2023
rii_change_2003_2023_f
clipr::write_clip(rii_change_2003_2023_f)

rii_change <- rii_sedentarism_CCAA_males_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023_m <- rii_change$change_2003_2023
rii_change_2003_2023_m
clipr::write_clip(rii_change_2003_2023_m)

#### RII in Sedentarism by CCAA, Survey and Sex with clase_tr_4 (numeric based on 3 categories of class) ----
## Create function to extract values from model (RECALL CHANGE SEXO FOR EACH MODEL OUTPUT)
extract_rii_by_group_CCAA <- function(
    model,
    ccaa_ref,
    outcome_label = "Sedentarism",
    effect_name = "clase_tr_4",
    random_group = "survey:ccaa"
) {
  
  ## Extract fixed-effect estimate and SE
  fe <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  ## Extract random effects 
  re_list <- ranef(model)$cond
  
  if (!random_group %in% names(re_list)) {
    stop(paste("Random effect", random_group, "not found in model"))
  }
  
  re_df <- as.data.frame(re_list[[random_group]]) %>%
    tibble::rownames_to_column("group")
  
  ## Combine fixed + random effects
  re_df <- re_df %>%
    mutate(
      linear_pred = fe + .data[[effect_name]],
      rii = exp(linear_pred),
      rii_infci = exp(linear_pred - 1.96 * fe_se),
      rii_supci = exp(linear_pred + 1.96 * fe_se)
    )
  
  ## Split survey and NUTS1
  re_df <- re_df %>%
    tidyr::separate(
      group,
      into = c("survey", "ccaa"),
      sep = ":",
      remove = TRUE
    ) %>%
    mutate(
      survey = as.integer(survey),
      sex = "Overall",
      Outcome = outcome_label
    )
  
  ## Final tidy output
  out <- re_df %>%
    select(
      survey,
      ccaa,
      rii,
      rii_infci,
      rii_supci,
      sex,
      Outcome
    ) %>%
    arrange(survey, ccaa)
  
  return(out)
}


# Overall
table_ccaa <- dt %>%
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_4 = min(table(clase_tr_4)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_4 < 15)
table_ccaa
clipr::write_clip(table_ccaa)

rii_sedentarism_CCAA <- glmmTMB(sedentarismo~clase_tr_4+edad+sexo+
                                  (1+clase_tr_4|survey) + # allows the baseline level of sedentarism and the effect of social class to vary between survey years
                                  (1+clase_tr_4|survey:ccaa), # allows the same variations to differ between autonomous communities within each survey year
                                data = dt,
                                family = "poisson", weights = factor2) # generalized linear mixed model
VarCorr(rii_sedentarism_CCAA)

rii_sedentarism_CCAA <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_CCAA

## Females
table_ccaa_f <- dt %>%
  filter(sexo == "Female") %>% 
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_4 = min(table(clase_tr_4)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_4 < 15)
clipr::write_clip(table_ccaa_f)

rii_sedentarism_CCAA_females <- glmmTMB(sedentarismo~clase_tr_4+edad+(1+clase_tr_4|survey) 
                                        + (1+clase_tr_4|survey:ccaa),  data = subset(dt, sexo == "Female"),
                                        family="poisson", weights = factor2)
VarCorr(rii_sedentarism_CCAA_females)

rii_sedentarism_CCAA_females_t <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA_females,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_females_t

## Males
table_ccaa_m <- dt %>%
  filter(sexo == "Male") %>% 
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(clase_tr < 15)
clipr::write_clip(table_ccaa_m)

rii_sedentarism_CCAA_males <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) 
                                      + (1+clase_tr|survey:ccaa), data = subset(dt, sexo == "Male"),
                                      family="poisson", weights = factor2) # generalized linear mixed model
VarCorr(rii_sedentarism_CCAA_males)

rii_sedentarism_CCAA_males_t <- extract_rii_by_group(
  model = rii_sedentarism_CCAA_males,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_males_t

#### Visualization RII by CCAA ####
fig_CCAA_multilineal <- ggplot(rii_sedentarism_CCAA, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = ccaa)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_ribbon(alpha = 0.3, fill = "#CC0033") +
  geom_line(color = "#CC0033", linewidth = 1) +
  geom_point(color = "#CC0033", size = 2) +   # <-- add points here
  facet_wrap(vars(ccaa), ncol = 3, scales = "free_y") +
  scale_y_continuous(trans = "log",
                     breaks = c(0.75, 1, 1.5, 2, 4, 8, 16, 32)) +
  labs(x = "", y = "RII (95% CI)") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
fig_CCAA_multilineal
ggsave("Figures/fig_rii_ccaa.png", width = 4000, height = 2200, dpi=300, units = "px")

## new figure of RII by CCAA
fig_CCAA_multilineal <- ggplot(rii_sedentarism_CCAA, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = ccaa)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray90") +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  geom_text(
    aes(label = sprintf("%.2f", rii)),
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  facet_wrap(vars(ccaa), ncol = 3, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 4, 8, 16, 32),
    labels = c("0.75", "1", "1.5", "2", "4", "8", "16", "32")
  ) +
  scale_color_viridis_d(option = "plasma")  +
  scale_fill_viridis_d(option = "plasma")  +
  labs(
    title = "Relative Index of Inequality (RII) in Sedentarism by Autonomous Community and Year",
    x = "",
    y = "RII (95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title = element_text(color = "black", size = 10, face = "bold"),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  )
fig_CCAA_multilineal 
ggsave("Figures/17-12/fig_rii_ccaa.png", width = 4000, height = 2200, dpi=300, units = "px")

## Overall, Boys and Girls
fig_CCAA_combined <- ggplot(
  rii_sedentarism_CCAA_combined,
  aes(x = as.factor(survey), y = rii, ymin = rii_infci, ymax = rii_supci,
      group = sex, color = sex, fill = sex)
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = rii_infci, ymax = rii_supci), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Add RII text labels
  geom_text_repel(aes(label = sprintf("%.2f", rii)),
                  size = 3, show.legend = FALSE, max.overlaps = 10) +
  facet_wrap(~ccaa, ncol = 3, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 4, 8, 16, 32),
    labels = c("0.75", "1", "1.5", "2", "4", "8", "16", "32")
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "RII (95% CI, log scale)",
    title = "Relative Index of Inequality (RII) in Sedentarism by CCAA, Sex, and Survey Year"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
fig_CCAA_combined
ggsave("Figures/17-12/fig_rii_ccaa_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

## CCAA Map ####
#Comunidades Autónomas Mapa RII Sedentarismo#

ccaa_mainland <- st_read("Resources/lineas_limite/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp") # Leemos los datos de capa
ccaa_canary <- st_read("Resources/lineas_limite/SHP_REGCAN95/recintos_autonomicas_inspire_canarias_regcan95/recintos_autonomicas_inspire_canarias_regcan95.shp") # Leemos los datos de capa
ccaa_mainland <- st_transform(ccaa_mainland, 25830)
st_crs(ccaa_mainland)
ccaa_canary <- st_transform(ccaa_canary, 25830)
st_crs(ccaa_canary)
data_ccaa <- rbind(ccaa_mainland, ccaa_canary)

## explore shapefile data
names(data_ccaa)
unique(data_ccaa$NAMEUNIT)
table(data_ccaa$NAMEUNIT)

names(rii_sedentarism_CCAA_combined)
unique(rii_sedentarism_CCAA_combined$ccaa)
unique(data_ccaa$NAMEUNIT)

data_ccaa <- data_ccaa %>%
  filter(NAMEUNIT != "Territorios no asociados a ninguna autonomía")

ccaa_crosswalk <- tibble::tribble(
  ~NAMEUNIT,                                      ~ccaa_en,
  "Andalucía",                                   "Andalusia",
  "Aragón",                                      "Aragon",
  "Principado de Asturias",                      "Asturias",
  "Illes Balears",                               "Balearic Islands",
  "Canarias",                                    "Canary Islands",
  "Cantabria",                                   "Cantabria",
  "Castilla y León",                             "Castile and Leon",
  "Castilla-La Mancha",                          "Castilla-La Mancha",
  "Cataluña/Catalunya",                          "Catalonia",
  "Comunitat Valenciana",                        "Valencian Community",
  "Extremadura",                                 "Extremadura",
  "Galicia",                                     "Galicia",
  "Comunidad de Madrid",                         "Madrid",
  "Región de Murcia",                            "Murcia",
  "Comunidad Foral de Navarra",                  "Navarre",
  "País Vasco/Euskadi",                          "Basque Country",
  "La Rioja",                                    "La Rioja",
  "Ciudad Autónoma de Ceuta",                    "Ceuta and Melilla",
  "Ciudad Autónoma de Melilla",                  "Ceuta and Melilla"
)

data_ccaa <- data_ccaa %>%
  left_join(ccaa_crosswalk, by = "NAMEUNIT")

data_ccaa %>%
  st_drop_geometry() %>%
  count(ccaa_en)
View(data_ccaa)

data_ccaa <- data_ccaa %>% ## merging ceuta and melilla to one geometry
  group_by(ccaa_en) %>%
  summarise(across(where(is.numeric), first),
            geometry = st_union(geometry),
            .groups = "drop")

## join RII database and shapefile
map_ccaa <- data_ccaa %>%
  left_join(rii_sedentarism_CCAA_combined,
            by = c("ccaa_en" = "ccaa"))
View(map_ccaa)

## plot map
theme_map <- function(bg_color = "white", title_size = 16){
  theme(
    panel.background = element_rect(fill = bg_color),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
}

ggplot(map_ccaa) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2)

rii_map_survey <- ggplot(
  map_ccaa %>% dplyr::filter(sex == "Male") # note change to Overall, Female, Male for diff maps
  ) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Boys: Inequalities in Childhood Sedentarism by Autonomous Community per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        fill = "Relative Index of Inequality") +
  theme_map()

rii_map_survey

ggsave(
  filename = "Figures/clase_tr/rii_map_survey_male.png",
  plot = rii_map_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

### Explore RII values ####
ccaa_survey_table <- dt %>%
  group_by(ccaa, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ccaa_survey_table
clipr::write_clip(ccaa_survey_table)

ccaa_survey_table_f <- dt_females %>%
  group_by(ccaa, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(ccaa_survey_table_f)
print(ccaa_survey_table_f, n=90)

ccaa_survey_table_m <- dt_males %>%
  group_by(ccaa, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(ccaa_survey_table_m)
print(ccaa_survey_table_m, n=90)

## NUTS1
NUTS1_survey_table <- dt %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
NUTS1_survey_table
clipr::write_clip(NUTS1_survey_table)

dt_males <- subset(dt, sexo == "Female")
NUTS1_survey_table_f <- dt_females %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(NUTS1_survey_table_f)

dt_males <- subset(dt, sexo == "Male")
NUTS1_survey_table_m <- dt_males %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr, na.rm = TRUE),
    p25_clase = quantile(clase_tr, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(NUTS1_survey_table_m)
print(NUTS1_survey_table_m, n=90)

# flagging small cell counts
table(dt$survey, dt$ccaa, dt$sexo)
dt %>%
  count(ccaa, sexo, survey) %>%
  mutate(flag_small = n < 30) %>% 
  print(n=180)

dt %>%
  group_by(survey, ccaa, sexo) %>%
  count(clase_tr) %>%
  mutate(flag_small = n < 10) %>% 
  arrange(flag_small, survey, ccaa, sexo, clase_tr) %>% 
  print(n=1068)

hist(dt$clase_tr)

dt %>%
  group_by(survey, ccaa, sexo) %>%
  count(clase_tr) %>%               
  mutate(flag_sparse = n < 10) %>%
  group_by(survey) %>% 
  summarise(n_sparse_cells = sum(flag_sparse), .groups = "drop")

dt %>%
  count(survey, ccaa, sexo, clase_tr) %>%
  ggplot(aes(x = clase_tr, y = n, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_grid(ccaa ~ survey)

sparse_strata <- dt %>%
  group_by(survey, ccaa) %>%
  summarise(min_n = min(table(clase_tr)), total_n = n(), .groups = "drop") %>%
  filter(min_n < 15) %>% 
  print(n=51)

sparse_strata <- dt %>%
  group_by(survey, ccaa, sexo) %>%
  summarise(min_n = min(table(clase_tr)), total_n = n(), .groups = "drop") %>%
  filter(min_n < 10) %>% 
  filter(sexo == "Female") %>% 
  print(n=64)

sparse_strata <- dt %>%
  group_by(survey, ccaa, sexo) %>%
  summarise(min_n = min(table(clase_tr)), total_n = n(), .groups = "drop") %>%
  filter(min_n < 10) %>% 
  filter(sexo == "Male") %>% 
  print(n=128)

#### RII in Sedentarism by NUTs1, Survey and Sex (Multi-level) ####
dt <- dt %>%
  mutate(
    NUTS1 = case_when(
      # Noroeste
      ccaa %in% c("Galicia", "Asturias", "Cantabria") ~ "North-West",
      # Noreste
      ccaa %in% c("Basque Country", "Navarre", "La Rioja", "Aragon") ~ "North-East",
      # Madrid
      ccaa == "Madrid" ~ "Madrid",
      # Centre
      ccaa %in% c("Castile and Leon", "Castilla-La Mancha", "Extremadura") ~ "Centre",
      # East
      ccaa %in% c("Catalonia", "Valencian Community", "Balearic Islands") ~ "East",
      # South
      ccaa %in% c("Andalusia", "Murcia", "Ceuta and Melilla") ~ "South",
      # Canary Islands
      ccaa == "Canary Islands" ~ "Canary Islands",
      TRUE ~ NA_character_
    ),
    NUTS1 = factor(NUTS1)
  )

extract_rii_by_group_NUTS1 <- function(
    model,
    NUTS1_ref,
    outcome_label = "Sedentarism",
    effect_name = "clase_tr",
    random_group = "survey:NUTS1"
) {
  
  ## Extract fixed-effect estimate and SE
  fe <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  ## Extract random effects 
  re_list <- ranef(model)$cond
  
  if (!random_group %in% names(re_list)) {
    stop(paste("Random effect", random_group, "not found in model"))
  }
  
  re_df <- as.data.frame(re_list[[random_group]]) %>%
    tibble::rownames_to_column("group")
  
  ## Combine fixed + random effects
  re_df <- re_df %>%
    mutate(
      linear_pred = fe + .data[[effect_name]],
      rii = exp(linear_pred),
      rii_infci = exp(linear_pred - 1.96 * fe_se),
      rii_supci = exp(linear_pred + 1.96 * fe_se)
    )
  
  ## Split survey and NUTS1
  re_df <- re_df %>%
    tidyr::separate(
      group,
      into = c("survey", "NUTS1"),
      sep = ":",
      remove = TRUE
    ) %>%
    mutate(
      survey = as.integer(survey),
      sex = "Females",
      Outcome = outcome_label
    )
  
  ## Final tidy output
  out <- re_df %>%
    select(
      survey,
      NUTS1,
      rii,
      rii_infci,
      rii_supci,
      sex,
      Outcome
    ) %>%
    arrange(survey, NUTS1)
  
  return(out)
}

# Overall
table_NUTS <- dt %>%
  group_by(survey, NUTS1) %>%
  summarise(
    clase_tr = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(clase_tr < 15)
clipr::write_clip(table_NUTS)

rii_sedentarism_NUTS1 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+
                                  (1+clase_tr|survey) + 
                                  (1+clase_tr|survey:NUTS1), 
                                data = dt,
                                family = "poisson", weights = factor2)
rii_sedentarism_NUTS1
VarCorr(rii_sedentarism_NUTS1)

rii_sedentarism_NUTS1 <- extract_rii_by_group_NUTS1(
  model = rii_sedentarism_NUTS1,
  NUTS1_ref = NUTS1,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1
clipr::write_clip(rii_sedentarism_NUTS1)
save(rii_sedentarism_NUTS1, file = "Datasets/rii_sedentarism_NUTS1.RData")

## wide table overall
rii_sedentarism_NUTS1_wide <- rii_sedentarism_NUTS1 %>%
  select(NUTS1 = NUTS1, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

rii_sedentarism_NUTS1_wide
clipr::write_clip(rii_sedentarism_NUTS1_wide)

## Girls
table_NUTS_f <- dt %>%
  filter(sexo == "Female") %>%
  group_by(survey, NUTS1) %>%
  summarise(
    clase_tr = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(clase_tr < 15)
clipr::write_clip(table_NUTS_f)

rii_sedentarism_NUTS1_females <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) 
                                        + (1+clase_tr|survey:NUTS1),  
                                        data = subset(dt, sexo == "Female"),
                                        family="poisson", weights = factor2)
VarCorr(rii_sedentarism_NUTS1_females)

rii_sedentarism_NUTS1_females <- extract_rii_by_group_NUTS1(
  model = rii_sedentarism_NUTS1_females,
  NUTS1_ref = NUTS1,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1_females
clipr::write_clip(rii_sedentarism_NUTS1_females)

## wide table females
rii_sedentarism_NUTS1_females_wide <- rii_sedentarism_NUTS1_females %>%
  select(NUTS1 = NUTS1, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )
rii_sedentarism_NUTS1_females_wide

## NOTE THAT COLUMNS ARE NOT IN ORDER
clipr::write_clip(rii_sedentarism_NUTS1_females_wide)

## Boys
table_NUTS_m <- dt %>%
  filter(sexo == "Male") %>% 
  group_by(survey, NUTS1) %>%
  summarise(
    clase_tr = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(clase_tr < 15)
clipr::write_clip(table_NUTS_m)

rii_sedentarism_NUTS1_males <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) 
                                       + (1+clase_tr|survey:NUTS1), 
                                       data = subset(dt, sexo == "Male"),
                                      family="poisson", weights = factor2) 
VarCorr(rii_sedentarism_NUTS1_males)

rii_sedentarism_NUTS1_males <- extract_rii_by_group_NUTS1(
  model = rii_sedentarism_NUTS1_males,
  NUTS1_ref = NUTS1,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1_males
clipr::write_clip(rii_sedentarism_NUTS1_males)

## wide table males
rii_sedentarism_NUTS1_males_wide <- rii_sedentarism_NUTS1_males %>%
  select(NUTS1 = NUTS1, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

## NOTE TABLE COLUMNS NOT IN RIGHT ORDER
rii_sedentarism_NUTS1_males_wide
clipr::write_clip(rii_sedentarism_NUTS1_males_wide)

## Combined dataframe of RII NUTS1 Overall and by Sex
rii_sedentarism_NUTS1_combined <- rii_sedentarism_NUTS1 %>% 
  rbind(rii_sedentarism_NUTS1_females) %>% 
  rbind(rii_sedentarism_NUTS1_males)

rii_sedentarism_NUTS1_combined
rii_sedentarism_NUTS1_combined <- rii_sedentarism_NUTS1_combined %>%
  mutate(sex = case_when(
    sex == "Females" ~ "Girls",
    sex == "Males" ~ "Boys",
    TRUE ~ sex  # keep any other values as they are
  ))
save(rii_sedentarism_NUTS1_combined, file = "Datasets/rii_sedentarism_NUTS1_combined.RData")

## Relative Rate of Change of sedentarism overtime per CCAA
rii_change <- rii_sedentarism_NUTS1_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023 <- rii_change$change_2003_2023
clipr::write_clip(rii_change_2003_2023)

rii_change <- rii_sedentarism_NUTS1_females_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023_f <- rii_change$change_2003_2023
clipr::write_clip(rii_change_2003_2023_f)

rii_change <- rii_sedentarism_NUTS1_males_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change_2003_2023_m <- rii_change$change_2003_2023
rii_change_2003_2023_m
clipr::write_clip(rii_change_2003_2023_m)

#### Visualization RII by NUTS1 ####
fig_NUTS1_multilineal <- ggplot(rii_sedentarism_NUTS1, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = NUTS1)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_ribbon(alpha = 0.3, fill = "#CC0033") +
  geom_line(color = "#CC0033", linewidth = 1) +
  geom_point(color = "#CC0033", size = 2) +   # <-- add points here
  facet_wrap(vars(NUTS1), ncol = 4, scales = "free_y") +
  scale_y_continuous(trans = "log",
                     breaks = c(0.75, 1, 1.5, 2, 4, 8)) +
  labs(x = "", y = "RII (95% CI)") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
fig_NUTS1_multilineal
ggsave("Figures/fig_rii_NUTS1.png", width = 4000, height = 2200, dpi=300, units = "px")

## new figure of RII by NUTS1
fig_NUTS1_multilineal <- ggplot(rii_sedentarism_NUTS1, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = NUTS1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray90") +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  geom_text(
    aes(label = sprintf("%.2f", rii)),
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  facet_wrap(vars(NUTS1), ncol = 2, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 4, 8),
    labels = c("0.75", "1", "1.5", "2", "4", "8")
  ) +
  scale_color_viridis_d(option = "plasma")  +
  scale_fill_viridis_d(option = "plasma")  +
  labs(
    title = "Relative Index of Inequality (RII) in Sedentarism by NUTS 1",
    x = "",
    y = "RII (95% CI)",
    caption = "Nomenclature of Territorial Units for Statistics (NUTS 1)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title = element_text(color = "black", size = 10, face = "bold"),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  )
fig_NUTS1_multilineal 
ggsave("Figures/17-12/fig_rii_NUTS1.png", width = 4000, height = 2200, dpi=300, units = "px")

## Overall, Boys and Girls
fig_NUTS1_combined <- ggplot(
  rii_sedentarism_NUTS1_combined,
  aes(x = as.factor(survey), y = rii, ymin = rii_infci, ymax = rii_supci,
      group = sex, color = sex, fill = sex)
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = rii_infci, ymax = rii_supci), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Add RII text labels
  # geom_text_repel(
  #   data = subset(rii_sedentarism_NUTS1_combined, survey == max(survey)),
  #   aes(label = sprintf("%.2f", rii)),
  #   size = 3,
  #   show.legend = FALSE
  # ) +
  facet_wrap(~NUTS1, ncol = 3, scales = "free_y", axes = "all_x") +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 2, 4, 8),
    labels = c("0.75", "1", "2", "4", "8")
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Social Inequalities in Sedentarism Across Spain",
    subtitle = "Relative Index of Inequality (RII) by NUTS 1 region, sex, and survey year",
    x = "Survey Year",
    y = "RII (log scale, 95% CI)",
    caption = paste(
      "Nomenclature of Territorial Units for Statistics (NUTS) Regions",
      "\nCanary Islands: Canary Islands;",
      "\nCentre: Castile and Leon, Castilla-La Mancha, Extremadura;",
      "\nEast: Catalonia, Valencian Community, Balearic Islands;",
      "\nMadrid: Madrid;",
      "\nNorth-East: Basque Country, Navarre, La Rioja, Aragon;",
      "\nNorth-West: Galicia, Asturias, Cantabria;",
      "\nSouth: Andalusia, Murcia, Ceuta and Melilla."
    )) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = NA)
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  ) +
  # theme(
  #   legend.position = "top",
  #   legend.title = element_blank(),
  #   axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1),
  #   axis.text.y = element_text(color = "black", size = 10),
  #   axis.title = element_text(color = "black", size = 10),
  #   strip.text = element_text(color = "black", size = 11, face = "bold"),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  # ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.2, "cm")
  )
fig_NUTS1_combined
ggsave("Figures/17-12/fig_rii_NUTS1_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

## NUTS1 Map ####
NUTS1 <- st_read("Resources/NUTS1_ES_20M_2024_3035.shp") # Leemos los datos de capa

## explore shapefile data
names(NUTS1)
unique(NUTS1$NAME_LATN)
names(rii_sedentarism_NUTS1_combined)
unique(rii_sedentarism_NUTS1_combined$NUTS1)

NUTS1_crosswalk <- tibble::tribble(
  ~NAME_LATN,                                   ~NUTS1_ENG,
  "Noroeste",                                   "North-West",
  "Noreste",                                    "North-East",
  "Comunidad de Madrid",                        "Madrid",
  "Centro (ES)",                                "Centre",
  "Este",                                       "East",
  "Sur",                                        "South",
  "Canarias",                                   "Canary Islands"
)
NUTS1_crosswalk

NUTS1_join <- NUTS1 %>%
  left_join(NUTS1_crosswalk, by = "NAME_LATN")
View(NUTS1_join)

NUTS1_join %>%
  st_drop_geometry() %>%
  count(NUTS1_ENG)

## join RII database and shapefile
map_NUTS1 <- NUTS1_join %>%
  left_join(rii_sedentarism_NUTS1_combined,
            by = c("NUTS1_ENG" = "NUTS1"))

## plot map
theme_map <- function(bg_color = "white", title_size = 16){
  theme(
    panel.background = element_rect(fill = bg_color),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
}

ggplot(map_NUTS1) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2)

rii_map_survey <- ggplot(
  map_NUTS1 %>% dplyr::filter(sex == "Boys") # note change to Overall, Girls, Boys for diff maps
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Boys: Inequalities in Childhood Sedentarism by NUTS per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        caption = "Nomenclature of Territorial Units for Statistics (NUTS) Regions",
        fill = "Relative Index of Inequality") +
  theme_map()

rii_map_survey

ggsave(
  filename = "Figures/clase_tr/rii_map_NUTS_survey_male.png",
  plot = rii_map_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

#### OLD: RII in Sedentarism by NUTs1, Survey, Autonomous Community and Sex (Multi-level) ####
extract_rii_multilevel <- function(
    model,
    outcome_label = "Sedentarism",
    effect_name = "clase_tr"
) {
  
  ## 1. Fixed effect
  fe <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  ## 2. Random effects
  re <- ranef(model)$cond
  
  ## Survey level
  re_survey <- as.data.frame(re$survey) |>
    tibble::rownames_to_column("survey") |>
    dplyr::select(survey, slope_survey = !!effect_name)
  
  ## Survey:NUTS1 level
  re_nuts1 <- as.data.frame(re$`survey:NUTS1`) |>
    tibble::rownames_to_column("survey_NUTS1") |>
    tidyr::separate(survey_NUTS1, into = c("survey", "NUTS1"), sep = ":") |>
    dplyr::select(survey, NUTS1, slope_nuts1 = !!effect_name)
  
  ## Survey:NUTS1:CCAA level
  re_ccaa <- as.data.frame(re$`survey:NUTS1:ccaa`) |>
    tibble::rownames_to_column("survey_NUTS1_ccaa") |>
    tidyr::separate(
      survey_NUTS1_ccaa,
      into = c("survey", "NUTS1", "ccaa"),
      sep = ":"
    ) |>
    dplyr::select(survey, NUTS1, ccaa, slope_ccaa = !!effect_name)
  
  ## 3. Combine all slope components
  out <- re_ccaa |>
    dplyr::left_join(re_nuts1, by = c("survey", "NUTS1")) |>
    dplyr::left_join(re_survey, by = "survey") |>
    dplyr::mutate(
      linear_pred =
        fe +
        slope_survey +
        slope_nuts1 +
        slope_ccaa,
      rii = exp(linear_pred),
      rii_infci = exp(linear_pred - 1.96 * fe_se),
      rii_supci = exp(linear_pred + 1.96 * fe_se),
      Outcome = outcome_label,
      sex = "Female",
      survey = as.integer(survey)
    ) |>
    dplyr::select(
      survey, NUTS1, ccaa,
      rii, rii_infci, rii_supci,
      sex, Outcome
    ) |>
    dplyr::arrange(survey, NUTS1, ccaa)
  
  return(out)
}

rii_sedentarism_NUTS1_ccaa <- glmmTMB(
  sedentarismo ~ clase_tr + edad + sexo +
    (1 + clase_tr | survey) +
    (1 + clase_tr | survey:NUTS1) +
    (1 + clase_tr | survey:NUTS1:ccaa),
  data = dt,
  family = poisson,
  weights = factor2
)

rii_sedentarism_NUTS1_ccaa

rii_sedentarism_NUTS1_ccaa <- extract_rii_multilevel(
  model = rii_sedentarism_NUTS1_ccaa,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1_ccaa
clipr::write_clip(rii_sedentarism_NUTS1_ccaa)

VarCorr(rii_sedentarism_NUTS1_ccaa)

## Girls
rii_sedentarism_NUTS1_ccaa_f <- glmmTMB(
  sedentarismo ~ clase_tr + edad +
    (1 + clase_tr | survey) +
    (1 + clase_tr | survey:NUTS1) +
    (1 + clase_tr | survey:NUTS1:ccaa),
  data = subset(dt, sexo == "Female"),
  family = poisson,
  weights = factor2
)

rii_sedentarism_NUTS1_ccaa_f

rii_sedentarism_NUTS1_ccaa_f <- extract_rii_multilevel(
  model = rii_sedentarism_NUTS1_ccaa_f,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1_ccaa_f
clipr::write_clip(rii_sedentarism_NUTS1_ccaa_f)

summary(rii_sedentarism_NUTS1_ccaa_f)
summary(rii_sedentarism_NUTS1_ccaa_f)$coefficients$cond

dt %>%
  filter(sexo == "Female") %>%
  group_by(survey, NUTS1, ccaa) %>%
  summarise(
    n_classes = n_distinct(clase_tr),
    min_n = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(n_classes < 3 | min_n < 5) %>% 
  print(n = 33)

fixef(rii_sedentarism_NUTS1_ccaa_f)$cond
VarCorr(rii_sedentarism_NUTS1_ccaa_f)

## Boys
rii_sedentarism_NUTS1_ccaa_m <- glmmTMB(
  sedentarismo ~ clase_tr + edad +
    (1 + clase_tr | survey) +
    (1 + clase_tr | survey:NUTS1) +
    (1 + clase_tr | survey:NUTS1:ccaa),
  data = subset(dt, sexo == "Male"),
  family = poisson,
  weights = factor2
)

summary(rii_sedentarism_NUTS1_ccaa_m)$coefficients$cond

rii_sedentarism_NUTS1_ccaa_m

rii_sedentarism_NUTS1_ccaa_m <- extract_rii_multilevel(
  model = rii_sedentarism_NUTS1_ccaa_m,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_NUTS1_ccaa_m
clipr::write_clip(rii_sedentarism_NUTS1_ccaa_m)

dt %>%
  filter(sexo == "Male") %>%
  group_by(survey, NUTS1, ccaa) %>%
  summarise(
    n_classes = n_distinct(clase_tr),
    min_n = min(table(clase_tr)),
    .groups = "drop"
  ) %>%
  filter(n_classes < 3 | min_n < 5) %>% 
  print(n = 33)

VarCorr(rii_sedentarism_NUTS1_ccaa_m)
