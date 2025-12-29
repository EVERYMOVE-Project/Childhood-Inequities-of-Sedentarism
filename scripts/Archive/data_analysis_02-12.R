# Data Analysis - RII and SII

## Load libraries
library(tidyverse)
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

## read joined data
dt <- get(load("joined_clean.RData"))

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
save(rii_sedentarism_overall_clase, file = "rii_sedentarism_overall_clase.RData")

# encuesta   rii rii_infci rii_supci risk_factor sexo   
# 1 2003      1.51      1.31      1.75 Sedentarism Overall # people in the lowest social class had a 51% higher prevalence of sedentarism
# 2 2006      1.50      1.28      1.76 Sedentarism Overall
# 3 2011      1.41      1.18      1.68 Sedentarism Overall
# 4 2017      1.70      1.43      2.02 Sedentarism Overall
# 5 2023      2.21      1.70      2.88 Sedentarism Overall # sharp increase this year, twice the risk of sedentarism

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
rii_sedentarism_m_clase
clipr::write_clip(rii_sedentarism_m_clase)

## Database for RII data by social class
rii_sedentarism_clase <- rii_sedentarism_overall_clase %>%
  rbind(rii_sedentarism_f_clase) %>%
  rbind(rii_sedentarism_m_clase) %>% 
  mutate(exp="RII Social Class") %>% 
  rename(est = rii, infci=rii_infci, supci=rii_supci, strata=sexo)
rii_sedentarism_clase
save(rii_sedentarism_clase, file = "rii_sedentarism.RData")
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
clipr::write_clip(sii_sedentarism_overall_clase)

# encuesta   sii sii_infci sii_supci risk_factor sexo   
# 1 2003     12.5       8.18     16.8  Sedentarism Overall # absolute difference in sedent between lowest and highest class is 12.5 percent points and statistically significant
# 2 2006      5.49      2.61      8.37 Sedentarism Overall # inequality decreased in 2006 as gap dropped by 5.5
# 3 2011      6.40      2.51     10.3  Sedentarism Overall # Moderate and stable inequality
# 4 2017     10.5       6.86     14.1  Sedentarism Overall # inequality rose again
# 5 2023     10.9       7.26     14.6  Sedentarism Overall # stable inequality

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
sii_sedentarism_f_clase
# females consistently show higher social class inequality in sedentarism than males, in 2003, sedentarism was 14.7 percentage points higher in the lowest vs highest class 

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
sii_sedentarism_m_clase
# there is a gendered dimension to social inequality in sedentary behaviour

# Database for SII by social class
sii_sedentarism_clase <- sii_sedentarism_overall_clase %>%
  rbind(sii_sedentarism_f_clase) %>%
  rbind(sii_sedentarism_m_clase) %>%
  mutate(exp="SII Social Class") %>% 
  rename(est = sii, infci = sii_infci, supci = sii_supci, strata = sexo)
sii_sedentarism_clase
clipr::write_clip(sii_sedentarism_clase)

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

sex_sedentarism_all

sex_sedentarism_all <- sex_sedentarism_all %>% 
  mutate(exp = "Sex Inequality") %>% 
  rename(est = rii, infci = rii_infci, supci = rii_supci, strata = strata)
sex_sedentarism_all

clipr::write_clip(sex_sedentarism_all)

# encuesta   est infci supci risk_factor strata  exp   
# 1 2003      1.30  1.19  1.41 Sedentarism Overall Sex Inequality # females had 30% higher risk of sedentarism compared to males
# 2 2006      1.24  1.13  1.36 Sedentarism Overall Sex Inequality
# 3 2011      1.34  1.20  1.49 Sedentarism Overall Sex Inequality
# 4 2017      1.28  1.15  1.42 Sedentarism Overall Sex Inequality
# 5 2023      1.24  1.06  1.44 Sedentarism Overall Sex Inequality

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

clipr::write_clip(sex_sedentarism_nationality)

#### Databases of RII, SII, Sex Inequality ####
inequalities_sedentarism <- bind_rows(
  rii_sedentarism_clase %>% mutate(exp = "RII Social Class"),
  rbind(sii_sedentarism_clase) %>% mutate(exp = "SII Social Class"),
  rbind(sex_sedentarism_all) %>% mutate(exp = "Sex Inequality Adjusted by Age"),
  rbind(sex_sedentarism_class) %>% mutate(exp = "Sex Inequality stratified by Social Class Adjusted by Age"),
  rbind(sex_sedentarism_geo) %>% mutate(exp = "Sex Inequality stratified by Region Adjusted by Age"),
  rbind(sex_sedentarism_nationality) %>% mutate(exp = "Sex Inequality stratified by Nationality Adjusted by Age"))
print(inequalities_sedentarism, n = 90)
  
## database of all regression
save(inequalities_sedentarism, file = "inequalities_sedentarism.RData")

#### Visualization of RII in Sedentarism ####
load("inequalities_sedentarism.RData")

# Define theme()
theme_inequalities <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 11),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

## Order variable levels for RII and SII
inequalities_sedentarism$strata <- factor(inequalities_sedentarism$strata, 
                                          levels = c("Overall", "Girls", "Boys",
                                                     "Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI",
                                                     "Urban", "Semi-urban", "Rural",
                                                     "Spanish", "Foreign"))
inequalities_sedentarism$exp <- factor(inequalities_sedentarism$exp, 
                                         levels = c("RII Social Class", "SII Social Class", "Sex Inequality Adjusted by Age",
                                                    "Sex Inequality stratified by Social Class Adjusted by Age",
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
  labs(
    title = "Relative Index of Inequality by Sex",
    x = NULL,
    y = "RII (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 2.5, 3.0, 3.5),
    limits = c(0.75, 4)
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

## new rii figure
fig_rii_class <- inequalities_sedentarism %>%
  filter(exp == "RII Social Class") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,                # vertical adjustment (move slightly above points)
    size = 5,
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
    breaks = c(0.75, 1, 1.5, 2, 2.5, 3.0, 3.5),
    limits = c(0.75, 4)
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
fig_rii_class
ggsave("Figures/fig_rii_class-Oct22.png", width = 4000, height = 2200, dpi=300, units = "px")

# RII Social Class by Sex - 3 Figures
fig_rii_separate_class <- inequalities_sedentarism %>% 
  filter(exp == "RII Social Class") %>% 
  ggplot(aes(x = encuesta, y = est, ymin = infci, ymax = supci, fill = strata, color = strata)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(alpha = 0.25) +
  geom_line(linewidth = 1.2) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2, 2.5, 3.0, 3.5),
    limits = c(0.75, 4.0)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  labs(
    x = NULL,
    y = "RII (95% CI)",
    title = "Relative Index of Inequality by Sex") +
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
fig_rii_separate_class
ggsave("Figures/fig_rii_separate_class.png", width = 4000, height = 2200, dpi=300, units = "px")

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

ggsave("Figures/fig_sii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

# SII Social Class by Sex - 3 Figures
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
ggsave("Figures/fig_sii_separate_class.png", width = 4000, height = 2200, dpi=300, units = "px")

#### RII and SII Figures Together ####
fig_sii_rii <- inequalities_sedentarism %>% 
  filter(exp == "SII Social Class"| exp == "RII Social Class") %>% 
  ggplot(aes(x = encuesta, y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, lty=2, color = "gray40") +
  geom_ribbon(alpha = 0.25, aes(fill=strata)) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  facet_grid(cols = vars(strata), rows = vars(exp), scales = "free_y") +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) +
  labs(
    x = NULL,
    y = "Inequality (95% CI)",
  title = "RII and SII by Sex") +
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
fig_sii_rii
ggsave("Figures/fig_sii_rii.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization 3 + 4. ####
# Relative Risk Difference by Sex
fig_sex_class <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  labs(
    title = "Sex Inequality Adjusted by Age",
    x = NULL,
    y = "Adjusted Relative Risk (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.75, 1, 1.5, 2),
    limits = c(0.75, 2.5)
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
ggsave("Figures/fig_sex_class.png", width = 4000, height = 2200, dpi=300, units = "px")

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

ggsave("Figures/fig_rii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

# Relative Risk Difference by Sex per Social Class - Separate Figures
fig_rr_separate_class <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Social Class Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1) +
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
fig_rr_separate_class

## Graph for Relative Rate of Change - Tasa Relativa de Variacion
## Overall, Girls, Boys
rii_data <- tribble(
  ~sex, ~year, ~RII,
  "Girls", 2003, 1.46,
  "Girls", 2006, 1.49,
  "Girls", 2011, 1.35,
  "Girls", 2017, 2.00,
  "Girls", 2023, 1.92,
  "Boys", 2003, 1.53,
  "Boys", 2006, 1.49,
  "Boys", 2011, 1.47,
  "Boys", 2017, 1.44,
  "Boys", 2023, 2.70,
  "Overall", 2003, 1.51,
  "Overall", 2006, 1.50,
  "Overall", 2011, 1.41,
  "Overall", 2017, 1.70,
  "Overall", 2023, 2.21
)

rrc_data <- rii_data %>%
  group_by(sex) %>%
  arrange(year) %>%
  mutate(
    RRC = (RII - lag(RII)) / lag(RII) * 100
  ) %>%
  filter(!is.na(RRC))

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
fig_rr_separate_region
ggsave("Figures/fig_rr_separate_region.png", width = 4000, height = 2200, dpi=300, units = "px")

#### Visualization 6. ####
# Relative Risk Difference by Sex per Region
fig_rr_nationality <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Nationality Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1.2) +
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
    limits = c(0.75, 3.5)
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
ggsave("Figures/fig_rr_nationality.png", width = 4000, height = 2200, dpi=300, units = "px")

# Relative Risk Difference by Sex per Social Class - Separate Figures
fig_rr_separate_region <- inequalities_sedentarism %>%
  filter(exp == "Sex Inequality stratified by Region Adjusted by Age") %>%
  ggplot(aes(x = as.numeric(encuesta), y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(fill = strata), alpha = 0.25) +
  geom_line(aes(color = strata), linewidth = 1) +
  facet_grid(cols = vars(strata), scales = "free_y") +
  labs(
    title = "Sex Inequality Stratified by Region Adjusted by Age",
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
ggsave("Figures/fig_rr_separate_region.png", width = 4000, height = 2200, dpi=300, units = "px")

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
  filename = paste0("Figures/sex_differences_forest.png"),
  width = 4000, height = 2500, dpi = 300, units = "px"
)

#### RII in Sedentarism by CCAA, Survey and Sex (Multi-level) ####
# This model estimates the association between social class and sedentarism for each year, while:
  # Adjusting for age (edad) and sex (sexo)
  # Accounting for random variation across regions (ccaa)
  # Allowing the effect of social class to vary by region (i.e., random slopes and intercepts)

## CCAA list
ccaas <- read_delim("ccaas.csv", delim = ";", 
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
extract_rii_by_group <- function(model, ccaa_ref, outcome_label = "Sedentarism", 
                                 random_group = "survey:ccaa", effect_name = "clase_tr") {
  
  # 1. Extract fixed effect and SE
  fe_value <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  # 2. Extract random effects for specified group
  re_df <- ranef(model)$cond[[random_group]]
  
  # 3. Add fixed effect to random effect
  re_df <- re_df %>%
    mutate(value = !!sym(effect_name) + fe_value)
  
  # 4. Approximate SE with fixed effect SE
  re_df <- re_df %>%
    mutate(se = fe_se)
  
  # 5. Compute RII and CIs
  re_df <- re_df %>%
    mutate(
      rii = exp(value),
      rii_infci = exp(value - 1.96 * se),
      rii_supci = exp(value + 1.96 * se),
      group = rownames(re_df)
    )
  
  # 6. Final formatting and join with ccaa reference
  out_df <- re_df %>%
    select(rii, rii_infci, rii_supci, group) %>%
    mutate(sex = "Males") %>%
    separate(group, into = c("survey", "ccaa"), sep = ":") %>%
    left_join(ccaa_ref, by = c("ccaa" = "nombre")) %>%
    mutate(Outcome = outcome_label)
  
  return(out_df)
}

# Overall
rii_sedentarism_CCAA <- glmmTMB(sedentarismo~clase_tr+edad+sexo+
                                  (1+clase_tr|survey) + # allows the baseline level of sedentarism and the effect of social class to vary between survey years
                                  (1+clase_tr|survey:ccaa), # allows the same variations to differ between autonomous communities within each survey year
                                data = dt,
                                 family = "poisson", weights = factor2) # generalized linear mixed model

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

rii_sedentarism_CCAA <- extract_rii_by_group(
  model = rii_sedentarism_CCAA,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA
clipr::write_clip(rii_sedentarism_CCAA)

## wide table overall
rii_sedentarism_CCAA_wide <- rii_sedentarism_CCAA %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

# View the result
rii_sedentarism_CCAA_wide

# Optional: copy to clipboard
clipr::write_clip(rii_sedentarism_CCAA_wide)

## Females
rii_sedentarism_CCAA_females <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) + (1+clase_tr|survey:ccaa), data = subset(dt, sexo == "Female"),
                                family="poisson", weights = factor2) # generalized linear mixed model

rii_sedentarism_CCAA_females <- extract_rii_by_group(
  model = rii_sedentarism_CCAA_females,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_females
clipr::write_clip(rii_sedentarism_CCAA_females)

## wide table females
rii_sedentarism_CCAA_females_wide <- rii_sedentarism_CCAA_females %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

# View the result
rii_sedentarism_CCAA_females_wide

# Optional: copy to clipboard
clipr::write_clip(rii_sedentarism_CCAA_females_wide)

## Males
rii_sedentarism_CCAA_males <- glmmTMB(sedentarismo~clase_tr+edad+(1+clase_tr|survey) + (1+clase_tr|survey:ccaa), data = subset(dt, sexo == "Male"),
                                        family="poisson", weights = factor2) # generalized linear mixed model

rii_sedentarism_CCAA_males <- extract_rii_by_group(
  model = rii_sedentarism_CCAA_males,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_males
clipr::write_clip(rii_sedentarism_CCAA_males)

## wide table males
rii_sedentarism_CCAA_males_wide <- rii_sedentarism_CCAA_males %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

# View the result
rii_sedentarism_CCAA_males_wide

# Optional: copy to clipboard
clipr::write_clip(rii_sedentarism_CCAA_males_wide)

rii_sedentarism_CCAA_combined <- rii_sedentarism_CCAA %>% 
  rbind(rii_sedentarism_CCAA_females) %>% 
  rbind(rii_sedentarism_CCAA_males)

rii_sedentarism_CCAA_combined
rii_sedentarism_CCAA_combined <- rii_sedentarism_CCAA_combined %>%
  mutate(sex = case_when(
    sex == "Females" ~ "Girls",
    sex == "Males" ~ "Boys",
    TRUE ~ sex  # keep any other values as they are
  ))
save(rii_sedentarism_CCAA_combined, file = "rii_sedentarism_CCAA_combined.RData")

## Relative Rate of Change of sedentarism overtime per CCAA
rii_change <- rii_sedentarism_CCAA_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change <- rii_sedentarism_CCAA_females_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

rii_change <- rii_sedentarism_CCAA_males_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )

print(rii_change$change_2003_2023)

#### 
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
ggsave("Figures/fig_rii_ccaa_Oct22.png", width = 4000, height = 2200, dpi=300, units = "px")

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
ggsave("Figures/fig_rii_ccaa_sex_Oct22_v2.png", width = 4000, height = 2200, dpi=300, units = "px")

## CCAA Map

#### Intra-Class Correlation of Inequality across Regions ####
  ## Calculating the ICC of the RII values across regions
  ## Each value represents how strongly socioeconomic position is associated with sedentarism within that region
  ## Taking RII values and comparing them across regions then calculating the ICC of the RII to find out
  ## how much do RII values vary between regions versus within them (and over time)

## Check - if model with random effects fits significantly better (small p-value), then the clustering is statistically significant
model_with_re <- glmmTMB(sedentarismo ~ clase_tr + edad + sexo + (1| ccaa), data = dt, family = "poisson")
model_without_re <- glmmTMB(sedentarismo ~ clase_tr + edad + sexo, data = dt, family = "poisson")
anova_model <- anova(model_without_re, model_with_re)
clipr::write_clip(anova_model)

## Create dataframe 
rii_model_icc <- rii_sedentarism_CCAA %>% 
  select(survey, rii, ccaa) %>% 
  pivot_wider(names_from = ccaa, values_from = rii) # pivoting data so that each ccaa becomes a column, rows correspond to survey years
rii_model_icc
## dataframe so that when I calculate the ICC I can tell if regional RII values are clustered or dispersed over time 

## Fit a random slope model for each survey year
# ICC calculated for each year: time trend of how much regional clustering of inequalities matters 
irr_model_icc_2003 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2003),
                              family="poisson", weights = factor2)

## (1+clase_tr|ccaa) : each region gets its own baseline level of sedentarism,
                    # the effect of social class on sedentarism is allowed to vary by region
                    # the variation happens at the region level
                    # the random part is: 
                                # a distribution of region-specific intercepts i.e., how high and low sedentarism is in each region on average
                                # a distribution of region-specific slopes i.e., how strongly social class affects sedentarism in each region
                                # With (1 + clase_tr | ccaa), regions can have different intercepts and different slopes. The lines can tilt differently, not just shift up and down.

icc_2003 <- performance::icc(irr_model_icc_2003) %>% 
  mutate(survey=2003)

icc_2003 <- data.frame(
  adjusted_icc = icc_2003$ICC_adjusted,
  unadjusted_icc = icc_2003$ICC_unadjusted,
  survey = 2003
)

irr_model_icc_2006 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2006),
                              family="poisson", weights = factor2)

icc_2006 <- performance::icc(irr_model_icc_2006) %>% 
  mutate(survey=2006)

icc_2006 <- data.frame(
  adjusted_icc = icc_2006$ICC_adjusted,
  unadjusted_icc = icc_2006$ICC_unadjusted,
  survey = 2006
)

irr_model_icc_2011 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2011),
                              family="poisson", weights = factor2)

icc_2011 <- performance::icc(irr_model_icc_2011) %>% 
  mutate(survey=2011)

icc_2011 <- data.frame(
  adjusted_icc = icc_2011$ICC_adjusted,
  unadjusted_icc = icc_2011$ICC_unadjusted,
  survey = 2011
  )

irr_model_icc_2017 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2017),
                              family="poisson", weights = factor2)

icc_2017 <- performance::icc(irr_model_icc_2017) %>% 
  mutate(survey=2017)

icc_2017 <- data.frame(
  adjusted_icc = icc_2017$ICC_adjusted,
  unadjusted_icc = icc_2017$ICC_unadjusted,
  survey = 2017
)

irr_model_icc_2023 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2023),
                              family="poisson", weights = factor2)

icc_2023 <- performance::icc(irr_model_icc_2023) %>% 
  mutate(survey=2023)

icc_2023 <- data.frame(
  adjusted_icc = icc_2023$ICC_adjusted,
  unadjusted_icc = icc_2023$ICC_unadjusted,
  survey = 2023
)

icc_ccaa <- icc_2003 %>% 
  rbind(icc_2006) %>% 
  rbind(icc_2011) %>% 
  rbind(icc_2017) %>% 
  rbind(icc_2023)

icc_ccaa
clipr::write_clip(icc_ccaa)

fig_icc <-  ggplot(icc_ccaa, aes(x=survey, y=adjusted_icc)) +
  geom_line()+
  geom_point()+
  labs(x="", y="ICC")+
  ylim(0, 0.2)+
  scale_x_continuous(breaks=c(2003, 2006, 2011, 2017, 2023))+
  theme_bw()+
  theme()
fig_icc # where higher ICC values suggest that region (ccaa) explains more of the variance in sedentarism
  # in 2017, where 12% of the variance in sedentarism is explained by CCAA ? 

## Singularity problem (1+clase_tr|ccaa)
  # irr_model_icc_2003 <- glmmTMB(sedentarismo~clase_tr+edad+sexo+(1+clase_tr|ccaa), data=subset(dt, dt$survey==2003),
  #                             family="poisson") # because was not weighting
  # performance::check_singularity(irr_model_icc_2003)
  ## Singularity happens when one or more random effect variance components in the model are estimated to be zero or very close
  ## Where the random effect doesn't explain any meaningful variation in the data,
  ## The model is overfitting or too complex for the data
  ## Happens if have too few groups or levels for a random effect
  ## Small sample size or limited variability in groups













