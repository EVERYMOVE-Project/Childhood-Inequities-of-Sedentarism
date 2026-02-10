## Author: Diana Juanita Mora
## Project: Childhood Inequities of Sedentarism
## Script: Data Analysis - RII and SII (clase_tr_2)
## Finalized: 28th of July 2025
## Edited: 9th of February of 2026

## Load libraries ----
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
library(extrafont)

font_import(prompt = FALSE)   # run once (can take a few minutes)
loadfonts(device = "win") 

## read joined data
dt <- get(load("joined_clean_rii.RData"))

#### Regression Models for Inequality ####
#### Relative Index of Inequality (RII) ####
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
# clase_tr_2, clase_tr_2_f, clase_tr_2_m

dt <- dt %>%
  mutate(sedentarismo = ifelse(sedentarismo == "Yes", 1, 0))

# RII Overall robust standard errors
rii_sedentarism_overall_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2 + edad + sexo,
               data = .x, family = poisson(link = "log"))  # log link for RII
    
    # Robust (sandwich) variance-covariance matrix
    robust_se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC0")))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2"]
    coef_se  <- robust_se["clase_tr_2"]
    
    # Build RII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      rii         = exp(coef_est),
      rii_infci   = exp(coef_est - 1.96 * coef_se),
      rii_supci   = exp(coef_est + 1.96 * coef_se),
      risk_factor = "Sedentarism",
      sexo        = "Overall"
    )
  })

rii_sedentarism_overall_clase
clipr::write_clip(rii_sedentarism_overall_clase)
save(rii_sedentarism_overall_clase, file = "Datasets/clase_tr_2/new/rii_sedentarism_overall_clase.RData")

# RII Females
dt_females <- subset(dt, sexo == "Female")
rii_sedentarism_f_clase <- dt_females %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2_f + edad,
               data = .x, family = poisson(link = "log"))  # log link for RII
    
    # Robust (sandwich) variance-covariance matrix
    robust_se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC0")))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2_f"]
    coef_se  <- robust_se["clase_tr_2_f"]
    
    # Build RII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      rii         = exp(coef_est),
      rii_infci   = exp(coef_est - 1.96 * coef_se),
      rii_supci   = exp(coef_est + 1.96 * coef_se),
      risk_factor = "Sedentarism",
      sexo        = "Girls"
    )
  })
rii_sedentarism_f_clase
clipr::write_clip(rii_sedentarism_f_clase)
save(rii_sedentarism_f_clase, file = "Datasets/clase_tr_2/new/rii_sedentarism_overall_clase_f.RData")

# RII Males
dt_males <- subset(dt, sexo == "Male")
rii_sedentarism_m_clase <- dt_males %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2_m + edad,
               data = .x, family = poisson(link = "log"))  # log link for RII
    
    # Robust (sandwich) variance-covariance matrix
    robust_se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC0")))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2_m"]
    coef_se  <- robust_se["clase_tr_2_m"]
    
    # Build RII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      rii         = exp(coef_est),
      rii_infci   = exp(coef_est - 1.96 * coef_se),
      rii_supci   = exp(coef_est + 1.96 * coef_se),
      risk_factor = "Sedentarism",
      sexo        = "Boys"
    )
  })
rii_sedentarism_m_clase
clipr::write_clip(rii_sedentarism_m_clase)
save(rii_sedentarism_m_clase, file = "Datasets/clase_tr_2/new/rii_sedentarism_overall_clase_m.RData")

## Database for RII data by social class
rii_sedentarism_clase <- rii_sedentarism_overall_clase %>%
  rbind(rii_sedentarism_f_clase) %>%
  rbind(rii_sedentarism_m_clase) %>% 
  mutate(exp="RII Social Class") %>% 
  rename(est = rii, infci=rii_infci, supci=rii_supci, strata=sexo)
save(rii_sedentarism_clase, file = "Datasets/clase_tr_2/new/rii_sedentarism.RData")

#### Slope Index of Inequality (SII) ####
# Tells us the absolute difference in prevalence of sedentarism between the
# highest and lowest levels of the social class ladder (higher SII higher inequality)
# Using a poisson with identity link: absolute risk difference, which tells us how 
# many percentage points difference there are across the social class spectrum
# i.e., how many more people per 100 are sedentary in the lowest social class 
# group compared to the highest?

## SII Overall
sii_sedentarism_overall_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2 + edad + sexo,
               data = .x, family = poisson(link = "identity"),
               start = c(1, 0, 0, 0))
    
    # Robust SE
    robust_vcov <- sandwich::vcovHC(fit, type = "HC0")
    robust_se   <- sqrt(diag(robust_vcov))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2"]
    coef_se  <- robust_se["clase_tr_2"]
    
    # Build SII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      sii         = (coef_est)*100,
      sii_infci   = (coef_est - 1.96 * coef_se)*100,
      sii_supci   = (coef_est + 1.96 * coef_se)*100,
      risk_factor = "Sedentarism",
      sexo        = "Overall"
    )
  })
sii_sedentarism_overall_clase
clipr::write_clip(sii_sedentarism_overall_clase)
save(sii_sedentarism_overall_clase, file = "Datasets/clase_tr_2/new/sii_sedentarism_overall_clase.RData")

# SII Females
sii_sedentarism_f_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2_f + edad,
               data = .x, family = poisson(link = "identity"),
               start = c(1, 0, 0))
    
    # Robust SE
    robust_vcov <- sandwich::vcovHC(fit, type = "HC0")
    robust_se   <- sqrt(diag(robust_vcov))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2_f"]
    coef_se  <- robust_se["clase_tr_2_f"]
    
    # Build SII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      sii         = (coef_est)*100,   # NO exp()
      sii_infci   = (coef_est - 1.96 * coef_se)*100,
      sii_supci   = (coef_est + 1.96 * coef_se)*100,
      risk_factor = "Sedentarism",
      sexo        = "Girls"
    )
  })
sii_sedentarism_f_clase
clipr::write_clip(sii_sedentarism_f_clase)
save(sii_sedentarism_f_clase, file = "Datasets/clase_tr_2/new/sii_sedentarism_f_clase.RData")

# SII Males
sii_sedentarism_m_clase <- dt %>%
  group_by(survey) %>%
  group_split() %>%
  map_dfr(~ {
    
    # Fit Poisson model
    fit <- glm(sedentarismo ~ clase_tr_2_m + edad,
               data = .x, family = poisson(link = "identity"),
               start = c(1, 0, 0))
    
    # Robust SE
    robust_vcov <- sandwich::vcovHC(fit, type = "HC0")
    robust_se   <- sqrt(diag(robust_vcov))
    
    # Extract coefficient of interest
    coef_est <- coef(fit)["clase_tr_2_m"]
    coef_se  <- robust_se["clase_tr_2_m"]
    
    # Build SII and robust CIs
    tibble(
      encuesta    = unique(.x$survey),
      sii         = (coef_est)*100,
      sii_infci   = (coef_est - 1.96 * coef_se)*100,
      sii_supci   = (coef_est + 1.96 * coef_se)*100,
      risk_factor = "Sedentarism",
      sexo        = "Boys"
    )
  })
sii_sedentarism_m_clase
clipr::write_clip(sii_sedentarism_m_clase)
save(sii_sedentarism_m_clase, file = "Datasets/clase_tr_2/new/sii_sedentarism_m_clase.RData")

# Database for SII by social class
sii_sedentarism_clase <- sii_sedentarism_overall_clase %>%
  rbind(sii_sedentarism_f_clase) %>%
  rbind(sii_sedentarism_m_clase) %>%
  mutate(exp="SII Social Class") %>% 
  rename(est = sii, infci = sii_infci, supci = sii_supci, strata = sexo)
sii_sedentarism_clase
save(sii_sedentarism_clase, file = "Datasets/clase_tr_2/new/sii_sedentarism_clase.RData")

#### Databases of RII, SII ####
inequalities_sedentarism <- bind_rows(
  rii_sedentarism_clase %>% mutate(exp = "RII Social Class"),
  rbind(sii_sedentarism_clase) %>% mutate(exp = "SII Social Class"))
print(inequalities_sedentarism, n = 30)
  
## database of all regression
save(inequalities_sedentarism, file = "Datasets/clase_tr_2/new/inequalities_sedentarism.RData")

#### Visualization of RII in Sedentarism ####
load("Datasets/clase_tr_2/new/inequalities_sedentarism.RData")

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
                                          levels = c("Overall", "Girls", "Boys"))
inequalities_sedentarism$exp <- factor(inequalities_sedentarism$exp, 
                                         levels = c("RII Social Class", "SII Social Class"))
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
ggsave("Figures/clase_tr_2/fig_rii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

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
    vjust = -1.5,
    # nudge_x = 1,
    size = 4,
    # fontface = "bold",
    family = "Times New Roman",
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
    limits = c(0.75, 12)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) + 
  scale_color_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  theme_inequalities() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )
fig_rii_class_sex
ggsave("Figures/clase_tr_2/fig_rii_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

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
ggsave("Figures/clase_tr_2/fig_sii_class.png", width = 4000, height = 2200, dpi=300, units = "px")

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
    vjust = -1.5,                # vertical adjustment (move slightly above points)
    size = 4,
    # fontface = "bold",
    show.legend = FALSE,
    family = "Times New Roman"
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
    limits = c(-0.75, 30)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023)
  ) + 
  scale_color_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  theme_inequalities() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )
fig_sii_class_sex
ggsave("Figures/clase_tr_2/fig_sii_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

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
ggsave("Figures/clase_tr_2/fig_sii_separate_class.png", width = 4000, height = 2200, dpi=300, units = "px")

#### RII and SII Figures Together ####
fig_sii_rii <- inequalities_sedentarism %>% 
  filter(exp == "SII Social Class"| exp == "RII Social Class") %>% 
  ggplot(aes(x = encuesta, y = est, ymin = infci, ymax = supci)) +
  geom_hline(yintercept = 1, lty=2, color = "gray40") +
  geom_ribbon(alpha = 0.25, aes(fill=strata)) +
  geom_line(aes(color = strata), linewidth = 1.2) +
  geom_text(
    aes(label = round(est, 2)),
    vjust = -1.35,              
    size = 4,
    # fontface = "bold",
    family = "Times New Roman",
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
  scale_color_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "#4BAE48",
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20"
    )
  ) +
  theme_inequalities()+
  theme(
    text = element_text(family = "Times New Roman")
  )
fig_sii_rii
ggsave("Figures/clase_tr_2/fig_sii_rii.png", width = 4000, height = 2200, dpi=300, units = "px")

#### RII in Sedentarism by CCAA, Survey and Sex (Multi-level) ####
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

## RECALL NEED TO CHANGE VARIABLES TO DO EXTRACTION PER SEX
## FUNCTION THAT WORKS WITH clase_tr_2
extract_rii_by_group_CCAA <- function(
    model,
    ccaa_ref,
    outcome_label = "Sedentarismo",
    effect_name = "clase_tr_2_m",
    random_group = "survey:ccaa"
) {
  
  # Fixed effect and SE
  fe <- fixef(model)$cond[effect_name]
  fe_se <- summary(model)$coefficients$cond[effect_name, "Std. Error"]
  
  # Random effects
  re_list <- ranef(model)$cond
  if (!random_group %in% names(re_list)) {
    stop(paste("Random effect", random_group, "not found in model"))
  }
  
  re_df <- re_list[[random_group]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("group")  # keep survey:ccaa
  
  # Combine fixed + random slope
  if (!effect_name %in% names(re_df)) stop("Slope column not found in random effects")
  re_df <- re_df %>%
    mutate(
      linear_pred = fe + .data[[effect_name]],
      rii = exp(linear_pred),
      rii_infci = exp(linear_pred - 1.96 * fe_se),
      rii_supci = exp(linear_pred + 1.96 * fe_se)
    )
  
  # Split survey and ccaa
  re_df <- re_df %>%
    tidyr::separate(group, into = c("survey", "ccaa"), sep = ":", remove = TRUE) %>%
    mutate(
      survey = as.integer(survey),
      sex = "Male",
      Outcome = outcome_label
    )
  
  # Final tidy output
  out <- re_df %>%
    select(survey, ccaa, rii, rii_infci, rii_supci, sex, Outcome) %>%
    arrange(survey, ccaa)
  
  return(out)
}

# Overall, where surveyxccaaxclase_tr_2 cells are less than 15 
table_ccaa <- dt %>%
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_2 = min(table(clase_tr_2)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2 < 15)
print(table_ccaa, n = 51)

clipr::write_clip(table_ccaa)

rii_sedentarism_CCAA <- glmmTMB(sedentarismo~clase_tr_2+edad+sexo+
                                  (1+clase_tr_2|survey) + # allows the baseline level of sedentarism and the effect of social class to vary between survey years
                                  (1+clase_tr_2|survey:ccaa), # allows the same variations to differ between autonomous communities within each survey year
                                data = dt,
                                 family = "poisson", weights = factor2) # generalized linear mixed model
rii_sedentarism_CCAA
VarCorr(rii_sedentarism_CCAA)

rii_sedentarism_CCAA <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)

rii_sedentarism_CCAA
clipr::write_clip(rii_sedentarism_CCAA)
save(rii_sedentarism_CCAA, file = "Datasets/clase_tr_2/new/rii_sedentarism_CCAA.RData")

## wide table overall
rii_sedentarism_CCAA_wide <- rii_sedentarism_CCAA %>%
  select(CCAA = ccaa, Survey = survey, RII = rii, Lower_CI = rii_infci, Upper_CI = rii_supci) %>%
  pivot_wider(
    names_from = Survey,
    values_from = c(RII, Lower_CI, Upper_CI)
  )

rii_sedentarism_CCAA_wide
clipr::write_clip(rii_sedentarism_CCAA_wide)
summary(dt$clase_tr_2)

## Females
table_ccaa_f <- dt %>%
  filter(sexo == "Female") %>% 
  group_by(survey, ccaa) %>%
  summarise(
    clase_tr_2_f = min(table(clase_tr_2_f)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2_f < 15)
clipr::write_clip(table_ccaa_f)
table_ccaa_f

rii_sedentarism_CCAA_females <- glmmTMB(sedentarismo~clase_tr_2_f+edad+(1+clase_tr_2_f|survey) 
                                        + (1+clase_tr_2_f|survey:ccaa),  data = subset(dt, sexo == "Female"),
                                family="poisson", weights = factor2)
rii_sedentarism_CCAA_females
VarCorr(rii_sedentarism_CCAA_females)

rii_sedentarism_CCAA_females_t <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA_females,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_females_t
clipr::write_clip(rii_sedentarism_CCAA_females_t)
save(rii_sedentarism_CCAA_females_t, file = "Datasets/clase_tr_2/new/rii_sedentarism_CCAA_females.RData")

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
    clase_tr_2_m = min(table(clase_tr_2_m)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2_m < 15)
table_ccaa_m

clipr::write_clip(table_ccaa_m)

rii_sedentarism_CCAA_males <- glmmTMB(sedentarismo~clase_tr_2_m+edad+(1+clase_tr_2_m|survey) 
                                      + (1+clase_tr_2_m|survey:ccaa), data = subset(dt, sexo == "Male"),
                                        family="poisson", weights = factor2) # generalized linear mixed model
VarCorr(rii_sedentarism_CCAA_males)

rii_sedentarism_CCAA_males_t <- extract_rii_by_group_CCAA(
  model = rii_sedentarism_CCAA_males,
  ccaa_ref = ccaas,
  outcome_label = "Sedentarismo"
)
rii_sedentarism_CCAA_males_t
clipr::write_clip(rii_sedentarism_CCAA_males_t)
save(rii_sedentarism_CCAA_males_t, file = "Datasets/clase_tr_2/rii_sedentarism_CCAA_males.RData")

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
    sex == "Female" ~ "Girls",
    sex == "Male" ~ "Boys",
    TRUE ~ sex  # keep any other values as they are
  ))
save(rii_sedentarism_CCAA_combined, file = "Datasets/clase_tr_2/new/rii_sedentarism_CCAA_combined.RData")

## Relative Rate of Change of sedentarism overtime per CCAA
rii_change <- rii_sedentarism_CCAA_wide %>%
  mutate(
    change_2003_2006 = (RII_2006 - RII_2003)/RII_2003 * 100,
    change_2006_2011 = (RII_2011 - RII_2006)/RII_2006 * 100,
    change_2011_2017 = (RII_2017 - RII_2011)/RII_2011 * 100,
    change_2017_2023 = (RII_2023 - RII_2017)/RII_2017 * 100,
    change_2003_2023 = (RII_2023 - RII_2003)/RII_2003 * 100
  )
View(rii_change)

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
ggsave("Figures/clase_tr_2/fig_rii_ccaa.png", width = 4000, height = 2200, dpi=300, units = "px")

## new figure of RII by CCAA
fig_CCAA_multilineal <- ggplot(rii_sedentarism_CCAA, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = ccaa)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray90") +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  geom_text(
    aes(label = sprintf("%.2f", rii)),
    family = "Times New Roman",
    vjust = 1.5,
    size = 4,
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
    text = element_text(family = "Times New Roman"),
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
ggsave("Figures/clase_tr_2/fig_rii_ccaa.png", width = 4000, height = 2200, dpi=300, units = "px")

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
                  size = 4, show.legend = FALSE, max.overlaps = 10, family = "Times New Roman", color = "black") +
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
    y = "RII (95% CI)",
    title = "Relative Index of Inequality (RII) in Sedentarism by CCAA, Sex, and Survey Year"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
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
ggsave("Figures/clase_tr_2/fig_rii_ccaa_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

## RII CCAA Map ####
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

data_ccaa <- data_ccaa %>% ## merging ceuta and melilla to one geometry
  group_by(ccaa_en) %>%
  summarise(across(where(is.numeric), first),
            geometry = st_union(geometry),
            .groups = "drop")

## join RII database and shapefile
map_ccaa <- data_ccaa %>%
  left_join(rii_sedentarism_CCAA_combined,
            by = c("ccaa_en" = "ccaa"))

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
  map_ccaa %>% dplyr::filter(sex == "Overall") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  labs( title = "Inequalities in Childhood Sedentarism by Autonomous Community per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
rii_map_survey

ggsave(
  filename = "Figures/clase_tr_2/rii_map_survey.png",
  plot = rii_map_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

rii_map_survey_f <- ggplot(
  map_ccaa %>% dplyr::filter(sex == "Girls") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs( title = "Girls: Inequalities in Childhood Sedentarism by Autonomous Community per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
rii_map_survey_f

ggsave(
  filename = "Figures/clase_tr_2/rii_map_survey_girls.png",
  plot = rii_map_survey_f,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

rii_map_survey_m <- ggplot(
  map_ccaa %>% dplyr::filter(sex == "Boys") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Boys: Inequalities in Childhood Sedentarism by Autonomous Community per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
rii_map_survey_m

ggsave(
  filename = "Figures/clase_tr_2/rii_map_survey_boys.png",
  plot = rii_map_survey_m,
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
    median_clase = median(clase_tr_2, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ccaa_survey_table
clipr::write_clip(ccaa_survey_table)

ccaa_survey_table_f <- dt_females %>%
  group_by(ccaa, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr_2, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(ccaa_survey_table_f)
print(ccaa_survey_table_f, n=90)

ccaa_survey_table_m <- dt_males %>%
  group_by(ccaa, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr_2, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(ccaa_survey_table_m)
print(ccaa_survey_table_m, n=90)

# flagging small cell counts
table(dt$survey, dt$ccaa, dt$sexo)
dt %>%
  count(ccaa, sexo, survey) %>%
  mutate(flag_small = n < 30) %>% 
  print(n=180)

dt %>%
  group_by(survey, ccaa, sexo) %>%
  count(clase_tr_2) %>%
  mutate(flag_small = n < 10) %>% 
  arrange(flag_small, survey, ccaa, sexo, clase_tr_2) %>% 
  print(n=1068)

hist(dt$clase_tr_2)

dt %>%
  group_by(survey, ccaa, sexo) %>%
  count(clase_tr_2) %>%               
  mutate(flag_sparse = n < 10) %>%
  group_by(survey) %>% 
  summarise(n_sparse_cells = sum(flag_sparse), .groups = "drop")

dt %>%
  count(survey, ccaa, sexo, clase_tr_2) %>%
  ggplot(aes(x = clase_tr_2, y = n, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_grid(ccaa ~ survey)

sparse_strata <- dt %>%
  group_by(survey, ccaa) %>%
  summarise(min_n = min(table(clase_tr_2)), total_n = n(), .groups = "drop") %>%
  filter(min_n < 15) %>% 
  print(n=51)

sparse_strata <- dt %>%
  group_by(survey, ccaa, sexo) %>%
  summarise(min_n = min(table(clase_tr_2)), total_n = n(), .groups = "drop") %>%
  filter(min_n < 10) %>% 
  filter(sexo == "Female") %>% 
  print(n=64)

sparse_strata <- dt %>%
  group_by(survey, ccaa, sexo) %>%
  summarise(min_n = min(table(clase_tr_2)), total_n = n(), .groups = "drop") %>%
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

## FUNCTION NEED TO CHANGE VARIABLE CLASE WHEN DOING BY SEX
extract_rii_by_group_NUTS1 <- function(
    model,
    NUTS1_ref,
    outcome_label = "Sedentarism",
    effect_name = "clase_tr_2_m",
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
      sex = "Male",
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
    clase_tr_2 = min(table(clase_tr_2)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2 < 15)
table_NUTS
clipr::write_clip(table_NUTS)

rii_sedentarism_NUTS1 <- glmmTMB(sedentarismo~clase_tr_2+edad+sexo+
                                  (1+clase_tr_2|survey) + 
                                  (1+clase_tr_2|survey:NUTS1), 
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
save(rii_sedentarism_NUTS1, file = "Datasets/clase_tr_2/new/rii_sedentarism_NUTS1.RData")

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
    clase_tr_2_f = min(table(clase_tr_2_f)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2_f < 15)
table_NUTS_f
clipr::write_clip(table_NUTS_f)

rii_sedentarism_NUTS1_females <- glmmTMB(sedentarismo~clase_tr_2_f+edad+
                                           (1+clase_tr_2_f|survey) 
                                        + (1+clase_tr_2_f|survey:NUTS1),  
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
    clase_tr_2_m = min(table(clase_tr_2_m)),
    .groups = "drop"
  ) %>%
  filter(clase_tr_2_m < 15)
table_NUTS_m
clipr::write_clip(table_NUTS_m)

rii_sedentarism_NUTS1_males <- glmmTMB(sedentarismo~clase_tr_2_m+edad+
                                         (1+clase_tr_2_m|survey) 
                                       + (1+clase_tr_2_m|survey:NUTS1), 
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
    sex == "Female" ~ "Girls",
    sex == "Male" ~ "Boys",
    TRUE ~ sex  # keep any other values as they are
  ))
save(rii_sedentarism_NUTS1_combined, file = "Datasets/clase_tr_2/new/rii_sedentarism_NUTS1_combined.RData")

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
## NUTS1
NUTS1_survey_table <- dt %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr_2, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
NUTS1_survey_table
clipr::write_clip(NUTS1_survey_table)

dt_males <- subset(dt, sexo == "Female")
NUTS1_survey_table_f <- dt_females %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr_2_f, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2_f, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2_f, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(NUTS1_survey_table_f)

dt_males <- subset(dt, sexo == "Male")
NUTS1_survey_table_m <- dt_males %>%
  group_by(NUTS1, survey) %>%
  summarise(
    n = n(),
    median_clase = median(clase_tr_2_m, na.rm = TRUE),
    p25_clase = quantile(clase_tr_2_m, 0.25, na.rm = TRUE),
    p75_clase = quantile(clase_tr_2_m, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
clipr::write_clip(NUTS1_survey_table_m)
print(NUTS1_survey_table_m, n=90)

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
ggsave("Figures/clase_tr_2/fig_rii_NUTS1.png", width = 4000, height = 2200, dpi=300, units = "px")

## new figure of RII by NUTS1
fig_NUTS1_multilineal <- ggplot(rii_sedentarism_NUTS1, 
                               aes(x = survey, y = rii, ymin = rii_infci, ymax = rii_supci, group = NUTS1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray90") +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  geom_text(
    family = "Times New Roman",
    aes(label = sprintf("%.2f", rii)),
    vjust = 1.5,
    size = 4,
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
    text = element_text(family = "Times New Roman"),
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
ggsave("Figures/clase_tr_2/fig_rii_NUTS1.png", width = 4000, height = 2200, dpi=300, units = "px")

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
    text = element_text(family = "Times New Roman"),
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
ggsave("Figures/clase_tr_2/fig_rii_NUTS1_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

## RII NUTS1 Map ####

NUTS1 <- st_read("Resources/NUTS1_ES_20M_2024_3035.shp") # Leemos los datos de capa
load("Datasets/clase_tr_2/new/rii_sedentarism_NUTS1_combined.RData")

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

rii_map_survey <- ggplot(
  map_NUTS1 %>% dplyr::filter(sex == "Overall") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  labs( title = "Inequalities in Childhood Sedentarism by NUTS per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        caption = "Nomenclature of Territorial Units for Statistics (NUTS) Regions",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))

rii_map_survey

ggsave(
  filename = "Figures/clase_tr_2/rii_map_NUTS_survey.png",
  plot = rii_map_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

rii_map_survey_NUTS_f <- ggplot(
  map_NUTS1 %>% dplyr::filter(sex == "Girls") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs( title = "Girls: Inequalities in Childhood Sedentarism by NUTS per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        caption = "Nomenclature of Territorial Units for Statistics (NUTS) Regions",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))

rii_map_survey_NUTS_f

ggsave(
  filename = "Figures/clase_tr_2/rii_map_NUTS_survey_girls.png",
  plot = rii_map_survey_NUTS_f,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

rii_map_survey_NUTS_m <- ggplot(
  map_NUTS1 %>% dplyr::filter(sex == "Boys") 
) +
  geom_sf(aes(fill = rii), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Boys: Inequalities in Childhood Sedentarism by NUTS per Survey Year",
        subtitle = "Unit: Relative Index of Inequality",
        caption = "Nomenclature of Territorial Units for Statistics (NUTS) Regions",
        fill = "Relative Index of Inequality") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))

rii_map_survey_NUTS_m

ggsave(
  filename = "Figures/clase_tr_2/rii_map_NUTS_survey_boys.png",
  plot = rii_map_survey_NUTS_m,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

## Inflection Point ----
## Segmented package to decide the inflection point in the inequalities of sedentarism
## linear model fitted with survey year as a predictor
rii_sedentarism_overall_clase$encuesta <- as.numeric(rii_sedentarism_overall_clase$encuesta)

m0 <- lm(rii ~ encuesta, data = rii_sedentarism_overall_clase)
summary(m0)

## fit segmented model
seg_m <- segmented(m0, seg.Z = ~encuesta, psi = 2011)
seg_m ## 2014 inflection point

## plot inflection point
plot(rii ~ encuesta, data = rii_sedentarism_overall_clase)
plot(seg_m, add = TRUE, col = "red")
ggsave("Figures/inflection.png", width = 4000, height = 2200, dpi=300, units = "px")
## Annual Percent Change ----
apc_dta <- rii_sedentarism_CCAA_combined %>%
  filter(
    Outcome == "Sedentarismo",
    sex == "Boys" # change to Girls or Boys
  ) %>%
  mutate(
    survey = as.integer(survey)
  )

apc_ccaa <- apc_dta %>%
  group_by(ccaa) %>%
  nest() %>%
  mutate(
    model = map(data, ~ rlm(log(rii) ~ survey, data = .x)),
    tidy  = map(model, tidy)
  ) %>%
  unnest(tidy) %>%
  filter(term == "survey") %>%
  mutate(
    APC = (exp(estimate) - 1) * 100,
    APC_low = (exp(estimate - 1.96 * std.error) - 1) * 100,
    APC_high = (exp(estimate + 1.96 * std.error) - 1) * 100
  ) %>%
  dplyr::select(
    ccaa,
    APC,
    APC_low,
    APC_high
  )
clipr::write_clip(apc_ccaa)

apc_dta2 <- rii_sedentarism_NUTS1_combined %>%
  filter(
    Outcome == "Sedentarismo",
    sex == "Boys"
  ) %>%
  mutate(
    survey = as.integer(survey)
  )

apc_nuts <- apc_dta2 %>%
  group_by(NUTS1) %>%
  nest() %>%
  mutate(
    model = map(data, ~ rlm(log(rii) ~ survey, data = .x)),
    tidy  = map(model, tidy)
  ) %>%
  unnest(tidy) %>%
  filter(term == "survey") %>%
  mutate(
    APC = (exp(estimate) - 1) * 100,
    APC_low = (exp(estimate - 1.96 * std.error) - 1) * 100,
    APC_high = (exp(estimate + 1.96 * std.error) - 1) * 100
  ) %>%
  dplyr::select(
    NUTS1,
    APC,
    APC_low,
    APC_high
  )
clipr::write_clip(apc_nuts)

## Prevalence Map CCAA ----
## join prevalence database and shapefile
map_ccaa_prevalence <- data_ccaa %>%
  left_join(prevalence_by_ccaa,
            by = c("ccaa_en" = "ccaa"))

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

prev_map_survey <- ggplot(data = map_ccaa_prevalence) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence by Autonomous Community per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_survey

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_survey.png",
  plot = prev_map_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

map_ccaa_prevalence_f <- data_ccaa %>%
  left_join(prevalence_by_ccaa_f,
            by = c("ccaa_en" = "ccaa"))

prev_map_survey_f <- ggplot(data = map_ccaa_prevalence_f) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence in Girls by Autonomous Community per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_survey_f

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_survey_f.png",
  plot = prev_map_survey_f,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

map_ccaa_prevalence_m <- data_ccaa %>%
  left_join(prevalence_by_ccaa_m,
            by = c("ccaa_en" = "ccaa"))

prev_map_survey_m <- ggplot(data = map_ccaa_prevalence_m) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence in Boys by Autonomous Community per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence") +
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_survey_m

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_survey_m.png",
  plot = prev_map_survey_m,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)
## Prevalence Map NUTS ----
## join prevalence database and shapefile
map_nuts_prevalence <- NUTS1_join %>%
  left_join(prevalence_by_nuts,
            by = c("NUTS1_ENG" = "NUTS1"))

prev_map_nuts_survey <- ggplot(data = map_nuts_prevalence) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence by NUTS region per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence",
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
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_nuts_survey

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_nuts_survey.png",
  plot = prev_map_nuts_survey,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

map_nuts_prevalence_f <- NUTS1_join %>%
  left_join(prevalence_by_nuts_f,
            by = c("NUTS1_ENG" = "NUTS1"))

prev_map_nuts_survey_f <- ggplot(data = map_nuts_prevalence_f) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence in Girls by NUTS region per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence",
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
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_nuts_survey_f

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_nuts_survey_f.png",
  plot = prev_map_nuts_survey_f,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)

map_nuts_prevalence_m <- NUTS1_join %>%
  left_join(prevalence_by_nuts_m,
            by = c("NUTS1_ENG" = "NUTS1"))

prev_map_nuts_survey_m <- ggplot(data = map_nuts_prevalence_m) +
  geom_sf(aes(fill = prevalence), color = "white", linewidth = 0.2) +
  facet_wrap(~ survey) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs( title = "Childhood Sedentarism Prevalence in Boys by NUTS region per Survey Year",
        # subtitle = "Unit: Relative Index of Inequality",
        fill = "Prevalence",
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
  theme_map()+
  theme(text = element_text(family = "Times New Roman"))
prev_map_nuts_survey_m

ggsave(
  filename = "Figures/clase_tr_2/prevalence_map_nuts_survey_m.png",
  plot = prev_map_nuts_survey_m,
  width = 13.3,
  height = 7.3,
  dpi = 300,
  units = "in"
)
