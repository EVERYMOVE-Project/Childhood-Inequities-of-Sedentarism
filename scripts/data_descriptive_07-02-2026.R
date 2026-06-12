## Author: Diana Juanita Mora
## Project: Childhood Inequities of Sedentarism
## Script: Data Descriptive INE
## Finalized: July 24 2025
## Edited: February 7 2026

## Load libraries ----
library(tidyverse)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)
library(ggrepel)
library(extrafont)

font_import(prompt = FALSE)   # run once (can take a few minutes)
loadfonts(device = "win") 

## Load data ----
dt <- get(load("joined_6.RData")) ## 6 to 15 without dropping NAs
dt_clean <- get(load("joined_clean_rii.RData")) ## 6 to 15 dropping NAs 

#### Baseline Characteristics without Survey Weights ####
## 6 to 15 with NAs
tbl_summary(
  dt,
  by = survey,  # or ~survey if you prefer formula style
  include = all_of(c("edad_cat3", "sexo", "sedentarismo", "nacionalidad",
                     "clase", "urb_rur", "ccaa")),
  label = list(
    edad_cat3 ~ "Child's Age (years)",
    sexo ~ "Child's Sex",
    nacionalidad ~ "Nationality",
    sedentarismo ~ "Sedentarism",
    clase ~ "Occupational Social Class",
    urb_rur ~ "Municipality Type",
    ccaa ~ "Autonomous Community"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(all_continuous() ~ 1),
  missing = "always"
) %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics by Survey Year (Unweighted)**<br>**Ages 6 to 15**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  modify_footnote(~ "Values are presented as n (%).")

## 6 to 15 without NAs
tbl_summary(
  dt_clean,
  by = survey,  # or ~survey if you prefer formula style
  include = all_of(c("edad_cat3", "sexo", "sedentarismo", "nacionalidad",
                     "clase", "urb_rur", "ccaa")),
  label = list(
    edad_cat3 ~ "Child's Age (years)",
    sexo ~ "Child's Sex",
    nacionalidad ~ "Nationality",
    sedentarismo ~ "Sedentarism",
    clase ~ "Occupational Social Class",
    urb_rur ~ "Municipality Type",
    ccaa ~ "Autonomous Community"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(all_continuous() ~ 1)
) %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics by Survey Year (Unweighted)**<br>**Ages 6 to 15**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  modify_footnote(~ "Values are as n (%).")

#### Baseline Characteristics with Survey Weights ####
my_labels <- set_names(
  list(
    "Age (years)",
    "Sex",
    "Nationality",
    "Sedentarism",
    "Occupational Social Class",
    "Municipality Type",
    "Autonomous Community",
    "NUTS Region"
  ),
  c("edad_cat3", "sexo", "nacionalidad", "sedentarismo", "clase",
    "urb_rur", "ccaa", "NUTS1")
)

valid_vars <- intersect(names(my_labels), names(dt_clean))

## Descriptive Unweighted + Weighted Table by Survey Year ----
# Create unweighted table (counts only)
tbl_unweighted <- tbl_summary(
  data = dt_clean,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{n}"),
  digits = list(all_continuous() ~ 1)) %>% 
  add_n()

tbl_unweighted

# Create weighted table (percentages only)
baseline <- svydesign(ids = ~1, weights = ~factor2, data = dt_clean)
# I am telling R that "each obs in my data represents factor2 people in the population"
# factor 2 acts like an expansion weight, scaling each row up to represent the population
# this generates weighted percentages (if urban kids are oversampled, it corrects for that)
# weighted totals or medians
# but also causes the N counts to be inflected to reflect the population, therefore, it is not the actual sample size

tbl_weighted <- tbl_svysummary(
  baseline,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{p}%"),
  digits = list(all_categorical() ~ 1)
)

# Merge both tables
tbl_combined <- tbl_merge(
  tbls = list(tbl_unweighted, tbl_weighted),
  tab_spanner = c("**Unweighted (N)**", "**Weighted (%)**")
)

# Final table
tbl_final <- tbl_combined %>% 
  bold_labels() %>% 
  modify_caption("**Baseline Characteristics by Survey Year**<br>**Ages 6 to 15**<br>n = unweighted counts, % = weighted proportions")
tbl_final

# Create descriptive table stratified by sedentarism
table_sedentary <- dt_clean %>%
  select(edad_cat3, sexo, nacionalidad, sedentarismo, clase, urb_rur, ccaa) %>%
  tbl_summary(
    by = sedentarismo,                   
    type = list(edad_cat3 ~ "categorical"),
    label = my_labels
  ) %>%
  add_overall() %>%                      
  add_p() %>%                            
  modify_header(label ~ "Variable") %>% 
  modify_caption("**Table 1. Descriptive characteristics of children aged 6–15, stratified by sedentarism**") %>%
  modify_footnote(
    all_stat_cols() ~ "Values are n (%) for categorical variables. P-values indicate differences between sedentary and non-sedentary children."
  )

table_sedentary

## Descriptive Unweighted + Weighted Table by Sedentarism ----
tbl_unweighted <- tbl_summary(
  data = dt_clean,
  by = sedentarismo,                    
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(
    all_categorical() ~ "{n}"
  ),
  digits = list(all_continuous() ~ 1)
) %>% 
  add_n()  

tbl_weighted <- tbl_svysummary(
  baseline,
  by = sedentarismo,                      
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(
    all_categorical() ~ "{p}%"
  ),
  digits = list(all_categorical() ~ 1)
)

tbl_combined <- tbl_merge(
  tbls = list(tbl_unweighted, tbl_weighted),
  tab_spanner = c("**Unweighted (N)**", "**Weighted (%)**")
)

tbl_final <- tbl_combined %>% 
  bold_labels() %>% 
  modify_caption("**Baseline Characteristics by Sedentarism**<br>**Ages 6 to 15**<br>n = unweighted counts, % = weighted proportions")
tbl_final

#### Descriptive on Clean Overall ####
## Update sedentarismo as numeric for survey function
dt_clean <- dt_clean %>%
  mutate(sedentarismo = ifelse(sedentarismo == "Yes", 1, 0))

# version 1
overall_prevalence <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")) %>%
  select(survey, sedentarismo, sedentarismo_low, sedentarismo_upp) %>%
  gt() %>%
  fmt_percent(columns = starts_with("sedentarismo"), decimals = 1) %>%
  cols_label(survey = "Survey",
             sedentarismo = "Prevalence",
             sedentarismo_low = "Lower CI",
             sedentarismo_upp = "Upper CI") %>% 
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Survey Year**<br>**Ages 6 to 15**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )
overall_prevalence

#### Descriptive by Sex Survey Weights ####
# Female prevalence summary
prev_female <- dt_clean %>%
  filter(sexo == "Female") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_female = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Male prevalence summary
prev_male <- dt_clean %>%
  filter(sexo == "Male") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_male = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

prevalence_joined_sex <- left_join(prev_female, prev_male, by = "survey")

prev_joined_sex <- prevalence_joined_sex %>%
  gt() %>%
  fmt_percent(columns = starts_with("prevalence"), decimals = 1) %>%
  cols_label(
    survey = "Survey",
    prevalence_female = "Female Prevalence",
    prevalence_female_low = "",
    prevalence_female_upp = "",
    prevalence_male = "Male Prevalence",
    prevalence_male_low = "",
    prevalence_male_upp = ""
  ) %>%
  cols_align(
    align = "center", columns = c("prevalence_female", "prevalence_male")) %>% 
  tab_header(title = md("**Sedentarism Prevalence by Sex and Survey Year**<br>**Ages 6 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_sex

#### Descriptive by Class 6 categories Survey Weights ####
# List of class categories
class_levels <- c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI")

# Compute prevalence per survey per class 
prevalence_by_class <- map_dfr(class_levels, function(cls) { #map_dfr binds rows
  dt_clean %>%
    filter(clase == cls) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(
      prevalence = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(clase = cls)
})

# Pivot wider so each class is a column
prevalence_wide <- prevalence_by_class %>%
  select(survey, clase, prevalence, prevalence_low, prevalence_upp) %>%
  pivot_wider(
    names_from = clase,
    values_from = c(prevalence, prevalence_low, prevalence_upp),
    names_glue = "{clase}_{.value}"
  )

prevalence_wide <- prevalence_wide %>%
  mutate(across(ends_with("_prevalence"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_low"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_upp"), ~ round(.x * 100, 1)))

prevalence_wide_str <- prevalence_wide %>%
  mutate(
    `Class I` = paste0(round(`Class I_prevalence`, 1), "% (", round(`Class I_prevalence_low`, 1), "% - ", round(`Class I_prevalence_upp`, 1), "%)"),
    `Class II` = paste0(round(`Class II_prevalence`, 1), "% (", round(`Class II_prevalence_low`, 1), "% - ", round(`Class II_prevalence_upp`, 1), "%)"),
    `Class III` = paste0(round(`Class III_prevalence`, 1), "% (", round(`Class III_prevalence_low`, 1), "% - ", round(`Class III_prevalence_upp`, 1), "%)"),
    `Class IV` = paste0(round(`Class IV_prevalence`, 1), "% (", round(`Class IV_prevalence_low`, 1), "% - ", round(`Class IV_prevalence_upp`, 1), "%)"),
    `Class V` = paste0(round(`Class V_prevalence`, 1), "% (", round(`Class V_prevalence_low`, 1), "% - ", round(`Class V_prevalence_upp`, 1), "%)"),
    `Class VI` = paste0(round(`Class VI_prevalence`, 1), "% (", round(`Class VI_prevalence_low`, 1), "% - ", round(`Class VI_prevalence_upp`, 1), "%)")
  ) %>%
  select(survey, `Class I`, `Class II`, `Class III`, `Class IV`, `Class V`, `Class VI`)

# formatted table
prevalence_wide_str %>%
  gt() %>%
  cols_label(
    survey = "Survey"
  ) %>%
  cols_align(
    align = "center", columns = vars(survey)) %>% 
  cols_align(
    align = "center", columns = vars(`Class I`, `Class II`, `Class III`, `Class IV`, `Class V`, `Class VI`)
  ) %>% 
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**<br>**Ages 6 to 15**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )
prevalence_wide_str
clipr::write_clip(prevalence_wide_str)

#### Descriptive by Class 3 categories Survey Weights ####
# List of class categories
class_levels3 <- c("Class I", "Class II", "Class III")

# Compute prevalence per survey per class 
prevalence_by_class <- map_dfr(class_levels3, function(cls) { #map_dfr binds rows
  dt_clean %>%
    filter(clase_3 == cls) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(
      prevalence = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(clase_3 = cls)
})

# Pivot wider so each class is a column
prevalence_wide <- prevalence_by_class %>%
  select(survey, clase_3, prevalence, prevalence_low, prevalence_upp) %>%
  pivot_wider(
    names_from = clase_3,
    values_from = c(prevalence, prevalence_low, prevalence_upp),
    names_glue = "{clase_3}_{.value}"
  )

prevalence_wide <- prevalence_wide %>%
  mutate(across(ends_with("_prevalence"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_low"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_upp"), ~ round(.x * 100, 1)))

prevalence_wide_str <- prevalence_wide %>%
  mutate(
    `Class I` = paste0(round(`Class I_prevalence`, 1), "% (", round(`Class I_prevalence_low`, 1), "% - ", round(`Class I_prevalence_upp`, 1), "%)"),
    `Class II` = paste0(round(`Class II_prevalence`, 1), "% (", round(`Class II_prevalence_low`, 1), "% - ", round(`Class II_prevalence_upp`, 1), "%)"),
    `Class III` = paste0(round(`Class III_prevalence`, 1), "% (", round(`Class III_prevalence_low`, 1), "% - ", round(`Class III_prevalence_upp`, 1), "%)")
  ) %>%
  select(survey, `Class I`, `Class II`, `Class III`)

# formatted table
prevalence_wide_str %>%
  gt() %>%
  fmt_percent(decimals = 1) %>%
  cols_label(
    survey = "Survey"
  ) %>%
  cols_align(
    align = "center", columns = vars(survey)) %>% 
  cols_align(
    align = "center", columns = vars(`Class I`, `Class II`, `Class III`)
  ) %>% 
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**<br>**Ages 6 to 15**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )
prevalence_wide_str
clipr::write_clip(prevalence_wide_str)

#### Descriptive by Age Survey Weights ####
summary(dt_clean$edad)
hist(dt_clean$edad)

# Function for each edad_cat3 group
get_prevalence3 <- function(data, age_group) {
  data %>%
    filter(edad_cat3 == age_group) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(sedentarismo = survey_mean(sedentarismo, vartype = "ci")) %>%
    mutate(edad_cat3 = age_group)
}

# Apply function to each group and bind results
combined_prevalence3 <- bind_rows(
  get_prevalence3(dt_clean, "6-9"),
  get_prevalence3(dt_clean, "10-12"),
  get_prevalence3(dt_clean, "13-15")
)

# Prepare wide table
combined_wide3 <- combined_prevalence3 %>%
  mutate(age_str = case_when(
    edad_cat3 == "6-9" ~ "6-9 years",
    edad_cat3 == "10-12" ~ "10-12 years",
    edad_cat3 == "13-15" ~ "13-15 years"
  )) %>%
  mutate(
    prevalence_str = paste0(
      round(sedentarismo * 100, 1), "% (",
      round(sedentarismo_low * 100, 1), "–",
      round(sedentarismo_upp * 100, 1), "%)"
    )
  ) %>%
  select(survey, age_str, prevalence_str) %>%
  pivot_wider(
    names_from = age_str,
    values_from = prevalence_str
  )

# Display with gt
combined_wide3 %>%
  gt() %>%
  cols_label(survey = "Survey") %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Age Group**<br>**Ages 6 to 15**")
  ) %>%
  tab_options(table.font.size = "small")
clipr::write_clip(combined_wide3)

#### Descriptive by Autonomous Community Survey Weights ####
## weighted prevalence by ccaa and survey
prevalence_by_ccaa <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey, ccaa) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_ccaa
save(prevalence_by_ccaa, file = "Datasets/clase_tr_2/new/prevalence_by_ccaa.RData")

prevalence_by_ccaa_m <- dt_clean %>%
  filter(sexo == "Male") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey, ccaa) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_ccaa_m
clipr::write_clip(prevalence_by_ccaa_m)
save(prevalence_by_ccaa_m, file = "Datasets/clase_tr_2/new/prevalence_by_ccaa_m.RData")

prevalence_by_ccaa_f <- dt_clean %>%
  filter(sexo == "Female") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey, ccaa) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_ccaa_f
clipr::write_clip(prevalence_by_ccaa_f)
save(prevalence_by_ccaa_f, file = "Datasets/clase_tr_2/new/prevalence_by_ccaa_f.RData")

## prevalence and CI as strings
prevalence_ccaa_str <- prevalence_by_ccaa %>%
mutate(
  prevalence_fmt = paste0(
    round(prevalence * 100, 1), "% (",
    round(prevalence_low * 100, 1), "% - ",
    round(prevalence_upp * 100, 1), "%)"
  )
)

# set columns as ccaa
prevalence_ccaa_wide <- prevalence_ccaa_str %>%
  select(ccaa, survey, prevalence_fmt) %>%
  pivot_wider(
    names_from = survey,
    values_from = prevalence_fmt
  )

# table
prevalence_ccaa_wide %>%
  gt() %>%
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Autonomous Community and Survey Year**<br>**Ages 6 to 15**")
  ) %>%
  cols_label(
    ccaa = "Autonomous Community") %>%
  cols_align(
    align = "center", columns = c("2003", "2006", "2011", "2017", "2023")
  ) %>% 
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )
clipr::write_clip(prevalence_ccaa_wide)

## test
table(dt_clean$survey[dt_clean$ccaa == "Extremadura"], 
      dt_clean$sedentarismo[dt_clean$ccaa == "Extremadura"])

#### Descriptive by NUTS1 Survey Weights ####
prevalence_by_nuts <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey, NUTS1) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_nuts
clipr::write_clip(prevalence_by_nuts)
save(prevalence_by_nuts, file = "Datasets/clase_tr_2/new/prevalence_by_nuts.RData")

prevalence_by_nuts_f <- dt_clean %>%
  filter(sexo == "Female") %>% 
  as_survey_design(weights = factor2) %>%
  group_by(survey, NUTS1) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_nuts_f
clipr::write_clip(prevalence_by_nuts_f)
save(prevalence_by_nuts, file = "Datasets/clase_tr_2/new/prevalence_by_nuts_f.RData")

prevalence_by_nuts_m <- dt_clean %>%
  filter(sexo == "Male") %>% 
  as_survey_design(weights = factor2) %>%
  group_by(survey, NUTS1) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")*100
  ) %>%
  ungroup()

prevalence_by_nuts_m
clipr::write_clip(prevalence_by_nuts_m)
save(prevalence_by_nuts_m, file = "Datasets/clase_tr_2/new/prevalence_by_nuts_m.RData")

#### Descriptive by Nationality Survey Weights ####
# Spanish prevalence summary
prev_spanish <- dt_clean %>%
  filter(nacionalidad == "Spanish") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_spanish = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Foreign prevalence summary
prev_foreign <- dt_clean %>%
  filter(nacionalidad == "Foreign") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_foreign = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

prevalence_joined_nationality <- left_join(prev_spanish, prev_foreign, by = "survey")

prev_joined_nationality <- prevalence_joined_nationality %>%
  gt() %>%
  fmt_percent(columns = starts_with("prevalence"), decimals = 1) %>%
  cols_label(
    survey = "Survey",
    prevalence_spanish = "Spanish Prevalence",
    prevalence_spanish_low = "",
    prevalence_spanish_upp = "",
    prevalence_foreign = "Foreign Prevalence",
    prevalence_foreign_low = "",
    prevalence_foreign_upp = ""
  ) %>%
  cols_align(
    align = "center", columns = c("prevalence_spanish", "prevalence_foreign")) %>% 
  tab_header(title = md("**Sedentarism Prevalence by Nationality and Survey Year**<br>**Ages 6 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_nationality
clipr::write_clip(prev_joined_nationality)

#### Descriptive by Urban/Rural Survey Weights ####
# Urban prevalence summary
prev_urb <- dt_clean %>%
  filter(urb_rur == "Urban") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_urb = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Semi-urban prevalence summary
prev_semi <- dt_clean %>%
  filter(urb_rur == "Semi-urban") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_semi = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Rural prevalence summary
prev_rur <- dt_clean %>%
  filter(urb_rur == "Rural") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_rur = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

prevalence_joined_urb <- left_join(prev_urb, prev_semi, by = "survey")
prevalence_joined_urb <- left_join(prevalence_joined_urb, prev_rur, by = "survey")
prevalence_joined_urb

prev_joined_urb <- prevalence_joined_urb %>%
  gt() %>%
  fmt_percent(columns = starts_with("prevalence"), decimals = 1) %>%
  cols_label(
    survey = "Survey",
    prevalence_urb = "Urban Prevalence",
    prevalence_urb_low = "",
    prevalence_urb_upp = "",
    prevalence_semi = "Semi-Urban Prevalence",
    prevalence_semi_low = "",
    prevalence_semi_upp = "",
    prevalence_rur = "Rural Prevalence",
    prevalence_rur_low = "",
    prevalence_rur_upp = ""
  ) %>%
  cols_align(
    align = "center", columns = c("prevalence_urb", "prevalence_semi", "prevalence_rur")) %>% 
  tab_header(title = md("**Sedentarism Prevalence by Urban/Rural and Survey Year**<br>**Ages 6 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_urb

#### Prevalences Dataset ####
# Prevalence by year and combined boys and girls
prevalences_spain_overall <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci")*100,
  ) %>%
  mutate(sexo="Overall")
prevalences_spain_overall
save(prevalences_spain_overall, file = "Datasets/clase_tr_2/new/prevalences_spain_overall.RData")

## wide table overall
prevalences_spain_overall_wide <- prevalences_spain_overall %>%
  pivot_wider(
    names_from = survey,
    values_from = c(sedentarismo, sedentarismo_low, sedentarismo_upp)
  )
prevalences_spain_overall_wide
clipr::write_clip(prevalences_spain_overall_wide)

# Prevalence by year and separate boys and girls
prevalences_spain_sexo <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci")*100,
  ) %>%
  mutate(sexo=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalences_spain_sexo

## wide table
prevalences_spain_sexo_wide <- prevalences_spain_sexo %>%
  pivot_wider(
    names_from = survey,
    values_from = c(sedentarismo, sedentarismo_low, sedentarismo_upp)
  )
prevalences_spain_sexo_wide
clipr::write_clip(prevalences_spain_sexo_wide)

# Prevalence by year and per age group
prevalences_spain_age <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(edad_cat3, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci")*100,
  ) %>%
  mutate(edad_cat3=case_when(edad_cat3=="6-9"~"6-9", edad_cat3=="10-12"~"10-12", edad_cat3=="13-15"~"13-15"))
prevalences_spain_age
clipr::write_clip(prevalences_spain_age)

prevalences_spain <- bind_rows(
  prevalences_spain_overall,
  prevalences_spain_sexo,
  prevalences_spain_age
)
View(prevalences_spain)

prevalences_spain <- prevalences_spain %>%
  mutate(survey = as.numeric(survey))
save(prevalences_spain, file = "Datasets/clase_tr_2/new/prevalences_spain.RData")

# Prevalence by social class and year combined boys and girls
prevalence_class_overall <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(clase, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
prevalence_class_overall

# Prevalence by social class 3
prevalence_class_overall3 <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(clase_3, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci")*100,
  ) %>%
  mutate(sex="Overall")
prevalence_class_overall3

## wide table
prevalence_class_overall_wide <- prevalence_class_overall %>%
  pivot_wider(
    names_from = survey,
    values_from = c(sedentarismo, sedentarismo_low, sedentarismo_upp)
  )
prevalence_class_overall_wide
clipr::write_clip(prevalence_class_overall_wide)

## wide table 3
prevalence_class_overall_wide3 <- prevalence_class_overall3 %>%
  pivot_wider(
    names_from = survey,
    values_from = c(sedentarismo, sedentarismo_low, sedentarismo_upp)
  )
prevalence_class_overall_wide3
clipr::write_clip(prevalence_class_overall_wide3)

# Prevalence by social class and year separate boys and girls
prevalence_class_sexo <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, clase, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_class_sexo

# Prevalence by social class 3 and year separate boys and girls
prevalence_class_sexo3 <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, clase_3, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci")*100,
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_class_sexo3

# Prevalence by class together
prevalence_class <- prevalence_class_overall %>% 
  rbind(prevalence_class_sexo) 
save(prevalence_class, file = "Datasets/clase_tr_2/new/prevalence_class.RData")
print(prevalence_class, n = 90)
clipr::write_clip(prevalence_class)

# Prevalence by class 3 together
prevalence_class3 <- prevalence_class_overall3 %>% 
  rbind(prevalence_class_sexo3) 
save(prevalence_class, file = "Datasets/clase_tr_2/new/prevalence_class3.RData")
prevalence_class3
clipr::write_clip(prevalence_class3)

# Prevalence by CCAA, social class, and year combined boys and girls
prevalence_overall_class_ccaa <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(ccaa, clase, survey) %>%
  summarize(poblacion = survey_total(sedentarismo, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95) , 
            sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
prevalence_overall_class_ccaa

# Prevalence by CCAA, social class, and year combined boys and girls
prevalence_overall_class_ccaa3 <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(ccaa, clase_3, survey) %>%
  summarize(poblacion = survey_total(sedentarismo, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95) , 
            sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
prevalence_overall_class_ccaa3

# Prevalence by CCAA and year combined boys and girls
prevalence_overall_ccaa <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(ccaa, survey) %>%
  summarize(poblacion = survey_total(sedentarismo, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95) , 
            sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
print(prevalence_overall_ccaa, n=90)
View(prevalence_overall_ccaa)

# Prevalence by CCAA, social class, and year separate boys and girls
prevalence_sex_class_ccaa <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, ccaa, clase, survey) %>%
  summarize(poblacion = survey_total(sedentarismo, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95,) , 
            sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_sex_class_ccaa

# Prevalence by CCAA, social class 3, and year separate boys and girls
prevalence_sex_class_ccaa3 <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, ccaa, clase_3, survey) %>%
  summarize(poblacion = survey_total(sedentarismo, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95,) , 
            sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_sex_class_ccaa3

# Prevalence by CCAA, social class, and year together
prevalence_class_ccaa <- prevalence_overall_class_ccaa %>% 
  rbind(prevalence_sex_class_ccaa)
save(prevalence_class_ccaa, file = "Datasets/clase_tr_2/new/prevalence_class_ccaa.RData")
clipr::write_clip(prevalence_class_ccaa)
prevalence_class_ccaa

# Prevalence by CCAA, social class 3, and year together
prevalence_class_ccaa3 <- prevalence_overall_class_ccaa3 %>% 
  rbind(prevalence_sex_class_ccaa3)
save(prevalence_class_ccaa3, file = "Datasets/clase_tr_2/new/prevalence_class_ccaa3.RData")
clipr::write_clip(prevalence_class_ccaa3)
prevalence_class_ccaa3

#### Set Graph Theme ####
theme_inequalities <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      # Force white backgrounds
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Grid lines
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # Axes
      axis.title       = element_text(size = 12, face = "bold"),
      axis.text        = element_text(size = 10),
      
      # Title + subtitle
      plot.title       = element_text(size = 15, face = "bold"),
      plot.subtitle    = element_text(size = 12, color = "grey30"),
      
      # Legend
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = NA)
    )
}
#### Visualization Descriptive Sedentarism Overall ####
load("Datasets/clase_tr_2/new/prevalences_spain.RData")

prevalences_spain$survey <- as.numeric(as.character(prevalences_spain$survey))

prevalences_spain <- prevalences_spain %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo, 1), "%")
  )

prevalences_spain <- prevalences_spain %>%
  mutate(sexo = case_when(
    sexo == "Female" ~ "Girls",
    sexo == "Male" ~ "Boys",
    TRUE ~ sexo  # keep any other values as they are
  ))

# Both sexes
fig_desc_sedentarism_overall <- prevalences_spain %>%
  filter(sexo == "Overall") %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo,
    ymin = sedentarismo_low,
    ymax = sedentarismo_upp,
    color = sexo,
    fill = sexo
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 30, by = 10),
    limits = c(0, 30)
  ) +
  geom_text_repel(
    aes(label = prevalence_label, family = "Times New Roman"),
    size = 4,         # Adjust for readability
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "#4BAE48"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Overall" = "#4BAE48"
    )) +
  labs(
    title = "Prevalence of Sedentarism Over Time (Girls and Boys)",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities()+
  theme(text = element_text(family = "Times New Roman"))
fig_desc_sedentarism_overall
ggsave("Figures/prevalence/fig_desc_sedentarism_overall.png", width = 4000, height = 2200, dpi=300, units = "px")

# Separate by sex
fig_desc_sedentarism_sex <- prevalences_spain %>%
  filter(sexo %in% c("Girls", "Boys", "Overall")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo,
    ymin = sedentarismo_low,
    ymax = sedentarismo_upp,
    color = sexo,
    fill = sexo
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  geom_text_repel(
    aes(label = prevalence_label, family = "Times New Roman"),
    size = 4,         # Adjust for readability
    nudge_y = 1,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20",
      "Overall" = "#4BAE48"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Girls"   = "#317AB6",
      "Boys"    = "#E41E20",
      "Overall" = "#4BAE48"
    )
    ) +
  labs(
    title = "Prevalence of Sedentarism Over Time by Sex",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities()+
  theme(text = element_text(family = "Times New Roman"))
fig_desc_sedentarism_sex
ggsave("Figures/prevalence/fig_desc_sedentarism_sex_new.png", width = 4000, height = 2200, dpi=300, units = "px")

# Separate by age group
prevalences_spain <- prevalences_spain %>%
  mutate(
    edad_cat3 = factor(
      edad_cat3,
      levels = c("6-9", "10-12", "13-15")
    )
  )

fig_desc_sedentarism_age <- prevalences_spain %>%
  filter(edad_cat3 %in% c("6-9", "10-12", "13-15")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo,
    ymin = sedentarismo_low,
    ymax = sedentarismo_upp,
    color = edad_cat3,
    fill = edad_cat3
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  geom_text_repel(
    aes(label = prevalence_label, family = "Times New Roman"),
    size = 3.5,
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "6-9" = "#f03b20",
      "10-12" = "#2c7fb8",
      "13-15" = "#2c7"
    )
  ) +
  scale_fill_manual(
    values = c(
      "6-9" = "#f03b20",
      "10-12" = "#2c7fb8",
      "13-15" = "#2c7"
    )
  )  +
  labs(
    title = "Prevalence of Sedentarism Over Time by Age Group",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Age",
    fill = "Age"
  ) +
  theme_inequalities()+
  theme(text = element_text(family = "Times New Roman"))
fig_desc_sedentarism_age
ggsave("Figures/prevalence/fig_desc_sedentarism_age.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure prevalence by social class
load("prevalence_class.RData")
load("Datasets/clase_tr_2/new/prevalence_class3.RData")

prevalence_class$survey <- as.numeric(as.character(prevalence_class$survey))
prevalence_class3$survey <- as.numeric(as.character(prevalence_class3$survey))

prevalence_class3 <- prevalence_class3 %>%
  mutate(sexo = case_when(
    sexo == "Female" ~ "Girls",
    sexo == "Male" ~ "Boys",
    TRUE ~ sexo  # keep any other values as they are
  ))

prevalence_class <- prevalence_class %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

prevalence_class3 <- prevalence_class3 %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo, 1), "%")
  )

fig_desc_sedentarism_class_overall <- prevalence_class %>%
  filter(sex == "Overall") %>%
  ggplot(aes(x = survey, y = sedentarismo * 100, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100, color = clase, fill = clase)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  scale_y_continuous(
    expand = c(0, 0), 
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  geom_text_repel(
    aes(label = prevalence_label),
    size = 3,         # Adjust for readability
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3",
      "Class IV" = "#e7298a",
      "Class V" = "#66a61e",
      "Class VI" = "#e6ab02"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3",
      "Class IV" = "#e7298a",
      "Class V" = "#66a61e",
      "Class VI" = "#e6ab02"
    )
  ) +
  labs(
    title = "Prevalence of Sedentarism Over Time by Social Class (Girls and Boys)",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Social Class",
    fill = "Social Class"
  ) +
  theme_inequalities()
fig_desc_sedentarism_class_overall
ggsave("Figures/02-12/fig_desc_sedentarism_class_overall.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure for Class 3 categories
fig_desc_sedentarism_class_overall3 <- prevalence_class3 %>%
  filter(sex == "Overall") %>%
  ggplot(aes(x = survey, y = sedentarismo, ymin = sedentarismo_low, ymax = sedentarismo_upp, color = clase_3, fill = clase_3)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text_repel(
    aes(label = prevalence_label, family = "Times New Roman"),
    size = 4,         # Adjust for readability
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  scale_y_continuous(
    expand = c(0, 0), 
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3"
    )
  ) +
  labs(
    title = "Prevalence of Sedentarism Over Time by Social Class (Girls and Boys)",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Social Class",
    fill = "Social Class"
  ) +
  theme_inequalities()+
  theme(text = element_text(family = "Times New Roman"))
fig_desc_sedentarism_class_overall3
ggsave("Figures/prevalence/fig_desc_sedentarism_class_overall3.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure prevalence social class by sex
fig_desc_sedentarism_class_sex <- prevalence_class %>%
  filter(sex %in% c("Female", "Male")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = clase,
    fill = clase
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text_repel(
    aes(label = prevalence_label),
    size = 3,         # Adjust for readability
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~ sex) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 50, by = 10),
    limits = c(0, 50)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3",
      "Class IV" = "#e7298a",
      "Class V" = "#66a61e",
      "Class VI" = "#e6ab02"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3",
      "Class IV" = "#e7298a",
      "Class V" = "#66a61e",
      "Class VI" = "#e6ab02"
    )
  ) +
  labs(
    title = "Prevalence of Sedentarism Over Time by Social Class and Sex",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Social Class",
    fill = "Social Class"
  ) +
  theme_inequalities()
fig_desc_sedentarism_class_sex
ggsave("Figures/02-12/fig_desc_sedentarism_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure prevalence social class 3 categories by sex
fig_desc_sedentarism_class_sex3 <- prevalence_class3 %>%
  filter(sexo %in% c("Girls", "Boys")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo,
    ymin = sedentarismo_low,
    ymax = sedentarismo_upp,
    color = clase_3,
    fill = clase_3
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text_repel(
    aes(label = prevalence_label, family = "Times New Roman"),
    size = 4,         
    nudge_y = 0.5,
    max.overlaps = Inf,
    show.legend =  FALSE,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~ sexo) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#1b9e77",
      "Class II" = "#d95f02",
      "Class III" = "#7570b3"
    )
  ) +
  labs(
    title = "Prevalence of Sedentarism Over Time by Social Class and Sex",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Social Class",
    fill = "Social Class"
  ) +
  theme_inequalities()+
  theme(text = element_text(family = "Times New Roman"))
fig_desc_sedentarism_class_sex3
ggsave("Figures/prevalence/fig_desc_sedentarism_class_sex3.png", width = 4000, height = 2200, dpi=300, units = "px")
