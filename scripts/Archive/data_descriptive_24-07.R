# Data Descriptive INE
# Finalized 24th of July 2025

## Load libraries
library(tidyverse)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)

## read joined data
dt <- get(load("joined.RData"))
View(dt)
dt_clean <- get(load("joined_clean.RData"))

dt_clean <- dt_clean %>% 
  mutate(
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III"))
  )

#### Baseline Characteristics without Survey Weights ####
## all with NAs
tbl_summary(
  dt,
  by = survey,  # or ~survey if you prefer formula style
  include = all_of(c("edad", "sexo", "nacionalidad", "sedentarismo",
                     "clase", "urb_rur", "ccaa")),
  label = list(
    edad ~ "Child's Age (years)",
    sexo ~ "Child's Sex",
    nacionalidad ~ "Child's Nationality",
    sedentarismo ~ "Sedentarism",
    clase ~ "Household Social Class",
    urb_rur ~ "Urban/Rural",
    ccaa ~ "Autonomous Community"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)", 
    all_continuous() ~ "{median} ({p25}, {p75})"
  ),
  digits = list(all_continuous() ~ 1),
  missing = "always"
) %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics by Survey Year (Unweighted)**<br>**Ages 0 to 15**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  modify_footnote(~ "Values are presented as Median (IQR) for continuous variables and n (%) for categorical variables.")

#### Baseline Characteristics with Survey Weights ####
my_labels <- set_names(
  list(
    "Age (years)",
    "Sex",
    "Nationality",
    "Sedentarism",
    "Household Social Class",
    "Urban/Rural",
    "Autonomous Community"
  ),
  c("edad_cat", "sexo", "nacionalidad", "sedentarismo", "clase",
    "urb_rur", "ccaa")
)

valid_vars <- intersect(names(my_labels), names(dt))

# Create unweighted table (counts only)
tbl_unweighted <- tbl_summary(
  data = dt,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{n}", all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_continuous() ~ 1),
  missing = "always") %>% 
  add_n()

# Create weighted table (percentages only)
baseline <- svydesign(ids = ~1, weights = ~factor2, data = dt)
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
  statistic = list(all_categorical() ~ "{p}%", all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_categorical() ~ 1),
  missing = "always"
)

# Merge both tables
tbl_combined <- tbl_merge(
  tbls = list(tbl_unweighted, tbl_weighted),
  tab_spanner = c("**Unweighted (N)**", "**Weighted (%)**")
)

# Final table
tbl_final <- tbl_combined %>% 
  bold_labels() %>% 
  modify_caption("**Baseline Characteristics by Survey Year with Missing**<br>**Ages 0 to 15**<br>n = unweighted counts, % = weighted proportions")
tbl_final

# with dt_clean
# Create unweighted table (counts only)
tbl_unweighted <- tbl_summary(
  data = dt_clean,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{n}", all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_continuous() ~ 1),
  missing = "no") %>% 
  add_n()

# Create weighted table (percentages only)
baseline_clean <- svydesign(ids = ~1, weights = ~factor2, data = dt_clean)

tbl_weighted <- tbl_svysummary(
  baseline_clean,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{p}%", 
                   all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_categorical() ~ c(1, 1, 1)),
  missing = "no"
)

# Merge both tables
tbl_combined <- tbl_merge(
  tbls = list(tbl_unweighted, tbl_weighted),
  tab_spanner = c("**Unweighted (N)**", "**Weighted (%)**")
)

# Final table
tbl_final_drop <- tbl_combined %>% 
  bold_labels() %>% 
  modify_caption("**Baseline Characteristics by Survey Year without Missing**<br>**Ages 0 to 15**<br>n = unweighted counts, % = weighted proportions")
tbl_final_drop

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
    title = md("**Sedentarism Prevalence in Spain by Survey Year**<br>**Ages 0 to 15**")
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
  tab_header(title = md("**Sedentarism Prevalence by Sex and Survey Year**<br>**Ages 0 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_sex

#### Descriptive by Class Survey Weights ####
# List of class categories
class_levels <- c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI")
class_levels3 <- c("Class I", "Class II", "Class III")

# Compute prevalence per survey per class 
prevalence_by_class <- map_dfr(class_levels, function(cls) { #map_dfr binds rows
  dt_clean %>%
    filter(clase == cls) %>%
    #filter(edad >= 5) %>% 
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(
      prevalence = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(clase = cls)
})
prevalence_by_class <- map_dfr(class_levels3, function(cls) { #map_dfr binds rows
  dt_clean %>%
    filter(clase_3 == cls) %>%
    #filter(edad >= 5) %>% 
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(
      prevalence = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
    ) %>%
    mutate(clase_3 = cls)
})

# Pivot wider so each class is a column
prevalence_wide <- prevalence_by_class %>%
  select(survey, clase, prevalence, prevalence_low, prevalence_upp) %>%
  pivot_wider(
    names_from = clase,
    values_from = c(prevalence, prevalence_low, prevalence_upp),
    names_glue = "{clase}_{.value}"
  )

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
    `Class III` = paste0(round(`Class III_prevalence`, 1), "% (", round(`Class III_prevalence_low`, 1), "% - ", round(`Class III_prevalence_upp`, 1), "%)"),
    `Class IV` = paste0(round(`Class IV_prevalence`, 1), "% (", round(`Class IV_prevalence_low`, 1), "% - ", round(`Class IV_prevalence_upp`, 1), "%)"),
    `Class V` = paste0(round(`Class V_prevalence`, 1), "% (", round(`Class V_prevalence_low`, 1), "% - ", round(`Class V_prevalence_upp`, 1), "%)"),
    `Class VI` = paste0(round(`Class VI_prevalence`, 1), "% (", round(`Class VI_prevalence_low`, 1), "% - ", round(`Class VI_prevalence_upp`, 1), "%)")
  ) %>%
  select(survey, `Class I`, `Class II`, `Class III`, `Class IV`, `Class V`, `Class VI`)

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
  cols_label(
    survey = "Survey"
  ) %>%
  cols_align(
    align = "center", columns = vars(survey)) %>% 
  cols_align(
    align = "center", columns = vars(`Class I`, `Class II`, `Class III`, `Class IV`, `Class V`, `Class VI`)
  ) %>% 
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**<br>**Ages 0 to 15**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )
prevalence_wide_str
clipr::write_clip(prevalence_wide_str)

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
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**<br>**Ages 0 to 15**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )
prevalence_wide_str
clipr::write_clip(prevalence_wide_str)

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
  tab_header(title = md("**Sedentarism Prevalence by Nationality and Survey Year**<br>**Ages 0 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_nationality

#### Descriptive by Age Survey Weights ####
summary(dt_clean$edad)
hist(dt_clean$edad)

# categorize age into infants, primaria, ESO
dt_clean <- dt_clean %>%
  mutate(edad_cat = case_when(
    edad >= 0 & edad <= 5 ~ "0-5",
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15"
  ))
dt_clean$edad_cat <- factor(dt_clean$edad_cat, levels = c("0-5", "6-11", "12-15"))
summary(dt_clean$edad_cat)
# 0-5   6-11   12-15 
# 10153 11092  8408  

# function for each age group
get_prevalence <- function(data, age_group) {
  data %>%
    filter(edad_cat == age_group) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(sedentarismo = survey_mean(sedentarismo, vartype = "ci")) %>%
    mutate(edad_cat = age_group)
}

# apply function to each group and bind results
combined_prevalence <- bind_rows(
  get_prevalence(dt_clean, "0-5"),
  get_prevalence(dt_clean, "6-11"),
  get_prevalence(dt_clean, "12-15")
)

# years as columns
combined_wide <- combined_prevalence %>%
  mutate(age_str = case_when(
    edad_cat == "0-5" ~ "0-5 years",
    edad_cat == "6-11" ~ "6-11 years",
    edad_cat == "12-15" ~ "12-15 years"
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
combined_wide %>%
  gt() %>%
  cols_label(survey = "Survey") %>%
  cols_align(
    align = "center", columns = c(survey)
  ) %>%
  cols_align(
    align = "center", columns = c("0-5 years", "6-11 years", "12-15 years")
  ) %>%
  tab_header(title = md("**Sedentarism Prevalence in Spain by Age Group**<br>**Ages 0 to 15**")) %>%
  tab_options(table.font.size = "small")

# table(dt_clean$edad_cat, dt_clean$survey)
#       2003 2006 2011 2017 2023
# 0-5   2013 3169 2035 1910 1026
# 6-11  2290 3094 1949 2338 1421
# 12-15 2079 2544 1116 1570 1099

#### Descriptive by Autonomous Community Survey Weights ####
## weighted prevalence by ccaa and survey
prevalence_by_ccaa <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey, ccaa) %>%
  summarize(
    prevalence = survey_mean(sedentarismo, vartype = "ci")
  ) %>%
  ungroup()

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
    title = md("**Sedentarism Prevalence in Spain by Autonomous Community and Survey Year**<br>**Ages 0 to 15**")
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
  tab_header(title = md("**Sedentarism Prevalence by Nationality and Survey Year**<br>**Ages 0 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_nationality

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
  tab_header(title = md("**Sedentarism Prevalence by Urban/Rural and Survey Year**<br>**Ages 0 to 15**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_urb

#### Prevalences Dataset ####
# Prevalence by year and combined boys and girls
prevalences_spain_overall <- dt_clean %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sexo="Overall")
prevalences_spain_overall

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
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
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
  group_by(edad_cat, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(edad_cat=case_when(edad_cat=="0-5"~"0-5", edad_cat=="6-11"~"6-11", edad_cat=="12-15"~"12-15"))
prevalences_spain_age
clipr::write_clip(prevalences_spain_age)

# Prevalence just by year together
prevalences_spain <- prevalences_spain_overall %>% 
  rbind(prevalences_spain_sexo) %>% 
  rbind(prevalences_spain_age)

save(prevalence_spain, file = "prevalence_spain.RData")
clipr::write_clip(prevalences_spain)
print(prevalences_spain, n = 90)

prevalences_spain <- bind_rows(
  prevalences_spain_overall,
  prevalences_spain_sexo,
  prevalences_spain_age
)
View(prevalences_spain)

prevalences_spain <- prevalences_spain %>%
  mutate(survey = as.numeric(survey))

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
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
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
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_class_sexo3

# Prevalence by class together
prevalence_class <- prevalence_class_overall %>% 
  rbind(prevalence_class_sexo) 
save(prevalence_class, file = "prevalence_class.RData")
print(prevalence_class, n = 90)
clipr::write_clip(prevalence_class)

# Prevalence by class 3 together
prevalence_class3 <- prevalence_class_overall3 %>% 
  rbind(prevalence_class_sexo3) 
save(prevalence_class, file = "prevalence_class3.RData")
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
save(prevalence_class_ccaa, file = "prevalence_class_ccaa.RData")
clipr::write_clip(prevalence_class_ccaa)
prevalence_class_ccaa

# Prevalence by CCAA, social class 3, and year together
prevalence_class_ccaa3 <- prevalence_overall_class_ccaa3 %>% 
  rbind(prevalence_sex_class_ccaa3)
save(prevalence_class_ccaa3, file = "prevalence_class_ccaa3.RData")
clipr::write_clip(prevalence_class_ccaa3)
prevalence_class_ccaa3

# sedentarism_prevalences <- prevalences_spain %>% ### unable because columns do not match, kept separate
#   rbind(prevalence_class) %>%
#   rbind(prevalence_class_ccaa)
# save(sedentarism_prevalences, file = "sedentarism_prevalences.RData")

#### Prevalences Dataset Over 5 ----
dt_clean_5 <- dt_clean %>% 
  dplyr::filter(edad >= 5)

summary(dt_clean_5)

# Prevalence by year and combined boys and girls
prevalences_spain_overall <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sexo="Overall")
prevalences_spain_overall

## wide table overall
prevalences_spain_overall_wide <- prevalences_spain_overall %>%
  pivot_wider(
    names_from = survey,
    values_from = c(sedentarismo, sedentarismo_low, sedentarismo_upp)
  )
prevalences_spain_overall_wide
clipr::write_clip(prevalences_spain_overall_wide)

# Prevalence by year and separate boys and girls
prevalences_spain_sexo <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
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
prevalences_spain_age <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(edad_cat, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(edad_cat=case_when(edad_cat=="0-5"~"0-5", edad_cat=="6-11"~"6-11", edad_cat=="12-15"~"12-15"))
prevalences_spain_age
clipr::write_clip(prevalences_spain_age)

# Prevalence just by year together
# prevalences_spain <- prevalences_spain_overall %>% 
#   rbind(prevalences_spain_sexo) %>% 
#   rbind(prevalences_spain_age)
# 
# save(prevalence_spain, file = "prevalence_spain.RData")
# clipr::write_clip(prevalences_spain)
# print(prevalences_spain, n = 90)

prevalences_spain_5 <- bind_rows(
  prevalences_spain_overall,
  prevalences_spain_sexo,
  prevalences_spain_age
)
View(prevalences_spain)
save(prevalences_spain, file = "prevalences_spain_5.RData")

prevalences_spain <- prevalences_spain %>%
  mutate(survey = as.numeric(survey))

# Prevalence by social class and year combined boys and girls
prevalence_class_overall <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(clase, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
prevalence_class_overall

# Prevalence by social class 3
prevalence_class_overall3 <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(clase_3, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
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
prevalence_class_sexo <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, clase, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_class_sexo

# Prevalence by social class 3 and year separate boys and girls
prevalence_class_sexo3 <- dt_clean_5 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, clase_3, survey) %>%
  summarize(sedentarismo = survey_mean(sedentarismo, na.rm = T, vartype = "ci"),
  ) %>% 
  mutate(sex=case_when(sexo=="Male"~"Male", sexo=="Female"~"Female"))
prevalence_class_sexo3

# Prevalence by class together
prevalence_class <- prevalence_class_overall %>% 
  rbind(prevalence_class_sexo) 
save(prevalence_class, file = "prevalence_class5.RData")
print(prevalence_class, n = 90)
clipr::write_clip(prevalence_class)

# Prevalence by class 3 together
prevalence_class3 <- prevalence_class_overall3 %>% 
  rbind(prevalence_class_sexo3) 
save(prevalence_class, file = "prevalence_class3_5.RData")
prevalence_class3
clipr::write_clip(prevalence_class3)

# Prevalence by CCAA, social class, and year combined boys and girls
prevalence_overall_class_ccaa <- dt_clean_5 %>%
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
prevalence_overall_class_ccaa3 <- dt_clean_5 %>%
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
prevalence_overall_ccaa <- dt_clean_5 %>%
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
prevalence_sex_class_ccaa <- dt_clean_5 %>%
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
prevalence_sex_class_ccaa3 <- dt_clean_5 %>%
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
save(prevalence_class_ccaa, file = "prevalence_class_ccaa_5.RData")
clipr::write_clip(prevalence_class_ccaa)
prevalence_class_ccaa

# Prevalence by CCAA, social class 3, and year together
prevalence_class_ccaa3 <- prevalence_overall_class_ccaa3 %>% 
  rbind(prevalence_sex_class_ccaa3)
save(prevalence_class_ccaa3, file = "prevalence_class_ccaa3_5.RData")
clipr::write_clip(prevalence_class_ccaa3)
prevalence_class_ccaa3
#### Visualization Descriptive Sedentarism Overall ####
load("prevalences_spain.RData")

prevalences_spain$survey <- as.numeric(as.character(prevalences_spain$survey))

prevalences_spain <- prevalences_spain %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

# Both sexes
fig_desc_sedentarism_overall <- prevalences_spain %>%
  filter(sexo == "Overall") %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = sexo,
    fill = sexo
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 50, by = 10),
    limits = c(0, 50)
  ) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = 1.5,
    nudge_x = 0.5,
    show.legend =  FALSE,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c("Overall" = "#1b9e77")
  ) +
  scale_fill_manual(
    values = c("Overall" = "#1b9e77")
  ) +
  labs(
    title = "Prevalence of Sedentarism Over Time (Girls and Boys)",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities()
fig_desc_sedentarism_overall
ggsave("Figures/fig_desc_sedentarism_overall.png", width = 4000, height = 2200, dpi=300, units = "px")

# Separate by sex
prevalences_spain <- prevalences_spain %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

fig_desc_sedentarism_sex <- prevalences_spain %>%
  filter(sexo %in% c("Female", "Male")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = sexo,
    fill = sexo
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = 1.5,
    nudge_x = 0.5,
    show.legend =  FALSE,
    fontface = "bold"
  ) +
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
      "Male" = "#f03b20",
      "Female" = "#2c7fb8"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Male" = "#f03b20",
      "Female" = "#2c7fb8"
    )
  )  +
  labs(
    title = "Prevalence of Sedentarism Over Time by Sex",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities()
fig_desc_sedentarism_sex
ggsave("Figures/fig_desc_sedentarism_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

# Separate by age group
prevalences_spain <- prevalences_spain %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

fig_desc_sedentarism_age <- prevalences_spain %>%
  filter(edad_cat %in% c("0-5", "6-11", "12-15")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = edad_cat,
    fill = edad_cat
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = 1,
    nudge_x = 0.5,
    show.legend =  FALSE,
    fontface = "bold"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 70, by = 10),
    limits = c(0, 70)
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c(
      "0-5" = "#f03b20",
      "6-11" = "#2c7fb8",
      "12-15" = "#2c7"
    )
  ) +
  scale_fill_manual(
    values = c(
      "0-5" = "#f03b20",
      "6-11" = "#2c7fb8",
      "12-15" = "#2c7"
    )
  )  +
  labs(
    title = "Prevalence of Sedentarism Over Time by Age Group",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Age",
    fill = "Age"
  ) +
  theme_inequalities()
fig_desc_sedentarism_age
ggsave("Figures/fig_desc_sedentarism_age.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure prevalence by social class
load("prevalence_class.RData")
load("prevalence_class3.RData")

prevalence_class$survey <- as.numeric(as.character(prevalence_class$survey))
prevalence_class3$survey <- as.numeric(as.character(prevalence_class3$survey))

fig_desc_sedentarism_class_overall <- prevalence_class %>%
  filter(sex == "Overall") %>%
  ggplot(aes(x = survey, y = sedentarismo * 100, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100, color = clase, fill = clase)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
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
    title = "Prevalence of Sedentarism Over Time by Social Class (Girls and Boys)",
    x = NULL,
    y = "Prevalence (%) (95% CI)",
    color = "Social Class",
    fill = "Social Class"
  ) +
  theme_inequalities()
fig_desc_sedentarism_class_overall
ggsave("Figures/fig_desc_sedentarism_class_overall.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure for Class 3 categories
fig_desc_sedentarism_class_overall3 <- prevalence_class3 %>%
  filter(sex == "Overall") %>%
  ggplot(aes(x = survey, y = sedentarismo * 100, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100, color = clase_3, fill = clase_3)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = 1,
    nudge_x = 0.5,
    show.legend =  FALSE,
    fontface = "bold"
  ) +
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
  theme_inequalities()
fig_desc_sedentarism_class_overall3
ggsave("Figures/fig_desc_sedentarism_class_overall3.png", width = 4000, height = 2200, dpi=300, units = "px")

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
ggsave("Figures/fig_desc_sedentarism_class_sex.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure prevalence social class 3 categories by sex
prevalence_class3 <- prevalence_class3 %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

fig_desc_sedentarism_class_sex3 <- prevalence_class3 %>%
  filter(sex %in% c("Female", "Male")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = clase_3,
    fill = clase_3
  )) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = -0.5,
    nudge_x = 0.75,
    show.legend =  FALSE,
    fontface = "bold"
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
  theme_inequalities()
fig_desc_sedentarism_class_sex3
ggsave("Figures/fig_desc_sedentarism_class_sex3.png", width = 4000, height = 2200, dpi=300, units = "px")

## Intersectional Figures ----
# Prevalence of sedentarism
