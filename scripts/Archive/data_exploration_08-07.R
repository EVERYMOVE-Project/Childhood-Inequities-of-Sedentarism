# Data Exploration INE

## Load libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)

## read joined data
dt <- get(load("joined.RData"))
dt_clean <- get(load("joined_clean.RData"))
dt_clean <- get(load("dt17-07.RData"))
dt_clean <- get(load("dt18-07.RData"))

#### Baseline Characteristics without Survey Weights ####
## all with NAs
## Table with variables although some later irrelevant
tbl_summary(data = dt,
            by = survey,
            include = all_of(c("edad", "sexo", "edad_i", "sexo_i", "tamano", "ccaa", "urb_rur",
                               "estrato", "nacionalidad", "clase", "clase_tr", "education_3", 
                               "education_3_tr", "education_5", "education_5_tr", 
                               "peso", "altura", "imc", "obesity", "overweight", "sedentarismo",
                               "PA", "percep_peso_menor", "rel_con_menor" # "v_limpieza", "v_verde"
                               )),
            label = list(
              edad ~ "Child's Age (years)",
              sexo ~ "Child's Sex",
              edad_i ~ "Informant's Age (years)",
              sexo_i ~ "Informant's Sex",
              urb_rur ~ "Urban/Rural",
              tamano ~ "Municipality of Residence Size (Tamaño)",
              estrato ~ "Municipality of Residence Size (Estrato)",
              ccaa ~ "Autonomous Community",
              nacionalidad ~ "Informant's Nationality",
              clase ~ "Household Social Class",
              clase_tr ~ "Household Social Class Numeric",
              education_3 ~ "Educational Attainment (3 Levels)",
              education_3_tr ~ "Educational Attainment (3 Levels) Numeric",
              education_5 ~ "Educational Attainment (5 Levels)",
              education_5_tr ~ "Educational Attainment (5 Levels) Numeric",
              peso ~ "Child's Weight (kg)",
              altura ~ "Child's Height (cm)",
              imc ~ "Body Mass Index Category",
              obesity ~ "Child Living with Obesity",
              overweight ~ "Child Living with Overweight",
              sedentarismo ~ "Child Living with Sedentarism",
              PA ~ "Physical Activity Level",
              percep_peso_menor ~ "Parental Perception of Child's Weight",
              rel_con_menor ~ "Relationship with Minor"
              # v_limpieza ~ "Perceived Lack of Cleanliness in Neighborhood",
              # v_verde ~ "Perceived Lack of Green Spaces"
            ),
            statistic = list(
              all_categorical() ~ "{n} ({p}%)", 
              all_continuous() ~ "{mean} ({sd})"
            ),
            digits = list(all_continuous() ~ 1),
            missing = "no") %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% # Correct header syntax
  modify_footnote(~ "Values are presented as Mean (SD) for continuous variables and n (%) for categorical variables.")

## Table with only chosen variables
tbl_summary(data = dt,
            by = survey,
            include = all_of(c("edad", "sexo", "nacionalidad", "sedentarismo",
                               "edad_i", "sexo_i", "education_3", "clase",
                               "ccaa", "urb_rur")),
            label = list(
              edad ~ "Child's Age (years)",
              sexo ~ "Child's Sex",
              edad_i ~ "Informant's Age (years)",
              sexo_i ~ "Informant's Sex",
              urb_rur ~ "Urban/Rural",
              ccaa ~ "Autonomous Community",
              nacionalidad ~ "Nationality",
              clase ~ "Household Social Class",
              education_3 ~ "Informant's Educational Attainment",
              sedentarismo ~ "Child Living with Sedentarism"
            ),
            statistic = list(
              all_categorical() ~ "{n} ({p}%)", 
              all_continuous() ~ "{median} ({p25}, {p75})"
            ),
            digits = list(all_continuous() ~ 1),
            missing = "always") %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  modify_footnote(~ "Values are presented as Median (IQR) for continuous variables and n (%) for categorical variables.")

## Complete Case Analysis
## Table with only chosen variables
tbl_summary(data = dt_clean,
            by = survey,
            include = all_of(c("edad", "sexo", "nacionalidad","sedentarismo",
                               "edad_i", "sexo_i", "education_3", "clase",
                               "ccaa", "urb_rur"
            )),
            label = list(
              edad ~ "Child's Age (years)",
              sexo ~ "Child's Sex",
              edad_i ~ "Informant's Age (years)",
              sexo_i ~ "Informant's Sex",
              urb_rur ~ "Urban/Rural",
              ccaa ~ "Autonomous Community",
              nacionalidad ~ "Nationality",
              clase ~ "Household Social Class",
              education_3 ~ "Informant's Educational Attainment",
              sedentarismo ~ "Child Living with Sedentarism"
            ),
            statistic = list(
              all_categorical() ~ "{n} ({p}%)", 
              all_continuous() ~ "{median} ({p25}, {p75})"
            ),
            digits = list(all_continuous() ~ 1),
            missing = "no") %>%
  add_n() %>%
  bold_labels() %>%
  modify_caption("**Baseline Characteristics**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% # Correct header syntax
  modify_footnote(~ "Values are presented as Mean (SD) for continuous variables and n (%) for categorical variables.")

#### Descriptive Survey Weights Overall ####
## Overall prevalence

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
    title = md("**Sedentarism Prevalence in Spain by Survey Year**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )
overall_prevalence

# version 2
prevalence_data <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prop = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  ) %>%
  mutate(
    prevalence_ci = paste0(
      percent(prop, accuracy = 0.1), 
      " (", 
      percent(prop_low, accuracy = 0.1), 
      "–", 
      percent(prop_upp, accuracy = 0.1), 
      ")"
    )
  )

prevalence_table <- prevalence_data %>%
  select(survey, prevalence_ci) %>%
  pivot_wider(names_from = survey, values_from = prevalence_ci)

prevalence_table %>%
  gt() %>%
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Survey Year**"),
    subtitle = md("With 95% Confidence Intervals")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )

# version 3
prevalence_data <- dt_clean %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    sedentarismo = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  ) %>%
  mutate(sexo = "Overall") %>%
  select(survey, sedentarismo, sedentarismo_low, sedentarismo_upp)

prevalence_wide <- prevalence_data %>%
  pivot_longer(cols = starts_with("sedentarismo"),
               names_to = "stat",
               values_to = "value") %>%
  pivot_wider(names_from = survey, values_from = value)

prevalence_wide %>%
  gt() %>%
  fmt_percent(columns = where(is.numeric), decimals = 1) %>%
  cols_label(
    stat = "Statistic"
  ) %>%
  tab_header(
    title = md("**Sedentarism Prevalence in Spain by Survey Year**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )
summary(dt_clean$sexo)
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
  tab_header(title = md("**Sedentarism Prevalence by Sex and Survey Year**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_sex

#### Descriptive by Class Survey Weights ####
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
prevalence_wide

prevalence_wide <- prevalence_wide %>%
  mutate(across(ends_with("_prevalence"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_low"), ~ round(.x * 100, 1)),
         across(ends_with("_prevalence_upp"), ~ round(.x * 100, 1)))

prevalence_wide_str <- prevalence_wide %>%
  mutate(
    `Class I` = paste0(round(`Class I_prevalence`), "% (", round(`Class I_prevalence_low`), "% - ", round(`Class I_prevalence_upp`), "%)"),
    `Class II` = paste0(round(`Class II_prevalence`), "% (", round(`Class II_prevalence_low`), "% - ", round(`Class II_prevalence_upp`), "%)"),
    `Class III` = paste0(round(`Class III_prevalence`), "% (", round(`Class III_prevalence_low`), "% - ", round(`Class III_prevalence_upp`), "%)"),
    `Class IV` = paste0(round(`Class IV_prevalence`), "% (", round(`Class IV_prevalence_low`), "% - ", round(`Class IV_prevalence_upp`), "%)"),
    `Class V` = paste0(round(`Class V_prevalence`), "% (", round(`Class V_prevalence_low`), "% - ", round(`Class V_prevalence_upp`), "%)"),
    `Class VI` = paste0(round(`Class VI_prevalence`), "% (", round(`Class VI_prevalence_low`), "% - ", round(`Class VI_prevalence_upp`), "%)")
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
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )

#### Descriptive by Age Survey Weights ####
# summary(dt_clean$edad)
# hist(dt_clean$edad)

# categorize age into three groups
dt_clean <- dt_clean %>%
  mutate(edad_cat = case_when(
    edad >= 0 & edad <= 5 ~ "0-5",
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15"
  ))
dt_clean$edad_cat <- factor(dt_clean$edad_cat, levels = c("0-5", "6-11", "12-15"))
summary(dt_clean$edad_cat)

# new age groups
dt_clean <- dt_clean %>%
  mutate(edad_cat2 = case_when(
    edad >= 0 & edad <= 2 ~ "0-2",
    edad >= 3 & edad <= 5 ~ "3-5",
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15"
  ))
dt_clean$edad_cat2 <- factor(dt_clean$edad_cat2, levels = c("0-2", "3-5", "6-11", "12-15"))
summary(dt_clean$edad_cat2)

# function for each age group
get_prevalence <- function(data, age_group) {
  data %>%
    filter(edad_cat2 == age_group) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(sedentarismo = survey_mean(sedentarismo, vartype = "ci")) %>%
    mutate(edad_cat2 = age_group)
}

# apply function to each group and bind results
combined_prevalence <- bind_rows(
  get_prevalence(dt_clean, "0-2"), # babies naturally sedentary
  get_prevalence(dt_clean, "3-5"), # do not start to have structured activity until 3 i.e., preschoolers capable of play
  get_prevalence(dt_clean, "6-11"), # primary school
  get_prevalence(dt_clean, "12-15") # early adolescence
)

# years as columns
combined_wide <- combined_prevalence %>%
  mutate(age_str = case_when(
    edad_cat2 == "0-2" ~ "0-2 years",
    edad_cat2 == "3-5" ~ "3-5 years",
    edad_cat2 == "6-11" ~ "6–11 years",
    edad_cat2 == "12-15" ~ "12–15 years"
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
    align = "center", columns = c(survey)) %>% 
  cols_align(
    align = "center", columns = c("0-2 years", "3-5 years", "6–11 years", "12–15 years")
  ) %>% 
  tab_header(title = md("**Sedentarism Prevalence in Spain by Age Group**")) %>%
  tab_options(table.font.size = "small")

## explore age prevalence
dt_clean %>%
  filter(edad_cat == "0-5") %>%
  count(survey)

# survey     n
# <chr>  <int>
# 1 2003    1986
# 2 2006    3147
# 3 2011    2026
# 4 2017    1889
# 5 2023     324

# density of weight per age group (more spread for older age group)
dt_clean %>%
  ggplot(aes(x = peso, fill = edad_cat2)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Density of Weight by Age Category", x = "Weight (kg)", y = "Density")


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
    title = md("**Sedentarism Prevalence in Spain by Autonomous Community and Survey Year**")
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
