# Data Exploration INE

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

#### Baseline Characteristics without Survey Weights ####
## all with NAs
dt3 <- dt %>% 
  filter(edad >= 3) # children older than or equal to 3 years

dt5 <- dt %>% 
  filter(edad >= 5) 

tbl_summary(
  dt5,
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
  modify_caption("**Baseline Characteristics by Survey Year (Unweighted)**<br>**Ages 5 to 18**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  modify_footnote(~ "Values are presented as Median (IQR) for continuous variables and n (%) for categorical variables.")

#### Baseline Characteristics with Survey Weights ####
## use dt for 0-18 and dt3 for 3-18
my_labels <- set_names(
  list(
    "Child's Age (years)",
    "Child's Sex",
    "Child's Nationality",
    "Child Living with Sedentarism",
    "Household Social Class",
    "Urban/Rural",
    "Autonomous Community"
  ),
  c("edad", "sexo", "nacionalidad", "sedentarismo", "clase",
    "urb_rur", "ccaa")
)

valid_vars <- intersect(names(my_labels), names(dt))

# Create unweighted table (counts only)
tbl_unweighted <- tbl_summary(
  data = dt5,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{n}", all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_continuous() ~ 1),
  missing = "always") %>% 
  add_n()

# Create weighted table (percentages only)
baseline <- svydesign(ids = ~1, weights = ~factor2, data = dt5)
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
  modify_caption("**Baseline Characteristics by Survey Year with Missing**<br>**Ages 5 to 18**<br>n = unweighted counts, % = weighted proportions")
tbl_final

# with dt_clean
# use dt_clean for 0-18 and dt_clean3 for 3-18
dt_clean3 <- dt_clean %>% 
  filter(edad >= 3)

dt_clean5 <- dt_clean %>% 
  filter(edad >= 5)

# Create unweighted table (counts only)
tbl_unweighted <- tbl_summary(
  data = dt_clean5,
  by = survey,
  include = all_of(valid_vars),
  label = my_labels,
  statistic = list(all_categorical() ~ "{n}", all_continuous() ~ "{median} ({p25}, {p75})"),
  digits = list(all_continuous() ~ 1),
  missing = "no") %>% 
  add_n()

# Create weighted table (percentages only)
baseline_clean <- svydesign(ids = ~1, weights = ~factor2, data = dt_clean5)

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
  modify_caption("**Baseline Characteristics by Survey Year**<br>**Ages 5 to 18**<br>n = unweighted counts, % = weighted proportions")
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
    title = md("**Sedentarism Prevalence in Spain by Survey Year**<br>**Ages 0 to 18**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )
overall_prevalence

# version 1 ages 3-18
overall_prevalence <- dt_clean %>%
  filter(edad >= 3) %>% 
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
    title = md("**Sedentarism Prevalence in Spain by Survey Year**<br>**Ages 3 to 18**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )
overall_prevalence

# version 2
prevalence_data <- dt_clean %>%
  filter(edad >= 3) %>% 
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

#### Descriptive by Sex Survey Weights ####
# Female prevalence summary
prev_female <- dt_clean %>%
  #filter(edad >= 5) %>% 
  filter(sexo == "Female") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_female = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Male prevalence summary
prev_male <- dt_clean %>%
  #filter(edad >= 5) %>% 
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
  tab_header(title = md("**Sedentarism Prevalence by Sex and Survey Year**<br>**Ages 0 to 18**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_sex

#### Descriptive by Class Survey Weights ####
# List of class categories
class_levels <- c("Class I", "Class II", "Class III", "Class IV", "Class V", "Class VI")

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
    title = md("**Sedentarism Prevalence in Spain by Social Class and Survey Year**<br>**Ages 0 to 18**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center"
  )

#### Descriptive by Age edad_cat Survey Weights ####
summary(dt_clean$edad)
hist(dt_clean$edad)

# categorize age into infants, primaria, ESO, bachillerato
dt_clean <- dt_clean %>%
  mutate(edad_cat = case_when(
    edad >= 0 & edad <= 5 ~ "0-5",
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15",
    edad >= 16 & edad <= 18 ~ "16-18"
  ))
dt_clean$edad_cat <- factor(dt_clean$edad_cat, levels = c("0-5", "6-11", "12-15", "16-18"))
summary(dt_clean$edad_cat)
# 0-5   6-11   12-15 16-18 
# 10153 11092  8408  2205 

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
  get_prevalence(dt_clean, "12-15"),
  get_prevalence(dt_clean, "16-18")
)

# years as columns
combined_wide <- combined_prevalence %>%
  mutate(age_str = case_when(
    edad_cat == "0-5" ~ "0-5 years",
    edad_cat == "6-11" ~ "6-11 years",
    edad_cat == "12-15" ~ "12-15 years",
    edad_cat == "16-18" ~ "16-18 years"
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
    align = "center", columns = c("0-5 years", "6-11 years", "12-15 years", "16-18 years")
  ) %>%
  tab_header(title = md("**Sedentarism Prevalence in Spain by Age Group**<br>**Ages 0 to 18**")) %>%
  tab_options(table.font.size = "small")

#       2003 2006 2011 2017 2023
# 0-5   2013 3169 2035 1910 1026
# 6-11  2290 3094 1949 2338 1421
# 12-15 2079 2544 1116 1570 1099
# 16-18  624  642    0  551  388

#### Descriptive by Age edad_cat3 Survey Weights ####
# new age groups # wrong categorization because have those aged 16-18
dt_clean <- dt_clean %>%
  mutate(edad_cat2 = case_when(
    edad >= 0 & edad <= 2 ~ "0-2",
    edad >= 3 & edad <= 5 ~ "3-5",
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15"
  ))
dt_clean$edad_cat2 <- factor(dt_clean$edad_cat2, levels = c("0-2", "3-5", "6-11", "12-15"))
summary(dt_clean$edad_cat2)
table(dt_clean$edad_cat2, dt_clean$survey)

# new age groups adults 16-18
dt_clean <- dt_clean %>%
  mutate(edad_cat3 = case_when(
    edad >= 0 & edad <= 2 ~ "0-2",  # Exclude from sedentarism analysis
    edad >= 3 & edad <= 5 ~ "3-5",  # Educación Infantil (last cycle)
    edad >= 6 & edad <= 11 ~ "6-11", # Educación Primaria
    edad >= 12 & edad <= 15 ~ "12-15",  # Educación Secundaria Obligatoria
    edad >= 16 & edad <= 18 ~ "16-18" # Bachillerato or Formación Profesional
  ))
summary(dt_clean$edad_cat3)
dt_clean$edad_cat3 <- factor(dt_clean$edad_cat3, levels = c("0-2", "3-5", "6-11", "12-15", "16-18"))
summary(dt_clean$edad_cat3)
# summary(dt_clean$edad_cat3)
# 0-2 3-5   6-11  12-15 16-18 
# 0   5229  11092 8408  2205

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
  get_prevalence(dt_clean, "3-5"),
  get_prevalence(dt_clean, "6-11"),
  get_prevalence(dt_clean, "12-15"),
  get_prevalence(dt_clean, "16-18")
)

# years as columns
combined_wide <- combined_prevalence %>%
  mutate(age_str = case_when(
    edad_cat3 == "3-5" ~ "3-5 years",
    edad_cat3 == "6-11" ~ "6-11 years",
    edad_cat3 == "12-15" ~ "12-15 years",
    edad_cat3 == "16-18" ~ "16-18 years"
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
    align = "center", columns = c("3-5 years", "6-11 years", "12-15 years", "16-18 years")
  ) %>%
  tab_header(title = md("**Sedentarism Prevalence in Spain by Age Group**<br>**Ages 3 to 18**")) %>%
  tab_options(table.font.size = "small")

dt_clean %>%
  count(survey)

#### Descriptive by Age edad_cat4 Survey Weights ####
# new age groups adults 16-18
dt_clean <- dt_clean %>%
  mutate(edad_cat4 = case_when(
    edad >= 0 & edad <= 2 ~ "0-2", # exclude
    edad >= 3 & edad <= 8 ~ "3-8",
    edad >= 9 & edad <= 13 ~ "9-13",
    edad >= 14 & edad <= 18 ~ "14-18"
  ))
summary(dt_clean$edad_cat4)
dt_clean$edad_cat4 <- factor(dt_clean$edad_cat4, levels = c("0-2", "3-8", "9-13", "14-18"))

# > summary(dt_clean$edad_cat4)
# 0-2 3-8   9-13  14-18 
# 0   10452 10456 6026 

# function for each age group
get_prevalence <- function(data, age_group) {
  data %>%
    filter(edad_cat4 == age_group) %>%
    as_survey_design(weights = factor2) %>%
    group_by(survey) %>%
    summarize(sedentarismo = survey_mean(sedentarismo, vartype = "ci")) %>%
    mutate(edad_cat4 = age_group)
}

# apply function to each group and bind results
combined_prevalence <- bind_rows(
  get_prevalence(dt_clean, "3-8"),
  get_prevalence(dt_clean, "9-13"),
  get_prevalence(dt_clean, "14-18")
)

# years as columns
combined_wide <- combined_prevalence %>%
  mutate(age_str = case_when(
    edad_cat4 == "3-8" ~ "3-8 years",
    edad_cat4 == "9-13" ~ "9-13 years",
    edad_cat4 == "14-18" ~ "14-18 years"
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
    align = "center", columns = c("3-8 years", "9-13 years", "14-18 years")
  ) %>%
  tab_header(title = md("**Sedentarism Prevalence in Spain by Age Group**<br>**Ages 3 to 18**")) %>%
  tab_options(table.font.size = "small")

dt_clean %>%
  count(survey)

# count overall 
# survey     n
# <chr>  <int>
# 1 2003    6314
# 2 2006    8774
# 3 2011    5087
# 4 2017    5865
# 5 2023    1175

# count those >= 3 years
# survey     n
# <chr>  <int>
#   1 2003    5359
# 2 2006    7142
# 3 2011    4108
# 4 2017    4969
# 5 2023    1042

# density of weight per age group (more spread for older age group)
dt_clean %>%
  ggplot(aes(x = peso, fill = edad_cat2)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Density of Weight by Age Category", x = "Weight (kg)", y = "Density")

#### Descriptive by Autonomous Community Survey Weights ####
## weighted prevalence by ccaa and survey
prevalence_by_ccaa <- dt_clean %>%
  #filter(edad  >= 5) %>% 
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
    title = md("**Sedentarism Prevalence in Spain by Autonomous Community and Survey Year**<br>**Ages 0 to 18**")
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
  #filter(edad >= 5) %>% 
  filter(nacionalidad == "Spanish") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_spanish = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Foreign prevalence summary
prev_foreign <- dt_clean %>%
  #filter(edad >= 5) %>% 
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
  tab_header(title = md("**Sedentarism Prevalence by Nationality and Survey Year**<br>**Ages 0 to 18**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_nationality

#### Descriptive by Urban/Rural Survey Weights ####
# Urban prevalence summary
prev_urb <- dt_clean %>%
  #filter(edad >= 5) %>% 
  filter(urb_rur == "Urban") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_urb = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Semi-urban prevalence summary
prev_semi <- dt_clean %>%
  #filter(edad >= 5) %>% 
  filter(urb_rur == "Semi-urban") %>%
  as_survey_design(weights = factor2) %>%
  group_by(survey) %>%
  summarize(
    prevalence_semi = survey_mean(sedentarismo, na.rm = TRUE, vartype = "ci")
  )

# Rural prevalence summary
prev_rur <- dt_clean %>%
  #filter(edad >= 5) %>% 
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
  tab_header(title = md("**Sedentarism Prevalence by Urban/Rural and Survey Year**<br>**Ages 0 to 18**")) %>%
  tab_options(table.font.size = "small", heading.align = "left")
prev_joined_urb
