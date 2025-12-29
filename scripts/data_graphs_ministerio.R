### Graphs for Ministry of Health Presentation Luis

## Load libraries
library(tidyverse)
library(gtsummary)
library(gt)
library(srvyr)
library(survey)
library(scales)
library(purrr)
library(ggrepel)

## Load data ----
load("prevalences_spain_5.RData")

prevalences_spain_5$survey <- as.numeric(as.character(prevalences_spain_5$survey))

prevalences_spain_5 <- prevalences_spain_5 %>%
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
    vjust = -1,    
    size = 3.5,         
    nudge_y = 1.5,
    nudge_x = 0.5,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c("Overall" = "black")
  ) +
  scale_fill_manual(
    values = c("Overall" = "black")
  ) +
  labs(
    title = "Prevalencia de sedentarismo en población infantil entre 2003-2023 en España",
    x = NULL,
    y = "Prevalencia (%) (95% IC)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities() +
  theme(
    panel.background = element_rect(fill = "white"),        # White panel background
    panel.grid.major.x = element_blank(),                   # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid"),  # Horizontal lines
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none"
  )

fig_desc_sedentarism_overall
ggsave("Figures/fig_desc_sedentarism_overall_MINISTERIO.png", width = 4000, height = 2200, dpi=300, units = "px")

# Both sexes
fig_desc_sedentarism_overall_over5 <- prevalences_spain_5 %>%
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
    breaks = seq(10, 40, by = 10),
    limits = c(0, 40)
  ) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,    
    size = 3.5,         
    nudge_y = 1.5,
    nudge_x = 0.5,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(2003, 2006, 2011, 2017, 2023),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = c("Overall" = "black")
  ) +
  scale_fill_manual(
    values = c("Overall" = "black")
  ) +
  labs(
    title = "Prevalencia de sedentarismo en población infantil entre 2003-2023 en España",
    x = NULL,
    y = "Prevalencia (%) (95% IC)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_inequalities() +
  theme(
    panel.background = element_rect(fill = "white"),        # White panel background
    panel.grid.major.x = element_blank(),                   # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid"),  # Horizontal lines
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  )

fig_desc_sedentarism_overall_over5
ggsave("Figures/fig_desc_sedentarism_overall_MINISTERIO_5.png", width = 4000, height = 2200, dpi=300, units = "px")

fig_desc_sedentarism_sex_over5 <- prevalences_spain_5 %>%
  filter(sexo %in% c("Female", "Male")) %>%
  ggplot(aes(
    x = survey,
    y = sedentarismo * 100,
    ymin = sedentarismo_low * 100,
    ymax = sedentarismo_upp * 100,
    color = sexo,
    fill = sexo
  )) +
  # Overall line (mapped to color = "Overall" so it appears in the legend)
  geom_line(
    data = prevalences_spain_5 %>% filter(sexo == "Overall"),
    aes(x = survey, y = sedentarismo * 100),
    color = "grey40",
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = TRUE
  ) +
  geom_text(
    data = prevalences_spain_5 %>% filter(sexo == "Overall"),
    aes(x = survey, y = sedentarismo * 100, label = prevalence_label),
    vjust = -1,
    size = 3.5,
    nudge_y = 0.5,
    nudge_x = 0.45,
    fontface = "bold",
    color = "black",
    show.legend = FALSE
  ) +
  # Overall ribbon (not in legend)
  geom_ribbon(
    data = prevalences_spain_5 %>% filter(sexo == "Overall"),
    aes(x = survey, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100, 
        fill = "Overall"),
    alpha = 0.4,
    show.legend = FALSE
  ) +
  # Male/Female lines and ribbons
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3) +
  geom_text(
    aes(label = prevalence_label),
    vjust = -1,
    size = 3.5,
    nudge_y = 0.5,
    nudge_x = 0.5,
    show.legend = FALSE,
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
      "Male" = "#f03b20",
      "Female" = "#2c7fb8",
      "Overall" = "grey40"
    ),
    labels = c(
      "Male" = "Niños",
      "Female" = "Niñas",
      "Overall" = "Total"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Male" = "#f03b20",
      "Female" = "#2c7fb8",
      "Overall" = "grey30"
    ),
    labels = c(
      "Male" = "Niños",
      "Female" = "Niñas",
      "Overall" = "Total"
    )
  ) +
  labs(
    title = "Prevalencia de sedentarismo en población infantil entre 2003-2023 en España por Sexo",
    x = NULL,
    y = "Prevalencia (%) (95% IC)",
    color = "Sexo",
    fill = "Sexo"
  ) +
  theme_inequalities() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    # legend.position = "none"
  )

fig_desc_sedentarism_sex_over5
ggsave("Figures/fig_desc_sedentarism_sex_MINISTERIO_5.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure for Class 3 categories
# Figure prevalence social class 3 categories by sex
prevalence_class3 <- prevalence_class3 %>%
  mutate(
    prevalence_label = paste0(round(sedentarismo * 100, 1), "%")
  )

prevalence_class3$survey <- as.numeric(as.character(prevalence_class3$survey))

fig_desc_sedentarism_class_overall3 <- prevalence_class3 %>%
  filter(sex == "Overall") %>%
  ggplot(aes(x = survey, y = sedentarismo * 100, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100, color = clase_3, fill = clase_3)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_text_repel(
    aes(label = prevalence_label),
    # vjust = -1,    # Moves text slightly above the point
    size = 3,         # Adjust for readability
    nudge_y = 0.5,
    # nudge_x = 0.5,
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
      "Class I" = "#4292c6",
      "Class II" = "#43A047",
      "Class III" = "#990000"
    ),
    labels = c(
      "Class I" = "Clase I",
      "Class II" = "Clase II",
      "Class III" = "Clase III"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#4292c6",
      "Class II" = "#43A047",
      "Class III" = "#990000"
    ),
    labels = c(
      "Class I" = "Clase I",
      "Class II" = "Clase II",
      "Class III" = "Clase III"
    )
  ) +
  labs(
    title = "Prevalencia de sedentarismo en población infantil entre 2003-2023 en España por clase social",
    x = NULL,
    y = "Prevalencia (%) (95% IC)",
    color = "Clase Social",
    fill = "Clase Social"
  ) +
  theme_inequalities() + 
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
fig_desc_sedentarism_class_overall3
ggsave("Figures/fig_desc_sedentarism_class_overall3_MINISTERIO_5.png", width = 4000, height = 2200, dpi=300, units = "px")


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
  # Add overall ribbon and line for both facets
  geom_ribbon(
    data = prevalence_class3 %>% filter(sex == "Overall"),
    aes(x = survey, ymin = sedentarismo_low * 100, ymax = sedentarismo_upp * 100),
    alpha = 0.2,
    show.legend = TRUE
  ) +
  geom_line(
    data = prevalence_class3 %>% filter(sex == "Overall"),
    aes(x = survey, y = sedentarismo * 100),
    linewidth = 0.8,
    # linetype = "dashed",
    show.legend = TRUE
  ) +
  # Male/Female lines and ribbons
  geom_line(linewidth = 0.75) +
  geom_ribbon(alpha = 0.3, color = NA) +
  # Labels for Male/Female
  geom_text_repel(
    aes(label = prevalence_label),
    vjust = -1,
    size = 3,
    max.overlaps = Inf,    # allow all labels
    nudge_y = 0.5,
    show.legend = FALSE,
    fontface = "bold",
    color = "black"
  ) +
  # Labels for overall trend
  geom_text_repel(
    data = prevalence_class3 %>% filter(sex == "Overall"),
    aes(x = survey, y = sedentarismo * 100, label = prevalence_label),
    vjust = -1,
    size = 3,
    nudge_y = 0.5,
    max.overlaps = Inf,
    color = "black",
    show.legend = FALSE,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ sex,
    labeller = labeller(
      sex = c(
        "Female" = "Niñas",
        "Male" = "Niños",
        "Overall" = "Total"
      )
    )
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
      "Class I" = "#4292c6",
      "Class II" = "#43A047",
      "Class III" = "#990000"
    ),
    labels = c(
      "Class I" = "Clase I",
      "Class II" = "Clase II",
      "Class III" = "Clase III"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Class I" = "#4292c6",
      "Class II" = "#43A047",
      "Class III" = "#990000"
    ),
    labels = c(
      "Class I" = "Clase I",
      "Class II" = "Clase II",
      "Class III" = "Clase III"
    )
  ) +
  labs(
    title = "Prevalencia de sedentarismo en población infantil entre 2003-2023 en España por sexo y clase social",
    x = NULL,
    y = "Prevalencia (%) (95% IC)",
    color = "Clase Social",
    fill = "Clase Social"
  ) +
  theme_inequalities() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
fig_desc_sedentarism_class_sex3
ggsave("Figures/fig_desc_sedentarism_class_sex_class3_MINISTERIO_5.png", width = 4000, height = 2200, dpi=300, units = "px")
