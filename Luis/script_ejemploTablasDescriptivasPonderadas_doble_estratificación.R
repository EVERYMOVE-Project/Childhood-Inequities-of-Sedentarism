#paquetes necesarios

library(survey)
library(gtsummary)
library(tidyverse)

##creas la base ponderada

svy <- svydesign(ids=~1, weights = ~ponde3, data= dt_fusion333)



##### Descriptvos muestra entorno combinado según cuartiles de densidad y el género de los menores ####


# En el siguiente código se muestra las características de la muestra según las categorías de la variable exposción y el género

  prueba <- svy %>%  tbl_strata(strata = SEXO1,
                     .tbl_fun =
                       ~ .x %>%
                       tbl_svysummary(by = Entorno400q, label = list(redada ~ "Age", fascat1 ~ "Family Affluence Scale", weight_status ~ "Body weight"),statistic = list(all_continuous() ~ "{mean} ({sd})"),missing = "no", percent = "column", include = c(redada, fascat1, weight_status), digits = list(fascat1 ~ c(0,1), weight_status ~ c(0,1)))) 
