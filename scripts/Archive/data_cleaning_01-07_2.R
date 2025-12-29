## Data cleaning of national health survey datasets

## Load libraries
library(tidyr)
library(tidyverse)
library(scales)
library(labelled)
library(readr)
library(readxl)

setwd("~/UAH/PhD Documents/INEdatos")

## Command to remove all lists from environment
rm(list=ls())

#### Databases: ####
### ENSE 2001 - INFANT01.txt
### ENSE 2003 - INFANT03.txt, HOGAR03.txt
### ENSE 2006 - INFANT06.txt, HOGAR06.txt
### ENSE 2012 - INFANT12.txt, HOGAR12.txt
### ENSE 2017 - INFANT17.xlsx, HOGAR17.xlsx
### ESDE 2023 - INFANT23.RData, HOGAR23.RData

#### ENSE 2001 ####
# Define start and end positions
# Define column widths and names based on documentation
start <- c(1, 5, 10, 12, 14, 17, 18, 19, 21, 24, 28, 31, 32, 33, 35, 
           37, 38, 40, 42, 92, 93, 97, 102, 104, 107, 122, 128, 130, 
           153, 154, 156, 179, 180, 182, 183, 227,228, 230, 232, 234, 
           237, 239, 242, 245, 247, 249, 251, 253, 263,264, 273, 274, 
           275, 278, 280, 282, 285, 287, 362, 365, 382, 385,388, 422, 
           439, 441, 443, 446, 448, 450, 453, 455, 456, 459, 461,463)
end <- c( 4, 9, 11, 13, 16, 17, 18, 
    20, 23, 27, 28, 31, 32, 34, 36, 37, 39, 41, 91, 92, 96, 101, 103,
    106, 110, 127, 129, 152, 153, 155, 178, 181, 181, 226, 226, 229,
    231, 233, 236, 238, 241, 244, 246, 248, 250, 252, 262, 263, 272,
    273, 274, 277, 279, 281, 284, 286, 293, 363, 378, 384, 387, 388,
    438, 440, 442, 445, 447, 449, 452, 454, 455, 458, 460, 462, 464)
names_menores2001 <- c("n_estudio", "n_cuestionario", "ccaa", "provincia", "municipio", "tam_habitat", "area_metro", 
    "distrito", "seccion", "n_entrevistador", "submuestra_1", "cuestionario_infantil",
    "p32_precod", "edad_meses_1", "edad_anos_1", "p37_precod", "edad_entrevistado",
    "n_personas_vivienda", "p2_p4_familia", "p5_precod", "p5a_edad_dedicacion",
    "p5b_p6a_precod", "p7_precod", "p8_p10_precod", "p10a_dolencia_10dias",
    "p12_p13_precod", "p13a_dias_lim_act_libre", "p13a_precod_rest", "p14_precod",
    "p14a_dias_act_principal", "p14b_precod", "p15_precod", "p15a_dias_en_cama",
    "p16_precod", "p16a_b_precod", "p17_precod", "p17a_anos", "p17a_meses",
    "p18_n_veces_consulta", "p18b_precod", "p18c_especialidad", "p18d_tiempo_domicilio",
    "p18e_tiempo_espera", "p18f_p19_precod", "p19a_dentista_veces", "p20_anos",
    "p20_meses", "p21_precod", "p22_precod", "p23_precod", "p23a_precod", "p24a_n_hospital",
    "p24b_dias_ingreso", "p24c_d_precod", "p24e_espera_meses", "p24f_p25_precod",
    "p25a_urgencias_veces", "p25b_p27_precod", "p38_horas_sueno", "p40_p43a_precod",
    "p45_peso", "p46_estatura", "p46a_precod", "p52_p57_precod", "p57a_estudios_entrevistado",
    "p58_p58a_precod", "p59_ocupacion", "p60_p60a_precod", "p61_rama_empresa",
    "p62_p64_precod", "p64a_estudios_cabeza", "p65_precod", "p66_ocupacion_cabeza",
    "p67_p67a_precod", "p67b_rama_empresa", "p69_p70_precod"
  )

# Calculate widths
width_menores2001 <- end - start + 1
menores2001 <- read_fwf("~/UAH/PhD Documents/INEdatos/2001ENSE/INFANT2001/INFANT01.txt", col_positions = fwf_widths(widths = width_menores2001, col_names = names_menores2001))
rm(names_menores2001, width_menores2001, start, end)

#### ENSE 2003 ####
## menores
menores2003 <- read_excel("~/UAH/PhD Documents/INEdatos/2003ENSE/Infantil-ENSE-2003/codebook_menores2003.xlsx")
names_menores2003 <- menores2003$VARIABLE
width_menores2003  <- menores2003$LONGITUD %>% as.numeric

menores2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/2003ENSE/Infantil-ENSE-2003/INFANT03.txt", col_positions = fwf_widths(widths = width_menores2003, col_names = names_menores2003))
#saveRDS(menores2003, "~/UAH/PhD Documents/INEdatos/2003ENSE/menores2003.rds")
rm(names_menores2003, width_menores2003)

## hogar
hogar2003 <- read_excel("~/UAH/PhD Documents/INEdatos/2003ENSE/Hogar-ENSE-2003/codebook_hogar2003.xlsx")
names_hogar2003 <- hogar2003$VARIABLE
width_hogar2003  <- hogar2003$LONGITUD %>% as.numeric

hogar2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/2003ENSE/Hogar-ENSE-2003/HOGAR03.txt", col_positions = fwf_widths(widths = width_hogar2003, col_names = names_hogar2003))
#saveRDS(hogar2003, "~/UAH/PhD Documents/INEdatos/2003ENSE/hogar2003.rds")
rm(names_hogar2003, width_hogar2003)

#### ENSE 2006 ####
menores2006 <- read_excel("~/UAH/PhD Documents/INEdatos/2006ENSE/Infantil-ENSE-2006/codebook_menores2006.xlsx")
names_menores2006 <- menores2006$VARIABLE
width_menores2006  <- menores2006$LONGITUD %>% as.numeric

menores2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/2006ENSE/Infantil-ENSE-2006/INFANT06.txt", col_positions = fwf_widths(widths = width_menores2006, col_names = names_menores2006))
#saveRDS(menores2006, "~/UAH/PhD Documents/INEdatos/2006ENSE/menores2006.rds")
rm(names_menores2006, width_menores2006)

## hogar
hogar2006 <- read_excel("~/UAH/PhD Documents/INEdatos/2006ENSE/Hogar-ENSE-2006/codebook_hogar2006.xlsx")
names_hogar2006 <- hogar2006$VARIABLE
width_hogar2006  <- hogar2006$LONGITUD %>% as.numeric

hogar2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/2006ENSE/Hogar-ENSE-2006/HOGAR06.txt", col_positions = fwf_widths(widths = width_hogar2006, col_names = names_hogar2006))
#saveRDS(hogar2006, "~/UAH/PhD Documents/INEdatos/2006ENSE/hogar2006.rds")
rm(names_hogar2006, width_hogar2006)

#### ENSE 2012 ####
menores2012 <- read_excel("~/UAH/PhD Documents/INEdatos/2012ENSE/codebook_menores2011.xlsx")
names_menores2012 <- menores2012$VARIABLE
width_menores2012  <- menores2012$LONGITUD %>% as.numeric

menores2012 <- read_fwf("~/UAH/PhD Documents/INEdatos/2012ENSE/datos_ensalud12/INFANT12.txt", col_positions = fwf_widths(widths = width_menores2012, col_names = names_menores2012))
#saveRDS(menores2012, "~/UAH/PhD Documents/INEdatos/2012ENSE/menores2012.rds")
rm(names_menores2012, width_menores2012)

## hogar
hogar2012 <- read_excel("~/UAH/PhD Documents/INEdatos/2012ENSE/codebook_hogar2011.xlsx")
names_hogar2012 <- hogar2012$VARIABLE
width_hogar2012  <- hogar2012$LONGITUD %>% as.numeric

hogar2012 <- read_fwf("~/UAH/PhD Documents/INEdatos/2012ENSE/datos_ensalud12/HOGAR12.txt", col_positions = fwf_widths(widths = width_hogar2012, col_names = names_hogar2012))
#saveRDS(hogar2012, "~/UAH/PhD Documents/INEdatos/2012ENSE/hogar2012.rds")
rm(names_hogar2012, width_hogar2012)

#### ENSE 2017 ####
menores2017 <- read_excel("~/UAH/PhD Documents/INEdatos/2017ENSE/codebook_menores2017.xlsx")
names_menores2017 <- menores2017$VARIABLE
width_menores2017  <- menores2017$LONGITUD %>% as.numeric

menores2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/2017ENSE/Menores_ENSE17/MICRODAT.CM.txt", col_positions = fwf_widths(widths = width_menores2017, col_names = names_menores2017))
#saveRDS(menores2017, "~/UAH/PhD Documents/INEdatos/2017ENSE/menores2017.rds")
rm(names_menores2017, width_menores2017)

## hogar
hogar2017 <- read_excel("~/UAH/PhD Documents/INEdatos/2017ENSE/codebook_hogar2017.xlsx")
names_hogar2017 <- hogar2017$VARIABLE
width_hogar2017  <- hogar2017$LONGITUD %>% as.numeric

hogar2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/2017ENSE/HOGAR_ENSE17/MICRODAT.CH.txt", col_positions = fwf_widths(widths = width_hogar2017, col_names = names_hogar2017))
#saveRDS(hogar2017, "~/UAH/PhD Documents/INEdatos/2017ENSE/hogar2017.rds")
rm(names_hogar2017, width_hogar2017)

#### ENSE 2023 ####
## menores
load("~/UAH/PhD Documents/INEdatos/2023ESdE/ESdEmenor_2023/R/INFANT23.RData")
menores2023 <- Microdatos
#saveRDS(menores2023, "~/UAH/PhD Documents/INEdatos/2023ESdE/menores2023.rds")

## hogar
load("~/UAH/PhD Documents/INEdatos/2023ESdE/ESdEhogar_2023/R/HOGAR23.RData")
hogar2023 <- Microdatos
#saveRDS(hogar2023, "~/UAH/PhD Documents/INEdatos/2023ESdE/hogar2023.rds")
rm(Microdatos, Metadatos)



#### Select variables ####
#### ENSE 2003 ####
# menores2003
ense2003 <- menores2003 %>% select(
  NIDENTIF, N_INF, SEXO, EDAD, SUJ_ENTR, SPCLASE, FACTOR, ESTAPESO, D_ACFISO,
  PESO, ALTURA, SPESTUDI, TMUNI, CCAA)

# rename with matching order ## do not have to run this code 
ense2003 <- ense2003 %>% rename(
  id = NIDENTIF,
  n_inf = N_INF,
  sexo = SEXO,
  edad = EDAD,
  n_orden_menor = SUJ_ENTR,
  clase = SPCLASE,
  factor = FACTOR,
  percep_peso_menor = ESTAPESO,
  sedentarismo = D_ACFISO,
  peso = PESO,
  altura = ALTURA,
  estudios = SPESTUDI,
  tamano = TMUNI,
  ccaa = CCAA
)

# hogar2003
hogar2003 <- hogar2003 %>% rename(edad_i = EDAD, sexo_i = SEXO, nacionalidad = NACION,
                                   pais = PAIS, ecivil = ECIVIL, ingresos = IN_MENS, 
                                  id = NIDENTIF, labor = SITUPR)

# remove last two digits from menor id and hogar id to create a variable to join databases
ense2003$ID <- substr(ense2003$id, 1, 5)
hogar2003$ID <- substr(hogar2003$id, 1, 5)

# join desired hogar variables to ense dataframe using ID
ense2003 <- ense2003 %>%
  left_join(hogar2003 %>% select(ID, edad_i, sexo_i, nacionalidad, pais, ecivil, ingresos, labor), by = "ID") 
# note variables of informant unsure what relationship have with child

# convert variable types appropriately
# str(ense2003)
ense2003 <- ense2003 %>%
  mutate(
    sexo = as.factor(sexo),
    clase = as.factor(clase),
    percep_peso_menor = as.factor(percep_peso_menor),
    sedentarismo = as.factor(sedentarismo),
    labor = as.factor(labor),
    estudios = as.factor(estudios),
    tamano = as.factor(tamano),
    ccaa = as.factor(ccaa),
    nacionalidad = as.factor(nacionalidad),
    pais = as.factor(pais),
    ecivil = as.factor(ecivil),
    ingresos = as.factor(ingresos)
  )

ense2003 <- ense2003 %>%
  mutate(
    edad = as.numeric(edad),
    peso = as.numeric(peso),
    altura = as.numeric(altura),
    edad_i = as.numeric(edad_i)
  )
# View(ense2003)

# new survey variable
ense2003 <- ense2003 %>% 
  mutate(encuesta = "2003")

## need to add variable labels to categories

#### ENSE 2006 ####
# menores2006
ense2006 <- menores2006 %>% select(
  NIDENTIF, SEXO, EDAD, NORDEN, SPCLASE, FACTOR, K97, G1_57,
 K95, K96, MSIMC, TMUNI, ESTRATO, CCAA, P4_1, P4_0_2)

# rename relevant variables
ense2006 <- ense2006 %>% rename(
  id = NIDENTIF,
  n_inf = P4_0_2,
  sexo = SEXO,
  edad = EDAD,
  n_orden_menor = NORDEN,
  clase = SPCLASE,
  factor = FACTOR,
  percep_peso_menor = K97,
  sedentarismo = G1_57,
  peso = K95,
  altura = K96,
  imc = MSIMC,
  estrato = ESTRATO,
  ccaa = CCAA,
  rel_con_menor = P4_1
)

# hogar2006
hogar2006 <- hogar2006 %>% rename(edad_i = EDAD, sexo_i = SEXO, nacionalidad = B3,
                                  pais = B4, ecivil = B5, ingresos = E3, 
                                  id = NIDENTIF, labor = C8, adultos_hogar = A9_1, 
                                  menores_hogar = A9_2, estudios = A12, v_limpieza = D3_4,
                                  v_verde = D3_7)

# remove last two digits from menor id and hogar id to create a variable to join databases
ense2006$ID <- substr(ense2006$id, 1, 5)
hogar2006$ID <- substr(hogar2006$id, 1, 5)

# join all relevant hogar variables to ense2006
ense2006 <- ense2006 %>%
  left_join(
    hogar2006 %>% select(
      ID, edad_i, sexo_i, nacionalidad, pais, ecivil, ingresos,
      labor, adultos_hogar, menores_hogar, estudios, v_limpieza, v_verde
    ),
    by = "ID"
  )

# convert variable types appropriately 
# str(ense2006)
ense2006 <- ense2006 %>%
  mutate(
    # numeric
    edad = as.numeric(edad),
    peso = as.numeric(peso),
    altura = as.numeric(altura),
    edad_i = as.numeric(edad_i),
    adultos_hogar = as.numeric(adultos_hogar),
    menores_hogar = as.numeric(menores_hogar),
    estudios = as.numeric(estudios),
    
    # factors
    sexo = factor(sexo),
    clase = factor(clase),
    percep_peso_menor = factor(percep_peso_menor),
    sedentarismo = factor(sedentarismo),
    imc = factor(imc),
    TMUNI = factor(TMUNI),
    estrato = factor(estrato),
    ccaa = factor(ccaa),
    rel_con_menor = factor(rel_con_menor),
    sexo_i = factor(sexo_i),
    nacionalidad = factor(nacionalidad),
    pais = factor(pais),
    ecivil = factor(ecivil),
    ingresos = factor(ingresos),
    labor = factor(labor),
    estudios = factor(estudios),
    v_limpieza = factor(v_limpieza),
    v_verde = factor(v_verde)
  )
# View(ense2006)
str(ense2006)

# new survey variable
ense2006 <- ense2006 %>% 
  mutate(encuesta = "2006")

# add categories to variables

#### ENSE 2012 ####
# menores2012
ense2011 <- menores2012 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, J59, K61,
  J57, J58, IMCm, ESTRATO, CCAA, Informante_5, A2_1b, A1_1)

# rename relevant variables
ense2011 <- ense2011 %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR,
  factor = FACTORMENOR,
  percep_peso_menor = J59,
  sedentarismo = K61,
  peso = J57,
  altura = J58,
  imc = IMCm,
  estrato = ESTRATO,
  ccaa = CCAA,
  rel_con_menor = Informante_5,
  nacionalidad = A2_1b,
  pais = A1_1
)

# hogar2012
hogar2012 <- hogar2012 %>% rename(edad_i = EDAD_i, sexo_i = SEXO_i, n_inf = NORDEN_Ai,
                                  ingresos = D28, id = IDENTHOGAR, labor = B21a, adultos_hogar = NADULTOS, 
                                  menores_hogar = NMENORES, estudios = A10_i, v_limpieza = C24_4, v_verde = C24_7)

# join databases
ense2011 <- ense2011 %>%
  left_join(hogar2012 %>% select(id, n_inf, edad_i, sexo_i, ingresos, labor, adultos_hogar, 
                                 menores_hogar, estudios, v_limpieza, v_verde), join_by("id", "n_inf"))

# convert variable types properly
# str(ense2011)
ense2011 <- ense2011 %>%
  mutate(
    # Convert characters to numeric
    edad = as.numeric(edad),
    edad_i = as.numeric(edad_i),
    peso = as.numeric(peso),
    altura = as.numeric(altura),
    ingresos = as.numeric(ingresos),
    adultos_hogar = as.numeric(adultos_hogar),
    menores_hogar = as.numeric(menores_hogar),
    estudios = as.numeric(estudios),
    
    # Convert appropriate variables to factors
    sexo = factor(sexo),
    clase = factor(clase),
    percep_peso_menor = factor(percep_peso_menor),
    sedentarismo = factor(sedentarismo),
    imc = factor(imc),
    estrato = factor(estrato),
    ccaa = factor(ccaa),
    rel_con_menor = factor(rel_con_menor),
    nacionalidad = factor(nacionalidad),
    pais = factor(pais),
    sexo_i = factor(sexo_i),
    labor = factor(labor),
    estudios = factor(estudios),
    v_limpieza = factor(v_limpieza),
    v_verde = factor(v_verde)
  )
# need to label categories of variables
# View(ense2011)

# new survey variable
ense2011 <- ense2011 %>% 
  mutate(encuesta = "2011")

#### ENSE 2017 ####
# menores2017
ense2017 <- menores2017 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, J59, K61,
  J57, J58, IMCm, CCAA, Informante_5, A2_1b, A1_1)

# rename relevant variables
ense2017 <- ense2017 %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR,
  factor = FACTORMENOR,
  percep_peso_menor = J59,
  sedentarismo = K61,
  peso = J57,
  altura = J58,
  imc = IMCm,
  ccaa = CCAA,
  rel_con_menor = Informante_5,
  nacionalidad = A2_1b,
  pais = A1_1
)

# hogar2017
hogar2017 <- hogar2017 %>% rename(edad_i = EDAD_i, sexo_i = SEXO_i, n_inf = NORDEN_Ai, estrato = ESTRATO,
                                  ingresos = D29, id = IDENTHOGAR, labor = B21a, adultos_hogar = NADULTOS, 
                                  menores_hogar = NMENORES, estudios = A10_i, v_limpieza = C24_4, v_verde = C24_7)

# join databases
ense2017 <- ense2017 %>%
  left_join(hogar2017 %>% select(id, n_inf, edad_i, sexo_i, ingresos, labor, adultos_hogar, 
                                 menores_hogar, estudios, v_limpieza, v_verde, estrato), join_by("id", "n_inf"))

# convert variable types properly
# str(ense2017)
ense2017 <- ense2017 %>%
  mutate(
    # Convert characters to numeric
    edad = as.numeric(edad),
    edad_i = as.numeric(edad_i),
    peso = as.numeric(peso),
    altura = as.numeric(altura),
    ingresos = as.numeric(ingresos),
    adultos_hogar = as.numeric(adultos_hogar),
    menores_hogar = as.numeric(menores_hogar),
    estudios = as.numeric(estudios),
    
    # Convert appropriate variables to factors
    sexo = factor(sexo),
    clase = factor(clase),
    percep_peso_menor = factor(percep_peso_menor),
    sedentarismo = factor(sedentarismo),
    imc = factor(imc),
    estrato = factor(estrato),
    ccaa = factor(ccaa),
    rel_con_menor = factor(rel_con_menor),
    nacionalidad = factor(nacionalidad),
    pais = factor(pais),
    sexo_i = factor(sexo_i),
    labor = factor(labor),
    estudios = factor(estudios),
    v_limpieza = factor(v_limpieza),
    v_verde = factor(v_verde)
  )
# need to label categories of variables

# new survey variable
ense2017 <- ense2017 %>% 
  mutate(encuesta = "2017")
# View(ense2017)

#### ENSE 2023 ####
# menores2023
ense2023 <- menores2023 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, NORDENm, FACTORMENOR, J3m, K2m,
  J1m, J2m, IMC, CCAA, Informante_5, A4_2m, A1m, A2m, A3m)

# rename relevant variables
ense2023 <- ense2023 %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = NORDENm,
  factor = FACTORMENOR,
  percep_peso_menor = J3m,
  sedentarismo = K2m,
  peso = J1m,
  altura = J2m,
  imc = IMC,
  ccaa = CCAA,
  rel_con_menor = Informante_5,
  nacionalidad = A4_2m,
  pais = A1m,
  pais_m = A2m,
  pais_p = A3m
)

# hogar2023
hogar2023 <- hogar2023 %>% rename(edad_i = EDAD, sexo_i = SEXO, n_inf = NORDEN, estrato = ESTRATO, clase = CLASE_PR,
                                  ingresos = INGRESOS, id = IDENTHOGAR, labor = B21a, adultos_hogar = NADULTOS, 
                                  menores_hogar = NMENORES, estudios = A10, v_limpieza = C22_4, v_verde = C22_7)

# join databases
ense2023 <- ense2023 %>%
  left_join(hogar2023 %>% select(id, n_inf, edad_i, sexo_i, estrato, clase, ingresos, labor, adultos_hogar, 
                                 menores_hogar, estudios, v_limpieza, v_verde), join_by("id", "n_inf"))

# convert variable types properly
# str(ense2023)
ense2023 <- ense2023 %>%
  mutate(
    # Convert characters to numeric
    edad = as.numeric(edad),
    edad_i = as.numeric(edad_i),
    peso = as.numeric(peso),
    altura = as.numeric(altura),
    ingresos = as.numeric(ingresos),
    adultos_hogar = as.numeric(adultos_hogar),
    menores_hogar = as.numeric(menores_hogar),
    estudios = as.numeric(estudios),
    
    # Convert appropriate variables to factors
    sexo = factor(sexo),
    clase = factor(clase),
    percep_peso_menor = factor(percep_peso_menor),
    sedentarismo = factor(sedentarismo),
    imc = factor(imc),
    estrato = factor(estrato),
    ccaa = factor(ccaa),
    rel_con_menor = factor(rel_con_menor),
    nacionalidad = factor(nacionalidad),
    pais = factor(pais),
    pais_m = factor(pais_m),
    pais_p = factor(pais_p),
    sexo_i = factor(sexo_i),
    labor = factor(labor),
    estudios = factor(estudios),
    v_limpieza = factor(v_limpieza),
    v_verde = factor(v_verde)
  )
# need to label categories of variables

# new survey variable
ense2023 <- ense2023 %>% 
  mutate(encuesta = "2023")
# View(ense2023)

#### Join surveys ####
ense_list <- list(ense2003, ense2006, ense2011, ense2017, ense2023)
ense_list

