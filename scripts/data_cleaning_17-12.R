## Data Cleaning INE Datasets
## Finalized 28th of July 2025
## Edited 3rd of December 2025

## Load libraries
library(tidyverse)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
library(scales)
library(labelled)
library(readr)
library(readxl)
library(segmented)
library(glmmTMB)
library(sjPlot)

setwd("~/UAH/PhD Documents/INEdatos/Analysis")

## Command to remove all lists from environment
rm(list=ls())

# Databases: ####
### ENSE 2001 - INFANT01.txt
### ENSE 2003 - INFANT03.txt, HOGAR03.txt, ADULTO03.txt
### ENSE 2006 - INFANT06.txt, HOGAR06.txt, ADULTO06.txt
### ENSE 2012 - INFANT12.txt, HOGAR12.txt, MicrodatoAdultos.txt
### ENSE 2017 - MICRODAT.CM.txt, MICRODAT.CH.txt, MICRODAT.CA.txt
### ESDE 2023 - INFANT23.RData, HOGAR23.RData, ADULTOS23.RData

## ENSE 2001 ####
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

## ENSE 2003 ####
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

## adultos (16-18)
adultos2003 <- read_excel("~/UAH/PhD Documents/INEdatos/2003ENSE/Adultos-ENSE-2003/codebook_adultos2003.xlsx")
names_adultos2003 <- adultos2003$VARIABLE
width_adultos2003  <- adultos2003$LONGITUD %>% as.numeric

adultos2003 <- read_fwf("~/UAH/PhD Documents/INEdatos/2003ENSE/Adultos-ENSE-2003/ADULTO03.txt", col_positions = fwf_widths(widths = width_adultos2003, col_names = names_adultos2003))
# saveRDS(adultos2003, "~/UAH/PhD Documents/INEdatos/2003ENSE/adultos2003.rds")
rm(names_adultos2003, width_adultos2003)

## ENSE 2006 ####
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

## adultos (16-18)
adultos2006 <- read_excel("~/UAH/PhD Documents/INEdatos/2006ENSE/Adulto-ENSE-2006/codebook_adultos2006.xlsx")
names_adultos2006 <- adultos2006$VARIABLE
width_adultos2006  <- adultos2006$LONGITUD %>% as.numeric

adultos2006 <- read_fwf("~/UAH/PhD Documents/INEdatos/2006ENSE/Adulto-ENSE-2006/ADULTO06.txt", col_positions = fwf_widths(widths = width_adultos2006, col_names = names_adultos2006))
#saveRDS(adultos2006, "~/UAH/PhD Documents/INEdatos/2006ENSE/adultos2006.rds")
rm(names_adultos2006, width_adultos2006)

## ENSE 2011 ####
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

## adultos (16-18)
adultos2011 <- read_excel("~/UAH/PhD Documents/INEdatos/2012ENSE/codebook_adultos2011.xlsx")
names_adultos2011 <- adultos2011$VARIABLE
width_adultos2011  <- adultos2011$LONGITUD %>% as.numeric

adultos2011 <- read_fwf("~/UAH/PhD Documents/INEdatos/2012ENSE/datos_ensalud12/ADULTO12.txt", col_positions = fwf_widths(widths = width_adultos2011, col_names = names_adultos2011))
# saveRDS(adultos2011, "~/UAH/PhD Documents/INEdatos/2012ENSE/adultos2011.rds")
rm(names_adultos2011, width_adultos2011)

## ENSE 2017 ####
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

## adultos (16-18)
adultos2017 <- read_excel("~/UAH/PhD Documents/INEdatos/2017ENSE/codebook_adultos2017.xlsx")
names_adultos2017 <- adultos2017$VARIABLE
width_adultos2017  <- adultos2017$LONGITUD %>% as.numeric

adultos2017 <- read_fwf("~/UAH/PhD Documents/INEdatos/2017ENSE/Adultos_ENSE2017/MICRODAT.CA.txt", col_positions = fwf_widths(widths = width_adultos2017, col_names = names_adultos2017))
# saveRDS(adultos2017, "~/UAH/PhD Documents/INEdatos/2017ENSE/adultos2017.rds")
rm(names_adultos2017, width_adultos2017)

## ENSE 2023 ####
## menores
load("~/UAH/PhD Documents/INEdatos/2023ESdE/ESdEmenor_2023/R/INFANT23.RData")
menores2023 <- Microdatos
#saveRDS(menores2023, "~/UAH/PhD Documents/INEdatos/2023ESdE/menores2023.rds")

## hogar
load("~/UAH/PhD Documents/INEdatos/2023ESdE/ESdEhogar_2023/R/HOGAR23.RData")
hogar2023 <- Microdatos
#saveRDS(hogar2023, "~/UAH/PhD Documents/INEdatos/2023ESdE/hogar2023.rds")
rm(Microdatos, Metadatos)

## adultos (16-18)
load("~/UAH/PhD Documents/INEdatos/2023ESdE/ESdEadulto_2023/R/ADULTO23.RData")
adultos2023 <- Microdatos
#saveRDS(adultos2023, "~/UAH/PhD Documents/INEdatos/2023ESdE/adultos2023.rds")
rm(Microdatos, Metadatos)

# Select variables ####
## ENSE 2003 ####
# menores2003
ense2003m <- menores2003 %>% dplyr::select(
  NIDENTIF, N_INF, SEXO, EDAD, SUJ_ENTR, SPCLASE, FACTOR, D_ACFISO,
  PESO, ALTURA, TMUNI, CCAA)

# rename with matching order ## do not have to run this code again
ense2003m <- ense2003m %>% rename(
  id = NIDENTIF,
  n_inf = N_INF,
  sexo = SEXO,
  edad = EDAD,
  n_orden_menor = SUJ_ENTR,
  clase = SPCLASE,
  factor = FACTOR,
  sedentarismo = D_ACFISO,
  peso = PESO,
  altura = ALTURA,
  tamano = TMUNI,
  ccaa = CCAA
)

# hogar2003
hogar2003 <- hogar2003 %>% 
  rename(nacionalidad = NACION, 
         id = NIDENTIF, n_inf = NORDEN)


# join home-level info directly to child ID
ense2003 <- ense2003m %>%
  left_join(hogar2003 %>% select(id, nacionalidad), by = "id")

# new survey variable
ense2003 <- ense2003 %>% 
  mutate(survey = "2003")

### FINAL DATABASE 2003 AGES 0 to 18
save(ense2003, file = "ense2003.Rdata")

## ENSE 2006 ####
# menores2006
ense2006m <- menores2006 %>%
  select(NIDENTIF, SEXO, EDAD, NORDEN, SPCLASE, FACTOR, G1_57,
         K95, K96, MSIMC, TMUNI, ESTRATO, CCAA, P4_0_2) %>%
  rename(
    id = NIDENTIF,
    sexo = SEXO,
    edad = EDAD,
    n_orden_menor = NORDEN,
    clase = SPCLASE,
    factor = FACTOR,
    sedentarismo = G1_57,
    peso = K95,
    altura = K96,
    imc = MSIMC, # n = 915 missing
    tamano = TMUNI,
    estrato = ESTRATO,
    ccaa = CCAA,
    n_inf = P4_0_2
  )
summary(ense2006m$nacionalidad)
hogar2006 <- hogar2006 %>%
  rename(
    nacionalidad = B3,
    id = NIDENTIF,
    n_inf = NORDEN
  )

# join home-level info directly to child ID
ense2006 <- ense2006m %>%
  left_join(hogar2006 %>% select(id, nacionalidad), by = "id")

# new survey variable
ense2006 <- ense2006 %>% 
  mutate(survey = "2006")

### FINAL DATABASE 2006 AGES 0 to 18
save(ense2006, file = "ense2006.Rdata")

## ENSE 2011 ####
# menores2012
ense2011m <- menores2012 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, K61,
  J57, J58, IMCm, ESTRATO, CCAA, A2_1b)

# rename relevant variables
ense2011m <- ense2011m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR, # 333 missing
  factor = FACTORMENOR,
  sedentarismo = K61, # nine entries with 9 
  peso = J57, # three entries with 999
  altura = J58, # seven entries with 999
  imc = IMCm,
  estrato = ESTRATO,
  ccaa = CCAA,
  nacionalidad = A2_1b
)

ense2011m %>% 
  count(peso = 999)
# adultos2011
adultos2011 <- adultos2011 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, A7_2a, CLASE_PR, FACTORADULTO, U129, ESTRATO,
  R102, R103, IMCa, CCAA, E2_1b
)

# rename relevant variables
adultos2011 <- adultos2011 %>% rename( # n = 21,007
  id = IDENTHOGAR,
  n_inf = PROXY_2b, # 0.379% missing n = 23
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = A7_2a,
  clase = CLASE_PR, # 93.5% missing n = 19,635 (all 566 of 16-18 are NA)
  factor = FACTORADULTO,
  sedentarismo = U129, # 100% missing n = 21,007
  estrato = ESTRATO,
  peso = R102, # 99.3% missing n = 20,855
  altura = R103,
  imc = IMCa, # 9.02% missing n = 547
  ccaa = CCAA,
  nacionalidad = E2_1b # 9.27% missing n = 562
)

# join adultos <= 18 and hogar databases
adultos2011$edad <- as.numeric(adultos2011$edad) # to be able to filter
ense2011m$edad <- as.numeric(ense2011m$edad) # to match variable type

ense2011a <- adultos2011 %>% 
  filter(edad <= 15)

# merge menores and adultos <= 18 databases
# check variable names 
names(ense2011m)
names(ense2011a)

# ensure variables all same type
ense2011a$n_inf <- as.character(ense2011a$n_inf)
ense2011m$n_orden_menor <- as.character(ense2011m$n_orden_menor)
ense2011m$peso <- as.numeric(ense2011m$peso)

# combined menores and adults aged 16-18
ense2011 <- bind_rows(ense2011m, ense2011a)

# new survey variable
ense2011 <- ense2011 %>% 
  mutate(survey = "2011")

### FINAL DATABASE 2011 AGES 0 to 18
save(ense2011, file = "ense2011.Rdata")

## ENSE 2017 ####
# menores2017
ense2017m <- menores2017 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, A7_2m, CLASE_PR, FACTORMENOR, K61,
  J57, J58, IMCm, CCAA, A2_1b)
# rename relevant variables
ense2017m <- ense2017m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a,
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = A7_2m,
  clase = CLASE_PR,
  factor = FACTORMENOR,
  sedentarismo = K61,
  peso = J57,
  altura = J58,
  imc = IMCm,
  ccaa = CCAA,
  nacionalidad = A2_1b
)

# hogar2017
hogar2017 <- hogar2017 %>% rename(n_inf = NORDEN_Ai, estrato = ESTRATO, 
                                  id = IDENTHOGAR)

# adultos2017
adultos2017 <- adultos2017 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, A7_2a, CLASE_PR, FACTORADULTO, T112,
  S110, S109, IMCa, CCAA, E2_1b
)

# rename relevant variables
adultos2017 <- adultos2017 %>% rename(
  id = IDENTHOGAR,
  n_inf = PROXY_2b, # 97% missing n = 22,404
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = A7_2a,
  clase = CLASE_PR,
  factor = FACTORADULTO,
  sedentarismo = T112,
  peso = S110,
  altura = S109,
  imc = IMCa,
  ccaa = CCAA,
  nacionalidad = E2_1b
)

# join databases
# menores and hogar
ense2017m <- ense2017m %>%
  left_join(hogar2017 %>% select(id, n_inf, estrato), join_by("id", "n_inf")) 
summary(ense2017m$estrato) 

# join adultos <= 15 and hogar databases
adultos2017$edad <- as.numeric(adultos2017$edad) # to apply filter
ense2017m$edad <- as.numeric(ense2017m$edad)

# join databases for estrato despite creating duplicates (get missing of estrato because many n_inf missing)
ense2017a <- adultos2017 %>% 
  filter(edad <= 15) %>% 
  left_join(hogar2017 %>% 
              select(id, estrato), join_by("id")) %>%
  distinct(id, .keep_all = TRUE)  # Keep only one row per id

# merge menores and adultos <= 15 databases
# check variable names 
names(ense2017m)
names(ense2017a)

# check all same variable types
ense2017m$n_orden_menor <- as.character(ense2017m$n_orden_menor)
ense2017m$altura <- as.numeric(ense2017m$altura)

# combined menores (all <= 15)
ense2017 <- bind_rows(ense2017m, ense2017a)

# new survey variable
ense2017 <- ense2017 %>% 
  mutate(survey = "2017")

### FINAL DATABASE 2017 AGES 0 to 15
save(ense2017, file = "ense2017.Rdata")

## ENSE 2023 ####
# menores2023
ense2023m <- menores2023 %>% select(
  IDENTHOGAR, SEXOm, EDADm, Informante_1a, NORDENm, FACTORMENOR, K2m,
  J1m, J2m, IMC, CCAA, A4_2m)

# rename relevant variables
ense2023m <- ense2023m %>% rename(
  id = IDENTHOGAR,
  n_inf = Informante_1a, # n = 2 missing
  sexo = SEXOm,
  edad = EDADm,
  n_orden_menor = NORDENm,
  factor = FACTORMENOR,
  sedentarismo = K2m,
  peso = J1m,
  altura = J2m,
  imc = IMC, # n = 339 missing prior to merge
  ccaa = CCAA,
  nacionalidad = A4_2m
)

# adultos2023
adultos2023 <- adultos2023 %>% select(
  IDENTHOGAR, SEXOa, EDADa, PROXY_2b, NORDENa, FACTORADULTO, O2, 
  N2, N1, IMC, CCAA, A2a_2)

# rename relevant variables
adultos2023 <- adultos2023 %>% rename(
  id = IDENTHOGAR,
  n_inf = PROXY_2b,
  sexo = SEXOa,
  edad = EDADa,
  n_orden_menor = NORDENa,
  factor = FACTORADULTO,
  sedentarismo = O2,
  peso = N2,
  altura = N1,
  imc = IMC,
  ccaa = CCAA,
  nacionalidad = A2a_2
)

# hogar2023
hogar2023 <- hogar2023 %>% rename(n_inf = NORDEN, estrato = ESTRATO, clase = CLASE_PR,
                                  id = IDENTHOGAR)
# join menores and hogar databases
ense2023m <- ense2023m %>%
  left_join(hogar2023 %>% select(id, n_inf, estrato, clase), join_by("id", "n_inf"))

# join adultos <= 18 and hogar databases
ense2023a <- adultos2023 %>% 
  filter(edad <= 15) %>% 
  left_join(hogar2023 %>% 
              select(id, estrato, clase), 
            join_by("id")) %>%
  distinct(id, .keep_all = TRUE)  # Keep only one row per id

# merge menores and adultos <= 15 databases
# check variable names 
names(ense2023m)
names(ense2023a)

# combined menores and adults (all <= 15)
ense2023 <- bind_rows(ense2023m, ense2023a)

# new survey variable
ense2023 <- ense2023 %>% 
  mutate(survey = "2023")

### FINAL DATABASE 2023 AGES 0 to 18
save(ense2023, file = "ense2023.Rdata")
# Homogenize variables ####

# Check missingness ####
# Missing values table
## ense2003
na_table_2003 <- ense2003 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2003)*100)

print(na_table_2003, n = 14)

## ense2006
na_table_2006 <- ense2006 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2006)*100)
print(na_table_2006, n = 17)

## ense2011
na_table_2011 <- ense2011 %>% 
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2011)*100)
print(na_table_2011, n = 15)

## raw adultos2011 data missingness
na_table_2011a <- adultos2011 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(adultos2011)*100)
print(na_table_2011a, n = 577)

## ense2017
na_table_2017 <- ense2017 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2017)*100)
print(na_table_2017, n = 15)

## ense2023
na_table_2023 <- ense2023 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2023)*100)
print(na_table_2023, n = 15)

# Label Definitions ####
ccaa_labels <- c(
  "Andalusia", "Aragon", "Asturias", "Balearic Islands", "Canary Islands",
  "Cantabria", "Castile and Leon", "Castilla-La Mancha", "Catalonia",
  "Valencian Community", "Extremadura", "Galicia", "Madrid", "Murcia",
  "Navarre", "Basque Country", "La Rioja", "Ceuta and Melilla")

clase_labels <- c(
  "Class I",
  "Class II",
  "Class III",
  "Class IV",
  "Class V",
  "Class VI")

tamano_labels <- c(
  "≤2,000 inhabitants",
  "2,001–10,000 inhabitants",
  "10,001–50,000 inhabitants",
  "50,001–100,000 inhabitants",
  "100,001–400,000 inhabitants",
  "400,001–1,000,000 inhabitants",
  ">1,000,000 inhabitants")

PA_labels <- c(
  "Does not exercise",
  "Occasional physical or sports activity",
  "Physical activity several times a month",
  "Sports or physical training several times a week"
)

estrato_labels <- c(
  "Municipalities with more than 500,000 inhabitants",
  "Provincial capital municipalities (except the prior)",
  "Municipalities with more than 100,000 inhabitants (except the prior)",
  "Municipalities with 50,000 to 100,000 inhabitants (except the prior)",
  "Municipalities with 20,000 to 50,000 inhabitants (except the prior)",
  "Municipalities with 10,000 to 20,000 inhabitants",
  "Municipalities with less than 10,000 inhabitants"
)

# Data Cleaning ####
## ENSE 2003 ####
ense2003_rename <- ense2003 %>%
  mutate(
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 6), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = factor(as.numeric(ccaa), levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Spanish", 
      nacionalidad == 6 ~ "Foreign"
    ),
    nacionalidad = factor(nacionalidad),
    
    # Occupation class
    clase = na_if(clase, 7),
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    # Municipality size
    tamano = factor(
      tamano,
      levels = 1:7,
      labels = tamano_labels,
      ordered = TRUE
    ),
    
    # Urban/rural
    urb_rur = case_when(
      as.numeric(tamano) %in% c(1, 2) ~ "Rural",
      as.numeric(tamano) == 3 ~ "Semi-urban",
      as.numeric(tamano) %in% c(4, 5, 6, 7) ~ "Urban",
      TRUE ~ NA_character_
    ),
    
    # Convert urb_rur to an ordered factor
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # Anthropometry
    peso = as.numeric(na_if(peso, "999")), 
    altura = as.numeric(na_if(altura, "999")),
    imc_num = round(peso / (altura / 100)^2, 2),
    imc = NA, # for now
    
    obesity = NA,
    overweight = NA,
    
    # Sedentarism
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes",
      sedentarismo == 2 ~ "No"
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes"))
  )

## ENSE 2006 ####
ense2006_rename <- ense2006 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 6), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = factor(as.numeric(ccaa), levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 | nacionalidad == 3 ~ "Spanish",
      nacionalidad == 2 ~ "Foreign",
      nacionalidad == 9 ~ NA_character_
    ),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # tamano
    tamano = factor(
      tamano,
      levels = 1:7,
      labels = tamano_labels,
      ordered = TRUE
    ),
    
    # # Urban/rural
    # urb_rur = case_when(
    #   as.numeric(tamano) %in% c(1, 2) ~ "Rural",
    #   as.numeric(tamano) == 3 ~ "Semi-urbano",
    #   as.numeric(tamano) %in% c(4, 5, 6, 7) ~ "Urbano",
    #   TRUE ~ NA_character_
    # ),
    
    # # Convert urb_rur to an ordered factor
    # urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    imc = na_if(imc, 9),  # Treat 9 ("Not reported") as missing
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),

    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    # PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No" # not sedentary
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes"))
  )

## ENSE 2011 ####
ense2011_rename <- ense2011 %>% 
  mutate( 
    # Weights and age
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad %in% c(2, 6) ~ "Spanish",
      nacionalidad == 1 ~ "Foreign",
      TRUE ~ NA_character_
    ),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),

    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    imc = na_if(imc, 9),  # Treat 9 ("Not reported") as missing
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),

    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      (sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4) ~ "No", # not sedentary
      sedentarismo == 9 ~ NA_character_
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor)
  )

## ENSE 2017 ####
ense2017_rename <- ense2017 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Foreign",
      nacionalidad == 2 ~ "Spanish"),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = na_if(clase, 9),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr=cume_dist(as.numeric(clase)),
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("998", "999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("998", "999"), NA, altura)),
    # Combine underweight and normal weight into one category
    imc = as.numeric(imc),
    imc = na_if(imc, 9),
    imc = case_when(
      imc %in% c(1, 2) ~ 1,  # Combine Peso insuficiente and Normopeso
      imc == 3 ~ 2,          # Sobrepeso
      imc == 4 ~ 3           # Obesidad
    ),
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),
    
    # Physical Activity
    sedentarismo = case_when(
      sedentarismo == 8 | sedentarismo == 9 ~ NA_real_,
      TRUE ~ as.numeric(sedentarismo)
    ),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels),
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No",  # not sedentary
      sedentarismo == 8 | sedentarismo == 9 ~ NA_character_
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor)
  )

## ENSE 2023 ####
ense2023_rename <- ense2023 %>% 
  mutate( 
    # Weights
    factor = as.numeric(factor),
    factor2 = rescale(factor, to = c(1, 100)),
    
    # Age
    edad = as.numeric(edad),
    
    # Sex
    sexo = factor(as.numeric(sexo), levels = c(1, 2), labels = c("Male", "Female")),
    
    # Autonomous Community
    ccaa = as.numeric(ccaa),
    ccaa = ifelse(ccaa == 19, 18, ccaa),  # Merge Melilla and Ceuta
    ccaa = factor(ccaa, levels = 1:18, labels = ccaa_labels),
    
    # Nationality
    nacionalidad = case_when(
      nacionalidad == 1 ~ "Foreign",
      nacionalidad == 2 ~ "Spanish"),
    nacionalidad = factor(nacionalidad, levels = c("Spanish", "Foreign")),
    
    # Occupation class
    clase = case_when(
      clase == 8 ~ NA_real_,
      clase == 9 ~ NA_real_,
      TRUE ~ as.numeric(clase)
    ),
    clase = factor(clase, levels = 1:6, labels = clase_labels),
    clase = relevel(clase, ref = "Class I"),
    clase_tr = cume_dist(as.numeric(clase)),
    
    # Municipality size
    # Urban/rural
    urb_rur = case_when(
      as.numeric(estrato) %in% c(6) ~ "Rural",             # < 10,000 inhabitants
      as.numeric(estrato) %in% c(4, 5) ~ "Semi-urban",    # 10,000–50,000
      as.numeric(estrato) %in% c(0, 1, 2, 3) ~ "Urban",   # >50,000 or provincial capital
      TRUE ~ NA_character_
    ), 
    
    urb_rur = factor(urb_rur, levels = c("Rural", "Semi-urban", "Urban"), ordered = TRUE),
    
    # estrato
    estrato = factor(estrato, levels = 0:6, labels = estrato_labels),
    
    # Anthropometry
    peso = as.numeric(ifelse(peso %in% c("999"), NA, peso)),
    altura = as.numeric(ifelse(altura %in% c("999"), NA, altura)),
    # Combine underweight and normal weight into one category
    imc = as.numeric(imc),
    imc = na_if(imc, 9),
    imc = case_when(
      imc %in% c(1, 2) ~ 1,  # Combine Peso insuficiente and Normopeso
      imc == 3 ~ 2,          # Sobrepeso
      imc == 4 ~ 3           # Obesidad
    ),
    imc = factor(
      imc,
      levels = c(1, 2, 3),
      labels = c("Normal or underweight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    
    obesity = ifelse(imc == "Obesity", "Yes", ifelse(is.na(imc), NA, "No")),
    overweight = ifelse(imc == "Overweight", "Yes", ifelse(is.na(imc), NA, "No")),
    obesity = factor(obesity, levels = c("No", "Yes")),
    overweight = factor(overweight, levels = c("No", "Yes")),
    
    # Physical Activity
    sedentarismo = na_if(as.numeric(sedentarismo), 9),
    PA = factor(sedentarismo, levels = 1:4, labels = PA_labels), 
    
    # create binary of sedentarismo
    sedentarismo = case_when(
      sedentarismo == 1 ~ "Yes", # sedentary
      sedentarismo == 2 | sedentarismo == 3 | sedentarismo == 4 ~ "No" # not sedentary
    ),
    sedentarismo = factor(sedentarismo, levels = c("No", "Yes")),
    
    n_orden_menor = as.numeric(n_orden_menor)
  )

# Check missingness prior to joining ####
# Missing values table
## ense2003
na_table_2003 <- ense2003_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2003_rename)*100)
print(na_table_2003, n = 21)

## ense2006
na_table_2006 <- ense2006_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2006_rename)*100)
print(na_table_2006, n = 22)

## ense2011
na_table_2011 <- ense2011_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2011_rename)*100)
print(na_table_2011, n = 21)

## ense2017
na_table_2017 <- ense2017_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2017_rename)*100)
print(na_table_2017, n = 21)

## ense2023
na_table_2023 <- ense2023_rename %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(N_percentage = NA_Count / nrow(ense2023_rename)*100)
print(na_table_2023, n = 21)

# Join surveys ####
ense_list <- list(ense2003_rename, ense2006_rename, ense2011_rename, ense2017_rename, ense2023_rename)

# join all datasets together
joined <- bind_rows(ense_list) ## 0 to 15 without dropping NAs
save(joined, file = "joined.RData")

joined_clean <- joined %>% ## 0 to 15 dropping NAs
  drop_na(edad, sexo, nacionalidad, sedentarismo, clase, ccaa, urb_rur)
save(joined_clean, file = "joined_clean.RData")

joined_6 <- joined %>%  ## 6 to 15 without dropping NAs
  filter(edad >= 6)
save(joined_6, file = "joined_6.RData")

joined_clean_6 <- joined_6 %>%  ## 6 to 15 dropping NAs
  drop_na(edad, sexo, nacionalidad, sedentarismo, clase, ccaa, urb_rur)

# clean those 0 to 15
joined_clean <- joined_clean %>% 
  mutate(
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III"))
  )

joined_clean <- joined_clean %>% 
  mutate(
    clase_2 = case_when(
      clase %in% c("Class I", "Class II", "Class III") ~ "Non-Manual Workers",
      clase %in% c("Class IV", "Class V", "Class VI") ~ "Manual Workers",
      TRUE ~ NA_character_
    ),
    clase_2 = factor(clase_2, levels = c("Non-Manual Workers", "Manual Workers"))
  )

save(joined_clean, file = "joined_clean.RData")

# filter joined and complete case data to those older than 5 years
# create two categorizations of age groups

joined_clean_6 <- joined_clean_6 %>% 
  mutate(
    edad_cat = case_when(
    edad >= 6 & edad <= 11 ~ "6-11",
    edad >= 12 & edad <= 15 ~ "12-15"
  ),
  edad_cat3 = case_when(
    edad >= 6 & edad <= 9 ~ "6-9",
    edad >= 10 & edad <= 12 ~ "10-12",
    edad >= 13 & edad <= 15 ~ "13-15"
  ),
  edad_cat = factor(edad_cat, levels = c("6-11", "12-15")),
  edad_cat3 = factor(edad_cat3, levels = c("6-9", "10-12", "13-15"))
  )

joined_clean_6 <- joined_clean_6 %>% 
  mutate(
    clase_3 = case_when(
      clase %in% c("Class I", "Class II") ~ "Class I",
      clase %in% c("Class III", "Class IV") ~ "Class II",
      clase %in% c("Class V", "Class VI") ~ "Class III",
      TRUE ~ NA_character_
    ),
    clase_3 = factor(clase_3, levels = c("Class I", "Class II", "Class III"))
  )

joined_clean_6 <- joined_clean_6 %>% 
  mutate(
    clase_2 = case_when(
      clase %in% c("Class I", "Class II", "Class III") ~ "Non-Manual Workers",
      clase %in% c("Class IV", "Class V", "Class VI") ~ "Manual Workers",
      TRUE ~ NA_character_
    ),
    clase_2 = factor(clase_2, levels = c("Non-Manual Workers", "Manual Workers"))
  )

save(joined_clean_6, file = "joined_clean_6.RData") ## FINAL CLEAN DATABASE FOR RII and MAIHDA AGES 6 to 15
