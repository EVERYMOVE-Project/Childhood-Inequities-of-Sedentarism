#### Limpieza base de datos de menores Encuesta Nacional SAlud España ####

####libraries ####

library(readr)
library(readxl)
library(xlsx)
library(tidyverse)

#### ENSE 2003 ####


## HOGAR ##

campos_hogar_2003 <- read_excel("Datos/2003/codebook_hogar2003.xlsx")

nombres_hogar_2003 <- campos_hogar_2003$VARIABLE
anchos_hogar_2003  <- campos_hogar_2003$LONGITUD %>% as.numeric

hogar2003 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2003/HOGAR03.txt", col_positions = fwf_widths(widths = anchos_hogar_2003, col_names = nombres_hogar_2003))


## MENORES ##

campos_menores_2003 <- read_excel("Datos/2003/codebook_menores2003.xlsx")

nombres_menores_2003 <- campos_menores_2003$VARIABLE
anchos_menores_2003  <- campos_menores_2003$LONGITUD %>% as.numeric

menores2003 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2003/INFANT03.txt", col_positions = fwf_widths(widths = anchos_menores_2003, col_names = nombres_menores_2003))



#### ENSE 2006 ####


## HOGAR ##

campos_hogar_2006 <- read_excel("Datos/2006/codebook_hogar2006.xlsx")

nombres_hogar_2006 <- campos_hogar_2006$VARIABLE
anchos_hogar_2006  <- campos_hogar_2006$LONGITUD %>% as.numeric

hogar2006 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2006/HOGAR06.txt", col_positions = fwf_widths(widths = anchos_hogar_2006, col_names = nombres_hogar_2006))


## MENORES ##

campos_menores_2006 <- read_excel("Datos/2006/codebook_menores2006.xlsx")

nombres_menores_2006 <- campos_menores_2006$VARIABLE
anchos_menores_2006  <- campos_menores_2006$LONGITUD %>% as.numeric

menores2006 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2006/INFANTIL06.txt", col_positions = fwf_widths(widths = anchos_menores_2006, col_names = nombres_menores_2006))





#### ENSE 2011-12 ####


## HOGAR ## (mirar nombre campos por variable)

campos_hogar_2011 <- read_excel("Datos/2011/codebook_hogar2011.xlsx")

nombres_hogar_2011 <- campos_hogar_2011$VARIABLE
anchos_hogar_2011  <- campos_hogar_2011$LONGITUD %>% as.numeric

hogar2011 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2011/HOGAR11.txt", col_positions = fwf_widths(widths = anchos_hogar_2011, col_names = nombres_hogar_2011))


## MENORES ##

campos_menores_2011 <- read_excel("Datos/2011/codebook_menores2011.xlsx")

nombres_menores_2011 <- campos_menores_2011$VARIABLE
anchos_menores_2011  <- campos_menores_2011$LONGITUD %>% as.numeric

menores2011 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2011/INFANTIL11.txt", col_positions = fwf_widths(widths = anchos_menores_2011, col_names = nombres_menores_2011))


#### ENSE 2017 ####


## HOGAR ##

## HOGAR ## (mirar nombre campos por variable)

campos_hogar_2017 <- read_excel("Datos/2017/codebook_hogar2017.xlsx")

nombres_hogar_2017 <- campos_hogar_2017$VARIABLE
anchos_hogar_2017  <- campos_hogar_2017$LONGITUD %>% as.numeric

hogar2017 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2017/MICRODAT.CH.txt", col_positions = fwf_widths(widths = anchos_hogar_2017, col_names = nombres_hogar_2017))


## MENORES ##

campos_menores_2017 <- read_excel("Datos/2017/codebook_menores2017.xlsx")

nombres_menores_2017 <- campos_menores_2017$VARIABLE
anchos_menores_2017  <- campos_menores_2017$LONGITUD %>% as.numeric

menores2017 <- read_fwf("C:/Users/luisc/OneDrive - Universidad de Alcala/UAH/articulo_MAIHDA/Datos/2017/MICRODAT.CM.txt", col_positions = fwf_widths(widths = anchos_menores_2017, col_names = nombres_menores_2017))



#### HOMOGENEIZAAR VARIABLES ####

####  ENSE 2003 ####


#### ENSE 2006 ####

ense2006 <- menores2006 %>% select(NIDENTIF, SEXO, EDAD, CCAA, P4_0_2, P4_3, P4_4,
                                   P4_1, MSIMC,K95, K96, K97, NORDEN, SPCLASE, FACTOR)

ense2006 <- ense2006 %>%
  left_join(hogar2006 %>% select(NIDENTIF, B3, B4), by = "NIDENTIF")

ense2006 <- ense2006 %>% rename(ID = NIDENTIF, SEXOm = SEXO, EDADm = EDAD, NORDEN_info = P4_0_2,
                                EDAD_info = P4_3, SEXO_info = P4_4,
                                REL_info = P4_1, IMCm = MSIMC, PESO_menor = K95,
                                ALTURA_menor = K96, PERCEP_PESO_MEN = K97, 
                                N_ORDEN = NORDEN, CLASE_PR = SPCLASE, FACTORMENOR = FACTOR)


#### ENSE 2011 ####

ense2011 <- menores2011 %>% select(IDENTHOGAR, SEXOm, EDADm, A1_1, CCAA, Informante_1a,
                                   Informante_3, Informante_4, Informante_5, A2_1a, A2_1b,
                                   A7_2m, IMCm, J57, J58, J59, CLASE_PR, FACTORMENOR)

ense2011 <- ense2011 %>% rename(ID = IDENTHOGAR, PAIS_NINO = A1_1, NORDEN_info = Informante_1a,
                               EDAD_info =  Informante_3, SEXO_info = Informante_4, 
                               REL_info =Informante_5, NAC_MEN_ESP = A2_1a, NAC_MEN_EXT = A2_1b,
                               N_ORDEN = A7_2m, PESO_menor = J57, ALTURA_menor =J58, 
                               PERCEP_PESO_MEN = J59)

#### ENSE 20017 ####

ense2017 <- menores2017 %>% select(IDENTHOGAR, SEXOm, EDADm, A1_1, CCAA, Informante_1a,
                                   Informante_3, Informante_4, Informante_5, A2_1a, A2_1b,
                                   A7_2m, IMCm, J57, J58, J59, CLASE_PR, FACTORMENOR)

ense2017 <- ense2017 %>% rename(ID = IDENTHOGAR, PAIS_NINO = A1_1, NORDEN_info = Informante_1a,
                                EDAD_info =  Informante_3, SEXO_info = Informante_4, 
                                REL_info =Informante_5, NAC_MEN_ESP = A2_1a, NAC_MEN_EXT = A2_1b,
                                N_ORDEN = A7_2m, PESO_menor = J57, ALTURA_menor =J58, 
                                PERCEP_PESO_MEN = J59)
