# ense2006 data education

## ENSE 2006 ####
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
  rel_con_menor = P4_1,
  tamano = TMUNI)

# hogar2006
hogar2006 <- hogar2006 %>% rename(edad_i = EDAD, sexo_i = SEXO, nacionalidad = B3,
                                  pais = B4, adultos_hogar = A9_1, id = NIDENTIF,
                                  menores_hogar = A9_2, estudios = A12, v_limpieza = D3_4,
                                  v_verde = D3_7)

unique(hogar2006$estudios)

# check if ids in common have estudios that are considered high i.e., 5:9 
common_ids <- intersect(hogar2006$id, ense2006$id)
str(common_ids) #9122 entries

# estudios as numeric
hogar2006 <- hogar2006 %>%
  mutate(
    estudios = as.numeric(estudios), 
    estudios = na_if(estudios, 99), 
    estudios = na_if(estudios, 0),
  )
unique(hogar2006$estudios)

hogar2006 %>%
  filter(estudios %in% 5:9)

summary(hogar2006$estudios)

result <- hogar2006 %>%
  filter(id %in% common_ids, estudios %in% 5:9)
print(result)

table(hogar2006$estudios, useNA = "ifany")

# even before joining hogar and ense databases, can see that the ids in common, 
# none have high level education? 

