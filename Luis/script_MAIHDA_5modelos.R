##### Analisis MAIHDA con 5 modelos random effects


#### packages  ####

library(lme4)
library(tidyverse)
library(dplyr)
library(haven)
library(ggeffects)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)


## Crear la variable stratum


# Generate the stratum ID
dta$stratum <- 1000*dta$sexo + 100*dta$edad_cat_ine + 10*dta$ses_dico + 
  1*dta$nuts1 


# Turn the stratum identifier into a factor variable
dta$stratum <- as.factor(dta$stratum)

# reattach factor labels from original Stata dataset
dta <- unlabelled(dta)

# sort data by stratum
dta <- dta[order(dta$stratum),]




# Read data
Data <- dta

# Ensure factors
Data$sexo <- factor(Data$sexo)
Data$edad_cat_ine <- factor(Data$edad_cat_ine)
Data$ses_dico <- factor(Data$ses_dico)
Data$nuts1 <- factor(Data$nuts1)
Data$survey_dico <- factor(Data$survey_dico)


##################################
# MODEL 1: NULL MODEL
##################################


model3A <- glmer(obesidad ~ (1|stratum), data=dta, family=binomial)

# Resumen del modelo, aquí veo random y fixed effects

summary(model3A)

## Aquí los fixed effects exponenciados

tab_model(model3A)

## calcular el VPC --> Var level2 / (Var level 2 + 3.29) *100

# Varianza level 2

vc3a <-as.data.frame(VarCorr(model3A))
vc3a

# VPC

VPC_n <- vc3a[1,4]/ (vc3a[1,4] +3.29) *100
VPC_n






##################################
# MODEL 2: FIXED survey_dico
##################################

## con lme4

# modelo con variable año pre/pos-crisis como fixed effect

model3B <- glmer(obesidad ~ survey_dico +  (1|stratum), data= Data, family=binomial)

summary(model3B)

tab_model(model3B)

# Extraer varianza nivel 2 del modelo

vc3b <-as.data.frame(VarCorr(model3B))

vc3b


## Calclar VPC --> Var level2 / (Var level 2 + 3.29) *100

VPC_1 <- vc3b[1,4]/ (vc3b[1,4] +3.29) *100
VPC_1


### Calcular el PVC 

PCV1 <- (vc3a[1,4] - vc3b[1,4]) / vc3a[1,4]
PCV1






##################################
# MODEL 3: RANDOM survey_dico
##################################

## modelo con variable año como efecto fijo y aleatorio


## con lme4

model3C <- glmer(obesidad ~ survey_dico +  (survey_dico|stratum), data= Data, family=binomial(link = "logit"))

summary(model3C)

tab_model(model3C)

## Extraer Varianzas


vc3c <-as.data.frame(VarCorr(model3C))
vc3c

# Varianza stratum (intercept) = varianza pre-crisis

vc3c0 <- vc3c[1,4]
vc3c0

# Varianza pos-crisis --> Varianza level 2 (stratum) + 2* covarianza + pendiente aleatoria 

# varianza stratum 

vc3c0

# covarianza

cov_vc3c <- vc3c[3,4]
cov_vc3c

# pendiente aleatoria

rs_vc3c <- vc3c[2,4]
rs_vc3c

# varianza post-crisis

vc3c1 <- vc3c0 + 2*cov_vc3c + rs_vc3c
vc3c1



### Calcual VPC

## VPC pre-crisis  --> var level 2 / var level 2 +3.29 *100

VPC_2 <- vc3c0/ (vc3c0 +3.29) *100
VPC_2

## VPC post-crisis

VPC_3 <- vc3c1/ (vc3c1 +3.29 )*100
VPC_3





##################################
# MODEL 4: additive effects
##################################


## MODELO CON ADDITIVE EFFECTS 

model3D <- glmer(obesidad ~ survey_dico + sexo + edad_cat_ine + ses_dico + nuts1 +  (1|stratum), data= Data, family=binomial)

summary(model3D)

tab_model(model3D)


# Extraer varianza nivel 2 del modelo

vc3d <-as.data.frame(VarCorr(model3D))

vc3d


## Calclar VPC --> Var level2 / (Var level 2 + 3.29) *100

VPC_4 <- vc3d[1,4]/(vc3d[1,4]+3.29) *100
VPC_4


### Calcular el PVC comparar model null vs model4

PCV2 <- ((vc3a[1,4] - vc3d[1,4]) / vc3a[1,4])*100
PCV2


####################################################
# MODEL 5 : random and fixed effets + year fixed
####################################################


## Con lme4

model3E <- glmer(obesidad ~ survey_dico + sexo + edad_cat_ine + ses_dico + nuts1 +  (survey_dico|stratum), data= Data, family=binomial)

summary(model3E)

tab_model(model3E)


### Con gmmTBM

m_5 <- glmmTMB(obesidad ~ 1 + survey_dico + sexo + edad_cat_ine + ses_dico + nuts1 + (survey_dico | sexo:edad_cat_ine:ses_dico:nuts1), 
               data = Data, family = binomial(link = 'logit'))



## Extraer Varianzas


vc3e <-as.data.frame(VarCorr(model3E))
vc3e


# Varianza stratum (intercept) = varianza pre-crisis

vc3e0 <- vc3e[1,4]
vc3e0

# Varianza pos-crisis --> Varianza level 2 (stratum) + 2* covarianza + pendiente aleatoria 

# varianza stratum 

vc3e0

# covarianza

cov_vc3e <- vc3e[3,4]
cov_vc3e

# pendiente aleatoria

rs_vc3e <- vc3e[2,4]
rs_vc3e

# varianza post-crisis

vc3e1 <- vc3e0 + 2*cov_vc3e + rs_vc3e
vc3e1



### Calcual VPC

## VPC pre-crisis  --> var level 2 / var level 2 +3.29 *100

VPC_4 <- vc3e0/ (vc3e0 +3.29) *100
VPC_4

## VPC post-crisis

VPC_5 <- vc3e1/ (vc3e1 +3.29 )*100
VPC_5




###PCV pre-crisis --> var level 2 (modelo3) - var level 2 (modelo5)/var level 2 (modelo3) *100

PCV3 <- ((vc3c0 - vc3e0) / vc3c0)*100
PCV3


### PCV post-crisis --> [ (var post modelo3/var post modelo3) - var post modelo 5/var post modelo 3)]


PCV3 <- (1- (vc3e1/vc3c1))*100
PCV3




m2Bu <- REsim(model3E)
plotREsim(m2Bu)






#### Predicciones obesidad por estratros

## Modelo 5


# predict the fitted linear predictor, and confidence intervals, on the 
# probability scale
m3Em_prob_total <- predictInterval(model3E, level=0.95, which = "full", include.resid.var=FALSE, type="probability")

m3Em_prob_fixed <- predictInterval(model3E, level=0.95, which = "fixed", include.resid.var=FALSE, type="probability")


# create a new id variable for this newly created dataframe
m3Em_prob_total <- mutate(m3Em_prob_total, id=row_number())

# create a new id variable for this newly created dataframe
m3Em_prob_fixed <- mutate(m3Em_prob_fixed, id=row_number())


# create an id variable for merging in the tut dataframe
Data$id <- seq.int(nrow(Data))



# merge in m3Cm_prob
dta3 <- merge(Data, m3Em_prob_total, by="id")

dta3 <- dta3 %>%
  rename(
    m3Cmfit_total=fit,
    m3Cmupr_total= upr,
    m3Cmlwr_total=lwr
  )

dta3 <- merge(dta3, m3Em_prob_fixed, by="id")

dta3 <- dta3 %>%
  rename(
    m3Cmfit_fixed=fit,
    m3Cmupr_fixed= upr,
    m3Cmlwr_fixed=lwr
  )



#### Con 84 estratos

stratum_level1 <- aggregate(
  x = dta3[c("obesidad", "m3Cmfit_total", "m3Cmlwr_total", "m3Cmupr_total", "m3Cmfit_fixed", "m3Cmlwr_fixed", "m3Cmupr_fixed")],
  by = dta3[c("stratum")],
  FUN = mean
)

# pasar a porcentaje
stratum_level1 <- stratum_level1 %>%
  mutate(across(
    c(m3Cmfit_total, m3Cmlwr_total, m3Cmupr_total,
      m3Cmfit_fixed, m3Cmlwr_fixed, m3Cmupr_fixed),
    ~ .x * 100
  ))


stratum_level1 <- stratum_level1 %>%
  mutate(
    se_total = (m3Cmupr_total - m3Cmlwr_total) / (2 * 1.96),
    se_fixed = (m3Cmupr_fixed - m3Cmlwr_fixed) / (2 * 1.96)
  )


stratum_level1 <- stratum_level1 %>%
  mutate(
    se_diff = sqrt(se_total^2 + se_fixed^2)
  )

stratum_level1 <- stratum_level1 %>%
  mutate(
    dif_mean = m3Cmfit_total - m3Cmfit_fixed,
    dif_lwr  = dif_mean - 1.96 * se_diff,
    dif_upr  = dif_mean + 1.96 * se_diff
  )

# Rank the predicted stratum probabilities
stratum_level1 <- stratum_level1 %>%
  mutate(rank3=rank(m3Cmfit_total))

# Rank the difference ofpredicted stratum probabilities

stratum_level1 <- stratum_level1 %>%
  mutate(rank4=rank(dif_mean))


# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level1, aes(x = rank3, y = m3Cmfit_total)) +
  geom_point() +
  geom_pointrange(aes(ymin = m3Cmlwr_total, ymax = m3Cmupr_total)) +
  geom_text(
    aes(
      x = rank3,
      y = m3Cmupr_total + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Predicted Percent Obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw()


# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level1, aes(x = rank4, y = dif_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = dif_lwr, ymax = dif_upr)) +
  geom_text(
    aes(
      x = rank4,
      y = dif_upr + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Probability difference in total and fixed effect for obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw()


# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level1 <- stratum_level1[order(stratum_level1$rank3),]
head(stratum_level1)
tail(stratum_level1)


#### Con 168 estratos

stratum_level <- aggregate(
  x = dta3[c("obesidad", "m3Cmfit_total", "m3Cmlwr_total", "m3Cmupr_total", "m3Cmfit_fixed", "m3Cmlwr_fixed", "m3Cmupr_fixed")],
  by = dta3[c("survey_dico", "stratum")],
  FUN = mean
)

# pasar a porcentaje
stratum_level <- stratum_level %>%
  mutate(across(
    c(m3Cmfit_total, m3Cmlwr_total, m3Cmupr_total,
      m3Cmfit_fixed, m3Cmlwr_fixed, m3Cmupr_fixed),
    ~ .x * 100
  ))


stratum_level <- stratum_level %>%
  mutate(
    se_total = (m3Cmupr_total - m3Cmlwr_total) / (2 * 1.96),
    se_fixed = (m3Cmupr_fixed - m3Cmlwr_fixed) / (2 * 1.96)
  )


stratum_level <- stratum_level %>%
  mutate(
    se_diff = sqrt(se_total^2 + se_fixed^2)
  )

stratum_level <- stratum_level %>%
  mutate(
    dif_mean = m3Cmfit_total - m3Cmfit_fixed,
    dif_lwr  = dif_mean - 1.96 * se_diff,
    dif_upr  = dif_mean + 1.96 * se_diff
  )


# Rank the predicted stratum probabilities
stratum_level <- stratum_level %>%
  mutate(rank3=rank(m3Cmfit_total))

# Rank the difference ofpredicted stratum probabilities

stratum_level <- stratum_level %>%
  mutate(rank4=rank(dif_mean))


### Averiguar media general para mostrarla en el grafico

mean(stratum_level$m3Cmfit_total)

# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level, aes(x = rank4, y = dif_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = dif_lwr, ymax = dif_upr)) +
  geom_text(
    aes(
      x = rank4,
      y = dif_upr + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("DiferencePredicted Percent Obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw()



### Predicciones 

stratum_level$survey_dico1 <- factor(
  stratum_level$survey_dico,
  levels = c(0, 1),
  labels = c("2003/2006/2011", "2017/2023")
)



## grafica con colores

ggplot(stratum_level, aes(x = rank4, y = dif_mean)) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  geom_point(color = "black") +
  geom_pointrange(
    aes(
      ymin = dif_lwr,
      ymax = dif_upr,
      color = survey_dico1
    ),
    linewidth = 0.8,
    position = position_jitter(width = 0.01, height = 0)
  ) +
  scale_color_manual(
    name = "Period",
    values = c(
      "2003/2006/2011" = "#1F78B4",
      "2017/2023" = "#E31A1C"
    )
  ) +
  ylab("Probability difference in total and fixed effect for obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )


### Si quiere incluir lbels a la figura

##+
#geom_text(
 # aes(
 #   x = rank4,
  #  y = dif_upr + 1,
   # label = stratum
  #),
  #angle = 90,
  #vjust = 0,
  #size = 3
#) 



#### predicciones obesidad por estarto y periodo de encuesta

ggplot(stratum_level, aes(x = rank3, y = m3Cmfit_total)) +
  geom_hline(yintercept = 12.54, color = "black", size = 0.3) +
  geom_point(color = "black") +
  geom_pointrange(
    aes(
      ymin = m3Cmlwr_total,
      ymax = m3Cmupr_total,
      color = survey_dico1
    ),
    linewidth = 0.8
  )  +
  scale_color_manual(
    name = "Period",
    values = c(
      "2003/2006/2011" = "#1F78B4",
      "2017/2023" = "#E31A1C"
    )
  ) +
  ylab("Predicted Percent Obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )





# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level, aes(x = rank3, y = m3Cmfit)) +
  geom_point() +
  geom_pointrange(aes(ymin = m3Cmlwr, ymax = m3Cmupr)) +
  geom_text(
    aes(
      x = rank3,
      y = m3Cmupr + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Predicted Percent Obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw()




ggplot(stratum_level, aes(x = rank3, y = m3Cmfit)) +
  geom_point(color = "black") +
  geom_pointrange(
    aes(
      ymin = m3Cmlwr,
      ymax = m3Cmupr,
      color = survey_dico1
    ),
    linewidth = 0.8
  ) +
  geom_text(
    aes(
      x = rank3,
      y = m3Cmupr + 1,
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  scale_color_manual(
    name = "Period",
    values = c(
      "Pre-crisis" = "#1F78B4",
      "Post-crisis" = "#E31A1C"
    )
  ) +
  ylab("Predicted Percent Obesity, Model 5") +
  xlab("Stratum Rank") +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )



# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank3),]
head(stratum_level)
tail(stratum_level)

stratum_level0 <- stratum_level %>%
  filter(survey_dico == 0) %>% 
  mutate(rank30=rank(m3Cmfit))

# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level0 <- stratum_level0[order(stratum_level0$rank30),]
head(stratum_level0)
tail(stratum_level0)

stratum_level1 <- stratum_level %>%
  filter(survey_dico == 1) %>% 
  mutate(rank30=rank(m3Cmfit))


stratum_level1 <- stratum_level1[order(stratum_level1$rank30),]
head(stratum_level1)
tail(stratum_level1)






## obtencion de predicciones con el modelo de gmmTMB

m3E_predictions_RE1 <- predict_response(m_5, terms = c("survey_dico", "sexo", "edad_cat_ine", "ses_dico", "nuts1"),type = "random",interval = "confidence")

