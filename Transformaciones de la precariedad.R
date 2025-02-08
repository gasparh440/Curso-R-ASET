library(tidyverse)
library(eph)
library(dplyr)
library(openxlsx)
#cargo la base y le hago transformaciones
base <-get_microdata( year = 2024, period = 2, type = "individual", vars = "all", destfile = NULL )

base <- base %>% mutate (registro = case_when(PP07I==1 ~ "ASAL_MONO",
                                              PP07I==2 ~ "ASAL_enNEGRO",
                                              PP07H==2 ~ "PRECARIO(n.esp)",
                                              PP07H==1 ~ "PROTEGIDO",
                                              TRUE ~ "0"))
C_ocup_catocup_registro<- base %>% filter(ESTADO==1) %>%    summarise(Ocupados    = sum(PONDERA[ESTADO == 1]),
                                                              Asal_protegidos = sum (PONDERA[CAT_OCUP==3& PP07H==1]),
                                                              Asal_precariosTot = sum (PONDERA[CAT_OCUP==3& PP07H==2]),
                                                              Asal_precarios_i1_mono = sum (PONDERA[CAT_OCUP==3& PP07I==1]),
                                                              Asal_precarios_i2_negr = sum (PONDERA[CAT_OCUP==3& PP07I==2]),
                                                              Cuenta_propia = sum (PONDERA[CAT_OCUP==2]),
                                                              Patron = sum (PONDERA[CAT_OCUP==1]),
                                                              Trafam = sum (PONDERA[CAT_OCUP==4]))
