library(eph)
library(openxlsx)
library(tidyverse)
library(usethis)
use_git_config(user.name="agustoromero", user.email="agus.romer.98@gmail.com.ar") # La dirección de correo debe coincidir con la ingresada en Github

#entorno de trabajo
"%+%" <- function(x,y) paste(x,y,sep="")
#wd GASPO
#setwd("C:/Users/Hache/Documents/GitHub/Curso-R-ASET") 
#wd AGUS
#setwd("C:/Users/agusr/Documents/GitHub/Curso-R-ASET") 

library(tidyverse)
library(openxlsx)
library(eph)
library(plotly)
library(flexdashboard)
library(usethis)
library(haven) #para 2014 en formato stata si lo descargo de indec: data_spss <- read_sav("ruta/del/archivo.sav")
##Sector Informal clase 3: -Asalariado/a en establecimientos de hasta 5 personas -Cuentapropistas no profesionales (con ocupación de baja calificación) -Trabajador/a familiar sin salario -Patrones no profesionales en establecimientos de hasta 5 personas


##Defino variables
variables <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE",                "AGLOMERADO","CH03","CH04","CH06","ESTADO","CAT_OCUP","CAT_INAC","PP04A",                "PP04B_COD","PP07H","P21","P47T","PONDERA","PP04D_COD","PP04C",                "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","PP04C","PP03G","PP3E_TOT")


## Importo bases
eph_2014_t2 <-eph::get_microdata( year = 2014, period = 2, type = "individual", vars =  c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE",                "AGLOMERADO","CH03","CH04","CH06","ESTADO","CAT_OCUP","CAT_INAC","PP04A",                "PP04B_COD","PP07H","P21","P47T","PONDERA","PP04D_COD","PP04C",                "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PP04C","PP03G","PP3E_TOT") , destfile = NULL ) #variables, sin PONDIIO e impide sacar el ingreso

eph_2024_t2 <-eph::get_microdata( year = 2024, period = 2, type = "individual", vars = variables, destfile = NULL )

organize_labels(eph_2014_t2, type = "individual")
organize_labels(eph_2024_t2, type = "individual")
#Población según: Máximo nivel educativo alcanzado Tamaño de establecimiento Percepción de descuento jubilatorio Trabajo a tiempo parcial o completo Trabajo con duración establecida


asalariados_2014_t2 <- eph_2014_t2 %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(
    nivel.ed = factor(
      case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
                NIVEL_ED == 6 ~ "Superior Completo",
                TRUE ~ "Ns/Nr"),
      levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    tamanio.establec = factor(
      case_when(PP04C %in% 1:6  ~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 ~ "Grande",
                PP04C %in% 99   ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
    descuento_jubil = case_when(PP07H == 1 ~ "Si",
                                PP07H == 2 ~ "No"),
    part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si",
                              TRUE ~ "No"),
    tiempo.determinado = case_when(PP07C ==  1 ~ "Si",
                                   TRUE ~ "No"))

asalariados_2024_t2 <- eph_2024_t2 %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(
    nivel.ed = factor(
      case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
                NIVEL_ED == 6 ~ "Superior Completo",
                TRUE ~ "Ns/Nr"),
      levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    tamanio.establec = factor(
      case_when(PP04C %in% 1:6  ~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 ~ "Grande",
                PP04C %in% 99   ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
    descuento_jubil = case_when(PP07H == 1 ~ "Si",
                                PP07H == 2 ~ "No"),
    part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si",
                              TRUE ~ "No"),
    tiempo.determinado = case_when(PP07C ==  1 ~ "Si",
                                   TRUE ~ "No"))
