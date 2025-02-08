install.packages("usethis")

library(eph)
library(openxlsx)
library(tidyverse)
library(usethis)
use_git_config(user.name="gasparh440", user.email="tucorreo@gmail.com.ar") # La dirección de correo debe coincidir con la ingresada en Github

#entorno de trabajo
"%+%" <- function(x,y) paste(x,y,sep="")
#wd GASPO
setwd("C:/Users/Hache/Documents/GitHub/Curso-R-ASET") 
#wd AGUS
setwd("")
getwd()
data_dir <- getwd() %+% "/data/"
results_dir <- getwd() %+% "/results/" 

#
#cargo las bases
BASE24 <-get_microdata(year = 2024 , period = 3, type= "individual")

#
gitstatus

