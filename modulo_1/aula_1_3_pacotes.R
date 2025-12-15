#Instalação de pacotes
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("arrow")

#Carregando os pacotes
library(tidyverse)
library(readxl)
library(arrow)

#Define caminho para o diretório do arquivo
setwd("/home/gio/RStudio")

#Retorna caminho
getwd()

#4.1 Importa arquivo .csv
df_csv <- read_csv("games.csv")

#4.2 Remove dataframes não utilizados (se houver)
#rm(df_csv)

#5.1 Retorna um resumo da estrutura de dados no console
glimpse(df_csv)

#5.2 Retorna primeiros registros
head(df_csv)

#5.3 Retorna últimos registros
tail(df_csv)

#5.4 Sumariza os dados
summary(df_csv)

#6.1 Contabiliza frequência dos dados, useNA="always" inclui nulos
table(df_csv$rating, useNA="always")