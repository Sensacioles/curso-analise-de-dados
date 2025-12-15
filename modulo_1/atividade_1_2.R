library(tidyverse)
library(readxl)
library(arrow)

setwd("/home/gio/RStudio/curso-analise-de-dados/dados")
df_csv <- read_csv("sim_salvador_2023_processado.csv")
view(df_csv)

# Atividade 1: Exploração e Transformação de Dados
# 1.1 Crie uma nova variável chamada "faixa_etaria" que classifique as idades em quatro categorias:
# "Criança" para idades de 0 a 12 anos, "Adolescente" para 13 a 17 anos, "Adulto" para 18 a 59 anos e
# "Idoso" para 60 anos ou mais.

df_faixa <- df_csv %>% mutate(faixa_etaria=case_when(
  idade_anos>=0 & idade_anos<=12 ~ "Criança",
  idade_anos>=13 & idade_anos<=17 ~ "Adolescente",
  idade_anos>=18 & idade_anos<=59 ~ "Adulto",
  idade_anos>60 ~ "Idoso"
))

# 1.2 Conte quantos óbitos há em cada faixa etária criada.

# Usando count()
df_obitos_count <- df_faixa %>% count(faixa_etaria)

#Usando group_by() + summarize()
df_obitos_group_summarize <- df_faixa %>% 
  group_by(faixa_etaria) %>% 
  summarize(qtd_obitos=n())

# Atividade 2 Manipulação de Datas e Agrupamento
# 2.1 Crie uma variável chamada "trimestre" que identifique em qual trimestre do ano ocorreu o
# óbito. Os trimestres devem ser classificados como: "1º Trimestre" para janeiro, fevereiro e março;
# "2º Trimestre" para abril, maio e junho; "3º Trimestre" para julho, agosto e setembro; e "4º
# Trimestre" para outubro, novembro e dezembro.
df_trimestre <- df_csv %>% mutate(mes_obito=month(DTOBITO_dt),
  trimestre=case_when(
    mes_obito %in% c(1,2,3) ~ "1o Trimestre",
    mes_obito %in% c(4,5,6) ~ "2o Trimestre",
    mes_obito %in% c(7,8,9) ~ "3o Trimestre",
    mes_obito %in% c(10,11,12) ~ "4o Trimestre",
))

# 2.2 Calcule o total de óbitos e a idade média por trimestre e por sexo.
df_obitos_trim_sexo <- df_trimestre %>%
  group_by(trimestre,sexo_p) %>% 
    summarize(qtd_obitos=n(),media_idade=round(mean(idade_anos),0),
              .groups="drop")

# Atividade 3: Análise Integrada
# 3.1 Identifique qual foi o mês com maior número de óbitos no ano de 2023.
df_ranking_mes <- df_trimestre %>%
  group_by(mes_obito) %>% 
    summarize(qtd_obitos=n()) %>% 
      arrange(desc(qtd_obitos))

# 3.2 Calcule a diferença percentual entre o número de óbitos masculinos e femininos.
df_dif_perc <- df_trimestre %>% 
  count(sexo_p) %>% 
    mutate(perc=(n/sum(n))*100,
           perc=round(perc,2),
           perc=paste(as.character(perc),"%"))
# 3.3 Determine qual faixa etária teve o maior número de óbitos ao longo do ano.
df_ranking_ano <- df_faixa %>%
  count(faixa_etaria,sort=TRUE) %>% 
    mutate(perc=(n/sum(n))*100,
           perc=round(perc,2),
           perc=paste(as.character(perc),"%"))
