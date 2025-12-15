#6.2.1 Operador pipe, encadeia operações. Atalho: Ctrl+Shift+M
# %>% 
#6.2.2 Usando count() do tidyverse 
df_csv %>% count(rating,sort=TRUE)

#6.3 Criar nova variável "ano" no df usando o mutate()
df_csv <- df_csv %>% mutate(ano=year(released))

#7.1.1 Variável condicional
df_csv <- df_csv %>%  mutate(nota=if_else(rating<=3,"Ruim",
                                          if_else(rating>=4,"Bom",NA_character_)))

#7.1.2 Utilização do case_when
df_csv <- df_csv %>% mutate(epoca=case_when(
  ano<=1986 ~ "Arcade",
  ano>1986 & ano<=1990 ~ "8-Bits",
  ano>1990 & ano<=1995 ~ "16-Bits",
  ano>1995 & ano<=1998 ~ "64-Bits",
  ano>1998 & ano<=2004 ~ "128-Bits",
  ano>2004 & ano<=2007 ~ "Oldschool 3D",
  ano>2007 & ano<=2013 ~ "Era HD",
  ano>2013 & ano<=2018 ~ "Era 4K",
  ano>2018 ~ "Era 8K Ray-tracing"
))

#7.2 Exibe dataframe completo
view(df_csv)

#8.1 Agrupamento de dados
df_reviews_ano <- df_csv %>% 
                    group_by(id,ano,mes=month(released)) %>% 
                      summarize(reviews_totais=sum(reviews_count))

#8.2 Join dos dados
df_joined <- df_csv %>% left_join(df_reviews_ano,by="id")
