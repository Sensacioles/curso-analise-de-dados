# Declara o pacote tidyverse e dinosauRus para trabalhar com os tipos de 
# visualizações diferentes.
#install.packages("tidyverse")
#install.packages("datasauRus")

library(tidyverse)
library(datasauRus)

# 1. Quarteto de Anscombe
# Visualizar os dados-base em formato wide pelo console
anscombe

# Visualizar os dados-base em formato tidy pelo console
anscombe_tidy <- anscombe %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "conjunto"),
    names_pattern = "(.)(.)" #padrão do nome das colunas
  ) %>%
  mutate(conjunto = paste("Conjunto", conjunto))
anscombe_tidy

# Extrai estatísticas dos conjuntos, todas elas sendo iguais menos a correlacao
# por 0.001.
estatisticas_anscombe <- anscombe_tidy %>%
  group_by(conjunto) %>%
  summarise(
    media_x = mean(x),
    media_y = mean(y),
    dp_x = sd(x),
    dp_y = sd(y),
    correlacao = cor(x, y)  # Coeficiente de correlação de Pearson
  )
estatisticas_anscombe

# Visualização dos conjuntos. Mesmo com estatísticas quase idênticas, o 
# comportamento gráfico é diferente entre cada conjunto.
grafico_anscombe <- anscombe_tidy %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 2, alpha=0.8) +
  geom_smooth(method = "lm", se = FALSE,   # Linha de regressão linear
              color = "red", linewidth = 0.8) +
  facet_wrap(~conjunto, ncol = 2) +
  labs(
    title = "Quarteto de Anscombe",
    subtitle = "Mesmas estatísticas, comportamentos gráficos distintos"
  ) +
  theme_minimal()

grafico_anscombe

# 2. Datasaurus Dozen, abordagem moderna do conceito
# Lista os datasets do datasauRus
datasaurus_dozen %>% 
  distinct(dataset)

# Estatísticas dos conjuntos
estatisticas_datasaurus <- datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    media_x = round(mean(x), 2),
    media_y = round(mean(y), 2),
    dp_x = round(sd(x), 2),
    dp_y = round(sd(y), 2),
    correlacao = round(cor(x, y), 2)
  )
estatisticas_datasaurus

# Plota o gráfico dos conjuntos do datasauRus
grafico_datasaurus <- datasaurus_dozen %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.6, size = 1, color = "steelblue") +
  facet_wrap(~dataset, ncol = 4) +
  labs(
    title = "Datasaurus Dozen",
    subtitle = "As mesmas estatísticas podem ter comportamentos gráficos diferentes"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))
grafico_datasaurus

# Filtra apenas no conjunto dino
dino <- datasaurus_dozen %>% filter(dataset == "dino")

# Plota o conjunto
grafico_dino <- dino %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Conjunto Dino",
    subtitle = paste0("Média X: ", round(mean(dino$x), 2), 
                      " | Média Y: ", round(mean(dino$y), 2),
                      " | Correlação: ", round(cor(dino$x, dino$y), 2))
  ) +
  theme_minimal() 

grafico_dino

# 3.1 Boas práticas para visualizações dos dados - Proporções
# Dados do IPCA (exemplo da aula sobre gráficos enganosos)
ipca <- tibble(
  ano = 2009:2013,
  valor = c(4.31, 5.92, 6.5, 5.84, 5.91)
)

# Gŕafico plotado de forma correta e não tendenciosa, mantendo 
# proporções corretas.
grafico_ipca_correto <- ipca %>%
  ggplot(aes(x = factor(ano), y = valor)) +
  geom_col(fill = "lightblue") +
  geom_hline(yintercept = 4.5, linetype = "dashed", color = "red") +
  annotate("text", x = 5.4, y = 4.7, label = "Meta: 4.5%", size = 3) +
  labs(
    title = "IPCA no Brasil plotado de forma correta",
    x = "Ano",
    y = "IPCA (%)"
  ) +
  theme_minimal()
grafico_ipca_correto

# 3.2.1 Boas práticas para visualizações dos dados - Cores

# Dados de eventos cardiovasculares após dengue 
# IRR = Incidence Rate Ratio (valores > 1 indicam aumento de risco)
eventos_dengue <- tibble(
  desfecho = rep(c("AVC hemorrágico", "AVC isquêmico", 
                   "Infarto agudo", "Insuficiência cardíaca"), 2),
  periodo = rep(c("Dias 1-7", "Dias 8-14"), each = 4),
  IRR = c(10.90, 15.56, 13.53, 27.24,   # Risco muito elevado logo após infecção
          4.33, 3.17, 1.16, 2.45)        # Risco diminui com o tempo
)
# Gráfico plotado utilizando com pontos em vermelho para 
# destacar perídodos de risco de problemas cardíacos 
grafico_dengue <- eventos_dengue %>%
  mutate(desfecho = fct_reorder(desfecho, IRR, .fun = max, .desc = TRUE)) %>%
  ggplot(aes(x = desfecho, y = IRR, color = periodo)) +
  geom_point(position = position_dodge(width = .5),size=3) +  # pontos lado a lado
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  # Linha auxiliar para apontar o IRR=1
  annotate("text",x=4.1,y=2,label="IRR=1 (sem aumento de risco)",size=3,color="gray50") +
  scale_fill_manual(
    values = c("Dias 1-7" = "red", "Dias 8-14" = "blue"),
    name = "Período após infecção"
  ) +
  labs(
    title = "Razão de Incidência de Problemas Cardiovasculares",
    subtitle = "por Período Após Dengue",
    x = "",
    y = "IRR (Razão da Taxa de Incidência)"
  ) +
  theme_minimal() +
  # Ajuste de orientação dos rótulos e legendas
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
grafico_dengue

# 3.2.2 Cores em gradiente 
consumo_montadoras <- mtcars %>%
  rownames_to_column("modelo") %>%
  mutate(montadora = word(modelo, 1)) %>%
  group_by(montadora) %>%
  summarise(mpg_medio = mean(mpg)) %>%
  arrange(mpg_medio) 
grafico_consumo <- consumo_montadoras %>%
  mutate(montadora = fct_reorder(montadora, mpg_medio)) %>%
  ggplot(aes(x = montadora, y = mpg_medio, fill = mpg_medio)) +
  geom_col() +
  scale_fill_gradient(low = "lightgreen", high = "darkred", guide = "none") +
  coord_flip() +
  labs(
    title = "Consumo Médio de Gasolina por Montadora",
    x = "Montadora",
    y = "Média de Milhas/Galão"
  ) +
  theme_minimal()
grafico_consumo

# 4. Alternativas visuais para os mesmos dados
dados_grupos <- tibble(
  grupo = c("A", "B", "C"),
  n = c(45, 35, 20)
)

# 4.1.1 Gráfico em pizza. Ângulos podem confundir a interpretação.
# Dica: Sempre incluir porcentagens para auxiliar o entendimento.
grafico_pizza <- dados_grupos %>%
  ggplot(aes(x = "", y = n, fill = grupo)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(n, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gráfico de Pizza",
       subtitle = "Sempre incluir as porcentagens!") +
  theme_void() +
  theme(legend.position = "right")
grafico_pizza

# 4.1.2 Alternativa mais eficiente: gráfico de barras horizontais
grafico_barras_h <- dados_grupos %>%
  mutate(grupo = fct_reorder(grupo, n)) %>%
  ggplot(aes(x = grupo, y = n, fill = grupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "%")), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gráfico de barras",
       subtitle = "Alternativa mais compreensiva ao gráfico de pizza",
       x = "", y = "Percentual (%)") +
  theme_minimal() +
  ylim(0, 55)
grafico_barras_h

# 4.2.1 Série temporal. Extremamente útil para identificar tendências e 
# sazonalidades ao decorrer do tempo
set.seed(42)
serie_influenza <- tibble(
  semana = 1:52,
  ano = 2023,
  casos = round(50 + 30 * sin((semana - 10) * 2 * pi / 52) + rnorm(52, 0, 10))
) %>%
  mutate(casos = pmax(casos, 5))
grafico_serie <- serie_influenza %>%
  ggplot(aes(x = semana, y = casos)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  labs(
    title = "Casos de Influenza - 2023",
    subtitle = "Gráfico de linha é ideal para séries temporais",
    x = "Semana epidemiológica",
    y = "Número de casos"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 52, by = 4))+
  scale_y_continuous(limits = c(0,NA)) #sempre incluir o 0
grafico_serie

# 5. Exercício final: Utilizando os ensinamentos em um gráfico completo
# Um bom gráfico deve ser AUTO-EXPLICATIVO, contendo:
# - Título claro e informativo
# - Subtítulo com contexto adicional
# - Rótulos dos eixos COM UNIDADES
# - Legenda clara (quando aplicável)
# - Fonte dos dados (caption)

recem_nascidos <- tibble(
  individuo = 1:20,
  peso_g = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
             2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834)
)
grafico_completo <- recem_nascidos %>%
  ggplot(aes(x = peso_g)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(peso_g)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(recem_nascidos$peso_g) + 10, y = 4.5,
           label = paste0("Média: ", round(mean(recem_nascidos$peso_g), 0), " g"),
           hjust = 0, color = "red", size = 3.5) +
  labs(
    title = "Distribuição do peso ao nascer",
    subtitle = "Amostra de 20 recém-nascidos de uma maternidade de São Paulo",
    x = "Peso (g)",
    y = "Frequência (n)",
    caption = "Fonte: Dados simulados para fins didáticos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray60")
  )
grafico_completo