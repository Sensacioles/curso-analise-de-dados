# Declara o pacote tidyverse para auxiliar a manipulação do dataset
#install.packages("tidyverse")
library(tidyverse)

# Cria o dataset
recem_nascidos <- tibble(
  individuo = 1:20,
  peso_g = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
             2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834),
  w = c(22,40,33,22,5,31,24,35,48,58,61,20,45,22,41,35,36,11,10,25)
)

# Visualiza um resumo do dataset:
glimpse(recem_nascidos) # via console
view(recem_nascidos) # via tabela

# ATIVIDADE 1: Trabalhando com Variáveis Aleatórias
# Cria um tibble manual com dados, visualiza a estrutura (ambos acima),
# categoriza faixas de peso, cria fatores ordenados e conta frequências. 
# Cria categoria para faixa de peso
recem_nascidos <- recem_nascidos %>% 
  mutate(faixa_peso = case_when(
    peso_g < 2500 ~ "Baixo peso",
    peso_g >= 2500 & peso_g < 3500 ~ "Peso adequado",
    peso_g >= 3500 ~ "Acima do esperado"
    ),
    faixa_peso = factor(faixa_peso,
      levels = c("Baixo peso", "Peso adequado", "Acima do esperado"),
      ordered = TRUE)
  )

# Conta a frequência de cada faixa
faixa_freq <- recem_nascidos %>% count(faixa_peso)

# ATIVIDADE 2: Medidas de Locação (Tendência Central)
# Calcular média aritmética, mediana, análise de assimetria da distribuição,
# percentis 10 e 90, quartis q1, q2, q3 e média ponderada.
faixa_stats <- recem_nascidos %>%
  summarize(media_aritmetica=mean(peso_g),
            mediana=median(peso_g),
            p_10 = quantile(peso_g, 0.10),
            q_1 = quantile(peso_g, 0.25),
            q_2 = quantile(peso_g, 0.50),
            q_3 = quantile(peso_g, 0.75),
            p_90 = quantile(peso_g, 0.90),
            media_ponderada=sum(peso_g*w)/sum(w)) %>% 
    mutate(diferenca=media_aritmetica-mediana)

# Exibe resultado
view(faixa_stats)

# ATIVIDADE 3: Medidas de Dispersão
# Calcular amplitude, variância, desvio-padrão, coeficiente de variação, 
# intervalo interquartil e estatísticas agrupadas.

# Cria novo tibble com dados de colesterol
# A função rep() repete um dado valor para cada linha. Utilizando o parâmetro
# "each", é definido o número total de repetições sequenciais.
# Se fosse utilizado o parâmetro "times", os valores do vetor seriam repetidos 
# x vezes alternadamente.
colesterol <- tibble(
  metodo=rep(c("Autoanalyzer (mg/dL)","Microenzymatic (mg/dL)"),each=5),
  valor=c(177,193,195,209,226,192,197,200,202,209)
)

# Calcula as medidas de dispersão, agrupando por método.
# Obs.: Coeficiente de variação = Desvio padrão / média;
#       Intervalo interquatil = Q3 (75%) - Q1 (25%). 
colesterol_disp <- colesterol %>% 
  group_by(metodo) %>%
  summarize(amplitude=max(valor)-min(valor),
            variancia=var(valor),
            desvio_padrao=sd(valor),
            coeficiente_var=round((desvio_padrao/(mean(valor))*100),2),
            intervalo_interq=quantile(valor,0.75)-quantile(valor,0.25))

# Exibe resultado
view(colesterol_disp)

# ATIVIDADE 4: Função para Resumo Estatístico Completo
# Criar função que aproveita parâmetro {{variavel}} e combina estatísticas 
# em um único resumo.
resumo <- function(dataset,variavel) {
  dataset %>% 
    summarise(
      n = n(),
      media = mean({{variavel}}),
      mediana = median({{variavel}}),
      desvio_padrao = sd({{variavel}}),
      cv_percent = (sd({{variavel}})/mean({{variavel}})) * 100,
      minimo = min({{variavel}}),
      q_1 = quantile({{variavel}},0.25),
      q_3 = quantile({{variavel}},0.75),
      maximo = max({{variavel}}),
      intervalo_interq = q_3 - q_1
    )
}

# Exibe resultado via console
resumo(recem_nascidos,peso_g)

# ATIVIDADE 5: Métodos Gráficos
# Criar gráficos de barra, boxplot, histograma, adicionar linhas de referência
# e alterar títulos e rótulos

# Gráfico de Barras
grafico_barras <- recem_nascidos %>%
  count(faixa_peso) %>% 
  mutate(percentual = n / sum(n) * 100) %>% 
  # Configuração do plot
  ggplot(aes(x = faixa_peso, y = percentual)) +
  # Utilização do gráfico de barras
  geom_col(fill = "darkblue") +
  # Labels (rótulos) dos dados
  geom_text(aes(label = paste0(round(percentual, 1), "%")), 
            vjust = -0.5, size = 4) +
  # Título do gráfico, labels (rótulos) dos eixos
  labs(
    title = "Distribuição de Recém-nascidos por Faixa de Peso",
    x = "Faixa de Peso",
    y = "Percentual (%)"
  ) +
  # Tema do gráfico
  theme_classic() +
  # Limite do eixo Y
  ylim(0, 100) 

# Plota o gráfico
grafico_barras

# Boxplot
grafico_boxplot <- colesterol %>% 
  # Configuração do plot
  ggplot(aes(x = metodo, y = valor, fill = metodo)) +
  # Utilização do boxplot, remove a legenda para não repetir a informação
  # trazida no eixo X
  geom_boxplot(show.legend = FALSE) +
  # Título do gráfico, labels (rótulos) dos eixos
  labs(
    title = "Comparação dos Métodos de Medição de Colesterol",
    x = "Método",
    y = "Colesterol (mg/dL)"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("steelblue", "coral"))

# Plota o gráfico
grafico_boxplot

# Histograma
grafico_histograma <- recem_nascidos %>%
  # Configuração do plot
  ggplot(aes(x = peso_g)) +
  # Utilização do histograma
  geom_histogram(bins = 5, fill = "steelblue", color = "white") +
  # Linha vermelha vertical para a média
  geom_vline(aes(xintercept = mean(peso_g), color = "Média"), 
             linetype = "dotted", linewidth = 1) +
  # Linha verde vertical para a mediana
  geom_vline(aes(xintercept = median(peso_g),color = "Mediana"), 
             linetype = "dotted", linewidth = 1) +
  # Edição manual da legenda para referenciar os xintercepts
  scale_colour_manual(
    name="Estatísticas",
    values=c("red","darkgreen")
  ) +
  # Título do gráfico, labels (rótulos) dos eixos
  labs(
    title = "Distribuição dos Pesos de Recém-nascidos",
    x = "Peso (g)",
    y = "Frequência"
  ) +
  theme_classic()

# Plota o gráfico
grafico_histograma