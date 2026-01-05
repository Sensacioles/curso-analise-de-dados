# Declara pacote tidyverse e define semente para replicar testes do material.
library(tidyverse)
set.seed(123456)

# 1.1 Simulação inicial
# Parâmetros 
media_populacional <- 26    # Média do IMC na população
desvio_populacional <- 2    # Desvio-padrão do IMC

# Inferência 
K <- 500   # Número de amostras
n <- 100   # Quantidade de pessoas por amostra

# Gera 500 amostras e calcula a média de cada uma
# rnorm() = Gera valores utilizando os parâmetros para simular o IMC 
# mean() = Calcula a média dos valores gerados
# replicate() = Replica o processo K vezes, retorna um vetor de K elementos
medias_amostrais <- replicate(K, mean(rnorm(n, media_populacional, desvio_populacional)))

# Histograma dos resultados. Apresenta distribuição normal, com acentuamento
# no centro próximo à média. É uma demonstração do Teorema Central do Limite.
hist(medias_amostrais,
     col = "lightblue",
     main = "Distribuição das Médias Amostrais (n=100)",
     xlab = "Média do IMC (kg/m²)",
     ylab = "Frequência",
     breaks = 30) 

# Linha abscissa na média verdadeira (26)
abline(v = media_populacional, col = "red", lwd = 2)

# 1.2 Validação estatística
# Média das K amostras
mean(medias_amostrais)

# Desvio-padrão das médias amostrais (ERRO PADRÃO)
sd(medias_amostrais)

# Erro Padrão a partir da fórmula matemática: ]
# SE = SD / n**(1/2) == desvio_populacional / raiz quadrada de n
desvio_populacional / sqrt(n)

# 2.1 Amostras pequenas
# Semente para gerar valores de uma população não-normal (assimétrica)
set.seed(42)

# Criar uma população com distribuição exponencial (assimétrica)
# rexp() gera números dessa forma
populacao <- rexp(10000, rate = 1)

# Gráfico mostrando a assimetria gerada, isso é representado pela cauda 
# longa em direção a direita
hist(populacao, breaks = 30, main = "População Original (Assimétrica)", 
     col = "lightblue", border = "white")

# Função criada para calcular médias amostrais. Ao receber uma amostra de 
# tamanho 'tam_amostra', a função calcula 'n_amostras' 
# vezes a média dessa amostra 
calcular_medias <- function(tam_amostra, n_amostras = 1000) {
  replicate(n_amostras, mean(sample(populacao, tam_amostra)))
}

# 3.1 Comparação entre diferentes tipos e tamanhos de amostras:

# Função par() divide a tela dos plots de acordo com o vetor passado como
# parâmetro. Nesse caso, foi dividida em 4 partes (2 linhas x 2 colunas)
par(mfrow = c(2, 2))

# Gráfico 1: A população original (assimétrica)
# Gráfico 2: Médias de amostras com n=5 (muito pequeno), continua assimétrico
# Gráfico 3: Médias de amostras com n=30, normaliza a distribuição
# Gráfico 4: Médias de amostras com n=100, distribuição praticamente normal
hist(populacao, breaks = 30, main = "População (Assimétrica)", 
     col = "lightblue", border = "white")
hist(calcular_medias(5), breaks = 30, main = "Médias (n=5)", 
     col = "lightgreen", border = "white")
hist(calcular_medias(30), breaks = 30, main = "Médias (n=30)", 
     col = "lightgreen", border = "white")
hist(calcular_medias(100), breaks = 30, main = "Médias (n=100)", 
     col = "lightgreen", border = "white")

# Restaura o comportamento original da tela dos plots
par(mfrow = c(1, 1))