# Módulo 3 - Aula 1 - Inferência estatística

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


# 4.1 Atividade sugerida no tópico 2: Calcule intervalos de confiança para 500
# amostras e visualizar quais contêm a média verdadeira. Espera-se em torno
# de 95% dos ICs em 95% capturam o parâmetro.
quantil_normal <- qnorm(0.975)
quantil_normal

# Erro padrão aproveitando a fórmula utilizada na sessão 1.2 do código
erro_padrao <- desvio_populacional / sqrt(n)
erro_padrao

# Fórmula do IC: média ± z * erro_padrão

# Estrutura de dados contendo todas as informações necessárias para 
# visualização do conteúdo. O parâmetro 'contem_media' checa se o intervalo
# de confiança contêm a média populacional
df_ic <- tibble(
  amostra = 1:K,
  media = medias_amostrais,
  limite_inferior = media - quantil_normal * erro_padrao,
  limite_superior = media + quantil_normal * erro_padrao,
  contem_media = limite_inferior < media_populacional 
                 & limite_superior > media_populacional
)

# Contagem de quantos ICs contêm a média (TRUE) e quais não contêm (FALSE)
table(df_ic$contem_media)

# Proporção de ICs que contêm a média. Espera-se em torno dos 95~96:
mean(df_ic$contem_media)

# Visualização dos primeiros 100 intervalos de confiança
# ICs em azul contêm a média, enquanto os em vermelho não
df_ic %>%
  slice(1:100) %>%                                       # Pegar apenas os 100 primeiros
  ggplot(aes(x = amostra, y = media, color = contem_media)) +
  geom_point(size = 2) +                                 # Ponto = média amostral
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior), width = 0.3) + # Barras = IC
  geom_hline(yintercept = media_populacional, color = "black", linewidth = 0.8) +  # Linha = média verdadeira
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) +
  labs(title = "Intervalos de Confiança de 95%",
       subtitle = "Linha preta = média verdadeira (26)",
       x = "Amostra", 
       y = "IMC (kg/m²)") +
  theme_minimal() +
  theme(legend.position = "none")


# 5.1 Atividade 4: Introdução ao teste t
# Dados de pesos (g) de 20 recém-nascidos de uma maternidade:
peso_rn <- c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 
             3031, 2069, 2581, 2841, 3609, 2838, 3541, 2759, 
             3248, 3314, 3101, 2834)
summary(peso_rn)  
sd(peso_rn) 
mean(peso_rn)

# Teste t para verificar se a média foge de x, nesse caso sendo 3200, 
# uma referência nacional para peso de recém-nascidos. Surgem então duas
# hipóteses: Sendo μ a média de peso, ela é igual (H0, TRUE) ou 
# diferente a 3200 (H1, FALSE)?
t.test(peso_rn, mu = 3200)

# No output da função são apresentados:
# t = valor da estatística t, se posto em módulo, quanto maior o valor mais 
# apresenta evidência contra H0;
# df = graus de liberdade (n-1, nesse caso, 19)
# p-value = probabilidade de observar esses dados se H0 fosse verdade

# p-value < 0.05 = H0 rejeitada, a média difere de 3200g
# p-value >= 0.05 = H0 preservada, não há evidência de diferença

# 95 percent confidence interval = intervalo de confiança para a média;
# sample estimates = média amostral.


# 5.2 Teste t em duas amostras
# H0: μ1 = μ2 (as médias são iguais)
# H1: μ1 ≠ μ2 (as médias são diferentes)

# Dados de colesterol medidos por dois métodos:
colesterol <- tibble(
  metodo = rep(c("AutoAnalyzer", "Microenzimatic"), each = 5),
  valor = c(177, 193, 195, 209, 226,    # AutoAnalyzer
            192, 197, 200, 202, 209)    # Microenzimatic
)
print(colesterol) 

# Calcular contagem, média e desvio-padrão de cada método:
colesterol %>%
  group_by(metodo) %>%
  summarise(
    n = n(),
    media = mean(valor),
    dp = sd(valor)
  )

# valor ~ metodo: Comparação dos valores dependentes entre os métodos"
t.test(valor ~ metodo, data = colesterol)

# Como p-value=1, então não há diferença evidente entre métodos (p>=0.05). Se
# p-value fosse menor que 0.05, essa diferença seria apresentada.


# 6.1 Método ANOVA (ANalysis Of VAriance)
# Útil para quando é necessário comparar as médias de TRÊS ou mais grupos.
# Evita-se utilizar vários teste t pois isso aumenta a chance de erro. ANOVA 
# faz diretamente todos os testes.

# As hipóteses dessa vez tomam a seguinte forma:
# H0: μ1 = μ2 = μ3 = ... = μN (todas as médias são iguais)
# H1: pelo menos uma média é diferente

# Lembrete: Se o resultado da ANOVA for significativa, não é possível 
# identificar qual(is) grupo(s) diferem. Para isso, utiliza-se o 
# teste de Tukey (comparações múltiplas).

# Fixa semente para reprodutibilidade
set.seed(123)

# Criar dados simulados de pressão arterial para 3 faixas etárias, 
# com 20 pessoas cada
dados_pa <- tibble(
  faixa_etaria = factor(
    rep(c("Jovem", "Adulto", "Idoso"), each = 20),
    levels = c("Jovem", "Adulto", "Idoso")      # Define ordem das categorias
  ),
  pressao = c(
    rnorm(20, mean = 115, sd = 10),   # Jovens: média 115, DP 10
    rnorm(20, mean = 125, sd = 12),   # Adultos: média 125, DP 12
    rnorm(20, mean = 135, sd = 15)    # Idosos: média 135, DP 15
  )
)
head(dados_pa)

# Calcular contagem, média e desvio-padrão dos dados, agrupados por faixa
dados_pa %>%
  group_by(faixa_etaria) %>%
  summarise(
    n = n(),
    media = mean(pressao),
    dp = sd(pressao)
  )

# Visualização dos dados em boxplot
dados_pa %>%
  ggplot(aes(x = faixa_etaria, y = pressao, fill = faixa_etaria)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +           # Boxplot
  geom_jitter(width = 0.2, alpha = 0.4, show.legend = FALSE) + # Pontos individuais
  labs(title = "Pressão Arterial por Faixa Etária", 
       x = "Faixa etária", 
       y = "Pressão arterial sistólica (mmHg)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")                        # Paleta de cores

# 6.2 Utilização da fução aov() para aplicar do método ANOVA
# Estrutura: variável_resposta ~ variável_grupo
modelo_anova <- aov(pressao ~ faixa_etaria, data = dados_pa)

# Resultado:
summary(modelo_anova)

# Legenda:
# Df = graus de liberdade
# Sum Sq = soma dos quadrados
# Mean Sq = média dos quadrados
# F value = estatística F (quanto maior, mais evidência de diferença)
# Pr(>F) = p-valor

# Se Pr(>F) < 0.05 → Há diferença significativa entre PELO MENOS dois grupos.
# Para identificá-los, utiliza-se o teste de Tukey:
TukeyHSD(modelo_anova)

# Legenda:
# diff = diferença entre as médias dos grupos
# lwr = limite inferior do IC 95% para a diferença
# upr = limite superior do IC 95% para a diferença
# p adj = p-valor ajustado
#
# p adj < 0.05 = Os dois grupos comparados diferem significativamente, no
# caso dos dados de pressão arterial, são os grupos Idoso x Adulto e,
# principalmente Idoso x Jovem.


# 7.1 Teste de Proporção
# Emprega-se quando é necessário testar se uma PROPORÇÃO difere
# de um valor de referência.
# Por exmeplo, em uma amostra de 200 adultos, 60 são hipertensos.
# Para testar se essa proporção difere de 25%, basta dividir a amostra
# pelo tamanho da amostra.
# Proporção amostral:
60 / 200   # = 0.30 = 30%

# 7.2 Utilizando prop.test() para realizar o teste de proporção
# prop.test(x, n, p), onde:
#   x = número de ocorrências (hipertensos)
#   n = tamanho da amostra
#   p = proporção esperada (referência)
prop.test(x = 60, n = 200, p = 0.25)

# Se p-value < 0.05: a proporção difere significativamente de 25%
# Se p-value >= 0.05: não há evidência de que difira de 25%


# 8. Conclusão: Qual teste usar em qual ocasião?
 
# Variável numérica (comparação entre médias e outras estatísticas)
# # 1 grupo vs referência = t.test(x,mu=valor);
# # 2 grupos independentes = t.test(y~grupo);
# # 3 ou mais grupos = aov() + TukeyHSD()

# Variável categórica (comparação entre proporções)
# # 1 proporção vs referência = prop.test()
# # 2 ou mais grupos = chisq.test()