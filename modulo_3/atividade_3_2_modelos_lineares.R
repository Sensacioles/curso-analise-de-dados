# Módulo 3 - Aula 2 - Modelos lineares e não lineares

# Declaração dos pacotes necessários para realizar as atividades
# install.packages("tidyverse")
# install.packages("broom")
library(tidyverse)
library(broom)


# 1.1. Correlação, modelos lineares e não-lineares
# Uma das formas de se medir uma relação entre duas variáveis numéricas é 
# calculando a correlação entre as duas, podendo ser medida através do 
# coeficiente de correlação linear.

# Observação: Correlação não denota causalidade. Não é porque uma variável
# está correlata a outra, não quer dizer que se influenciem diretamente.
# Exemplo: O aumento da temperatura global está correlacionado com a diminuição
# na população de piratas. 

# Plotando um gráfico de dispersão das variáveis, é possível observar pelo 
# menos 5 tipos de comportamento no gráfico:
# # a) Correlação linear negativa, quando são inversamente proporcionais;
# # b) Correlação linear positiva, quando são diretamente proporcionais;
# # c) Sem correlação, quando não se afetam diretamente;
# # d) Padrão não linear, quando não necessariamente se afetam em 
# # todos os casos. O comportamento no gráfico deixa de ser em formato de 
# # linha e passa a tomar formato de curva.

# A força entre essas variáveis pode ser medida através do coeficiente de
# correlação de Pearson, onde:
# # a) Correlação linear perfeita positiva: r = +1
# # b) Correlação linear perfeita negativa: r = -1
# # c) Sem correlação linear: r = 0
# # d) Correlação linear positiva: 0 < r < 1
# # e) Correlação linear negativa: -1 < r < 0

# Caso a suposta correlação não for válida através do método de Pearson
# utiliza-se a correlação de Spearman, que não assume distribuição normal para
# as duas variáveis e sim através de postos ordenados (ranqueamento).

# 1.2. Atividade: Cálculo da correlação entre pressão arterial e idade, 
# plotando num gráfico para visualização dos resultados.

# Definir semente para reprodutibilidade
set.seed(42)

# Número amostral
n <- 30

# Simula dados de pressão arterial e idade
pasis <- tibble(
  idade = round(runif(n, min = 25, max = 75)),
  pa = round(100 + 0.8 * idade + rnorm(n, 0, 10)),
  sexo = if_else(runif(n) >= 0.5, "Feminino", "Masculino")
)
print(pasis)

# Funções utilizadas:
# # runif(n, min, max): Gera n números aleatórios entre limites min e max;
# # round(): Arredonda para número inteiro;
# # rnorm(n, avg, sd): Adiciona "ruído" com média 0 e DP 10;
# Fórmula para criação de pressão arterial: 100 + 0.8*idade + ruído 

# Cálculo da correlação utilizando cor.test() e teste se ela é significativa
cor.test(pasis$pa, pasis$idade)

# Output:
# t = Estatística t do teste;
# df = Graus de liberdade;
# p-value = probabilidade de observar essa correlação se não houvesse relação.
# Se p < 0.05, então há correlação significativa;

# cor = Coeficiente de correlação (r). Parâmetro mais importante nessa análise.
# Permite verificar se é positivo/negativo e quão forte é:
# # |r| < 0.3 = Correlação FRACA
# # 0.3 ≤ |r| < 0.7 = Correlação MODERADA
# # |r| ≥ 0.7 = Correlação FORTE

# No caso dos dados mockados (r=0.659), a correlação é MODERADA.

# 95 percent confidence interval = Intervalo de confiança para o coeficiente
# de correlação.

# Também é possível calcular o coeficiente sem os testes:
cor(pasis$pa, pasis$idade)

# Visualização a partir de gráfico de dispersão:
ggplot(pasis, aes(x = idade, y = pa)) +
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  labs(title = "Relação entre Idade e Pressão Arterial",
       x = "Idade (anos)",
       y = "PA Sistólica (mmHg)") +
  theme_bw()

# Interpretação do resultado:
# a) Os pontos tendem a subir proporcionalmente em direção ao canto superior
# direito;
# b) Indicativo de correlação POSITIVA (maior idade = maior pressão arterial);
# c) Os pontos ainda estão dispersos e r=0.659, indicando uma correlação
# positiva NÃO PERFEITA, já que 0 < r < 1.


# 2.1 Modelos de regressão linear simples
# Modela a relação entre variáveis a partir de uma reta

# Fórmula: Y = β0 + β1*X + ε onde:

# # Y = Variável DEPENDENTE (objetivo da previsão), exemplo: pressão arterial;
# # X = Variável INDEPENDENTE (parâmetro), exemplo: idade;
# # β0 = Intercepto, valor de Y quando X = 0;
# # β1 = Inclinação, quanto Y muda para cada unidade de X;
# # ε = Erro, variação não explicada pelo modelo

# Correlação vs Regressão
# Correlação mede a força de uma relação enquanto regressão quantifica a 
# relação (variação de Y em função de X).


# 2.1.1 Ajuste do modelo para regressão linear simples

# lm() = linear model (modelo linear);
# Fórmula: variável dependente ~ variável independente
# Ou seja: "pa EM FUNÇÃO DE idade" ou "pa EXPLICADA POR idade"
modelo_simples <- lm(pa ~ idade, data = pasis)

# Output dos coeficientes β0 (intercepto) e β1 (inclinação)
modelo_simples

# (Intercept) = β0 = Valor de pa quando idade=0. Nesse caso não tem sentido
# prático pois não há elementos com "0 anos" nos dados mockados;
# idade = β1 = Variação de pa para cada idade+=1. Ou seja, para cada ano que
# e passa, pa cresce aumenta em β1 mmHg

# Resumo do modelo
summary(modelo_simples)

# Coefficients:
# # Estimate = Valor estimado do coeficiente;
# # Std. Error = Erro padrão (incerteza do coeficiente);
# # t value = estatística t;
# # Pr(>|t|) = p-valor. Se p < 0.05, então coeficiente significativo;

# Signif. codes = Legenda a partir de asteriscos para representar a 
# significância dos coeficientes β0 e β1 ('***' muito significativo, 
# '*' pouco significativo, '' nenhuma significância);

# Residual standard error: Ou RSE, medida da diferença entre as variáveis 
# observadas e as projetadas, em n-2 (n sendo a quantidade de elementos
# observados) graus de liberdade;

# Multiple R-squared: Proporção da variação de Y explicada por X, pode ser lida
# em porcentagem. Exemplo: X explica 43,5% da variação de Y 
# nos dados projetados;
# # OBSERVAÇÃO: Quanto mais se aproxima de 1, melhor o modelo. Porém R²=1
# # pode significar um modelo em overfit.

# F-statistic e p-value: Significância do modelo como um todo.

# Traz somente o R² do modelo
summary(modelo_simples)$r.squared


# 2.1.2 Interpretação simplificada dos resultados

# Executando as linhas abaixo, observa-se que R² = 0.435 e β1 ≃ 0.7
summary(modelo_simples)$r.squared; coef(modelo_simples)["idade"]

# Pode-se dizer que:
# # a) idade explica 43,5% da variação na pressão arterial;
# # b) Para cada ano em idade, a pressão arterial aumenta em média 
# # aproximadamente 0.7 mmHg.


# 2.1.3 Visualização gráfica
ggplot(pasis, aes(x = idade, y = pa)) + 
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Regressão Linear: PA ~ Idade",
       x = "Idade (anos)", 
       y = "PA Sistólica (mmHg)") +
  theme_bw()

# Onde:
# geom_smooth(method = "lm"): Adiciona a reta de regressão;
# se = TRUE: Mostra a faixa de incerteza (intervalo de confiança);
# A reta de regressão basicamente é: PA = β0 + β1*idade

# Observações:
# # a) A reta corta a dispersão dos pontos praticamente ao meio;
# # b) A faixa cinza mostra a incerteza da estimativa.


# 2.2 Modelos de regressão linear múltipla
#


# 2.3 Modelos de regressão logística
#


# 2.4 Modelos de Poisson
#


# 3.1 Diagnóstico dos modelos
#