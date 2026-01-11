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
# Modela a relação de múltiplas variáveis independentes para explicar Y. 
# Permite ajustar o efeito de uma variável pelas outras.
#
# Exemplo: Para saber o efeito da idade na pressão arterial, utiliza-se o sexo
# pois é uma variável que também afeta a PA. 
# A regressão múltipla dá o efeito da idade "ajustado" pelo sexo.
#
# Fórmula: Y = β0 + β1*X1 + β2*X2 + ... + ε

# Modelo múltiplo (idade + sexo). O sinal + adiciona outra variável ao modelo.
modelo_multiplo <- lm(pa ~ idade + sexo, data = pasis)

# Resumo do modelo
summary(modelo_multiplo)

# Output:
# (Intercept) = Pressão arterial estimada para idade=0 e 
# sexo=Feminino (categoria de referência);
# idade = efeito da idade AJUSTADO por sexo. Basicamente: "Para cada ano a 
# mais, PA aumenta β1 mmHg, mantendo sexo constante";
# sexoMasculino = diferença entre homens e mulheres AJUSTADA por idade. "Homens
# têm PA β2 mmHg maior/menor que mulheres, na mesma idade".
#
# Nota: No summary(), é mostrado "sexoMasculino" porque "Feminino" é a 
# categoria de referência.

# Plot de gráfico de retas por sexo
ggplot(pasis, aes(x = idade, y = pa, color = sexo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title = "Regressão Múltipla: PA ~ Idade + Sexo",
       x = "Idade (anos)",
       y = "PA Sistólica (mmHg)") +
  theme_bw() +
  scale_color_manual(values = c("Feminino" = "deeppink", "Masculino" = "deepskyblue"))

# Intepretação do gráfico:
# a) Há duas retas paralelas, uma para cada sexo;
# b) A distância vertical entre elas é o coeficiente do sexo;
# c) As retas são PARALELAS porque assume-se que o efeito da idade é 
# o mesmo para homens e mulheres.

# 2.2.1 Modelos com interação
# Caso o efeito da idade for diferente para homens e mulheres, utiliza-se o
# operador * para incluir a interação
modelo_interacao <- lm(pa ~ idade * sexo, data = pasis)
summary(modelo_interacao)


# 2.3 Modelos de regressão logística
# São utilizados quando deseja-se modelar a relação quando Y é binário.
# A regressão linear não é útil pois pode trazer valores contínuos fora do 
# intervalo de 0 e 1.
# Para observar a relação, é utilizada a medida Odds Ratio (OR, ou Razão de 
# Chances). Onde:
# OR = 1, sem associação;
# OR > 1, fator de risco (aumenta chance do evento);
# OR < 1, fator de proteção (diminui a chance do evento)
# Exemplo: OR = 2.5 para tabagismo. Portanto, fumantes tem "2.5 vezes mais" de
# apresentar a doença.

#Dados mockados de hipertensão
set.seed(123)
n_pac <- 100
dados_hiper <- tibble(
  idade = round(runif(n_pac, 30, 70)),    # Idade: 30-70 anos
  imc = round(rnorm(n_pac, 26, 4), 1),    # IMC: média 26, DP 4
  sexo = sample(c("Feminino", "Masculino"), n_pac, replace = TRUE)  # Sexo aleatório
) %>%
  mutate(
    # Calcular probabilidade de hipertensão baseada nas variáveis:
    prob_hiper = plogis(-8 + 0.05 * idade + 0.15 * imc + 
                          0.3 * (sexo == "Masculino")),
    # Gerar o desfecho (0 ou 1) com essa probabilidade:
    hipertensao = rbinom(n_pac, 1, prob_hiper)
  )
head(dados_hiper)

# plogis() converte valores em probabilidades (de 0 a 1);
# rbinom() gera flag de 0 ou 1 com base na probabilidade;
# Dados criados onde idade, IMC e sexo afetam a chance de hipertensão.

# Valida quantos elementos apresentaram hipertensão
# 0 = não hipertenso, 1 = apresenta hipertensão
table(dados_hiper$hipertensao)


# 2.3.1 Ajuste do modelo logístico

# glm() = generalized linear model (modelo linear generalizado);
# family = binomial indica que Y é binária;

modelo_logistico <- glm(hipertensao ~ idade + imc + sexo, 
                        data = dados_hiper,
                        family = binomial(link = "logit"))
summary(modelo_logistico)

# Observação: Os coeficientes estão na escala de LOG-ODDS. Para interpretar,
# é necessário calcular o OR ao exponenciar os coeficientes.

# A função tidy() do pacote broom facilita a extração dos resultados
# exponentiate = TRUE transforma os coeficientes em OR

tidy(modelo_logistico, conf.int = TRUE, exponentiate = TRUE)

# No output:
# term = Variável;
# estimate = OR (Odds Ratio);
# conf.low e conf.high = IC 95% para o OR;
# p.value = p-valor;

# Interpretação resumida dos ORs 
# idade: OR ≈ 1.05
# a) Para cada ano a mais de idade, a chance de hipertensão aumenta ~5%;
# b) (OR - 1) * 100 = porcentagem de aumento

# imc: OR ≈ 1.15
# a) Para cada unidade a mais de IMC, a chance aumenta ~15%

# sexoMasculino: OR ≈ 1.35
# a) Homens têm ~35% mais chance de hipertensão que mulheres

# Se OR < 1: Torna-se fator de proteção. 
# Exemplo: OR = 0.5 significa 50% MENOS chance.


# 2.3.2 Inferência em um novo paciente
# Inferindo a probabilidade de hipertensão para um homem de 60 anos com IMC 28?
novo_paciente <- tibble(idade = 60, imc = 28, sexo = "Masculino")

# predict() com type = "response" retorna a PROBABILIDADE
predict(modelo_logistico, newdata = novo_paciente, type = "response")

# Resultado: Probabilidade entre 0 e 1. 0.4055... = 40% = 40% de chance 
# de ser hipertenso
# Exemplo: 0.45 significa 45% de chance de ser hipertenso


# 3.1 Diagnóstico dos modelos
# Dividir a tela em 4 partes (2x2)
par(mfrow = c(2, 2))

# Gerar os 4 gráficos de diagnóstico
plot(modelo_simples)

# Restaura tela para estado original
par(mfrow = c(1, 1))


# 3.1.1 Interpretação dos gráficos

# a) Residuals vs Fitted (Resíduos vs Valores Ajustados):
# Verifica LINEARIDADE e HOMOCEDASTICIDADE (igualdade da variância
# dos ruídos). Pontos devem estar dispersos aleatoriamente, sem padrão.
# Caso formem curva ou funil, significa que foram dados mal ajustados;

# b) Normal Q-Q (Quantil-Quantil):
# Verifica NORMALIDADE dos resíduos. Pontos seguindo a linha diagonal são um
# bom sinal, enquanto os que se afastam estão não-normalizados;

# c) Scale-Location:
# Verifica HOMOCEDASTICIDADE. Linha aproximadamente horizontal e pontos 
# dispersos são bons indicativos, enquanto linha inclinada ou pontos 
# em forma de funil não;

# d) Residuals vs Leverage
# Identifica pontos INFLUENTES (que afetam muito o modelo). Pontos fora das 
# linhas pontilhadas (distância de Cook) são preocupantes


# RESUMO: Situações para uso de cada modelo

# Y CONTÍNUA (peso, PA, glicemia, altura...): 
# lm() - Regressão Linear; 
# Medida: Coeficiente β.

# Y BINÁRIA (sim/não, 0/1, doente/saudável): 
# glm(family = binomial) - Regressão Logística;
# Medida: OR (Odds Ratio)  

# Y TEMPO ATÉ EVENTO (tempo até óbito, etc.): 
# coxph() - Modelo de Cox  
# Medida: HR (Hazard Ratio)