# Regress-o_Receita_Broadway
Análise de Regressão Linear aplicada a Receita de Shows da Broadway em Nova York com a Utilização da Linguagem "R"

# Variáveis
# Receita (milhões de $)
# Expectadores pagantes
# Quantidade de shows
# Preço médio dos ingressos
# Semana (meio de semana ou final de semana)

# Pacotes utilizados:
# readxl
# stats
# corrplot
# graphics
# car
# lmtest
# psych
# MASS
# tseries

library(readxl)
Base1 <- read_excel("Base1.xls", sheet = "BD", na = c("*"))


# Removendo a coluna de datas
Base <- Base1[,2:6]
View(Base)

# Elaborando a Matriz de Correlação
library(stats)
corBase <- cor(Base)
View(corBase)
summary(Base)

# Removendo os valores faltantes
Base <- na.omit(Base)
View(Base)
summary(Base)

# Analisando os dados
boxplot(Base)
par(mfrow=c(1,4))
boxplot(Base$Receita)
boxplot(Base$`Publico Pagante`)
boxplot(Base$`Qtd Shows`)
boxplot(Base$`Preço Médio do Ingresso`)
par(mfrow=c(1,1))

# Elaborando a Matriz de Correlação
library(stats)
corBase <- cor(Base)
View(corBase)

# Para melhorar a matriz de correlação, podemos usar o pacote "corplot"
# Podemos tentar outras formas (circle, pie, color)
library(corrplot)
corrplot(corBase, method = "number")
corrplot(corBase, method = "color", type = "lower", addCoef.col = "white")

# Elaborando os gráficos de correlação:
library(graphics)
par(mfrow=c(1,3))
plot(Base$`Publico Pagante`, Base$Receita, xlab = "Público Pagante", ylab = "Receita")
plot(Base$`Qtd Shows`, Base$Receita, xlab = "Shows", ylab = "Receita")
plot(Base$`Preço Médio do Ingresso`,Base$Receita, xlab = "Preço Médio do Ingresso", ylab = "Receita")

par(mfrow=c(1,1))

# Outra opação é utilizar a função "pairs" do pacote "graphics":
library(graphics)
pairs(Base)

# Realizando a Regressão Linear:
library(stats)
RLBase <- lm(Base$Receita ~ Base$`Publico Pagante` + Base$`Qtd Shows` + Base$`Preço Médio do Ingresso` + Base$Semana)
summary(RLBase)

# Renomeando colunas
names(Base)[2:4] <- c("Pub_Pag", "Shows", "Preco_Ing")
View(Base)

RLBase <- lm(Receita ~ ., data = Base)
summary(RLBase)

library(QuantPsyc)
lm.beta(RLBase)

# Observando os resultados
plot(RLBase)

# Teste de normalidade dos resíduos: 
library(graphics)
hist(RLBase$residuals)

# H0: Distribuição normal
library(stats)
shapiro.test(RLBase$residuals)

# H0: Distribuição normal (Curtose = 3 e Assimetria = 0)
library(tseries)
jarque.bera.test(RLBase$residuals)

# Outliers nos resíduos (padromizando resíduos)
summary(rstandard(RLBase))

# Independência dos Resíduos (Valor da estatística entre 1,5 a 2,5 não há 
# correlação entre os resíduos. H0: Não há autocorrelação)
library(car)
durbinWatsonTest(RLBase)

# Teste de Homocedasticidade (hipótese nula que há  homocedasticidade)
library(lmtest)
bptest(RLBase)

# Ausência de multicolineraridade (r>0,9 ou VIF>10)
library(psych)
pairs.panels(Base)
vif(RLBase)

# Obtenção do IC para os coeficientes
library(stats)
confint(RLBase)

# Comparação entre modelos
library(MASS)
stepAIC(RLBase, direction = "backward")

# Glossário:
# Item 6 é uma varável dummy. Variáveis dummy são variáveis binárias (0 ou 1) criadas para representar uma variável com duas ou mais categorias.
