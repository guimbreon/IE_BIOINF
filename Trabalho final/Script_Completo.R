#Leitura de dados
# load package "readxl"


#Fim da leitura dos dados

#LIMPEZA DE DADOS
#Horas Estudo
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasEstudo[i])) & Grupo10$HorasEstudo[i] < 168 ){
    Grupo10$HorasEstudo[i] = Grupo10$HorasEstudo[i]
  }else {
    Grupo10$HorasEstudo[i] = NA
  }


#Horas Redes
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasRedes[i])) & Grupo10$HorasRedes[i] < 24 ){
    Grupo10$HorasRedes[i] = Grupo10$HorasRedes[i]
  }else {
    Grupo10$HorasRedes[i] = NA
  }

#Horas TV
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasTV[i])) & Grupo10$HorasTV[i] < 24 ){
    Grupo10$HorasTV[i] = Grupo10$HorasTV[i]
  }else {
    Grupo10$HorasTV[i] = NA
  }

#Horas Sono
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasSono[i])) & Grupo10$HorasSono[i] < 24 ){
    Grupo10$HorasSono[i] = Grupo10$HorasSono[i]
  }else {
    Grupo10$HorasSono[i] = NA
  }
#FIM LIMPEZA DE DADOS

#Organizar a Base de Dados
Grupo10$Sexo[Grupo10$Sexo == 1] = "Feminino"
Grupo10$Sexo[Grupo10$Sexo == 2] = "Masculino"


Grupo10$opcao1[Grupo10$opcao1 == 1] = "Sim"
Grupo10$opcao1[Grupo10$opcao1 == 2] = "Nao"


Grupo10$Escolhi[Grupo10$Escolhi == 1] = "Sim"
Grupo10$Escolhi[Grupo10$Escolhi == 2] = "Nao"


Grupo10$PMentoria[Grupo10$PMentoria == 1] = "Sim"
Grupo10$PMentoria[Grupo10$PMentoria == 2] = "Nao"

#FIM de organizar a base de dados

#ANALISE DESCRITIVA UNIVARIADA
if(!require("pander")) install.packages('pander')
library(pander)

#Idade
# Calcular Frequencia Absoluta

Objeto <- cut(Grupo10$Idade, breaks = c(17, 22, 27, 52), labels = c("17-22", "23-27", "28-52"))
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
tabela
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'Idade'")
var(Grupo10$Idade,
    na.rm = TRUE)
# desvio padrao
sd(Grupo10$Idade,
   na.rm = TRUE)


nomes = sort(unique(Objeto))
# plot the table as a bar chart
barplot(perc, names.arg = nomes,
        ylim = c(0,100),
        xlab = "Valores da Variavel",
        ylab = "Percentagem",
        main = "Grafico de barras para a variavel 'Idade'")

text(x = barplot(perc, names.arg = nomes,
                 ylim = c(0,100),
                 xlab = "Valores da Variavel",
                 ylab = "Percentagem",
                 main = "Grafico de barras para a variavel 'Idade'"),
     y = perc,
     label = perc,
     pos = 3)



#Sexo
# Calcular Frequencia Absoluta
Objeto = Grupo10$Sexo
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
plot()
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'Sexo'")

nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de Sexo")

#Curso que frequenta
# Calcular Frequencia Absoluta

Objeto = Grupo10$Curso
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'Curso'")


nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de Curso")



#Ano curricular que frequenta
# Calcular Frequencia Absoluta

Objeto = Grupo10$AnoCurricular
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)

## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'AnoCurricular'")


nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de AnoCurricular")



var(na.omit(Objeto))

#Este curso foi a minha 1º opcao?
# Calcular Frequencia Absoluta

Objeto = Grupo10$opcao1
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'opcao1'")


nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de opcao1")




#Fui eu que escohi este curso
# Calcular Frequencia Absoluta

Objeto = Grupo10$Escolhi
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'Escolhi'")



nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de Escolhi")


# Tempo de deslocacao
# Calcular Frequencia Absoluta
library(pander)
Objeto <- Grupo10$TempoDesloca
absol <- table(Objeto)

# Calcular Frequencia Relativa
relat <- round(prop.table(table(Objeto)), 3)

# Calcular Frequencia Relativa em percentagem
perc <- round(prop.table(table(Objeto)) * 100, 2)

## Tabela com a library pander
tabela <- cbind(absol, relat, perc)
colnames(tabela) <- c("Fr.Absoluta", "Fr.Relativa", "Percentagem")
pandoc.table(tabela, plain.ascii = TRUE, caption = "Tabela de frequencia para variavel 'TempoDesloca'")

nomes <- sort(unique(Objeto))
# plot the table as a bar chart
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'TempoDesloca'")

# Boxplot
boxplot(Objeto, main = "Boxplot para a variavel 'TempoDesloca'")

var(Grupo10$TempoDesloca, na.rm = TRUE)
# desvio padrao
sd(Grupo10$TempoDesloca, na.rm = TRUE)
var(na.omit(Objeto))

# HorasEStudo
# Calcular Frequencia Absoluta
Objeto <- Grupo10$HorasEstudo
absol <- table(Objeto)

# Calcular Frequencia Relativa
relat <- round(prop.table(table(Objeto)), 3)

# Calcular Frequencia Relativa em percentagem
perc <- round(prop.table(table(Objeto)) * 100, 2)

## Tabela com a library pander
tabela <- cbind(absol, relat, perc)
colnames(tabela) <- c("Fr.Absoluta", "Fr.Relativa", "Percentagem")
pandoc.table(tabela, plain.ascii = TRUE, caption = "Tabela de frequencia para variavel 'HorasEstudo'")

nomes <- sort(unique(Objeto))
# plot the table as a bar chart
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'HorasEstudo'")

# Boxplot
boxplot(Objeto, main = "Boxplot para a variavel 'HorasEstudo'")

var(na.omit(Objeto))
var(Grupo10$HorasEstudo, na.rm = TRUE)
# desvio padrao
sd(Grupo10$HorasEstudo, na.rm = TRUE)

# HorasRedes
# Calcular Frequencia Absoluta
Objeto <- Grupo10$HorasRedes
absol <- table(Objeto)

# Calcular Frequencia Relativa
relat <- round(prop.table(table(Objeto)), 3)

# Calcular Frequencia Relativa em percentagem
perc <- round(prop.table(table(Objeto)) * 100, 2)

## Tabela com a library pander
tabela <- cbind(absol, relat, perc)
colnames(tabela) <- c("Fr.Absoluta", "Fr.Relativa", "Percentagem")
pandoc.table(tabela, plain.ascii = TRUE, caption = "Tabela de frequencia para variavel 'HorasRedes'")

nomes <- sort(unique(Objeto))

var(Grupo10$Idade, na.rm = TRUE)
# desvio padrao
sd(Grupo10$Idade, na.rm = TRUE)

# plot the table as a bar chart
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'HorasRedes'")

# Boxplot
boxplot(Objeto, main = "Boxplot para a variavel 'HorasRedes'")

var(na.omit(Objeto))

# HorasTV
# Calcular Frequencia Absoluta
Objeto <- Grupo10$HorasTV
absol <- table(Objeto)

# Calcular Frequencia Relativa
relat <- round(prop.table(table(Objeto)), 3)

# Calcular Frequencia Relativa em percentagem
perc <- round(prop.table(table(Objeto)) * 100, 2)

## Tabela com a library pander
tabela <- cbind(absol, relat, perc)
colnames(tabela) <- c("Fr.Absoluta", "Fr.Relativa", "Percentagem")
pandoc.table(tabela, plain.ascii = TRUE, caption = "Tabela de frequencia para variavel 'HorasTV'")

nomes <- sort(unique(Objeto))
# plot the table as a bar chart
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'HorasTV'")

# Boxplot
boxplot(Objeto, main = "Boxplot para a variavel 'HorasTV'")

var(na.omit(Objeto))

var(Grupo10$HorasTV, na.rm = TRUE)
# desvio padrao
sd(Grupo10$HorasTV, na.rm = TRUE)

# HorasSono
# Calcular Frequencia Absoluta
Objeto <- Grupo10$HorasSono
absol <- table(Objeto)

# Calcular Frequencia Relativa
relat <- round(prop.table(table(Objeto)), 3)

# Calcular Frequencia Relativa em percentagem
perc <- round(prop.table(table(Objeto)) * 100, 2)

## Tabela com a library pander
tabela <- cbind(absol, relat, perc)
colnames(tabela) <- c("Fr.Absoluta", "Fr.Relativa", "Percentagem")
pandoc.table(tabela, plain.ascii = TRUE, caption = "Tabela de frequencia para variavel 'HorasSono'")

nomes <- sort(unique(Objeto))
var(Grupo10$HorasSono, na.rm = TRUE)
# desvio padrao
sd(Grupo10$HorasSono, na.rm = TRUE)
# plot the table as a bar chart
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'HorasSono'")

# Boxplot
boxplot(Objeto, main = "Boxplot para a variavel 'HorasSono'")

var(na.omit(Objeto))


#Se obtivemos conehcimento do programa de mentoria
# Calcular Frequencia Absoluta

Objeto = Grupo10$PMentoria
absol<-table(Objeto)

# Calcular Frequencia Relativa
relat<-round(prop.table(table(Objeto)),3)

# Calcular Frequencia Relativa em percentagem
perc<-round(prop.table(table(Objeto))*100,2)


## Tabela com a library pander

tabela<-cbind(absol,relat,perc)
colnames(tabela)<-c("Fr.Absoluta","Fr.Relativa",
                    "Percentagem")

pandoc.table(tabela, plain.ascii = TRUE,
             caption = "Tabela de frequencia para variavel 'PMentoria'")



nomes <- sort(unique(Objeto))

labels <- paste(nomes, sprintf("(%1.1f%%)", perc), sep=" ")

pie(perc, labels=labels, col=rainbow(length(nomes)), main="Grafico circular de PMentoria")


#ANALISE DESCRITIVA BIVARIAVA
if(!require(dplyr)) install.packege("dplyr")
library(dplyr)
#PARTE 1

# Criar grupos de idade
Grupo10$GrupoIdade <- cut(Grupo10$Idade, breaks = c(17, 22, 27, 52), labels = c("17-22", "23-27", "28-52"))

# Calcular as médias de horas de sono por grupo de idade
my_table <- aggregate(HorasSono ~ GrupoIdade, data = Grupo10, FUN = mean, na.rm = TRUE)

# Visualizar a tabela resultante
print(my_table)

# Boxplot das horas de sono por grupo de idade
boxplot(HorasSono ~ GrupoIdade, data = Grupo10, main = "Boxplot das Horas de Sono por Grupo de Idade", xlab = "Grupo de Idade", ylab = "Horas de Sono")


#PARTE 2
#Comparacao da media de Horas de estudo entre Homem e mulher

library(dplyr)
library(pander)
Female_HorasEstudo = filter(Grupo10, Sexo == "Feminino")

Male_HorasEstudo = filter(Grupo10, Sexo == "Masculino")


Male_HorasEstudo = round(mean(Male_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Female_HorasEstudo = round(mean(Female_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Mean_HorasEstudo = data.frame(Female_HorasEstudo,Male_HorasEstudo,row.names = "Media de Horas a Estudar")

perc_incr_F_M = round(((Mean_HorasEstudo$Female_HorasEstudo - Mean_HorasEstudo$Male_HorasEstudo)/Mean_HorasEstudo$Male_HorasEstudo) * 100, 2)

perc_incr_M_F = round(((Mean_HorasEstudo$Male_HorasEstudo - Mean_HorasEstudo$Female_HorasEstudo)/Mean_HorasEstudo$Female_HorasEstudo) * 100, 2)


pandoc.table(Mean_HorasEstudo,plain.ascii = TRUE,caption = "Média de Horas a Estudar") 




# Boxplot
boxplot(Grupo10$HorasEstudo ~ Grupo10$Sexo, main = "Boxplot das Horas de Estudo por Gênero", xlab = "Gênero", ylab = "Horas de Estudo")

View(Mean_HorasEstudo)  #Ver a media de Horas de Estudo
#PARTE 3


# Crie a tabela de contingência
TabelaCont1 <- table(Grupo10$Sexo, Grupo10$Burnout_P1)

# Calcule a porcentagem de votos de cada sexo em cada variável
porcentagens <- prop.table(TabelaCont1, margin = 1) * 100

# Crie o gráfico de barras
barplot(porcentagens, beside = TRUE, col = c("green", "purple"),
        main = "Distribuição da Resposta da questão do Burnout 1",
        ylim = c(0, 35),
        legend = c("Feminino", "Masculino"),
        xlab = 'Questões burnout',
        ylab = 'Porcentagem de Respostas')
text(c(2, 20), par("usr")[3] - 4, labels = c("Nunca", "Sempre"), xpd = TRUE, pos = 1)




#REGRESSAO LINEAR

#Parte 1


if(!require(dplyr)) install.packege("ggplot2")
library(ggplot2)

regressao <- plot(Grupo10$HorasTV,Grupo10$HorasSono,
          xlim=c(1,8),ylim=c(1,9),
          xlab="Horas de Sono",
          ylab="Horas de TV")


cor(Grupo10$HorasSono, Grupo10$TempoDesloca, use = "pairwise")

modelo <- lm(Grupo10$HorasTV ~ Grupo10$HorasSono)
abline(modelo)

R2 <- summary(modelo)$r.squared
cat("O coeficiente de determinação tem o valor ", round(R2, 4))

# Coeficientes da reta de regressão
intercepto <- coef(modelo)[1]
coef_inclinacao <- coef(modelo)[2]

cat("A reta de regressão linear é dada pela equação:")
cat(paste("HorasTV =", coef_inclinacao, "* HorasSono +", intercepto))



#Parte 2
regressao = plot(Grupo10$HorasSono,Grupo10$HorasRedes,
     xlim=c(1,10),ylim=c(1,15),
     xlab="Horas de Redes",
     ylab="Horas de Sono")


cor(Grupo10$HorasRedes,Grupo10$TempoDesloca,use = "pairwise")

modelo = lm(Grupo10$HorasSono ~ Grupo10$HorasRedes)

modelo
abline(modelo)

R2 <- summary(modelo)$r.squared
cat("O coeficiente de determinacao tem o valor ",round(R2,4))



# Coeficientes da reta de regressão
intercepto <- coef(modelo)[1]
coef_inclinacao <- coef(modelo)[2]

cat("A reta de regressão linear é dada pela equação:")
cat(paste("HorasSono =", coef_inclinacao, "* HorasRedes +", intercepto))



install.packages("nortest")

library(nortest)
#intervalo de confiança
# Quest?o 1: Qual ? a m?dia de idade dos estudantes?
variavel = Grupo10$Idade

# Cálculo do intervalo de confiança para a média populacional
resultado <- t.test(variavel)

# Exibição do intervalo de confiança
print(resultado$conf.int)

# Quest?o 2: Qual ? o intervalo de confian?a para a propor??o populacional de homens que escolhem um curso espec?fico como primeira op??o?

# Variável de escolha do curso como primeira opção para homens
escolha_homem <- Grupo10$opcao1[Grupo10$Sexo == "Masculino"]


# Cálculo do intervalo de confiança para a proporção populacional
resultado <- prop.test(
  x = table(escolha_homem),
  n = length(escolha_homem),
)

# Exibição do intervalo de confiança
print(resultado$conf.int)




####### Quest?o 3: Com uma confiança de 95%, qual o numero médio de horas dos estudantes do primeiro ano que dedicam ás redes sociais?

# Número de horas diárias dedicadas as redes sociais para estudantes de diferentes anos curriculares
horas_por_ano <- list(ano1 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "1"]),
                      ano2 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "2"]),
                      ano3 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "3"]))

View(horas_por_ano)

mean(horas_por_ano$ano1) #média de horas

# Cálculo do intervalo de confiança para a média populacional
resultado <- t.test(horas_por_ano$ano1, horas_por_ano$ano3)

# Exibição do intervalo de confiança
print(resultado$conf.int)


# Quest?o 4: Qual ? o intervalo de confian?a para a propor??o populacional de estudantes de Bioinform?tica que possuem conhecimento no Programa de Mentoria?

# Dados de conhecimento do Programa de Mentoria para estudantes de Bioinformática
conhecimento_BioInf <- Grupo10$PMentoria[Grupo10$Curso == "L. Bioinformática"]

# Cálculo do intervalo de confiança para a proporção populacional
resultado_BioInf <- prop.test(table(conhecimento_BioInf))

# Exibição dos intervalos de confiança
print(resultado_BioInf$conf.int)



# Teste de hipótese binomial

#Quest?o 1: Será que a proporção populacional de estudantes que escolheram o curso como 1º opção é superiror á proporção dos que não escolheram esse curso como primeira opção?


variavel <- na.omit((Grupo10$opcao1))

p <- 0.5  # Probabilidade teórica para o teste de igualdade
n <- length(variavel)  # Tamanho da amostra
x <- sum(variavel == "Sim")  # Número de sucessos na amostra (quando é sim)

# Realização do teste de hipótese binomial
binom.test(x, n, p, alternative = "greater", conf.level = 0.95)

#Quest?o 2: Será que a proporção populacional de horas dedicadas às redes sociais que são maiores ou iguais a 3 é superior à proporção das horas dedicadas que são menores que 3?

# Dados amostrais
variavel <- na.omit(as.numeric(Grupo10$HorasRedes))

# Teste de hipótese binomial
p <- 0.5  # Probabilidade teórica para o teste de igualdade
n <- length(variavel)  # Tamanho da amostra
x <- sum(variavel >= 3)  # Número de sucessos na amostra (horas dedicadas às redes sociais >= 3)

# Realizar o teste de hipótese
binom.test(x, n, p, alternative = "greater", conf.level = 0.95)


# 3- Ser? que, com uma confianca de 95% , se pode admitir que o numero medio de horas de studo semanais dos estudantes da ESTnbarreiro que frequentam o 1 ano e de 12 horas?

###Normalidade, usar teste de wilcoxon

primeiro_ano <- subset(Grupo10, Grupo10$AnoCurricular == 1)$HorasEstudo

resultado_teste <- wilcox.test(x = primeiro_ano, mu = 12, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

print(resultado_teste)


#Quest?o 4: H? diferen?a significativa nas horas dedicadas ?s redes sociais entre estudantes e a m?dia populacional?


## Normalidade, usar teste de wilcoxon

# Dados de exemplo (substitua pelos seus próprios dados)
horas_estudo <-  Grupo10$HorasEstudo

# Teste de hipótese T-Student
resultado_teste <- wilcox.test(horas_estudo,mu = 6, alternative = "less", var.equal = FALSE, conf.level = 0.95)

# Exibindo o resultado do teste
print(resultado_teste)

#usamos os valores da questao 3 pois o p value era demasiado pequeno


# Quest?o 5:Sera que em termos populacionais pode-se admitir que o tempo medio por semana
#que os estudantes de ESTBarreiro demoram a deslocar-se entre casa e a escola é de 50 horas?"

lillie.test(Grupo10$TempoDesloca)
median(na.omit(Grupo10$TempoDesloca))
# Realização do teste de hipótese de Wilcoxon
library(nortest)
resultado = wilcox.test(Grupo10$TempoDesloca,correct=FALSE,mu=50)

# Resultado
print(resultado)


# Quest?o 6:Sera que em termos populacionais pode-se admitir que o tempo medio por semana
#que os estudantes de ESTBarreiro estudam é de 10 horas?


lillie.test(Grupo10$HorasEstudo)
median(na.omit(Grupo10$HorasEstudo))
# Realização do teste de hipótese de Wilcoxon
library(nortest)
resultado1 = wilcox.test(Grupo10$HorasEstudo,correct=FALSE,mu=10)

# Resultado
print(resultado1)

