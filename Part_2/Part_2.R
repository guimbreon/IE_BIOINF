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

Objeto = Grupo10$Idade
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
barplot(perc, names.arg = nomes, xlab = "Valores da Variavel", ylab = "Percentagem", main = "Grafico de barras para a variavel 'Idade'")


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
Dormem = unique(Grupo10$Idade, is.na=TRUE)

my_table <- data.frame(
  Idade = numeric(),
  HorasSono = numeric(),
  stringsAsFactors = FALSE
)

# Loop os valores de Dormem e analiza os dados referentes 
#A cada idade no Grupo 10 
# e faz a média da hora de sono
for(i in na.omit(Dormem)) {
  Sleep = na.omit(filter(Grupo10, Idade == i))
  mean_sono <- mean(Sleep$HorasSono)
  row <- data.frame(Idade = i, HorasSono = mean_sono)
  my_table <- rbind(my_table, row)
}

# View the resulting table
View(my_table)
na.omit(my_table)



# Boxplot
boxplot(Grupo10$HorasSono ~ Grupo10$Idade, main = "Boxplot das Horas de Sono por Idade", xlab = "Idade", ylab = "Horas de Sono")



#PARTE 2
#Comparacao da media de Horas de estudo entre Homem e mulher

library(dplyr)
library(pander)
Female_HorasEstudo = filter(Grupo10, Sexo == "Feminino")

Male_HorasEstudo = filter(Grupo10, Sexo == "Masculino")
View(Male_HorasEstudo)
Male_HorasEstudo = round(mean(Male_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Female_HorasEstudo = round(mean(Female_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Mean_HorasEstudo = data.frame(Female_HorasEstudo,Male_HorasEstudo,row.names = "Media de Horas a Estudar")

perc_incr_F_M = round(((Mean_HorasEstudo$Female_HorasEstudo - Mean_HorasEstudo$Male_HorasEstudo)/Mean_HorasEstudo$Male_HorasEstudo) * 100, 2)

perc_incr_M_F = round(((Mean_HorasEstudo$Male_HorasEstudo - Mean_HorasEstudo$Female_HorasEstudo)/Mean_HorasEstudo$Female_HorasEstudo) * 100, 2)



diff_horas_estudo <- data.frame("Feminino-Masculino" = paste0(perc_incr_F_M, "%"),
                                "Masculino-Feminino" = paste0(perc_incr_M_F, "%"),
                                row.names = "Diferença entre:")

pandoc.table(Mean_HorasEstudo,plain.ascii = TRUE,caption = "Média de Horas a Estudar") 

pandoc.table(diff_horas_estudo,plain.ascii = TRUE, caption = "Diferença entre as Horas de Estudo")


View(Mean_HorasEstudo)  #Ver a media de Horas de Estudo
View(Diff_HorasEstudo)  #Ver a diferenca entre as Horas de estudo

# Boxplot
boxplot(Grupo10$HorasEstudo ~ Grupo10$Sexo, main = "Boxplot das Horas de Estudo por Gênero", xlab = "Gênero", ylab = "Horas de Estudo")


#PARTE 3
TabelaCont1 = table(Grupo10$Sexo,Grupo10$Burnout_P1)

TabelaCont1 = cbind(TabelaCont1)

rownames(TabelaCont1) = c("Female","Male")


barplot((TabelaCont1), beside = TRUE,col = c("green","purple"),
        main = "Distribuição da Resposta da questão do Burnout 1",
        ylim = c(0,25),
        legend = c("Feminino","Masculino"))


#REGRESSAO LINEAR

#Parte 1


if(!require(dplyr)) install.packege("ggplot2")
library(ggplot2)
-
  regressao <- plot(Grupo10$HorasSono,Grupo10$HorasTV,
                    xlim=c(1,9),ylim=c(1,8),
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
regressao = plot(Grupo10$HorasRedes,Grupo10$HorasSono,
                 xlim=c(1,10),ylim=c(1,15),
                 xlab="Horas de Redes",
                 ylab="Horas de Sono")


cor(Grupo10$HorasRedes,Grupo10$HorasSono,use = "pairwise")

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
