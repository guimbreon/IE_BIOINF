#LIMPEZA DE DADOS
#Horas Estudo
for(i in 1:nrow(Grupo10))
  if(!(is.na(Grupo10$HorasEstudo[i])) & Grupo10$HorasEstudo[i] < 24 ){
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
my_table
#PARTE 2
#Comparação da média de Horas de estudo entre Homem

if(!require(dplyr)) install.packege("dplyr")
library(dplyr)

Female_HorasEstudo = filter(Grupo10, Sexo == 1)

Male_HorasEstudo = filter(Grupo10, Sexo == 2)

Male_HorasEstudo = round(mean(Male_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Female_HorasEstudo = round(mean(Female_HorasEstudo$HorasEstudo, na.rm = TRUE), 2)

Mean_HorasEstudo = data.frame(Female_HorasEstudo,Male_HorasEstudo,row.names = "Média de Horas a Estudar")

perc_incr_F_M = round(((Mean_HorasEstudo$Female_HorasEstudo - Mean_HorasEstudo$Male_HorasEstudo)/Mean_HorasEstudo$Male_HorasEstudo) * 100, 2)

perc_incr_M_F = round(((Mean_HorasEstudo$Male_HorasEstudo - Mean_HorasEstudo$Female_HorasEstudo)/Mean_HorasEstudo$Female_HorasEstudo) * 100, 2)

Diff_HorasEstudo = data.frame("Feminino-Masculino" = perc_incr_F_M,"Masculino-Feminino" = perc_incr_M_F,row.names = "Diferença entre:")

View(Mean_HorasEstudo)  #Ver a média de Horas de Estudo
View(Diff_HorasEstudo)  #Ver a diferença entre as Horas de estudo


#PARTE 3
TabelaCont1 = table(Grupo10$Sexo,Grupo10$Burnout_P1)

TabelaCont1 = cbind(TabelaCont1)

rownames(TabelaCont1) = c("Female","Male")


barplot((TabelaCont1), beside = TRUE,col = c("green","purple"),
        main = "Distribuição da Resposta da questão do Burnout 1",
        ylim = c(0,25),
        legend = c("Feminino","Masculino"))

