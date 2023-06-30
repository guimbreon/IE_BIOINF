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

#Pacotes necessarios

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

