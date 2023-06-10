# GRUPO 10
# 202200646
# 202200596
# 202101267





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
#intervalo de confian√ßa
# Quest„o 1: Qual È a mÈdia de idade dos estudantes?/Qual È o intervalo de confianÁa para a mÈdia populacional da idade dos estudantes?

variavel = Grupo10$Idade

# C√°lculo do intervalo de confian√ßa para a m√©dia populacional
resultado <- t.test(variavel)

# Exibi√ß√£o do intervalo de confian√ßa
print(resultado$conf.int)

# Quest„o 2: Qual È o intervalo de confianÁa para a proporÁ„o populacional de homens que escolhem um curso especÌfico como primeira opÁ„o?

# Vari√°vel de escolha do curso como primeira op√ß√£o para homens
escolha_homem <- Grupo10$opcao1[Grupo10$Sexo == "Masculino"]


# C√°lculo do intervalo de confian√ßa para a propor√ß√£o populacional
resultado <- prop.test(
  x = table(escolha_homem),
  n = length(escolha_homem),
  p = 0.5  #Valor dado pela professora
)

# Exibi√ß√£o do intervalo de confian√ßa
print(resultado$conf.int)




# Quest„o 3: Qual È o intervalo de confianÁa para a diferenÁa mÈdia populacional nas horas di·rias dedicadas ‡s redes sociais entre estudantes do primeiro e do terceiro ano curricular?

# N√∫mero de horas di√°rias dedicadas as redes sociais para estudantes de diferentes anos curriculares
horas_por_ano <- list(ano1 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "1"]),
                      ano2 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "2"]),
                      ano3 = na.omit(Grupo10$HorasRedes[Grupo10$AnoCurricular == "3"]))

View(horas_por_ano)


# C√°lculo do intervalo de confian√ßa para a m√©dia populacional
resultado <- t.test(horas_por_ano$ano1, horas_por_ano$ano3)

# Exibi√ß√£o do intervalo de confian√ßa
print(resultado$conf.int)


# Quest„o 4: Qual È o intervalo de confianÁa para a proporÁ„o populacional de estudantes de Bioinform·tica que possuem conhecimento no Programa de Mentoria?

# Dados de conhecimento do Programa de Mentoria para estudantes de Bioinform√°tica
conhecimento_BioInf <- Grupo10$PMentoria[Grupo10$Curso == "L. Bioinform√°tica"]

# C√°lculo do intervalo de confian√ßa para a propor√ß√£o populacional
resultado_BioInf <- prop.test(table(conhecimento_BioInf))

# Exibi√ß√£o dos intervalos de confian√ßa
print(resultado_BioInf$conf.int)



# Teste de hip√≥tese binomial

#Quest„o 1: Qual È o resultado do teste de hipÛtese binomial para a proporÁ„o populacional de estudante que escolhem a opÁ„o 1?
  

variavel <- na.omit((Grupo10$opcao1))

p <- 0.5  # Probabilidade te√≥rica para o teste de igualdade
n <- length(variavel)  # Tamanho da amostra
x <- sum(variavel == "Sim")  # N√∫mero de sucessos na amostra (quando √© sim)

# Realiza√ß√£o do teste de hip√≥tese binomial
binom.test(x, n, p, alternative = "greater", conf.level = 0.95)

#Quest„o 2: Qual È o resultado do teste de hipÛtese binomial para a proporÁ„o populacional de horas dedicadas ‡s redes sociais que s„o maiores ou iguais a 0?


# Dados amostrais
variavel <- na.omit(as.numeric(Grupo10$HorasRedes))

# Teste de hip√≥tese binomial
p <- 0.5  # Probabilidade te√≥rica para o teste de igualdade
n <- length(variavel)  # Tamanho da amostra
x <- sum(variavel >= 0)  # N√∫mero de sucessos na amostra (horas dedicadas √†s redes sociais >= 0)

# Realizar o teste de hip√≥tese
binom.test(x, n, p, alternative = "greater", conf.level = 0.95)


# 3- Ser· que, com uma confianca de 95% , se pode admitir que o numero medio de horas de studo semanais dos estudantes da ESTnbarreiro que frequentam o 1 ano e de 12 horas?
  
primeiro_ano <- subset(Grupo10, Grupo10$AnoCurricular == 1)$HorasEstudo

resultado_teste <- t.test(x = primeiro_ano, mu = 12, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

print(resultado_teste)


#Quest„o 4: H· diferenÁa significativa nas horas dedicadas ‡s redes sociais entre estudantes e a mÈdia populacional?

# Dados de exemplo (substitua pelos seus pr√≥prios dados)
horas_estudo <-  Grupo10$HorasEstudo

# Teste de hip√≥tese T-Student
resultado_teste <- t.test(horas_estudo,mu = mean(na.omit(horas_estudo)), alternative = "less", var.equal = FALSE, conf.level = 0.95)

# Exibindo o resultado do teste
print(resultado_teste)



# Quest„o 5: Ser· que, com uma confianca de 95% , se pode admitir que o numero medio de horas de estudo semanais dos
#estudantes da ESTbarreiro que frequentam o 1 ano e a mediana populacional (8)? 	
sdf = median(primeiro_ano)
# Dados amostrais

#usamos os valores da questao 3 pois o p value era demasiado pequeno

# Realiza√ß√£o do teste de hip√≥tese de Wilcoxon
resultado = wilcox.test(primeiro_ano ,conf.level = 0.95, mu = median(na.omit(primeiro_ano)))

# Resultado
print(resultado)


#Quest„o 6: Ser· que, com uma confianca de 95% , se pode admitir que o numero medio de horas de dedicadas ·s redes sociais
#dos estudantes da ESTB e a mediana populacional (2)?
# Dados amostrais

#usamos os valores da questao 4 pois o p value era demasiado pequeno

# Realiza√ß√£o do teste de hip√≥tese de Wilcoxon
resultado <- wilcox.test(horas_estudo, paired = FALSE,conf.level = 0.95, mu = median(na.omit(horas_redes)))

# Imprimir o resultado
print(resultado)
