
library(pander)

# Criar uma variável com os dados a serem analisados
Var1 <- Grupo10$Idade
Var2 <- Grupo10$Sexo

Var2

# Calcular a tabela de contingência
contingencia <- table(Var1, Var2)

# Calcular a frequência relativa
relat <- prop.table(contingencia, margin = 1) # por linha

# Calcular a frequência relativa em percentagem
perc <- round(relat * 100, 2)

# Criar a tabela com a library pander
tabela <- cbind(contingencia, relat, perc)
colnames(tabela) <- c("M", "F", "Fr. Relativa M", "Fr. Relativa F", "Percentagem M", "Percentagem F")

pandoc.table(tabela,plain.ascii = TRUE ,caption = "Tabela de contingência para as variáveis 'Idade' e 'Gênero'")
