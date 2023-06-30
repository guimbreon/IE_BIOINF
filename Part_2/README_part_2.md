# README

Este arquivo README fornece uma visão geral e instruções para o script em R fornecido.

### Conjunto de Dados

O conjunto de dados "Grupo10" inclui informações sobre um grupo de indivíduos, como idade, sexo, horas de estudo, horas de sono, horas de uso de mídia social e horas de assistir TV. Também inclui as respostas deles a várias perguntas relacionadas ao esgotamento.

### Descrição

O script em R realiza várias tarefas de limpeza de dados e análise estatística em um conjunto de dados chamado "Grupo10". O script inclui as seguintes seções principais:

1. Limpeza de Dados: O script limpa os dados removendo valores inválidos e substituindo-os por `NA` (valores ausentes) para as variáveis: `HorasEstudo`, `HorasRedes`, `HorasTV` e `HorasSono`.

2. Organização de Dados: O script organiza os dados convertendo códigos numéricos em rótulos significativos para as variáveis: `Sexo`, `opcao1`, `Escolhi` e `PMentoria`.

3. Pacotes Necessários: O script instala e carrega o pacote "nortest", que é usado para realizar testes estatísticos.

4. Análise Estatística: O script realiza várias análises estatísticas e calcula intervalos de confiança para diferentes variáveis e questões de pesquisa. As análises incluem:

   - Cálculo do intervalo de confiança para a média de idade dos estudantes.
   - Cálculo do intervalo de confiança para a proporção de estudantes do sexo masculino que escolhem um curso específico como primeira opção.
   - Cálculo do intervalo de confiança para a diferença na média de horas diárias gastas em redes sociais entre estudantes do primeiro e terceiro ano.
   - Cálculo do intervalo de confiança para a proporção de estudantes de Bioinformática com conhecimento do Programa de Mentoria.
   - Teste de hipótese binomial para a proporção de estudantes escolhendo a opção 1 e a proporção de horas gastas em redes sociais maior ou igual a 0.
   - Teste de hipótese para a média do número de horas semanais de estudo de estudantes do primeiro ano e sua comparação com a média populacional de 12.
   - Teste de hipótese para a diferença nas horas dedicadas a redes sociais entre estudantes e a média populacional.
   - Teste de hipótese para a mediana do número de horas semanais de estudo de estudantes do primeiro ano e sua comparação com a mediana populacional de 8.
   - Teste de hipótese para a mediana das horas dedicadas a redes sociais entre estudantes e sua comparação com a mediana populacional de 2.

### Uso

1. Preparação dos Dados: Antes de executar o script, verifique se você possui um conjunto de dados chamado "Grupo10" disponível em seu ambiente R.

2. Instalar Pacotes Necessários: O script requer o pacote "nortest". Se você não tiver o pacote instalado, descomente a linha `

install.packages("nortest")` para instalá-lo.

3. Executar o Script: Execute o script em R em seu ambiente R preferido (por exemplo, RStudio) ou execute-o linha por linha.

4. Rever os Resultados: O script fornecerá uma saída para cada análise estatística, incluindo intervalos de confiança e resultados de testes de hipótese. Os resultados serão exibidos no console.

## Observação

Por favor, observe que o script pressupõe a disponibilidade do conjunto de dados "Grupo10" e nomes de variáveis específicas, como "Idade", "Sexo", "opcao1", "Escolhi", "PMentoria", "HorasEstudo", "HorasRedes", "HorasTV" e "HorasSono". Verifique se o seu conjunto de dados possui essas variáveis ou modifique o script de acordo.

Sinta-se à vontade para modificar o script para atender às suas necessidades específicas e adaptá-lo a diferentes conjuntos de dados ou questões de pesquisa.

Para quaisquer perguntas ou problemas, entre em contato com os autores do script (Grupo 10) usando os IDs de estudante fornecidos.


## In English:
[../english/README_2.MD]
