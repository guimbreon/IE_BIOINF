## IE

Este projeto é para a disciplina de Introdução à Estatística e inclui limpeza de dados, análise descritiva bivariada e Análise de Regressão de um conjunto de dados chamado "Grupo10".

### Conjunto de Dados

O conjunto de dados "Grupo10" inclui informações sobre um grupo de indivíduos, como idade, sexo, horas de estudo, horas de sono, horas de uso de mídia social e horas de assistir TV. Também inclui as respostas deles a várias perguntas relacionadas ao esgotamento.

Todas essas informações foram coletadas por meio de uma pesquisa, na qual os participantes eram pessoas cursando as disciplinas IE, PE, ATED, EA e ATMD na ESTBarreiro/IPS.

### Limpeza e Organização de Dados

Na primeira parte do projeto, os dados foram limpos para remover quaisquer valores que não estivessem dentro do intervalo esperado de horas. Por exemplo, quaisquer horas de estudo ou sono maiores que 24 foram substituídas por "NA". Isso foi feito para garantir que os dados utilizados na análise fossem precisos e confiáveis. Em seguida, a variável "Sexo" é convertida de numérica para categórica, onde 1 representa "Feminino" e 2 representa "Masculino". As variáveis "opcao1", "Escolhi" e "PMentoria" também são convertidas de numéricas para categóricas, onde 1 representa "Sim" e 2 representa "Não".

### Análise Descritiva Univariada

Na segunda parte, é realizada uma análise descritiva univariada em algumas variáveis do conjunto de dados "Grupo10". A biblioteca "pander" é utilizada para criar tabelas com frequência absoluta, frequência relativa e frequência relativa em porcentagem para as variáveis "Idade", "Sexo", "Curso" e "Ano Acadêmico". Cada tabela é criada em três etapas: cálculo da frequência absoluta, cálculo da frequência relativa e cálculo da frequência relativa em porcentagem. Em seguida, as três medidas são organizadas em uma tabela utilizando a biblioteca "pander".

### Análise Descritiva Bivariada

Perguntas de Estudo:

1. Como a média de horas de sono varia de acordo com a idade.
2. As mulheres estudam mais do que os homens? Ou é o contrário?
3. Existe alguma relação entre exaustão emocional decorrente dos estudos entre os gêneros?

Na terceira parte do projeto, foi realizada uma análise descritiva bivariada para explorar a relação entre duas variáveis no conjunto de dados.

Na primeira parte da análise, foi calculada a média de horas de sono para cada grupo etário no conjunto de dados e exibida em uma tabela. Na segunda parte da análise, as horas médias de estudo para homens e mulheres no conjunto de dados foram comparadas, e a diferença percentual foi calculada e exibida em uma tabela. Na terceira parte da análise, uma tabela de contingência e um gráfico de barras foram criados para visualizar a distribuição das respostas relacionadas ao esgotamento para homens e mulheres no conjunto de dados.

### An

álise de Regressão

Perguntas de Estudo:

1. As pessoas assistem mais TV do que dormem?
2. A geração TikTok dorme menos do que usa mídia social?

Na quarta parte do projeto, foi realizada uma análise de regressão linear simples para explorar a relação entre duas variáveis no conjunto de dados. Especificamente, foi examinada a relação entre as horas de assistir TV e as horas de sono, e as horas de uso de mídia social e sono. Um gráfico de dispersão foi criado para visualizar a relação, e um modelo linear foi ajustado aos dados. O coeficiente de determinação (R-quadrado) foi calculado para determinar a força da relação.

### Tabela de Contingência Bivariada

Este código R cria uma tabela de contingência para analisar a relação entre duas variáveis. O código calcula a frequência de cada combinação das duas variáveis e exibe os resultados em uma tabela que mostra tanto as frequências absolutas quanto as relativas. O pacote "pander" é usado para formatar a tabela para facilitar a leitura. O código pode ser usado com quaisquer duas variáveis para explorar a distribuição e relação entre elas.

NOTA: Este código está em um arquivo separado [aqui](bivariate_contigence.R).

## In English:
[README.MD](../english/README.MD)

### Conclusão

No geral, este projeto fornece um exemplo de como limpar e analisar um conjunto de dados usando R. Os resultados da análise podem ser usados para obter insights sobre as relações entre as variáveis no conjunto de dados e para informar futuros processos de tomada de decisão.
