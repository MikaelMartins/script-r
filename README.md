# 🌸 Análise Descritiva do Dataset Iris em R

Análise Descritiva do dataset Iris em R. Usa tidyverse para estatísticas por espécie e gera boxplots (`ggplot2`) com cores customizadas. O script automatiza a exportação dos 4 gráficos (Sepal/Petal Length/Width) para um único PDF, ideal para exploração inicial de dados.

## 🚀 Como Usar

**Pré-requisitos**

Certifique-se de ter o R instalado, juntamente com os seguintes pacotes:

1. `tidyverse`

2. `gridExtra`

Você pode instalá-los no console do R, se necessário:

```
install.packages(c("tidyverse", "gridExtra"))
```


**Execução***

1. Clone este repositório ou baixe o arquivo de script R.

2. Abra o script no RStudio ou em sua IDE R preferida.

3. Execute o script completo.

O script irá:

1. Carregar o dataset iris.

2. Exibir estatísticas descritivas globais e por espécie no console.

3. Gerar e exibir os 4 gráficos de boxplot na tela de visualização.

## 💾 Saída (Output)


O script salva automaticamente um arquivo PDF contendo todos os quatro gráficos de boxplot em um layout 2x2:

- `Boxplots_Data_Iris.pdf`
