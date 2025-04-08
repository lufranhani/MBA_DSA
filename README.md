# Análise de Redes Sociais de Dados de Solos

Este repositório contém o script R e os dados utilizados para realizar uma análise de redes sociais (SNA) em dados de características do solo de uma área de transição entre Cerrado e Mata Atlântica.

## Arquivos

* `data/MBA.csv`: Arquivo contendo os dados das características do solo.
* `script/MBA_Complete.R`: Script R para realizar a análise de redes sociais.
* `README.md`: Este arquivo.
* `.gitignore`: Especifica arquivos que o Git deve ignorar.

## Como Executar o Script

1.  Clone este repositório para o seu computador:
    ```bash
    git clone [https://github.com/SEU_USUARIO/MBA_DSA.git](https://github.com/SEU_USUARIO/MBA_DSA.git)
    ```
2.  Navegue até o diretório do repositório:
    ```bash
    cd MBA_DSA
    ```
3.  Abra o R (ou RStudio).
4.  Defina o diretório de trabalho para a raiz do repositório:
    ```R
    setwd("./")
    ```
5.  Execute o script R:
    ```R
    source("./script/MBA_Complete.R")
    ```

## Dependências

Certifique-se de que os seguintes pacotes R estejam instalados:

* igraph
* tidyverse
* qgraph
* GGally
* dplyr
* purrr
* RColorBrewer
* viridis
* ggcorrplot
* tidyr
* corrplot

Você pode instalá-los usando o seguinte comando no R:

```R
install.packages(c("igraph", "tidyverse", "qgraph", "GGally", "dplyr", "purrr", "RColorBrewer", "viridis", "ggcorrplot", "tidyr", "corrplot"))
