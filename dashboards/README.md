---
editor_options: 
  markdown: 
    wrap: 72
---

# Dashboard de completude BRINDI x RIPSA (Livro Verde)

Este diretório contém o material de acompanhamento de quais indicadores
da Matriz de Indicadores do **Livro Verde da RIPSA** já estão
implementados no pacote `brindi`.

## Arquivos

-   `ripsa_completude.qmd` — dashboard em formato Quarto
    (`format: dashboard`). Requer [Quarto](https://quarto.org) e Pandoc
    instalados para ser renderizado
    (`quarto render ripsa_completude.qmd`).

-   `ripsa_completude.html` — versão estática, autocontida (HTML puro +
    gráfico embutido em base64), gerada por
    `generate_static_dashboard.R`. Pode ser aberta diretamente em
    qualquer navegador, sem depender de Quarto/Pandoc.

-   `generate_static_dashboard.R` — script que gera
    `ripsa_completude.html` a partir das funções do pacote `brindi`
    (`ripsa_match_indicators()` e `ripsa_completeness_summary()`). Rode
    com:

    ``` r
    # a partir da raiz do pacote, com o brindi instalado ou carregado via load_all()
    source("dashboards/generate_static_dashboard.R")
    ```

## Fonte dos dados

-   **Matriz RIPSA**: extraída do sumário oficial do PDF do Livro Verde
    (`inst/extdata/ripsa_livro_verde_indicadores.csv`), 168 indicadores
    nas dimensões `DEM`, `SOC`, `MRT`, `MRB`, `FRP`, `REC` e `COB`.
-   **Catálogo BRINDI**: títulos das 58 funções `indi_*()` implementadas
    (`inst/extdata/brindi_indicadores_catalogo.csv`).

## Funções do pacote usadas

| Função | Descrição |
|----|----|
| `ripsa_matrix()` | Carrega a matriz de indicadores do Livro Verde. |
| `brindi_indicator_catalog()` | Carrega o catálogo de funções `indi_*` do pacote. |
| `ripsa_match_indicators(threshold = 0.55)` | Casa cada indicador RIPSA com a função BRINDI mais similar (Jaccard de tokens), marcando `implementado`. |
| `ripsa_completeness_summary(threshold = 0.55)` | Resumo de completude por dimensão. |

## Limitações

O pareamento é heurístico (similaridade textual, não um mapeamento de
código oficial). Indicadores implementados sob um título muito diferente
do Livro Verde podem não ser detectados automaticamente — revisão manual
é recomendada antes de decisões definitivas sobre lacunas.
