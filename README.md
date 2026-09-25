# BRINDI

O **BRINDI** é um pacote R para obtenção e cálculo de indicadores
demográficos, socioeconômicos e de saúde no Brasil a partir de fontes
oficiais de dados.

O pacote reúne funções padronizadas para acessar, processar, agregar e
calcular indicadores provenientes de diferentes sistemas de informação,
incluindo dados do DATASUS/PCDaS, IBGE, SIDRA e outras bases acessíveis
por pacotes R e APIs oficiais.

## Instalação

O pacote pode ser instalado diretamente do GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rfsaldanha/brindi")
```

Depois da instalação:

``` r
library(brindi)
```

## Fontes de dados

O BRINDI utiliza diferentes pacotes e fontes oficiais brasileiras para
obtenção dos dados necessários ao cálculo dos indicadores.

Entre os principais recursos utilizados estão:

  -----------------------------------------------------------------------
  Pacote / fonte                      Uso no BRINDI
  ----------------------------------- -----------------------------------
  `rpcdas`                            Acesso a sistemas de informação em
                                      saúde disponíveis por meio do
                                      PCDaS, incluindo SIM, SINASC e
                                      SIH/SUS

  `brpop`                             Dados populacionais utilizados
                                      principalmente como denominadores

  `recbilis`                          Acesso a dados epidemiológicos
                                      utilizados em indicadores de
                                      morbidade

  `sidrar`                            Acesso às tabelas agregadas do
                                      SIDRA/IBGE

  IBGE / SIDRA                        Dados demográficos e
                                      socioeconômicos oficiais

  DATASUS / PCDaS                     Dados de mortalidade, nascimentos,
                                      internações e outros sistemas de
                                      saúde
  -----------------------------------------------------------------------

O acesso, tratamento e transformação dos dados são realizados pelas
funções do pacote de acordo com as características de cada indicador.

## Indicadores disponíveis

Os indicadores são organizados em funções numeradas seguindo o padrão:

``` text
indi_0001()
indi_0002()
...
indi_0058()
```

A versão atual do BRINDI possui **58 funções de indicadores**,
abrangendo principalmente mortalidade, morbidade, internações
hospitalares e indicadores demográficos e socioeconômicos.

### Mortalidade

O pacote possui indicadores relacionados a diferentes causas de
mortalidade, incluindo:

-   causas externas;
-   doenças cerebrovasculares;
-   doenças isquêmicas do coração;
-   lesões de trânsito;
-   lesões autoprovocadas;
-   câncer de mama;
-   câncer do colo do útero;
-   câncer de próstata;
-   mortalidade infantil;
-   mortalidade em menores de cinco anos;
-   doenças cardiovasculares;
-   dengue;
-   doença de Chagas;
-   leptospirose;
-   malária;
-   asma;
-   bronquite;
-   pneumonia;
-   doença pulmonar obstrutiva crônica;
-   infarto agudo do miocárdio;
-   agressões.

### Morbidade e incidência

O BRINDI também possui indicadores de incidência para doenças
infecciosas e transmitidas por vetores, incluindo:

-   dengue;
-   Zika;
-   chikungunya;
-   febre amarela;
-   doença de Chagas;
-   leishmaniose tegumentar;
-   leishmaniose visceral.

### Internações hospitalares

Os indicadores de internação utilizam dados hospitalares do SUS e
contemplam diferentes causas e condições, como:

-   fraturas;
-   afogamentos;
-   quedas;
-   cólera;
-   esquistossomose;
-   hepatite A;
-   leptospirose;
-   tripanossomíase;
-   asma;
-   bronquite;
-   pneumonia;
-   doença pulmonar obstrutiva crônica;
-   infarto agudo do miocárdio;
-   acidente vascular cerebral.

### Indicadores demográficos

O pacote também contém indicadores demográficos, como:

-   taxa de crescimento anual da população;
-   esperança de vida ao nascer;
-   esperança de vida aos 60 anos.

### Indicadores socioeconômicos

Entre os indicadores socioeconômicos disponíveis estão:

-   proporção da população ocupada sem contribuição para a previdência
    social;
-   proporção de jovens que não estudam e não trabalham;
-   proporção da população de 5 a 17 anos em situação de trabalho
    infantil;
-   razão entre a renda total dos 10% mais ricos e a dos 40% mais
    pobres;
-   proporção de analfabetismo na população;
-   proporção da população sem educação básica.

## Uso básico

Cada indicador pode ser acessado por sua respectiva função
`indi_XXXX()`.

Por exemplo:

``` r
library(brindi)

indi_0001(
  agg = "mun_res",
  ano = 2023
)
```

A função `indi_0001()` calcula a taxa de mortalidade por causas
externas.

Os argumentos disponíveis podem variar de acordo com o indicador e com a
fonte de dados utilizada.

## Agregação espacial

Diversos indicadores permitem definir o nível de agregação espacial.

Dependendo do indicador, podem estar disponíveis opções como:

``` text
mun_res
mun_ocor
uf_res
uf_ocor
regsaude_res
regsaude_ocor
```

Essas opções permitem trabalhar, por exemplo, com município ou Unidade
da Federação de residência ou ocorrência.

A disponibilidade de cada nível de agregação depende da base utilizada
pelo indicador.

## Agregação temporal

Algumas funções também permitem diferentes níveis de agregação temporal,
como:

``` text
year
month
week
```

A disponibilidade depende do indicador e da fonte original dos dados.

## Denominadores populacionais

Indicadores que necessitam de denominadores populacionais podem utilizar
dados obtidos por meio do `brpop`.

Em funções que permitem escolher a fonte populacional, essa seleção pode
ser realizada pelo argumento `pop_source`.

Exemplo:

``` r
indi_0001(
  agg = "mun_res",
  ano = 2023,
  pop_source = "datasus"
)
```

Dessa forma, o numerador proveniente dos sistemas de informação em saúde
pode ser combinado com o denominador populacional adequado para o
cálculo da taxa.

## Padronização de taxas por idade

Alguns indicadores de mortalidade permitem o cálculo de taxas ajustadas
por idade por meio do argumento:

``` r
adjust_rates = TRUE
```

Por exemplo:

``` r
indi_0001(
  agg = "uf_res",
  ano = 2023,
  adjust_rates = TRUE
)
```

Quando solicitado, o BRINDI utiliza suas funções internas para calcular
os componentes específicos por idade e realizar o ajuste da taxa.

## Acesso ao PCDaS

Os indicadores que obtêm dados por meio do `rpcdas` necessitam de acesso
à API do PCDaS.

O token pode ser informado diretamente:

``` r
indi_0001(
  agg = "mun_res",
  ano = 2023,
  pcdas_token = "SEU_TOKEN"
)
```

Também é possível armazenar o token no ambiente do R para evitar a
inclusão da credencial diretamente nos scripts.

Por exemplo, no arquivo `.Renviron`:

``` text
PCDAS_TOKEN=seu_token_aqui
```

## Resultado dos indicadores

As funções do BRINDI realizam internamente as etapas necessárias para
combinar numeradores, denominadores e demais informações utilizadas no
cálculo.

Dependendo do indicador e dos argumentos utilizados, a saída pode conter
informações como:

-   unidade geográfica;
-   período de referência;
-   valor do indicador;
-   numerador;
-   denominador.

Algumas funções também permitem controlar o fator multiplicador e o
número de casas decimais.

Por exemplo:

``` r
indi_0001(
  agg = "mun_res",
  ano = 2023,
  multi = 100000,
  decimals = 2
)
```

## Estrutura do pacote

Além das funções individuais `indi_XXXX()`, o BRINDI possui funções
internas utilizadas para operações comuns entre diferentes indicadores.

Essas funções auxiliam em tarefas como:

-   obtenção de denominadores populacionais;
-   cálculo dos indicadores;
-   cálculo de taxas ajustadas por idade;
-   agregação espacial;
-   agregação temporal;
-   tratamento das combinações sem registros;
-   organização e expansão dos resultados.

Essa estrutura permite que diferentes indicadores compartilhem
procedimentos comuns, enquanto cada função `indi_XXXX()` mantém a lógica
específica de obtenção e definição do respectivo indicador.

## Armazenamento dos resultados

A estrutura atual do BRINDI também contém recursos para trabalhar com
diferentes formas de armazenamento dos indicadores, incluindo:

-   SQLite;
-   PostgreSQL;
-   Parquet.

Esses recursos permitem armazenar e organizar resultados produzidos pelo
pacote de acordo com diferentes necessidades de uso.

## Informações do pacote

``` text
Package: brindi
Version: 0.2.0
License: MIT
```

O BRINDI tem como foco tornar mais padronizada e reprodutível a obtenção
e o cálculo de indicadores brasileiros a partir de fontes oficiais de
dados.
