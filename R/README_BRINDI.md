# BRINDI | Nomenclatura dos indicadores

Os arquivos `.R` seguem uma nomenclatura inspirada na classificação temática da RIPSA:

`CATEGORIA_DIMENSÃO_INDICADOR_DERIVAÇÃO.R`

- **Categoria:** sigla temática, como `MRT` (mortalidade).
- **Dimensão e indicador:** numeração da referência RIPSA (por exemplo, `5_04`).
- **Derivação:** sufixo `01`, `02` etc. para indicadores específicos derivados da mesma referência.
- **`00`:** reservado, nesta convenção do BRINDI, para um indicador agregado da dimensão, sem correspondência direta com um indicador individual da RIPSA.

**Exemplo:** `MRT_5_04_01.R` identifica uma derivação de mortalidade vinculada à referência temática `MRT.5.04`. Os códigos completos com sufixos são próprios da organização do BRINDI; não devem ser confundidos com códigos oficiais da RIPSA. O nome da função acompanha o arquivo, sem `.R`.

## Quatro indicadores específicos padronizados

| Arquivo / função | Indicador |
|---|---|
| `MRT_5_04_01.R` / `MRT_5_04_01()` | Mortalidade por AVC |
| `MRT_5_04_02.R` / `MRT_5_04_02()` | Mortalidade por doenças isquêmicas do coração |
| `MRT_4_03_01.R` / `MRT_4_03_01()` | Mortalidade por acidentes de transporte terrestre |
| `MRT_4_02_01.R` / `MRT_4_02_01()` | Mortalidade por suicídio |

**Indicador agregado já padronizado:** `MRT_4_00.R` — mortalidade por causas externas.

> Observação: os códigos identificam a organização dos scripts, não atestam equivalência metodológica integral com a RIPSA. Os filtros CID-10 e os resultados devem ser validados antes da publicação.
