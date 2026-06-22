# Tempo de vida de componentes (didatico)

Dados **simulados** de tempo ate a falha de componentes, com censura a
direita, para ilustrar analise de sobrevivencia e confiabilidade.

## Usage

``` r
rnp_vida_util
```

## Format

Um `data.frame` com 150 linhas e 4 variaveis:

- tempo:

  Tempo observado ate a falha ou censura (horas).

- evento:

  Indicador: 1 = falhou; 0 = censurado.

- fornecedor:

  Fornecedor do componente (`Nacional`, `Importado`).

- temperatura:

  Temperatura de operacao (graus Celsius).

## Details

Dados simulados para fins didaticos; ver `data-raw/datasets.R`.

## Examples

``` r
rnp_kaplan_meier(rnp_vida_util$tempo, rnp_vida_util$evento,
                 grupo = rnp_vida_util$fornecedor)$mediana
#> # A tibble: 2 × 6
#>   grupo         n eventos mediana ic_inf ic_sup
#>   <chr>     <int>   <int>   <dbl>  <dbl>  <dbl>
#> 1 Importado    79      62    1030    855   1212
#> 2 Nacional     71      65     668    559    792
```
