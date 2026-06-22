# Defeitos por lote de producao (didatico)

Dados **simulados** de contagem de defeitos por lote, por turno e
maquina, para ilustrar a distribuicao de Poisson, superdispersao e
analise de dados categoricos.

## Usage

``` r
rnp_defeitos
```

## Format

Um `data.frame` com 120 linhas e 4 variaveis:

- defeitos:

  Numero de defeitos no lote (contagem).

- turno:

  Turno de producao (`manha`, `tarde`, `noite`).

- maquina:

  Maquina (`A`, `B`, `C`).

- tamanho_lote:

  Tamanho do lote (100, 200 ou 500 pecas).

## Details

Dados simulados para fins didaticos; ver `data-raw/datasets.R`.

## Examples

``` r
rnp_glm(defeitos ~ turno + maquina, data = rnp_defeitos, familia = "poisson")$coeficientes
#> # A tibble: 5 × 7
#>   termo       estimativa erro_padrao estatistica p_valor  ic_inf  ic_sup
#>   <chr>            <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
#> 1 (Intercept)      0.795       0.126        6.31  0       0.548   1.04  
#> 2 turnonoite       0.475       0.127        3.72  0.0002  0.225   0.724 
#> 3 turnotarde       0.226       0.143        1.58  0.115  -0.0551  0.506 
#> 4 maquinaB         0.356       0.124        2.87  0.0041  0.113   0.599 
#> 5 maquinaC        -0.388       0.148       -2.61  0.009  -0.679  -0.0966
```
