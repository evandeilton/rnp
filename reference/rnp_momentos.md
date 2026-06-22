# Momentos amostrais, assimetria e curtose

Calcula momentos centrais ate a ordem desejada, alem de media,
variancia, desvio-padrao, coeficiente de assimetria (g1) e curtose em
excesso (g2), usando backend C++ (RcppArmadillo) numericamente estavel.

## Usage

``` r
rnp_momentos(x, ordem = 4L, na.rm = TRUE, digits = 4L)
```

## Arguments

- x:

  Vetor numerico.

- ordem:

  Inteiro \>= 2. Ordem maxima dos momentos centrais.

- na.rm:

  Logico. Remove NA antes do calculo.

- digits:

  Inteiro. Casas decimais.

## Value

Uma lista com:

- `resumo`: tibble com `media`, `variancia`, `desvio_padrao`,
  `assimetria`, `curtose_excesso`, `n`.

- `momentos`: tibble com `ordem` (2..ordem) e `momento_central`.

## See also

Other descritiva:
[`rnp_intervalo_classes()`](https://evandeilton.github.io/rnp/reference/rnp_intervalo_classes.md),
[`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md),
[`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md),
[`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md)

## Examples

``` r
rnp_momentos(rnorm(500))
#> 
#> ── Momentos amostrais ──────────────────────────────────────────────────────────
#> 
#> ── Resumo 
#> # A tibble: 1 × 6
#>     media variancia desvio_padrao assimetria curtose_excesso     n
#>     <dbl>     <dbl>         <dbl>      <dbl>           <dbl> <dbl>
#> 1 -0.0539     0.973         0.986      0.149          -0.504   500
#> 
#> ── Momentos 
#> # A tibble: 3 × 2
#>   ordem momento_central
#>   <int>           <dbl>
#> 1     2           0.971
#> 2     3           0.143
#> 3     4           2.35 
rnp_momentos(mtcars$mpg, ordem = 6)
#> 
#> ── Momentos amostrais ──────────────────────────────────────────────────────────
#> 
#> ── Resumo 
#> # A tibble: 1 × 6
#>   media variancia desvio_padrao assimetria curtose_excesso     n
#>   <dbl>     <dbl>         <dbl>      <dbl>           <dbl> <dbl>
#> 1  20.1      36.3          6.03      0.640          -0.200    32
#> 
#> ── Momentos 
#> # A tibble: 5 × 2
#>   ordem momento_central
#>   <int>           <dbl>
#> 1     2            35.2
#> 2     3           134. 
#> 3     4          3466. 
#> 4     5         26134. 
#> 5     6        465163. 
```
