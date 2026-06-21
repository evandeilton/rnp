# Tamanho de efeito

Calcula medidas de tamanho de efeito: Cohen's d, Hedges' g, r,
eta-quadrado.

## Usage

``` r
rnp_tamanho_efeito(x, y = NULL, paired = FALSE, digits = 4L)
```

## Arguments

- x, y:

  Vetores numericos (para d, g, r) ou objeto `aov` (para eta2).

- paired:

  Logico. Amostras pareadas (apenas para d, g, r).

- digits:

  Inteiro.

## Value

tibble com `cohens_d`, `hedges_g`, `r`, `eta2`.

## Examples

``` r
rnp_tamanho_efeito(rnorm(30, 5), rnorm(30, 5.5))
#> # A tibble: 1 × 4
#>   cohens_d hedges_g       r   eta2
#>      <dbl>    <dbl>   <dbl>  <dbl>
#> 1   -0.232   -0.229 -0.0305 0.0138
```
