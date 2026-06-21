# Correlacao parcial

Calcula correlacao parcial entre x e y, controlando por z (vetor ou
matriz).

## Usage

``` r
rnp_correlacao_parcial(x, y, z, method = "pearson", digits = 4L)
```

## Arguments

- x, y:

  Vetores numericos.

- z:

  Vetor numerico ou matriz de variaveis de controle.

- method:

  String. Apenas `"pearson"` implementado por enquanto.

- digits:

  Inteiro.

## Value

tibble com `correlacao_parcial`, `gl`, `p_valor`, `n`.

## Examples

``` r
rnp_correlacao_parcial(mtcars$mpg, mtcars$hp, mtcars$wt)
#> # A tibble: 1 × 4
#>   correlacao_parcial[,"y"]    gl p_valor[,"y"]     n
#>                      <dbl> <dbl>         <dbl> <int>
#> 1                   -0.161    29         0.388    32
```
