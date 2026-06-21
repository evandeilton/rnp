# Correlacao com teste de hipotese

Calcula correlacao (Pearson/Spearman/Kendall) com p-valor e IC 95% (para
Pearson, via transformacao de Fisher).

## Usage

``` r
rnp_correlacao_teste(
  x,
  y,
  method = c("pearson", "spearman", "kendall"),
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores numericos.

- method:

  String: `"pearson"`, `"spearman"`, `"kendall"`.

- conf:

  Nivel de confianca para o IC (apenas Pearson).

- digits:

  Inteiro.

## Value

tibble com `correlacao`, `p_valor`, `ic_inf`, `ic_sup`, `metodo`, `n`.

## Examples

``` r
rnp_correlacao_teste(mtcars$mpg, mtcars$wt)
#> # A tibble: 1 × 6
#>   correlacao p_valor ic_inf ic_sup metodo      n
#>        <dbl>   <dbl>  <dbl>  <dbl> <chr>   <int>
#> 1     -0.868       0 -0.934 -0.744 pearson    32
rnp_correlacao_teste(mtcars$mpg, mtcars$hp, method = "spearman")
#> # A tibble: 1 × 6
#>   correlacao p_valor ic_inf ic_sup metodo       n
#>        <dbl>   <dbl>  <dbl>  <dbl> <chr>    <int>
#> 1     -0.895       0     NA     NA spearman    32
```
