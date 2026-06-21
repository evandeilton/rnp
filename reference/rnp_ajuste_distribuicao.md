# Ajuste de distribuicao por maxima verossimilhanca

Ajusta os parametros de uma distribuicao a um vetor de dados via maxima
verossimilhanca (otimizacao numerica) e reporta log-verossimilhanca,
AIC, BIC e a estatistica de Kolmogorov-Smirnov.

## Usage

``` r
rnp_ajuste_distribuicao(
  x,
  dist = c("norm", "exp", "gamma", "lnorm", "weibull", "pois"),
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico de dados.

- dist:

  String: `"norm"`, `"exp"`, `"gamma"`, `"lnorm"`, `"weibull"`,
  `"pois"`.

- digits:

  Inteiro. Casas decimais.

## Value

Uma lista com `parametros` (tibble) e `qualidade` (tibble com
`log_veross`, `aic`, `bic`, `ks_estatistica`, `n`).

## See also

Other distribuicoes:
[`rnp_distribuicao_beta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_beta.md),
[`rnp_distribuicao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_exponencial.md),
[`rnp_distribuicao_f()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_f.md),
[`rnp_distribuicao_gama()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_gama.md),
[`rnp_distribuicao_lognormal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_lognormal.md),
[`rnp_distribuicao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_multinomial.md),
[`rnp_distribuicao_normal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_normal.md),
[`rnp_distribuicao_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_qui_quadrado.md),
[`rnp_distribuicao_t()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_t.md),
[`rnp_distribuicao_uniforme()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_uniforme.md),
[`rnp_distribuicao_weibull()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_weibull.md),
[`rnp_grafico_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_distribuicao.md)

## Examples

``` r
set.seed(1)
rnp_ajuste_distribuicao(rexp(200, 0.5), "exp")
#> $parametros
#> # A tibble: 1 × 2
#>   parametro estimativa
#>   <chr>          <dbl>
#> 1 rate             0.5
#> 
#> $qualidade
#> # A tibble: 1 × 5
#>   log_veross   aic   bic ks_estatistica     n
#>        <dbl> <dbl> <dbl>          <dbl> <int>
#> 1      -339.  679.  683.         0.0695   200
#> 
rnp_ajuste_distribuicao(rnorm(200, 10, 2), "norm")
#> $parametros
#> # A tibble: 2 × 2
#>   parametro estimativa
#>   <chr>          <dbl>
#> 1 mean           10.0 
#> 2 sd              1.99
#> 
#> $qualidade
#> # A tibble: 1 × 5
#>   log_veross   aic   bic ks_estatistica     n
#>        <dbl> <dbl> <dbl>          <dbl> <int>
#> 1      -421.  846.  852.         0.0532   200
#> 
```
