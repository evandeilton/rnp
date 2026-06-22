# Regressao linear (simples ou multipla) com saida tidy

Ajusta modelo linear e retorna coeficientes, IC, R2, R2-ajustado,
estatistica-F global e residuos.

## Usage

``` r
rnp_regressao(formula, data, conf = 0.95, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ x1 + x2 + ...`.

- data:

  data.frame.

- conf:

  Nivel de confianca para IC dos coeficientes.

- digits:

  Inteiro.

## Value

lista com:

- `coeficientes`: tibble com termo, estimativa, erro_padrao,
  estatistica_t, p_valor, ic_inf, ic_sup.

- `modelo`: tibble com r2, r2_ajustado, f_statistic, f_pvalor, sigma,
  gl_residuos, nobs.

## Examples

``` r
rnp_regressao(mpg ~ wt + hp, mtcars)
#> 
#> ── Regressao linear ────────────────────────────────────────────────────────────
#> 
#> ── Coeficientes 
#> # A tibble: 3 × 7
#>   termo       estimativa erro_padrao estatistica_t p_valor  ic_inf  ic_sup
#>   <chr>            <dbl>       <dbl>         <dbl>   <dbl>   <dbl>   <dbl>
#> 1 (Intercept)    37.2          1.60          23.3   0      34.0    40.5   
#> 2 wt             -3.88         0.633         -6.13  0      -5.17   -2.58  
#> 3 hp             -0.0318       0.009         -3.52  0.0015 -0.0502 -0.0133
#> 
#> ── Modelo 
#> # A tibble: 1 × 7
#>      r2 r2_ajustado f_statistic f_pvalor sigma gl_residuos  nobs
#>   <dbl>       <dbl>       <dbl>    <dbl> <dbl>       <int> <int>
#> 1 0.827       0.815        69.2        0  2.59          29    32
```
