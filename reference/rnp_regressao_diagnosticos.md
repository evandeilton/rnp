# Diagnosticos de regressao linear

Retorna diagnosticos clássicos: hat, residuos studentizados, Cook's D,
DFBETAS, DFFITS, leverage, e testes de normalidade e homocedasticidade.

## Usage

``` r
rnp_regressao_diagnosticos(modelo, data = NULL, digits = 4L)
```

## Arguments

- modelo:

  Objeto `lm` ou formula.

- data:

  data.frame (se modelo for formula).

- digits:

  Inteiro.

## Value

lista:

- `pontos`: tibble com indice, residuo, studentizado, hat, cooks_d,
  dffits.

- `testes`: tibble com Shapiro-Wilk residuos, Breusch-Pagan
  (homocedasticidade), Durbin-Watson (autocorrelacao).

## Examples

``` r
fit <- lm(mpg ~ wt + hp, mtcars)
rnp_regressao_diagnosticos(fit)
#> $pontos
#> # A tibble: 32 × 12
#>    indice residuo studentizado    hat cooks_d  dffits dfbeta_1 dfbeta_2 dfbeta_3
#>     <int>   <dbl>        <dbl>  <dbl>   <dbl>   <dbl>    <dbl>    <dbl>    <dbl>
#>  1      1  -2.57       -1.02   0.0443  0.0159 -0.218   -0.161    0.0639   0.033 
#>  2      2  -1.58       -0.617  0.0405  0.0055 -0.127   -0.0693  -0.0004   0.0458
#>  3      3  -2.48       -0.984  0.0602  0.0207 -0.249   -0.211    0.0972   0.0434
#>  4      4   0.135       0.0524 0.0475  0       0.0117   0.0027   0.0045  -0.0068
#>  5      5   0.373       0.144  0.0369  0.0003  0.0282   0.0018  -0.0016   0.0092
#>  6      6  -2.37       -0.946  0.0672  0.0216 -0.254   -0.006   -0.152    0.180 
#>  7      7  -1.30       -0.526  0.117   0.0126 -0.192    0.0047   0.0781  -0.16  
#>  8      8   1.51        0.614  0.116   0.0168  0.222    0.0343   0.122   -0.190 
#>  9      9   0.806       0.316  0.06    0.0022  0.0798   0.0198   0.0333  -0.0551
#> 10     10  -0.780      -0.303  0.0469  0.0016 -0.0672  -0.0032  -0.0337   0.0367
#> # ℹ 22 more rows
#> # ℹ 3 more variables: leverage_alta <lgl>, cooks_alto <lgl>, outlier_t <lgl>
#> 
#> $testes
#> # A tibble: 3 × 4
#>   teste                   estatistica p_valor interpretacao                   
#>   <chr>                         <dbl>   <dbl> <chr>                           
#> 1 shapiro-wilk (residuos)       0.928  0.0343 Rejeita normalidade             
#> 2 breusch-pagan                10.2    0.006  Heterocedasticidade             
#> 3 durbin-watson                 1.36  NA      Possivel autocorrelacao positiva
#> 
```
