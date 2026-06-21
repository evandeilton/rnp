# Regressao logistica binaria

Ajusta GLM binomial com saida tidy.

## Usage

``` r
rnp_logistic(formula, data, conf = 0.95, digits = 4L)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

lista:

- `coeficientes`: tibble.

- `modelo`: tibble com aic, deviance_residual, nobs, null_deviance,
  residual_deviance.

## Examples

``` r
rnp_logistic(am ~ mpg + wt, mtcars)
#> $coeficientes
#> # A tibble: 3 × 8
#>   termo  estimativa erro_padrao estatistica_z p_valor odds_ratio ic_inf   ic_sup
#>   <chr>       <dbl>       <dbl>         <dbl>   <dbl>      <dbl>  <dbl>    <dbl>
#> 1 (Inte…     25.9        12.2            2.12  0.0338   1.75e+11  7.30  4.18e+21
#> 2 mpg        -0.324       0.240         -1.35  0.176    7.23e- 1  0.452 1.16e+ 0
#> 3 wt         -6.42        2.55          -2.52  0.0118   1.6 e- 3  0     2.40e- 1
#> 
#> $modelo
#> # A tibble: 1 × 5
#>     aic null_deviance residual_deviance  nobs df_residuos
#>   <dbl>         <dbl>             <dbl> <int>       <int>
#> 1  23.2          43.2              17.2    32          29
#> 
```
