# Teste de Tukey HSD

Teste de Tukey HSD

## Usage

``` r
rnp_tukey_hsd(modelo, data = NULL, conf = 0.95, digits = 4L)
```

## Arguments

- modelo:

  Objeto `aov` ou formula.

- data:

  data.frame (se modelo for formula).

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble com comparacoes.

## Examples

``` r
fit <- aov(mpg ~ factor(cyl), mtcars)
rnp_tukey_hsd(fit)
#> # A tibble: 3 × 6
#>   termo       comparacao   diff limite_inferior limite_superior p_ajustado
#>   <chr>       <chr>       <dbl>           <dbl>           <dbl>      <dbl>
#> 1 factor(cyl) 6-4         -6.92          -10.8           -3.07      0.0003
#> 2 factor(cyl) 8-4        -11.6           -14.8           -8.36      0     
#> 3 factor(cyl) 8-6         -4.64           -8.33          -0.958     0.0112
```
