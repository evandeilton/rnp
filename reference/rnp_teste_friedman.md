# Teste de Friedman (blocos)

Teste de Friedman (blocos)

## Usage

``` r
rnp_teste_friedman(formula, data = NULL, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ grupo | bloco`.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
# delineamento em blocos completo (uma observacao por trat x bloco)
df <- data.frame(
  resp  = c(1, 2, 3, 2, 3, 1, 3, 1, 2, 1, 3, 2),
  trat  = factor(rep(1:3, times = 4)),
  bloco = factor(rep(1:4, each = 3))
)
rnp_teste_friedman(resp ~ trat | bloco, df)
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo  
#>         <dbl> <dbl>   <dbl> <chr>   
#> 1         0.5     2   0.779 friedman
```
