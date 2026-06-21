# Intervalo de confianca para proporcao

Metodo de Wald (default), Wilson, Agresti-Coull ou Clopper-Pearson
(exact).

## Usage

``` r
rnp_ic_proporcao(
  sucesso,
  n,
  conf = 0.95,
  method = c("wald", "wilson", "agresti", "clopper"),
  digits = 4L
)
```

## Arguments

- sucesso:

  Numerico. Numero de sucessos.

- n:

  Inteiro. Tamanho da amostra.

- conf:

  Nivel de confianca.

- method:

  String: `"wald"`, `"wilson"`, `"agresti"`, `"clopper"`.

- digits:

  Inteiro.

## Value

tibble com `proporcao`, `limite_inferior`, `limite_superior`, `metodo`,
`n`.

## Examples

``` r
rnp_ic_proporcao(45, 100)
#> # A tibble: 1 × 5
#>   proporcao limite_inferior limite_superior metodo     n
#>       <dbl>           <dbl>           <dbl> <chr>  <dbl>
#> 1      0.45           0.352           0.548 wald     100
rnp_ic_proporcao(45, 100, method = "wilson")
#> # A tibble: 1 × 5
#>   proporcao limite_inferior limite_superior metodo     n
#>       <dbl>           <dbl>           <dbl> <chr>  <dbl>
#> 1      0.45           0.356           0.548 wilson   100
```
