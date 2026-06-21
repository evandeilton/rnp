# Tamanho de amostra para proporcao

Tamanho de amostra para proporcao

## Usage

``` r
rnp_tamanho_amostra_proporcao(p = 0.5, E, conf = 0.95, N = Inf)
```

## Arguments

- p:

  Proporcao esperada (default 0.5 = maximo).

- E:

  Margem de erro.

- conf:

  Nivel de confianca.

- N:

  Tamanho da populacao (default Inf).

## Value

tibble.

## Examples

``` r
rnp_tamanho_amostra_proporcao(E = 0.05, conf = 0.95)
#> # A tibble: 1 × 4
#>   n_infinita n_finita     E  conf
#>        <dbl>    <dbl> <dbl> <dbl>
#> 1        385      385  0.05  0.95
```
