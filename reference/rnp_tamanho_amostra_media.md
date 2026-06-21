# Tamanho de amostra para media

Calcula n necessario para estimar media populacional com margem de erro
E.

## Usage

``` r
rnp_tamanho_amostra_media(sigma, E, conf = 0.95, N = Inf)
```

## Arguments

- sigma:

  Desvio-padrao populacional (estimativa).

- E:

  Margem de erro desejada.

- conf:

  Nivel de confianca.

- N:

  Tamanho da populacao (default Inf = populacao infinita).

## Value

tibble com `n_infinita`, `n_finita`, `E`, `conf`.

## Examples

``` r
rnp_tamanho_amostra_media(sigma = 10, E = 2, conf = 0.95)
#> # A tibble: 1 × 4
#>   n_infinita n_finita     E  conf
#>        <dbl>    <dbl> <dbl> <dbl>
#> 1         97       97     2  0.95
```
