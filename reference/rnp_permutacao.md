# Numero de permutacoes

Numero de permutacoes

## Usage

``` r
rnp_permutacao(n, elementos = NULL)
```

## Arguments

- n:

  Inteiro. Total de elementos.

- elementos:

  Vetor opcional de frequencias (permutacao com repeticao).

## Value

Escalar numerico.

## Examples

``` r
rnp_permutacao(5)
#> [1] 120
rnp_permutacao(7, elementos = c(2, 2, 3))
#> [1] 210
```
