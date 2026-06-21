# Combinatorial utilities

Calcula numero de combinacoes, arranjos, permutacoes com e sem repeticao
usando aritmetica de lgamma para evitar overflow.

## Usage

``` r
rnp_combinacao(n, k, repeticao = FALSE)
```

## Arguments

- n:

  Inteiro. Total de elementos.

- k:

  Inteiro. Tamanho do subconjunto.

- repeticao:

  Logico. Com repeticao (TRUE) ou sem (FALSE).

## Value

Escalar numerico (double para suportar grandes valores).

## Examples

``` r
rnp_combinacao(10, 3)
#> [1] 120
rnp_combinacao(10, 3, repeticao = TRUE)
#> [1] 220
```
