# Probabilidade condicional

Calcula P(A\|B) = P(A & B) / P(B).

## Usage

``` r
rnp_probabilidade_condicional(pa_e_b, pb)
```

## Arguments

- pa_e_b:

  Numerico em \[0, 1\]. Probabilidade da interseccao.

- pb:

  Numerico em (0, 1\]. Probabilidade do evento condicionante.

## Value

Escalar numerico.

## Examples

``` r
rnp_probabilidade_condicional(0.12, 0.30)
#> [1] 0.4
```
