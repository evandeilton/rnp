# Paletas de cores do projeto R NA PRATICA

Paletas qualitativa, sequencial e divergente.

## Usage

``` r
rnp_paleta_rnp(nome = c("rnp_qual", "rnp_seq", "rnp_div"), n = NULL)
```

## Arguments

- nome:

  String: `"rnp_qual"`, `"rnp_seq"`, `"rnp_div"`.

- n:

  Inteiro. Numero de cores.

## Value

Vetor de cores (hex).

## Examples

``` r
rnp_paleta_rnp("rnp_qual", 5)
#> [1] "#2E86AB" "#A23B72" "#F18F01" "#C73E1D" "#3B1F2B"
rnp_paleta_rnp("rnp_seq", 10)
#> Warning: n > {length(p)}. Interpolando cores.
#>  [1] "#F7FBFF" "#E0ECF7" "#CBDEF0" "#ABCFE5" "#81BADA" "#58A1CE" "#3787C0"
#>  [8] "#1B69AF" "#084D96" "#08306B"
```
