# Amostragem estratificada

Amostragem estratificada

## Usage

``` r
rnp_amostra_estratificada(base, estrato, n, seed = NULL)
```

## Arguments

- base:

  data.frame.

- estrato:

  Nome da coluna (string) com os estratos.

- n:

  Vetor nomeado com tamanho por estrato, ou escalar para proporcional.

- seed:

  Inteiro.

## Value

data.frame com amostra estratificada.

## Examples

``` r
rnp_amostra_estratificada(mtcars, "cyl", n = c("4" = 3, "6" = 2, "8" = 3))
#>                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> 4.Merc 230         22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> 4.Merc 240D        24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> 4.Fiat X1-9        27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> 6.Merc 280C        17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> 6.Ferrari Dino     19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> 8.Dodge Challenger 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> 8.Ford Pantera L   15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> 8.Pontiac Firebird 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
```
