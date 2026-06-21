# Amostragem sistematica

Amostragem sistematica

## Usage

``` r
rnp_amostra_sistematica(base, n, seed = NULL)
```

## Arguments

- base:

  data.frame ou vetor.

- n:

  Inteiro. Tamanho da amostra.

- seed:

  Inteiro. Semente aleatoria (para o inicio).

## Value

Amostra.

## Examples

``` r
rnp_amostra_sistematica(mtcars, 10)
#>                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710         22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Valiant            18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Merc 230           22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 450SE         16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Fiat 128           32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corona      21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Camaro Z28         13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Porsche 914-2      26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Ferrari Dino       19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
```
