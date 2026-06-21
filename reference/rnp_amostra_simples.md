# Amostragem aleatoria simples

Amostragem aleatoria simples

## Usage

``` r
rnp_amostra_simples(base, n, seed = NULL)
```

## Arguments

- base:

  data.frame ou vetor.

- n:

  Inteiro. Tamanho da amostra.

- seed:

  Inteiro. Semente aleatoria.

## Value

Amostra (mesmo tipo de base).

## Examples

``` r
rnp_amostra_simples(mtcars, 10)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Lotus Europa      30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ferrari Dino      19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Chrysler Imperial 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Ford Pantera L    15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Fiat X1-9         27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
```
