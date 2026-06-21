# Deteccao de outliers

Identifica outliers por metodo configuravel. Retorna posicoes e valores.

## Usage

``` r
rnp_outliers(
  x,
  method = c("iqr", "zscore", "modzscore", "chebyshev"),
  k = NULL,
  na.rm = TRUE
)
```

## Arguments

- x:

  Vetor numerico.

- method:

  String: `"iqr"` (Tukey), `"zscore"`, `"modzscore"` (Iglewicz-Cohn),
  `"chebyshev"`.

- k:

  Escalar positivo. Limiar: `k` IQR para metodo IQR; `k` desvios para
  zscore/modified zscore; `k` desvios garantidos (prob. min. 1 - 1/k^2)
  para Chebyshev.

- na.rm:

  Logico.

## Value

tibble com colunas `indice` e `valor`.

## Examples

``` r
x <- c(rnorm(100), 50, -30)
rnp_outliers(x, method = "iqr")
#> # A tibble: 2 × 2
#>   indice valor
#>    <int> <dbl>
#> 1    101    50
#> 2    102   -30
rnp_outliers(x, method = "modzscore", k = 3.5)
#> # A tibble: 2 × 2
#>   indice valor
#>    <int> <dbl>
#> 1    101    50
#> 2    102   -30
```
