# Matriz de confusao

Matriz de confusao

## Usage

``` r
rnp_matriz_confusao(observado, predito, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor de classes observadas (factor/character).

- predito:

  Vetor de classes preditas (mesmos niveis).

- positivo:

  String. Nivel da classe positiva (default primeiro nivel).

- digits:

  Inteiro.

## Value

lista:

- `matriz`: tabela.

- `metricas`: tibble com sensibilidade, especificidade, precisao, f1,
  acuracia, prevalence, npv.

## Examples

``` r
obs <- sample(c("Sim", "Nao"), 100, TRUE)
pred <- sample(c("Sim", "Nao"), 100, TRUE)
rnp_matriz_confusao(obs, pred, positivo = "Sim")
#> 
#> ── Matriz de confusao ──────────────────────────────────────────────────────────
#> Matriz: 19, 33, 30, and 18
#> 
#> ── Metricas 
#> # A tibble: 1 × 8
#>   sensibilidade especificidade precisao   npv    f1 acuracia prevalencia     n
#>           <dbl>          <dbl>    <dbl> <dbl> <dbl>    <dbl>       <dbl> <int>
#> 1         0.353          0.388    0.375 0.365 0.364     0.37        0.51   100
```
