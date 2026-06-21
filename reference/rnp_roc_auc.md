# AUC (somente)

Atalho para `rnp_curva_roc(...)$auc`.

## Usage

``` r
rnp_roc_auc(observado, escores, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario (0/1 ou factor 2 niveis).

- escores:

  Vetor numerico de probabilidades preditas.

- positivo:

  Nivel da classe positiva (default primeiro nivel).

- digits:

  Inteiro.

## Value

Escalar numerico.
