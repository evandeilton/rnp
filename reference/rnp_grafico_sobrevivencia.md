# Curva de Kaplan-Meier (grafico)

Curva de Kaplan-Meier (grafico)

## Usage

``` r
rnp_grafico_sobrevivencia(km, intervalo = TRUE, titulo = NULL)
```

## Arguments

- km:

  Saida de
  [`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md)
  (ou objeto `survfit`).

- intervalo:

  Logico. Exibe a faixa de confianca.

- titulo:

  Titulo opcional.

## Value

Objeto `ggplot`.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
km <- rnp_kaplan_meier(survival::lung$time, survival::lung$status == 2,
                       grupo = survival::lung$sex)
rnp_grafico_sobrevivencia(km)
```
