# Estimador de Kaplan-Meier

Estima a funcao de sobrevivencia \$S(t) = P(T \> t)\$ pelo metodo de
Kaplan-Meier, com intervalo de confianca, para uma ou varias curvas.

## Usage

``` r
rnp_kaplan_meier(tempo, evento, grupo = NULL, conf = 0.95, digits = 4L)
```

## Arguments

- tempo:

  Vetor de tempos de seguimento.

- evento:

  Vetor 0/1 (1 = evento ocorreu; 0 = censura).

- grupo:

  Fator opcional para curvas estratificadas.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

Uma lista com `tabela` (passos da curva), `mediana` (sobrevida mediana
por grupo) e `modelo` (objeto `survfit`, para
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md)).

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
km <- rnp_kaplan_meier(survival::lung$time, survival::lung$status == 2,
                       grupo = survival::lung$sex)
km$mediana
#> # A tibble: 2 × 6
#>   grupo     n eventos mediana ic_inf ic_sup
#>   <chr> <int>   <int>   <dbl>  <dbl>  <dbl>
#> 1 1       138     112     270    212    310
#> 2 2        90      53     426    348    550
```
