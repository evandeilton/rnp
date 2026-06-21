# Aplica classes do dicionario INEP na base

Junta descricoes das categorias com os codigos numericos.

## Usage

``` r
rnp_aplica_classes(base, classes)
```

## Arguments

- base:

  Base de dados INEP.

- classes:

  Base de classes (de
  [`rnp_get_classes_inep()`](https://evandeilton.github.io/rnp/reference/rnp_get_classes_inep.md)).

## Value

data.frame com colunas \_DESC adicionadas.

## Examples

``` r
if (FALSE) { # \dontrun{
classes <- rnp_get_classes_inep("Dicionario.xlsx", aba = "DM_IES")
base <- rnp_read("DM_IES.csv", sep = "|")
base_desc <- rnp_aplica_classes(base, classes)
} # }
```
