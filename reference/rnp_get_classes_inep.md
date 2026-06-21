# Obtem classes das variaveis do dicionario INEP

Extrai categorias e descricoes do dicionario de dados do INEP.

## Usage

``` r
rnp_get_classes_inep(caminho, aba = 1, pula_linha = 1, retorna_lista = FALSE)
```

## Arguments

- caminho:

  Caminho do arquivo Excel.

- aba:

  Nome ou numero da aba.

- pula_linha:

  Linhas a pular.

- retorna_lista:

  Logico.

## Value

data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
rnp_get_classes_inep("Dicionario_de_Dados.xlsx", aba = "DM_IES")
} # }
```
