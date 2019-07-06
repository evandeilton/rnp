# Introdução

[![Travis build status](https://travis-ci.org/evandeilton/rnp.svg?branch=master)](https://travis-ci.org/evandeilton/rnp)

A linguagem R é uma das que mais cresce atualmente no mundo das linguagens de programação científicas. Esta linguagem tem sido uma das mais requisitadas por empresas, centros de pesquisa e por recrutadores das áreas de analytics e data science. Isso graças ao poder de processamento, aos recursos e à sua capacidade de expansão. R é uma linguagem científica, cross-plataforma, orientada a objetos, free e open source com uma comunidade de desenvolvedores e colaboradores global extremamente ativa.

## R NA PRÁTICA

O **R NA PRÁTICA** foi originalmente uma ideia que surgiu com o desejo de ajudar a disseminar  a linguagem R como uma ferramenta de apoio para o estudo de ciência de dados para pessoas de todos os níveis e lançamos o primeiro curso, o [R NA PRÁTICA: Data Wrangling com R para Ciência de Dados](https://www.udemy.com/r-na-pratica-ciencia-de-dados/). A ideia do **R NA PRATICA** cresceu e tornou-se um projeto de maior abrangência envolvendo os temas como a propría programação em R, Estatística e probabilidade e _Data Science_.

## O pacote `rnp`

Muitas funções estão sendo criadas à medida em que o projeto ganha volume e os novos módulos vão sendo escritos. Para condensar tudo em um local apropriado e de fácil consulta, surgiu a ideia de empacotar estes recursos e disponibilizá-los aqui no github. Nasce o pacote `rnp`.

Os materiais como funções, algumas bases e documentos dsiponibilizadas aqui são para ajudar a todos que desejam aprender, melhorar, atualizar seus conhecimentos em R, Estatística e Ciência de dados e assim se destacar na descoberta de conhecimentos através de dados. 

### Instalação do pacote

```{r, eval = FALSE}
# Se não tiver o devtools, instalar.
if(!require(devtools)){
   install.packages("devtools")
}
 
# Instalar o rnp
devtools::install_github("evandeilton/rnp")

# Carregar o pacote
require("rnp")
vignette("rnp")
```

### Bugs

Caso encontre algum bug, favor postar nas _issues_ do pacote [aqui](https://github.com/evandeilton/rnp/issues) para que possamos corrigir nas próximas atualizações do pacote.

