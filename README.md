# Introdução

A linguagem R é uma das que mais cresce atualmente no mundo das linguagens de programação científicas. Esta linguagem tem sido uma das mais requisitadas por empresas, centros de pesquisa e por recrutadores das áreas de analytics e data science. Isso graças ao poder de processamento, aos recursos e à sua capacidade de expansão. R é uma linguagem científica, cross-plataforma, orientada a objetos, free e open source com uma comunidade de desenvolvedores e colaboradores global extremamente ativa.

## R NA PRÁTICA

O Curso `R NA PRÁTICA: Data Wrangling com R para Ciência de Dados` foi uma ideia que surgiu com o desejo de ajudar a disseminar  a linguagem R como uma ferramenta de apoio para o estudo de ciênci de dados. O conceito Data Wrangling é um tanto genérico em nosso português brasileiro, mas poderia ser traduzido como disputa/briga/luta de dados. Esta disputa está intimamente ligada ao processo de transformação de dados e isso inclui: obtenção, transformação, limpeza, agregação, visualização e criação de bases limpas para fins de Analytics na Ciência de Dados.

## O pacote `rnp`

A ideia do R NA PRATICA cresceu e tornou-se um projeto de maior abrangência com o bjetivo de ensinar estatística de _Data Science_ com auxílio do _R_. Diante disso, muitas funções estão sendo criadas à medida em que o projeto ganha volume. Para condensar tudo em um local apropriado e de fácil consulta, surgiu a ideia de empacotar estes recursos e disponibilizá-los aqui no github.

Os materiais como funções, alguns documentos e as funções dsiponibilizadas aqui são para ajudar a todos que desejam aprender, melhorar, atualizar seus conhecimentos em R, estatísticas e ciência de dados e assim se destacar na descoberta de conhecimentos através de dados. 

### Instalação do pacote

```{r}
  # Se não tiver o devtools, instala.
  if(!require(devtools)){
    install.packages("devtools")
  }
  
  # Instala o rnp
  devtools::install_github("evandeilton/rnp")
  
  # carrega o pacote e a vinheta
  vignette("rnp", package = "rnp")
```

## Programa integrado

O R NA PRATICA agora engloba cinco módulos:
  
  * Módulo - I   (Data Wrangling com R para Ciência de Dados)
  * Módulo - II  (Estatística descomplicada com R)
  * Módulo - III (Introdução à probabilidade com R)
  * Módulo - VI  (Testes de hipóteses com R)
  * Módulo - V   (Modelagem estatística com R)

### Módulo I - R NA PRÁTICA: Data Wrangling com R para Ciência de Dados

Neste módulo o aluno aprenderá:
<https://www.udemy.com/r-na-pratica-ciencia-de-dados/>

  * Operadores, sequências, funções, loops, família apply e gráficos dos sistemas base, lattice e ggplot2;   
  * Agregação, transformação, estatística descritiva de dados e tabelas de frequências;   
  * Dominará a criação de códigos otimizados em R com o moderno operador pipe "%>%" e os pacotes do tidyverse;   
  * Terá domínio sobre o processo de obtenção de conhecimento a partir de dados passando pelo ciclo de análise de situação problema, obtenção de dados, preparação, análise, visualização e comunicação de resultados com R Markdown;   
  * Será capaz de resolver cases de estudo e problemas com dados e situações reais.
  * Terá um vasto material de consulta com amostras de códigos e bases de dados de exemplos de todas as video aulas para reforçar seus conhecimentos e aplicações no dia-a-dia.

### Módulo - II (R NA PRÁTICA: Estatística descomplicada com R )

Em desenvolvimento (0%================70%=======100%)

### Módulo - III (Introdução à probabilidade com R)

Em desenvolvimento (0%====10%===================100%)

### Módulo - VI  (Testes de hipóteses com R)

Em desenvolvimento (0%==5%======================100%)

### Módulo - V   (Modelagem estatística com R)

Em desenvolvimento (0%=1%=======================100%)

