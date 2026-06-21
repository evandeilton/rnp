<!-- README.md is generated from README.Rmd. Please edit that file -->

# rnp <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/evandeilton/rnp)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Version](https://img.shields.io/badge/version-3.0.0-orange.svg)](https://github.com/evandeilton/rnp)
<!-- badges: end -->

> O **canivete suico do estatistico**: um pacote didatico e de producao que cobre
> as ementas dos tres primeiros anos de um bacharelado em Estatistica, com
> nucleos de alto desempenho em C++ (Rcpp/RcppArmadillo) e saidas tidy.

O `rnp` (do projeto **R NA PRATICA**) reune mais de **180 funcoes** de estatistica
descritiva, probabilidade, inferencia, regressao, analise multivariada, series
temporais, dados categoricos e pre-processamento. Todas com documentacao e
mensagens em **portugues**, saidas em `tibble` e graficos em `ggplot2`.

## Filosofia de projeto

- **Didatico e correto.** Cada funcao foi pensada para ensinar *e* para produzir.
  As vinhetas tratam a estatistica como um professor faria — conceito,
  interpretacao e armadilhas, nao apenas codigo.
- **Rapido.** Lacos pesados e algebra matricial rodam em **C++** (12 nucleos
  Rcpp/RcppArmadillo, verificados numericamente contra a referencia do R).
- **Enxuto.** Depende apenas de **R base + tidyverse + tidymodels + Rcpp**. Sem
  cipoal de dependencias.
- **Consistente.** Prefixo `rnp_`, saidas tidy, validacao de argumentos clara.

## Instalacao

``` r
# install.packages("devtools")
devtools::install_github("evandeilton/rnp")
```

## Exemplo rapido

``` r
library(rnp)

# Estatistica descritiva completa numa linha
rnp_descritiva(airquality$Ozone)

# Intervalo de confianca e teste de hipotese
rnp_ic_media(airquality$Temp)
rnp_teste_t(airquality$Temp, mu = 75)

# Regressao com diagnosticos
fit <- rnp_regressao(medv ~ rm + lstat, data = MASS::Boston)
fit$coeficientes

# PCA e visualizacao
p <- rnp_pca(iris[, 1:4])
rnp_biplot(p)
```

## Funcoes por ementa

<details>
<summary><b>1. Descritiva e Exploratoria</b></summary>

`rnp_descritiva()`, `rnp_descritiva_by()`, `rnp_momentos()`, `rnp_quantis()`,
`rnp_skewness()`, `rnp_kurtosis()`, `rnp_outliers()`, `rnp_medias()`,
`rnp_tabela_frequencia()`, `rnp_tabela_classes()`, `rnp_tabela_contingencia()`,
`rnp_estrutura()`, e graficos `rnp_grafico_histograma/_boxplot/_dispersao/_violino/_qq/_barras()`.
</details>

<details>
<summary><b>2. Probabilidade e Distribuicoes</b></summary>

`rnp_distribuicao_*()` (normal, exp, gama, beta, uniforme, t, qui-quadrado, F,
binomial, poisson, lognormal, weibull, multinomial...), `rnp_grafico_distribuicao()`,
`rnp_bayes()`, `rnp_distribuicao_conjunta()`, `rnp_esperanca_condicional()`,
`rnp_lei_grandes_numeros()`, `rnp_tcl_simulacao()`, `rnp_cadeia_markov()`,
`rnp_monte_carlo()`, `rnp_simula_inversao()`, `rnp_ajuste_distribuicao()`.
</details>

<details>
<summary><b>3. Inferencia</b></summary>

`rnp_emv()`, `rnp_metodo_momentos()`, `rnp_informacao_fisher()`,
`rnp_ic_media/_proporcao/_variancia/_diff_medias()`, `rnp_teste_t/_z_media/_z_proporcao()`,
`rnp_bootstrap()`, `rnp_ic_bootstrap()`, `rnp_jackknife()`, `rnp_teste_permutacao()`,
`rnp_teste_qui_quadrado/_ks/_normalidade/_grubbs/_runs/_sinais()`,
`rnp_poder_teste()`, `rnp_tamanho_amostra_teste()`, `rnp_bayes_conjugada()`,
`rnp_teste_razao_veross/_wald/_score()`.
</details>

<details>
<summary><b>4. Regressao e Modelagem</b></summary>

`rnp_regressao()`, `rnp_regressao_diagnosticos()`, `rnp_grafico_residuos()`,
`rnp_vif()`, `rnp_predicao()`, `rnp_regressao_polinomial/_ponderada/_stepwise()`,
`rnp_regressao_ridge/_lasso()`, `rnp_elastic_net()`, `rnp_regressao_robusta()`,
`rnp_regressao_poisson/_nao_linear/_multinomial()`, `rnp_box_cox()`,
`rnp_logistic()`, `rnp_matriz_confusao()`, `rnp_curva_roc()`.
</details>

<details>
<summary><b>5. Multivariada</b></summary>

`rnp_pca()`, `rnp_biplot()`, `rnp_matriz_correlacao()`, `rnp_grafico_correlograma()`,
`rnp_kmeans()`, `rnp_kmedoids()`, `rnp_cluster_hierarquico()`, `rnp_grafico_dendrograma()`,
`rnp_silhueta()`, `rnp_lda()`, `rnp_distancia()`, `rnp_mds()`, `rnp_hotelling()`,
`rnp_manova()`, `rnp_analise_fatorial()`, `rnp_correspondencia()`,
`rnp_correlacao_canonica()`, `rnp_normalidade_multivariada()`.
</details>

<details>
<summary><b>6. Series Temporais</b></summary>

`rnp_ts_decomposicao()`, `rnp_media_movel()`, `rnp_suavizacao_exponencial()`,
`rnp_ts_acf()`, `rnp_ts_pacf()`, `rnp_ts_diferenciacao()`, `rnp_ts_ljung_box()`,
`rnp_ts_holt_winters()`, `rnp_ts_periodograma()`, `rnp_grafico_serie/_acf()`.
</details>

<details>
<summary><b>7. Categoricos, Experimental e Pre-processamento</b></summary>

`rnp_teste_fisher()`, `rnp_odds_ratio()`, `rnp_risco_relativo()`, `rnp_kappa()`,
`rnp_kruskal()`, `rnp_mann_whitney()`, `rnp_wilcoxon()`, `rnp_anova()`,
`rnp_ancova()`, `rnp_dbc()`, `rnp_contrastes()`, `rnp_anova_medidas_repetidas()`,
`rnp_padroniza()`, `rnp_normaliza()`, `rnp_winsoriza()`, `rnp_imputa()`,
`rnp_discretiza()`, `rnp_dummy()`.
</details>

## Vinhetas (tutoriais)

O pacote acompanha seis tutoriais que percorrem a progressao 1o -> 3o ano,
com dados reais e enfase conceitual:

1. **Estatistica Descritiva e Analise Exploratoria**
2. **Probabilidade, Distribuicoes e os Teoremas Fundamentais** (Bayes, LGN, TCL)
3. **Inferencia Estatistica** (p-valor, IC, bootstrap, poder)
4. **Regressao Linear e Modelagem** (projecao, pressupostos, regularizacao)
5. **Analise Multivariada** (PCA, cluster, LDA, Hotelling)
6. **Dados Categoricos e Metodos Nao-Parametricos**

``` r
browseVignettes("rnp")
```

## Desempenho (C++)

Doze nucleos compilados via Rcpp/RcppArmadillo aceleram as operacoes mais
pesadas, todos validados contra a referencia do R (tolerancia ~1e-8): distancias,
covariancia/correlacao, OLS por QR, momentos, cadeias de Markov, reamostragem
(bootstrap/jackknife/permutacao), regularizacao (ridge/elastic net), regressao
robusta (IRLS), silhueta, series temporais (ACF/PACF) e imputacao kNN.

## Sobre o projeto R NA PRATICA

O **R NA PRATICA** nasceu para disseminar o R como ferramenta de ensino de
ciencia de dados e estatistica. O pacote `rnp` condensa esses recursos num lugar
unico, de facil consulta e instalacao.

## Licenca

GPL-3. Contribuicoes e relatos de bugs sao bem-vindos em
[issues](https://github.com/evandeilton/rnp/issues).
