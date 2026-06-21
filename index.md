# rnp ![](reference/figures/logo.png)

> O **canivete suico do estatistico**: um pacote didatico e de producao
> que cobre as ementas dos tres primeiros anos de um bacharelado em
> Estatistica, com nucleos de alto desempenho em C++
> (Rcpp/RcppArmadillo) e saidas tidy.

O `rnp` (do projeto **R NA PRATICA**) reune mais de **180 funcoes** de
estatistica descritiva, probabilidade, inferencia, regressao, analise
multivariada, series temporais, dados categoricos e pre-processamento.
Todas com documentacao e mensagens em **portugues**, saidas em `tibble`
e graficos em `ggplot2`.

## Filosofia de projeto

- **Didatico e correto.** Cada funcao foi pensada para ensinar *e* para
  produzir. As vinhetas tratam a estatistica como um professor faria —
  conceito, interpretacao e armadilhas, nao apenas codigo.
- **Rapido.** Lacos pesados e algebra matricial rodam em **C++** (12
  nucleos Rcpp/RcppArmadillo, verificados numericamente contra a
  referencia do R).
- **Enxuto.** Depende apenas de **R base + tidyverse + tidymodels +
  Rcpp**. Sem cipoal de dependencias.
- **Consistente.** Prefixo `rnp_`, saidas tidy, validacao de argumentos
  clara.

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

**1. Descritiva e Exploratoria**

[`rnp_descritiva()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva.md),
[`rnp_descritiva_by()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva_by.md),
[`rnp_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_momentos.md),
[`rnp_quantis()`](https://evandeilton.github.io/rnp/reference/rnp_quantis.md),
[`rnp_skewness()`](https://evandeilton.github.io/rnp/reference/rnp_skewness.md),
[`rnp_kurtosis()`](https://evandeilton.github.io/rnp/reference/rnp_kurtosis.md),
[`rnp_outliers()`](https://evandeilton.github.io/rnp/reference/rnp_outliers.md),
[`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md),
[`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md),
[`rnp_tabela_classes()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_classes.md),
[`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md),
[`rnp_estrutura()`](https://evandeilton.github.io/rnp/reference/rnp_estrutura.md),
e graficos
`rnp_grafico_histograma/_boxplot/_dispersao/_violino/_qq/_barras()`.

**2. Probabilidade e Distribuicoes**

`rnp_distribuicao_*()` (normal, exp, gama, beta, uniforme, t,
qui-quadrado, F, binomial, poisson, lognormal, weibull, multinomial…),
[`rnp_grafico_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_distribuicao.md),
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_ajuste_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_ajuste_distribuicao.md).

**3. Inferencia**

[`rnp_emv()`](https://evandeilton.github.io/rnp/reference/rnp_emv.md),
[`rnp_metodo_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_metodo_momentos.md),
[`rnp_informacao_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_informacao_fisher.md),
`rnp_ic_media/_proporcao/_variancia/_diff_medias()`,
`rnp_teste_t/_z_media/_z_proporcao()`,
[`rnp_bootstrap()`](https://evandeilton.github.io/rnp/reference/rnp_bootstrap.md),
[`rnp_ic_bootstrap()`](https://evandeilton.github.io/rnp/reference/rnp_ic_bootstrap.md),
[`rnp_jackknife()`](https://evandeilton.github.io/rnp/reference/rnp_jackknife.md),
[`rnp_teste_permutacao()`](https://evandeilton.github.io/rnp/reference/rnp_teste_permutacao.md),
`rnp_teste_qui_quadrado/_ks/_normalidade/_grubbs/_runs/_sinais()`,
[`rnp_poder_teste()`](https://evandeilton.github.io/rnp/reference/rnp_poder_teste.md),
[`rnp_tamanho_amostra_teste()`](https://evandeilton.github.io/rnp/reference/rnp_tamanho_amostra_teste.md),
[`rnp_bayes_conjugada()`](https://evandeilton.github.io/rnp/reference/rnp_bayes_conjugada.md),
`rnp_teste_razao_veross/_wald/_score()`.

**4. Regressao e Modelagem**

[`rnp_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_regressao.md),
[`rnp_regressao_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_diagnosticos.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
`rnp_regressao_polinomial/_ponderada/_stepwise()`,
`rnp_regressao_ridge/_lasso()`,
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
`rnp_regressao_poisson/_nao_linear/_multinomial()`,
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_logistic()`](https://evandeilton.github.io/rnp/reference/rnp_logistic.md),
[`rnp_matriz_confusao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_confusao.md),
[`rnp_curva_roc()`](https://evandeilton.github.io/rnp/reference/rnp_curva_roc.md).

**5. Multivariada**

[`rnp_pca()`](https://evandeilton.github.io/rnp/reference/rnp_pca.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_grafico_correlograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_correlograma.md),
[`rnp_kmeans()`](https://evandeilton.github.io/rnp/reference/rnp_kmeans.md),
[`rnp_kmedoids()`](https://evandeilton.github.io/rnp/reference/rnp_kmedoids.md),
[`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md),
[`rnp_grafico_dendrograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_dendrograma.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md),
[`rnp_lda()`](https://evandeilton.github.io/rnp/reference/rnp_lda.md),
[`rnp_distancia()`](https://evandeilton.github.io/rnp/reference/rnp_distancia.md),
[`rnp_mds()`](https://evandeilton.github.io/rnp/reference/rnp_mds.md),
[`rnp_hotelling()`](https://evandeilton.github.io/rnp/reference/rnp_hotelling.md),
[`rnp_manova()`](https://evandeilton.github.io/rnp/reference/rnp_manova.md),
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_correspondencia()`](https://evandeilton.github.io/rnp/reference/rnp_correspondencia.md),
[`rnp_correlacao_canonica()`](https://evandeilton.github.io/rnp/reference/rnp_correlacao_canonica.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md).

**6. Series Temporais**

[`rnp_ts_decomposicao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_decomposicao.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_suavizacao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_suavizacao_exponencial.md),
[`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
[`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
[`rnp_ts_diferenciacao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_diferenciacao.md),
[`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
[`rnp_ts_holt_winters()`](https://evandeilton.github.io/rnp/reference/rnp_ts_holt_winters.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md),
`rnp_grafico_serie/_acf()`.

**7. Categoricos, Experimental e Pre-processamento**

[`rnp_teste_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_teste_fisher.md),
[`rnp_odds_ratio()`](https://evandeilton.github.io/rnp/reference/rnp_odds_ratio.md),
[`rnp_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_risco_relativo.md),
[`rnp_kappa()`](https://evandeilton.github.io/rnp/reference/rnp_kappa.md),
[`rnp_kruskal()`](https://evandeilton.github.io/rnp/reference/rnp_kruskal.md),
[`rnp_mann_whitney()`](https://evandeilton.github.io/rnp/reference/rnp_mann_whitney.md),
[`rnp_wilcoxon()`](https://evandeilton.github.io/rnp/reference/rnp_wilcoxon.md),
[`rnp_anova()`](https://evandeilton.github.io/rnp/reference/rnp_anova.md),
[`rnp_ancova()`](https://evandeilton.github.io/rnp/reference/rnp_ancova.md),
[`rnp_dbc()`](https://evandeilton.github.io/rnp/reference/rnp_dbc.md),
[`rnp_contrastes()`](https://evandeilton.github.io/rnp/reference/rnp_contrastes.md),
[`rnp_anova_medidas_repetidas()`](https://evandeilton.github.io/rnp/reference/rnp_anova_medidas_repetidas.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md).

## Vinhetas (tutoriais)

O pacote acompanha seis tutoriais que percorrem a progressao 1o -\> 3o
ano, com dados reais e enfase conceitual:

1.  **Estatistica Descritiva e Analise Exploratoria**
2.  **Probabilidade, Distribuicoes e os Teoremas Fundamentais** (Bayes,
    LGN, TCL)
3.  **Inferencia Estatistica** (p-valor, IC, bootstrap, poder)
4.  **Regressao Linear e Modelagem** (projecao, pressupostos,
    regularizacao)
5.  **Analise Multivariada** (PCA, cluster, LDA, Hotelling)
6.  **Dados Categoricos e Metodos Nao-Parametricos**

``` r

browseVignettes("rnp")
```

## Desempenho (C++)

Doze nucleos compilados via Rcpp/RcppArmadillo aceleram as operacoes
mais pesadas, todos validados contra a referencia do R (tolerancia
~1e-8): distancias, covariancia/correlacao, OLS por QR, momentos,
cadeias de Markov, reamostragem (bootstrap/jackknife/permutacao),
regularizacao (ridge/elastic net), regressao robusta (IRLS), silhueta,
series temporais (ACF/PACF) e imputacao kNN.

## Sobre o projeto R NA PRATICA

O **R NA PRATICA** nasceu para disseminar o R como ferramenta de ensino
de ciencia de dados e estatistica. O pacote `rnp` condensa esses
recursos num lugar unico, de facil consulta e instalacao.

## Licenca

GPL-3. Contribuicoes e relatos de bugs sao bem-vindos em
[issues](https://github.com/evandeilton/rnp/issues).
