<!-- README.md is generated from README.Rmd. Please edit that file -->

# rnp

<!-- badges: start -->
[![R-CMD-check](https://github.com/evandeilton/rnp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/evandeilton/rnp/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/evandeilton/rnp/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/evandeilton/rnp/actions/workflows/pkgdown.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## R NA PRÁTICA

O **R NA PRÁTICA** surgiu com o desejo de ajudar a disseminar a linguagem R como
ferramenta de apoio ao estudo de estatística e ciência de dados, para pessoas de
todos os níveis. Com o tempo, o projeto cresceu e passou a abranger a própria
programação em R, a estatística e a probabilidade aplicadas à análise de dados.

## O pacote `rnp`

À medida que o projeto ganhou volume, muitas funções foram sendo escritas. Para
condensar esse material em um lugar de fácil consulta, surgiu a ideia de
empacotá-lo. É o pacote `rnp`.

O pacote reúne mais de 230 funções que cobrem desde os temas dos primeiros anos
de um curso de estatística — análise descritiva, probabilidade, inferência,
regressão, análise multivariada, séries temporais, amostragem e dados
categóricos — até tópicos de estatística aplicada e ciência de dados: análise de
sobrevivência, modelos ARIMA/SARIMA, modelos lineares generalizados e mistos,
aprendizado de máquina com `tidymodels` e avaliação de modelos. As funções
analíticas retornam saídas em formato `tibble` e os gráficos são feitos com
`ggplot2`. As rotinas que exigem mais cálculo (distâncias, álgebra matricial,
reamostragem) são escritas em C++ com `Rcpp` e `RcppArmadillo`. Toda a
documentação e as mensagens estão em português.

O `rnp` depende apenas do R base, do `tidyverse`, do `tidymodels` e do `Rcpp`,
evitando uma cadeia extensa de dependências externas.

## Instalação

``` r
# Se ainda não tiver o devtools, instale-o
if (!require(devtools)) install.packages("devtools")

# Instalar o rnp
devtools::install_github("evandeilton/rnp")

# Carregar
library(rnp)
```

## Uso

As funções seguem a convenção de nome `rnp_<area>_<metodo>()` e devolvem
`tibble`, prontas para o pipe.

``` r
library(rnp)

# Estatística descritiva
rnp_descritiva(airquality$Ozone)

# Intervalo de confiança e teste de hipótese
rnp_ic_media(airquality$Temp)
rnp_teste_t(airquality$Temp, mu = 75)

# Regressão
fit <- rnp_regressao(medv ~ rm + lstat, data = MASS::Boston)
fit$coeficientes

# Componentes principais
p <- rnp_pca(iris[, 1:4])
rnp_biplot(p)
```

As funções que produzem modelos devolvem um objeto `rnp_resultado`: uma lista de
componentes (tabelas, escalares, gráficos) com impressão organizada e acesso
direto pelo `$`. Esses resultados implementam os genéricos `tidy()` e `glance()`
do `broom`, de modo que conversam com o ecossistema `tidymodels`.

O pacote inclui três conjuntos de dados simulados para praticar — `rnp_concreto`
(resistência de concreto), `rnp_defeitos` (contagem de defeitos) e `rnp_vida_util`
(tempo até a falha, com censura).

## Funções por área

<details>
<summary><b>Descritiva e exploratória</b></summary>

`rnp_descritiva()`, `rnp_descritiva_by()`, `rnp_momentos()`, `rnp_quantis()`,
`rnp_skewness()`, `rnp_kurtosis()`, `rnp_outliers()`, `rnp_medias()`,
`rnp_tabela_frequencia()`, `rnp_tabela_classes()`, `rnp_tabela_contingencia()`,
`rnp_estrutura()`, e os gráficos `rnp_grafico_histograma/_boxplot/_dispersao/_violino/_qq/_barras()`.
</details>

<details>
<summary><b>Probabilidade e distribuições</b></summary>

`rnp_distribuicao_*()` (normal, exponencial, gama, beta, uniforme, t,
qui-quadrado, F, binomial, Poisson, lognormal, Weibull, multinomial),
`rnp_grafico_distribuicao()`, `rnp_bayes()`, `rnp_distribuicao_conjunta()`,
`rnp_esperanca_condicional()`, `rnp_lei_grandes_numeros()`, `rnp_tcl_simulacao()`,
`rnp_cadeia_markov()`, `rnp_monte_carlo()`, `rnp_simula_inversao()`,
`rnp_ajuste_distribuicao()`.
</details>

<details>
<summary><b>Inferência</b></summary>

`rnp_emv()`, `rnp_metodo_momentos()`, `rnp_informacao_fisher()`,
`rnp_ic_media/_proporcao/_variancia/_diff_medias()`, `rnp_teste_t/_z_media/_z_proporcao()`,
`rnp_bootstrap()`, `rnp_ic_bootstrap()`, `rnp_jackknife()`, `rnp_teste_permutacao()`,
`rnp_teste_qui_quadrado/_ks/_normalidade/_grubbs/_runs/_sinais()`,
`rnp_poder_teste()`, `rnp_tamanho_amostra_teste()`, `rnp_bayes_conjugada()`,
`rnp_teste_razao_veross/_wald/_score()`.
</details>

<details>
<summary><b>Regressão e modelagem</b></summary>

`rnp_regressao()`, `rnp_regressao_diagnosticos()`, `rnp_grafico_residuos()`,
`rnp_vif()`, `rnp_predicao()`, `rnp_regressao_polinomial/_ponderada/_stepwise()`,
`rnp_regressao_ridge/_lasso()`, `rnp_elastic_net()`, `rnp_regressao_robusta()`,
`rnp_regressao_poisson/_nao_linear/_multinomial()`, `rnp_box_cox()`,
`rnp_logistic()`, `rnp_matriz_confusao()`, `rnp_curva_roc()`.
</details>

<details>
<summary><b>Multivariada</b></summary>

`rnp_pca()`, `rnp_biplot()`, `rnp_matriz_correlacao()`, `rnp_grafico_correlograma()`,
`rnp_kmeans()`, `rnp_kmedoids()`, `rnp_cluster_hierarquico()`, `rnp_grafico_dendrograma()`,
`rnp_silhueta()`, `rnp_lda()`, `rnp_distancia()`, `rnp_mds()`, `rnp_hotelling()`,
`rnp_manova()`, `rnp_analise_fatorial()`, `rnp_correspondencia()`,
`rnp_correlacao_canonica()`, `rnp_normalidade_multivariada()`.
</details>

<details>
<summary><b>Séries temporais</b></summary>

`rnp_ts_decomposicao()`, `rnp_media_movel()`, `rnp_suavizacao_exponencial()`,
`rnp_ts_acf()`, `rnp_ts_pacf()`, `rnp_ts_diferenciacao()`, `rnp_ts_ljung_box()`,
`rnp_ts_holt_winters()`, `rnp_ts_periodograma()`, `rnp_grafico_serie/_acf()`.
</details>

<details>
<summary><b>Categóricos, experimental e pré-processamento</b></summary>

`rnp_teste_fisher()`, `rnp_odds_ratio()`, `rnp_risco_relativo()`, `rnp_kappa()`,
`rnp_kruskal()`, `rnp_mann_whitney()`, `rnp_wilcoxon()`, `rnp_anova()`,
`rnp_ancova()`, `rnp_dbc()`, `rnp_contrastes()`, `rnp_anova_medidas_repetidas()`,
`rnp_padroniza()`, `rnp_normaliza()`, `rnp_winsoriza()`, `rnp_imputa()`,
`rnp_discretiza()`, `rnp_dummy()`.
</details>

<details>
<summary><b>Análise de sobrevivência</b></summary>

`rnp_kaplan_meier()`, `rnp_grafico_sobrevivencia()`, `rnp_log_rank()`,
`rnp_nelson_aalen()`, `rnp_cox()`, `rnp_cox_diagnosticos()`,
`rnp_cox_risco_relativo()`, `rnp_sobrevivencia_parametrica()`, `rnp_tabela_vida()`.
</details>

<details>
<summary><b>Séries ARIMA/SARIMA</b></summary>

`rnp_arima()`, `rnp_sarima()`, `rnp_auto_arima()`, `rnp_ts_previsao()`,
`rnp_ts_adf()`, `rnp_ts_kpss()`, `rnp_ts_var()`, `rnp_ts_ccf()`, `rnp_ts_garch()`,
`rnp_ts_residuos()`.
</details>

<details>
<summary><b>GLM, modelos mistos e aditivos</b></summary>

`rnp_glm()`, `rnp_glm_diagnosticos()`, `rnp_binomial_negativa()`,
`rnp_regressao_ordinal()`, `rnp_modelo_misto()`, `rnp_gam()`,
`rnp_grafico_efeitos()`.
</details>

<details>
<summary><b>Aprendizado de máquina (tidymodels)</b></summary>

`rnp_ml_particao()`, `rnp_ml_cv()`, `rnp_ml_receita()`, `rnp_ml_arvore()`,
`rnp_ml_floresta()`, `rnp_ml_boosting()`, `rnp_ml_knn()`, `rnp_ml_svm()`,
`rnp_ml_regularizada()`, `rnp_ml_ajustar()`, `rnp_ml_tunagem()`,
`rnp_ml_comparar()`, `rnp_ml_prever()`, `rnp_ml_importancia()`.
</details>

<details>
<summary><b>Avaliação de modelos</b></summary>

`rnp_metricas_classificacao()`, `rnp_metricas_regressao()`, `rnp_curva_lift()`,
`rnp_curva_ganho()`, `rnp_calibracao()`, `rnp_brier()`, `rnp_ks_classificador()`,
`rnp_curva_precisao_revocacao()`, `rnp_comparar_roc()`, `rnp_acuracia_diagnostica()`.
</details>

## Tutoriais

O pacote acompanha onze tutoriais que seguem a progressão típica de um curso
de estatística, com dados reais e ênfase na interpretação dos resultados:

1. Estatística descritiva e análise exploratória
2. Probabilidade, distribuições e os teoremas fundamentais
3. Inferência estatística
4. Regressão linear e modelagem
5. Análise multivariada
6. Dados categóricos e métodos não-paramétricos
7. Análise de sobrevivência
8. Séries temporais: modelos ARIMA e SARIMA
9. Modelos lineares generalizados e extensões
10. Aprendizado de máquina com tidymodels
11. Avaliação de modelos preditivos

Além dos tutoriais, há uma **referência rápida** (_cheatsheet_) das funções por
área e uma coletânea de **soluções comentadas** dos exercícios dos primeiros
capítulos.

``` r
browseVignettes("rnp")
```

## Bugs e sugestões

Caso encontre algum problema ou queira sugerir a inclusão de funções, abra uma
_issue_ [aqui](https://github.com/evandeilton/rnp/issues) para que possamos
atender nas próximas atualizações.

## Licença

MIT &copy; José E. Lopes.
