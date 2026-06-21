# rnp

## R NA PRÁTICA

O **R NA PRÁTICA** surgiu com o desejo de ajudar a disseminar a
linguagem R como ferramenta de apoio ao estudo de estatística e ciência
de dados, para pessoas de todos os níveis. Com o tempo, o projeto
cresceu e passou a abranger a própria programação em R, a estatística e
a probabilidade aplicadas à análise de dados.

## O pacote `rnp`

À medida que o projeto ganhou volume, muitas funções foram sendo
escritas. Para condensar esse material em um lugar de fácil consulta,
surgiu a ideia de empacotá-lo. É o pacote `rnp`.

O pacote reúne mais de 230 funções que cobrem desde os temas dos
primeiros anos de um curso de estatística — análise descritiva,
probabilidade, inferência, regressão, análise multivariada, séries
temporais, amostragem e dados categóricos — até tópicos de estatística
aplicada e ciência de dados: análise de sobrevivência, modelos
ARIMA/SARIMA, modelos lineares generalizados e mistos, aprendizado de
máquina com `tidymodels` e avaliação de modelos. As funções analíticas
retornam saídas em formato `tibble` e os gráficos são feitos com
`ggplot2`. As rotinas que exigem mais cálculo (distâncias, álgebra
matricial, reamostragem) são escritas em C++ com `Rcpp` e
`RcppArmadillo`. Toda a documentação e as mensagens estão em português.

O `rnp` depende apenas do R base, do `tidyverse`, do `tidymodels` e do
`Rcpp`, evitando uma cadeia extensa de dependências externas.

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

## Funções por área

**Descritiva e exploratória**

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
e os gráficos
`rnp_grafico_histograma/_boxplot/_dispersao/_violino/_qq/_barras()`.

**Probabilidade e distribuições**

`rnp_distribuicao_*()` (normal, exponencial, gama, beta, uniforme, t,
qui-quadrado, F, binomial, Poisson, lognormal, Weibull, multinomial),
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

**Inferência**

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

**Regressão e modelagem**

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

**Multivariada**

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

**Séries temporais**

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

**Categóricos, experimental e pré-processamento**

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

**Análise de sobrevivência**

[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md).

**Séries ARIMA/SARIMA**

[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
[`rnp_sarima()`](https://evandeilton.github.io/rnp/reference/rnp_sarima.md),
[`rnp_auto_arima()`](https://evandeilton.github.io/rnp/reference/rnp_auto_arima.md),
[`rnp_ts_previsao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_previsao.md),
[`rnp_ts_adf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_adf.md),
[`rnp_ts_kpss()`](https://evandeilton.github.io/rnp/reference/rnp_ts_kpss.md),
[`rnp_ts_var()`](https://evandeilton.github.io/rnp/reference/rnp_ts_var.md),
[`rnp_ts_ccf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ccf.md),
[`rnp_ts_garch()`](https://evandeilton.github.io/rnp/reference/rnp_ts_garch.md),
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md).

**GLM, modelos mistos e aditivos**

[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md).

**Aprendizado de máquina (tidymodels)**

[`rnp_ml_particao()`](https://evandeilton.github.io/rnp/reference/rnp_ml_particao.md),
[`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md),
[`rnp_ml_receita()`](https://evandeilton.github.io/rnp/reference/rnp_ml_receita.md),
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
[`rnp_ml_floresta()`](https://evandeilton.github.io/rnp/reference/rnp_ml_floresta.md),
[`rnp_ml_boosting()`](https://evandeilton.github.io/rnp/reference/rnp_ml_boosting.md),
[`rnp_ml_knn()`](https://evandeilton.github.io/rnp/reference/rnp_ml_knn.md),
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md),
[`rnp_ml_regularizada()`](https://evandeilton.github.io/rnp/reference/rnp_ml_regularizada.md),
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
[`rnp_ml_tunagem()`](https://evandeilton.github.io/rnp/reference/rnp_ml_tunagem.md),
[`rnp_ml_comparar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_comparar.md),
[`rnp_ml_prever()`](https://evandeilton.github.io/rnp/reference/rnp_ml_prever.md),
[`rnp_ml_importancia()`](https://evandeilton.github.io/rnp/reference/rnp_ml_importancia.md).

**Avaliação de modelos**

[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md).

## Tutoriais

O pacote acompanha seis tutoriais que acompanham a progressão típica de
um curso de estatística, com dados reais e ênfase na interpretação dos
resultados:

1.  Estatística descritiva e análise exploratória
2.  Probabilidade, distribuições e os teoremas fundamentais
3.  Inferência estatística
4.  Regressão linear e modelagem
5.  Análise multivariada
6.  Dados categóricos e métodos não-paramétricos
7.  Análise de sobrevivência
8.  Séries temporais: modelos ARIMA e SARIMA
9.  Modelos lineares generalizados e extensões
10. Aprendizado de máquina com tidymodels
11. Avaliação de modelos preditivos

``` r

browseVignettes("rnp")
```

## Bugs e sugestões

Caso encontre algum problema ou queira sugerir a inclusão de funções,
abra uma *issue* [aqui](https://github.com/evandeilton/rnp/issues) para
que possamos atender nas próximas atualizações.

## Licença

MIT © José E. Lopes.
