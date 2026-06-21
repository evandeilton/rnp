# Changelog

## rnp 3.1.0

Amplia o pacote para alem da graduacao classica, cobrindo temas centrais
de estatistica aplicada e ciencia de dados. Cerca de 49 funcoes novas,
organizadas em cinco areas.

### Novas funcionalidades

- **Analise de sobrevivencia** (apoio em `survival`): Kaplan-Meier e
  curva, teste log-rank, Nelson-Aalen, modelo de Cox e diagnostico da
  hipotese de riscos proporcionais, modelos parametricos (AFT) e tabua
  de vida.
- **Series temporais ARIMA/SARIMA** (base `stats`): ajuste e selecao
  automatica de ordem, previsao com intervalos, testes ADF e KPSS
  reimplementados, autorregressao vetorial (VAR) com causalidade de
  Granger, correlacao cruzada e volatilidade GARCH.
- **Modelos lineares generalizados e extensoes** (`MASS`, `nlme`,
  `mgcv`): GLM unificado e diagnostico de superdispersao, regressao
  binomial negativa, ordinal, modelos mistos e aditivos (GAM).
- **Aprendizado de maquina com tidymodels**: particao, validacao
  cruzada, receitas de pre-processamento, especificacoes de arvore,
  floresta, boosting, k-vizinhos, SVM e modelos regularizados, alem de
  ajuste, tunagem, comparacao, predicao e importancia de variaveis.
  Engines em `Suggests`.
- **Avaliacao de modelos**: metricas de classificacao e regressao,
  curvas de lift e ganho, calibracao com Hosmer-Lemeshow, escore de
  Brier, estatistica KS, curva precisao-revocacao, comparacao de ROC
  pelo teste de DeLong e acuracia diagnostica.

### Dependencias

- Adicionadas aos Imports (pacotes recomendados, ja distribuidos com o
  R): `survival`, `nlme`, `mgcv`, `MASS`.
- Em Suggests (carregados sob demanda): pacotes do tidymodels e engines
  de ML (`ranger`, `xgboost`, `kknn`, `kernlab`, `glmnet`, `rpart`).

## rnp 3.0.0

Esta versao reescreve boa parte do pacote e amplia a cobertura para os
temas dos primeiros anos de um curso de estatistica. **Contem mudancas
incompativeis** com a serie 2.x (ver “Quebra de compatibilidade”).

### Novas funcionalidades

- Cerca de 60 funcoes novas em descritiva, probabilidade, processos
  estocasticos, simulacao, inferencia (maxima verossimilhanca,
  bootstrap, testes classicos e nao-parametricos), regressao
  (regularizada, robusta, nao-linear, Box-Cox, multinomial),
  multivariada (analise discriminante, Hotelling, MANOVA, fatorial,
  correspondencia, k-medoids), dados categoricos, delineamento
  experimental, series temporais e pre-processamento.
- Rotinas numericamente intensivas passaram a ser implementadas em C++
  (Rcpp/RcppArmadillo): distancias, covariancia e correlacao, ajuste por
  QR, momentos, cadeias de Markov, reamostragem, regularizacao,
  regressao robusta, silhueta, ACF/PACF e imputacao por vizinhos. Os
  resultados foram conferidos contra as funcoes equivalentes do R base.
- As dependencias foram restritas ao R base, ao tidyverse, ao tidymodels
  e ao Rcpp/RcppArmadillo.

### Quebra de compatibilidade

- Removidas funcoes legadas/redundantes, substituidas por equivalentes
  modernos:
  - `rnp_freq()` -\>
    [`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md)
    /
    [`rnp_tabela_classes()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_classes.md)
  - `rnp_2freq()` -\>
    [`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md)
    (corrige a frequencia relativa, que estava incorreta)
  - `rnp_summary()`, `rnp_summary_all()`, `rnp_summary_by()` -\>
    [`rnp_descritiva()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva.md)
    /
    [`rnp_descritiva_by()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva_by.md)
  - `rnp_associacao()` -\>
    [`rnp_teste_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_teste_qui_quadrado.md)
  - `rnp_correlacao()` (legada) -\>
    [`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md)
  - `media_aritmetica()`, `media_geometrica()`, `media_harmonica()`,
    `rnp_media()` -\>
    [`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md)
  - `rnp_atributos()` -\>
    [`rnp_estrutura()`](https://evandeilton.github.io/rnp/reference/rnp_estrutura.md)
  - `rnp_load_packages()`, `rnp_try_error()` removidas (anti-padroes)
  - `rnp_ts_arima()`, `rnp_ts_teste_estacionariedade()` removidas
    (dependiam de forecast/tseries); use
    [`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
    [`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
    [`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
    [`rnp_ts_holt_winters()`](https://evandeilton.github.io/rnp/reference/rnp_ts_holt_winters.md),
    [`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md).
- [`rnp_read()`](https://evandeilton.github.io/rnp/reference/rnp_read.md)
  agora usa `readr` (retorna tibble) em vez de `data.table`.

### Correcoes

- [`rnp_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao.md)
  e
  [`rnp_grafico_qq()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_qq.md)
  montavam o nome da funcao de distribuicao de forma invertida (ex.:
  `"nd"` em vez de `"dnorm"`), o que impedia o uso de varias
  distribuicoes.
- [`rnp_descritiva()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva.md):
  a coluna `n` mascarava a variavel `n`, retornando `n_validos`
  incorreto.
- Corrigida a formula da curtose amostral (Joanes-Gill).
- [`rnp_distribuicao_hipergeometrica()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_hipergeometrica.md):
  argumento `n` duplicado.
- `nclass.*` movidas de `stats` para `grDevices`;
  `scales::colorRampPalette` corrigido para
  [`grDevices::colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html);
  `stats::pnt` para [`stats::pt`](https://rdrr.io/r/stats/TDist.html).

### Dependencias

- Removidas: `curl`, `data.table`, `multcomp`, `forecast`, `tseries`,
  `scales` (uso direto), entre outras.
- Adicionadas: `Rcpp`, `RcppArmadillo` (LinkingTo), `readr`.
