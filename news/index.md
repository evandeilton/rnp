# Changelog

## rnp 3.0.0

Versao maior com reconstrucao ampla do pacote, foco em performance
(backends C++ via Rcpp/RcppArmadillo), correcao de bugs e cobertura das
ementas dos tres primeiros anos de um bacharelado em Estatistica. **Esta
versao contem mudancas incompativeis** (ver “Quebra de
compatibilidade”).

### Destaques

- **Backends C++ (Rcpp/RcppArmadillo).** 12 nucleos compilados e
  verificados contra a referencia do R (tolerancia ~1e-8): distancias,
  covariancia/correlacao, OLS por QR, momentos, utilitarios empiricos
  (ECDF/ranks/binning), cadeias de Markov, reamostragem
  (bootstrap/jackknife/permutacao), regularizacao (ridge/elastic net),
  regressao robusta (IRLS), silhueta, series temporais (ACF/PACF/media
  movel/EWMA) e imputacao kNN.
- **~60 funcoes novas** cobrindo descritiva, probabilidade, processos
  estocasticos, simulacao, inferencia (EMV, verossimilhanca, bootstrap,
  testes classicos e nao-parametricos), regressao (regularizada,
  robusta, nao-linear, Box-Cox, multinomial), multivariada (LDA,
  Hotelling, MANOVA, fatorial, correspondencia, k-medoids), dados
  categoricos, delineamento experimental, series temporais e
  pre-processamento.
- **Politica de dependencias enxuta:** apenas base R, tidyverse,
  tidymodels e Rcpp/RcppArmadillo.

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
