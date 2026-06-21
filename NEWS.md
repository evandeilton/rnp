# rnp 3.0.0

Esta versao reescreve boa parte do pacote e amplia a cobertura para os temas dos
primeiros anos de um curso de estatistica. **Contem mudancas incompativeis** com
a serie 2.x (ver "Quebra de compatibilidade").

## Novas funcionalidades

* Cerca de 60 funcoes novas em descritiva, probabilidade, processos estocasticos,
  simulacao, inferencia (maxima verossimilhanca, bootstrap, testes classicos e
  nao-parametricos), regressao (regularizada, robusta, nao-linear, Box-Cox,
  multinomial), multivariada (analise discriminante, Hotelling, MANOVA, fatorial,
  correspondencia, k-medoids), dados categoricos, delineamento experimental,
  series temporais e pre-processamento.
* Rotinas numericamente intensivas passaram a ser implementadas em C++
  (Rcpp/RcppArmadillo): distancias, covariancia e correlacao, ajuste por QR,
  momentos, cadeias de Markov, reamostragem, regularizacao, regressao robusta,
  silhueta, ACF/PACF e imputacao por vizinhos. Os resultados foram conferidos
  contra as funcoes equivalentes do R base.
* As dependencias foram restritas ao R base, ao tidyverse, ao tidymodels e ao
  Rcpp/RcppArmadillo.

## Quebra de compatibilidade

* Removidas funcoes legadas/redundantes, substituidas por equivalentes modernos:
  * `rnp_freq()` -> `rnp_tabela_frequencia()` / `rnp_tabela_classes()`
  * `rnp_2freq()` -> `rnp_tabela_contingencia()` (corrige a frequencia relativa,
    que estava incorreta)
  * `rnp_summary()`, `rnp_summary_all()`, `rnp_summary_by()` -> `rnp_descritiva()`
    / `rnp_descritiva_by()`
  * `rnp_associacao()` -> `rnp_teste_qui_quadrado()`
  * `rnp_correlacao()` (legada) -> `rnp_matriz_correlacao()`
  * `media_aritmetica()`, `media_geometrica()`, `media_harmonica()`, `rnp_media()`
    -> `rnp_medias()`
  * `rnp_atributos()` -> `rnp_estrutura()`
  * `rnp_load_packages()`, `rnp_try_error()` removidas (anti-padroes)
  * `rnp_ts_arima()`, `rnp_ts_teste_estacionariedade()` removidas (dependiam de
    forecast/tseries); use `rnp_ts_acf()`, `rnp_ts_pacf()`, `rnp_ts_ljung_box()`,
    `rnp_ts_holt_winters()`, `rnp_ts_periodograma()`.
* `rnp_read()` agora usa `readr` (retorna tibble) em vez de `data.table`.

## Correcoes

* `rnp_distribuicao()` e `rnp_grafico_qq()` montavam o nome da funcao de
  distribuicao de forma invertida (ex.: `"nd"` em vez de `"dnorm"`), o que
  impedia o uso de varias distribuicoes.
* `rnp_descritiva()`: a coluna `n` mascarava a variavel `n`, retornando
  `n_validos` incorreto.
* Corrigida a formula da curtose amostral (Joanes-Gill).
* `rnp_distribuicao_hipergeometrica()`: argumento `n` duplicado.
* `nclass.*` movidas de `stats` para `grDevices`; `scales::colorRampPalette`
  corrigido para `grDevices::colorRampPalette`; `stats::pnt` para `stats::pt`.

## Dependencias

* Removidas: `curl`, `data.table`, `multcomp`, `forecast`, `tseries`, `scales`
  (uso direto), entre outras.
* Adicionadas: `Rcpp`, `RcppArmadillo` (LinkingTo), `readr`.
