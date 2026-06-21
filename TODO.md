# TODO.md — Plano de Reconstrução do Pacote `rnp`

> **Objetivo:** transformar o `rnp` no "canivete suíço do estatístico" — rápido,
> correto, bem testado e didático — cobrindo as ementas dos 3 primeiros anos de um
> Bacharelado em Estatística (padrão UFPR). Versão alvo: **`rnp 3.0.0`**.
>
> **Este documento é o contrato de execução. Seguir irrestritamente, na ordem das fases.**

---

## ✅ ESTADO ATUAL (log de execução)

**FATIA 0 — Fundação: CONCLUÍDA** (suíte de testes 100% verde)
- Infra Rcpp/RcppArmadillo operante: `src/` + `Makevars`/`Makevars.win`, `useDynLib`,
  `compileAttributes`. Pacote compila e carrega.
- Backends C++ entregues e **verificados vs. R (erro ~1e-15)**: `dist_pairwise_cpp`
  (C-01, 5 métricas), `cov_cpp`/`cor_cpp` (C-02), `ols_fit_cpp` (C-03), `momentos_cpp`
  (C-06), `ecdf_cpp`/`ranks_cpp`/`binning_cpp` (C-09). Testes em `test-cpp-backends.R` (C-15).
- Deps removidas: `curl`→`utils::download.file`; `data.table`→`readr` (1.1);
  `multcomp`→`stats::qtukey` (1.3, era bug latente). ORCID inválido removido.
- Bugs corrigidos: **B-01** (typo `rnp_ic_meta`), **B-02** (fr de `rnp_2freq`, via nova
  `rnp_tabela_contingencia`), **B-03/B-05** (Mahalanobis/distâncias → C++), **B-06**
  (vários: `rnp_distribuicao`/`rnp_grafico_qq` com nome de função invertido `"nd"`→`"dnorm"`;
  `nclass.*` de `stats`→`grDevices`; mascaramento de `n` no `tibble` de `rnp_descritiva`;
  fórmula de curtose; `rnp_descritiva_by` com dplyr atual; `rnp_distribuicao_hipergeometrica`
  com `n` duplicado). NOTES de globals limpas via `@importFrom` + `globalVariables`.
- Remoções (quebra limpa): **D-01** `rnp_load_packages`, **D-02** `rnp_try_error`,
  **D-03** `rnp_freq`, **D-04** `rnp_2freq`, **D-05** `rnp_summary*`, **D-06** `rnp_associacao`,
  **D-07** `rnp_correlacao` legada, **D-08** `media_*`/`rnp_media`→`rnp_medias()`,
  **D-09** `rnp_atributos`→`rnp_estrutura()`.

**FATIA 1 — Descritiva & Probabilidade: CONCLUÍDA** (suíte 100% verde)
- ✅ **F-01** `rnp_momentos` (C++ C-06), **F-02** `rnp_intervalo_classes`,
  **F-03** `rnp_tabela_frequencia`/`rnp_tabela_contingencia` (corrige B-02). `test-frequencias.R`.
- ✅ **F-04** `rnp_bayes`, **F-05** `rnp_distribuicao_conjunta`,
  **F-06** `rnp_esperanca_condicional`, **F-07** `rnp_lei_grandes_numeros` [ggplot],
  **F-08** `rnp_tcl_simulacao` [ggplot]. `test-probabilidade-teoria.R`.
- ✅ **F-09..F-16** wrappers contínuos (normal, exp, gama, beta, uniforme, t, qui-quadrado, F),
  **F-17** `rnp_grafico_distribuicao` [ggplot], **F-62/63/64** lognormal/weibull/multinomial,
  **F-65** `rnp_ajuste_distribuicao` (MLE+AIC/BIC/KS). `test-distribuicoes-continuas.R`.
- ✅ **F-56** `rnp_cadeia_markov` (C++ C-11), **F-57** `rnp_passeio_aleatorio` [ggplot],
  **F-58** `rnp_processo_poisson`, **F-59** `rnp_simula_inversao`,
  **F-60** `rnp_simula_aceitacao_rejeicao`, **F-61** `rnp_monte_carlo`. `test-processos.R`.
- Backend C++ novo: **C-11** `src/markov.cpp` (`markov_npassos_cpp`, `markov_estacionaria_cpp`),
  verificado vs. estacionária analítica.

**FATIA 2 — Inferência: CONCLUÍDA** (suíte 100% verde, 55 testes novos)
- ✅ Reamostragem: **F-22** `rnp_bootstrap` (C++ C-05), **F-23** `rnp_ic_bootstrap`
  (percentil/normal/básico/BCa), **F-24** `rnp_jackknife` (C++), **F-70**
  `rnp_bootstrap_parametrico`, **F-71** `rnp_teste_permutacao` (C++ `permutacao_difmedias_cpp`).
  Backend novo `src/reamostragem.cpp`. `test-reamostragem.R`.
- ✅ Verossimilhança: **F-18** `rnp_emv` (optim+Hessiana), **F-19** `rnp_metodo_momentos`,
  **F-20** `rnp_log_verossimilhanca` [ggplot], **F-21** `rnp_informacao_fisher`,
  **F-69** `rnp_ic_verossimilhanca` (perfilada), **F-66** `rnp_teste_razao_veross` (LRT),
  **F-67** `rnp_teste_wald`, **F-68** `rnp_teste_score`, **F-72** `rnp_bayes_conjugada`.
  `test-verossimilhanca.R`.
- ✅ Testes: **F-25** `rnp_teste_qui_quadrado`, **F-26** `rnp_teste_aderencia`,
  **F-27** `rnp_teste_ks`, **F-28** `rnp_teste_proporcoes`, **F-29** `rnp_teste_binomial`,
  **F-30** `rnp_poder_teste` [ggplot, t não-central], **F-31** `rnp_tamanho_amostra_teste`,
  **F-73** `rnp_teste_normalidade` (Shapiro/Jarque-Bera/Anderson-Darling),
  **F-74** `rnp_teste_grubbs`, **F-75** `rnp_teste_runs`, **F-76** `rnp_teste_sinais`.
  `test-testes-inferencia.R`. (**F-32** `rnp_tamanho_efeito` já existia em `misc.R`.)
- Backend C++ novo: **C-05** `src/reamostragem.cpp` (bootstrap/jackknife/permutação).

**FATIA 3 — Regressão & Modelagem: CONCLUÍDA** (suíte 100% verde, 25 testes novos)
- ✅ Regularizada: **F-34** `rnp_regressao_ridge` (C++ `ridge_cpp`), **F-35**
  `rnp_regressao_lasso` (C++ coordinate descent), **F-87** `rnp_elastic_net`.
  Backend novo `src/regularizacao.cpp`. `test-regressao-avancada.R`.
- ✅ **F-33** `rnp_regressao_polinomial` (C++ OLS-QR), **F-36** `rnp_regressao_ponderada`
  (WLS, C++), **F-37** `rnp_regressao_stepwise` (AIC/BIC), **F-38** `rnp_regressao_poisson`,
  **F-39** `rnp_vif`, **F-40** `rnp_anova_modelos`, **F-41** `rnp_predicao`,
  **F-42** `rnp_grafico_residuos` [4 ggplots].
- ✅ Avançada: **F-85** `rnp_regressao_robusta` (IRLS Huber/bisquare, C++ `irls_cpp`),
  **F-86** `rnp_regressao_nao_linear`, **F-88** `rnp_box_cox` [ggplot],
  **F-89** `rnp_regressao_multinomial` (softmax MLE próprio, sem deps externas).
- Backends C++ novos: **C-04** `src/regularizacao.cpp`, **C-13** `src/robusta.cpp`.

**Bugs/itens corrigidos via `R CMD check` na FATIA 3**: exemplo inválido de
`rnp_teste_friedman` (warpbreaks → bloco completo construído); doc `rnp_kruskal$data`;
`scales` removido (não usado); `LazyDataCompression: xz` (data 13.8 MB); `.travis.yml`
ignorado; bug de escopo de `data` em `step()` dentro de função.

**Bugs adicionais corrigidos na FATIA 1** (descobertos via `R CMD check`):
- `rnp_paleta_rnp`: `scales::colorRampPalette` (inexistente) → `grDevices::colorRampPalette` (era **ERROR**).
- `.duncan_interna`: `stats::pnt` (inexistente) → `stats::pt` + parênteses do `qt` corrigidos.
- `rnp_distribuicao`: faltava `"lnorm"` na lista de distribuicoes.
- Imports limpos: `+readr`, `-cli/-lubridate/-tidyr` (não usados); doc de `rnp_anova$data`.

> Pendências conhecidas p/ fechamento global: vinheta usa `rnp_freq` removida (R-06);
> `dm_docente.rda` 5 MB (D-11); refatorar `series_temporais` sem `forecast`/`tseries` (FATIA 6).

---

## 0. Princípios inegociáveis (valem para TODO o código)

1. **Idioma:** toda **documentação, mensagens (`abort_*`/`cli`), comentários e nomes
   de colunas de saída** em **PT-BR**. Nomes de **argumentos** em PT-BR quando natural
   (`lado`, `digits`, `conf`). **Exceção (DECISÃO):** as **strings de método
   estatístico consagradas permanecem em inglês** (`method = "wald"`, `"wilson"`,
   `"euclidean"`, `"pearson"`, etc.) — são termos técnicos internacionais e não se
   traduzem. Identificadores em `snake_case`; ASCII no código-fonte, acentuação só
   em strings/docs.
2. **Nomenclatura:** prefixo `rnp_` + substantivo/área + qualificador.
   Ex.: `rnp_<area>_<metodo>()`. Funções internas: prefixo `.` (ex.: `.moda_interna`).
   Backends C++: sufixo `_cpp` (ex.: `dist_pairwise_cpp`), nunca exportados ao usuário.
3. **Dependências permitidas (política rígida):**
   - **Base/core R:** `stats`, `utils`, `methods`, `grDevices`, `graphics`.
   - **Tidyverse:** `dplyr`, `tibble`, `tidyr`, `purrr`, `stringr`, `ggplot2`,
     `rlang`, `forcats`, `readr`, `lubridate`, `glue`, `scales`, `magrittr`, `tidyselect`.
   - **Tidymodels (DECISÃO: manter):** `rsample` (reamostragem/bootstrap/CV),
     `yardstick` (métricas de classificação/regressão, ROC/AUC), `recipes`
     (pré-processamento), `parsnip`/`broom` (tidy de modelos). Usar onde poupar
     código robusto; ainda assim oferecer implementação própria C++ didática nos
     backends (bootstrap, métricas) para fins de ensino.
   - **C++:** `Rcpp`, `RcppArmadillo` (via `LinkingTo`).
   - **PROIBIDO** introduzir qualquer outra. **REMOVER:** `data.table`, `forecast`,
     `tseries`, `multcomp`, `readxl`, `naniar`, `curl`, `corrplot`, `effectsize`,
     `pwr`, `mice`, `ggVennDiagram`, `pillar`, `vctrs` (uso direto), `prettydoc`,
     `patchwork`, `kableExtra` (mover p/ Suggests apenas se vinheta exigir).
4. **Performance:** todo laço numérico O(n²) ou maior, reamostragem, e álgebra
   matricial pesada **devem** ter backend em C++ (Rcpp/RcppArmadillo). R puro só
   para orquestração, validação e montagem de `tibble`.
5. **Gráficos:** exclusivamente `ggplot2`. Toda função gráfica retorna um objeto
   `ggplot` (componível), usa `rnp_tema_rnp()` e `rnp_paleta_rnp()`.
6. **Saídas:** funções analíticas retornam `tibble` (ou `list` de `tibble`s),
   nunca `print` como efeito colateral. Colunas em PT-BR.
7. **Validação:** toda função pública valida argumentos via helpers `abort_*`
   no topo, com mensagens `cli`/`rlang` claras em PT-BR.
8. **Documentação:** roxygen2 markdown, com `@examples` executáveis (sem erro em
   `R CMD check`), `@return` descrevendo cada coluna, `@references` quando couber,
   e `@family` agrupando por ementa.
9. **Testes:** cada função pública tem teste `testthat` (edition 3). Backends C++
   testados contra a implementação R/`stats` de referência (tolerância `1e-8`).
   Meta de cobertura: **≥ 90%**.
10. **Definition of Done por função:** (a) implementada; (b) documentada; (c) testada;
    (d) exemplo roda; (e) `devtools::check()` sem ERROR/WARNING; (f) entrada marcada
    `[x]` neste arquivo.

---

## 1. Infraestrutura (FASE 0 — pré-requisito de tudo)

- [ ] **0.1** Criar `src/` com toolchain Rcpp/RcppArmadillo:
  - `src/Makevars` e `src/Makevars.win` (`CXX_STD = CXX17`, flags Armadillo).
  - `src/rnp_init.c` (registro de rotinas) ou `useDynLib(rnp, .registration = TRUE)`.
  - `R/rnp-package.R`: adicionar `@useDynLib rnp, .registration = TRUE` e
    `@importFrom Rcpp sourceCpp`.
- [ ] **0.2** Atualizar `DESCRIPTION`:
  - `LinkingTo: Rcpp, RcppArmadillo`; `Imports: Rcpp (>= 1.0.0)`.
  - Remover dependências proibidas (ver §0.3). Bump `Version: 3.0.0`.
- [ ] **0.3** Configurar `roxygen2` para gerar `NAMESPACE` (já em uso).
- [ ] **0.4** Criar `R/rnp-cpp-docs.R` documentando os backends como `@keywords internal`.
- [ ] **0.5** Padronizar infra de testes: `tests/testthat/helper-rnp.R` com
  geradores de dados sintéticos reprodutíveis (`set.seed`) e tolerâncias.
- [ ] **0.6** CI local: script `dev/check.R` rodando `devtools::document()`,
  `Rcpp::compileAttributes()`, `devtools::test()`, `devtools::check()`.
- [ ] **0.7** `.Rbuildignore`: adicionar `dev/`, `TODO.md`, `REPOSITORY_ANALYSIS_REPORT.md`.

---

## 2. Remoção de dependências externas (FASE 1)

Substituir cada uso por implementação base/tidyverse/C++:

- [ ] **1.1** `data.table::fread` em `rnp_read` → `readr::read_delim` (tidyverse),
  mantendo assinatura e default INEP (`sep="|"`, `encoding="Latin-1"`).
- [ ] **1.2** `forecast`/`tseries` em `R/series_temporais.R` → reimplementar
  decomposição, ACF/PACF, ADF e ARIMA-base com `stats` + backends C++ (ver §6).
- [ ] **1.3** `multcomp` no Dunnett interno → implementar Dunnett próprio
  (estatística t + correção) ou documentar limitação e usar `stats`.
- [ ] **1.4** `curl` em `R/inep.R` → `utils::download.file()` / `readr`.
- [ ] **1.5** `naniar` em `rnp_na_summary` → implementação própria (já é trivial).
- [ ] **1.6** `corrplot`/`effectsize`/`pwr`/`mice` → remover de Suggests ou substituir
  por funções `rnp_*` próprias (correlograma, tamanho de efeito, poder — ver §5, §7).
- [ ] **1.7** Rodar `grep -rE "[a-z]+::" R/` e garantir que só restam namespaces permitidos.

---

## 3. Correção de bugs auditados (FASE 1)

- [ ] **B-01** `R/inferencia.R:18` — corrigir exemplo `rnp_ic_meta` → `rnp_ic_media`.
- [ ] **B-02** `R/core-functions.R:50` (`rnp_2freq`) — `fr` deve ser `t1/sum(t1)`
  (frequência relativa), não `t1/max(t1)`. Revisar também `fr_lin`/`fr_col`
  (devem dividir por total da linha/coluna, não pelo máximo).
- [ ] **B-03** `R/multivariada.R:145-164` (`rnp_distancia`, Mahalanobis) — remover
  `dist()` descartado; substituir laço duplo R por backend `dist_mahalanobis_cpp`.
- [ ] **B-04** `R/core-functions.R:14` (`rnp_freq`) — quebras por quantis com
  `unique(round(...))` podem colapsar bins; usar `rnp_intervalo_classes()` (§5).
- [ ] **B-05** `rnp_distancia` Minkowski/Canberra/etc. — migrar para `dist_pairwise_cpp`.
- [ ] **B-06** Varredura geral: revisar todas as funções `[FUN]` (123) procurando
  `na.rm` inconsistente, divisão por zero não tratada, e `where(is.numeric)` em
  colunas que não devem ser arredondadas (ex.: `n`, `gl`). Registrar achados aqui.
- [ ] **B-07** Auditar exemplos de **todas** as funções (`grep '@examples'`) garantindo
  que nenhum chama função inexistente (caçar typos como B-01).

---

## 3.5. Remoção de funções sem sentido / redundantes (FASE 1 — quebra limpa)

> DECISÃO do usuário: recomeço limpo em 3.0.0. As funções abaixo são removidas
> (ou consolidadas). Cada remoção é registrada no `NEWS.md` com a função substituta.

**Remover (anti-padrão / fora do escopo de um pacote estatístico):**
- [ ] **D-01** `rnp_load_packages()` — instala pacotes em runtime (anti-padrão). Remover.
- [ ] **D-02** `rnp_try_error()` — duplica `tryCatch`/`rlang::try_fetch`. Remover.

**Remover (redundante com função moderna superior):**
- [ ] **D-03** `rnp_freq()` → substituída por `rnp_tabela_frequencia()` (F-03/B-04).
- [ ] **D-04** `rnp_2freq()` → substituída por `rnp_tabela_contingencia()` (F-03; corrige B-02).
- [ ] **D-05** `rnp_summary()`, `rnp_summary_all()`, `rnp_summary_by()` → `rnp_descritiva()`/`rnp_descritiva_by()`.
- [ ] **D-06** `rnp_associacao()` → `rnp_teste_qui_quadrado()` (F-25).
- [ ] **D-07** `rnp_correlacao()` (legada) → `rnp_matriz_correlacao()` (F-43).
- [ ] **D-08** `rnp_media()` + `media_aritmetica()`/`media_geometrica()`/`media_harmonica()`
  → consolidar em **`rnp_medias(x, peso, tipo)`** única (aritm./geom./harm./quadrática),
  removendo os 4 wrappers legados.

**Avaliar / refatorar (manter, mas modernizar):**
- [ ] **D-09** `rnp_atributos()` → renomear para `rnp_estrutura()`, saída `tibble` (glance de objeto).
- [ ] **D-10** `rnp_read()` → manter, trocar `data.table` por `readr` (1.1).
- [ ] **D-11** Decidir sobre dados `data/dm_docente.rda` (5 MB) — comprimir (`tools::resaveRdaFiles`,
  `xz`) ou mover para pacote de dados externo; `R CMD check` reclama de >5 MB.

## 4. Camada C++ — backends de performance (FASE 2, RcppArmadillo)

Cada backend em `src/`, testado contra referência R. Expostos só por wrappers `rnp_*`.

- [ ] **C-01** `src/distancias.cpp` — `dist_pairwise_cpp(X, metodo, p)`:
  euclidiana, manhattan, minkowski, canberra, mahalanobis (recebe `inv_cov`).
- [ ] **C-02** `src/covariancia.cpp` — `cov_cpp(X)`, `cor_cpp(X, metodo)`
  (Pearson/Spearman via ranks), matriz completa em uma passada.
- [ ] **C-03** `src/ols.cpp` — `ols_fit_cpp(X, y)` via decomposição QR (coef, fitted,
  resíduos, XtX⁻¹, sigma²). Base para regressões.
- [ ] **C-04** `src/regularizacao.cpp` — `ridge_cpp(X, y, lambda)` (solução fechada
  Armadillo) e `lasso_cd_cpp(X, y, lambda, ...)` (coordinate descent).
- [ ] **C-05** `src/reamostragem.cpp` — `bootstrap_cpp(x, B, estatistica)` e
  `jackknife_cpp(x)`; reamostragem vetorizada de índices.
- [ ] **C-06** `src/momentos.cpp` — `momentos_cpp(x, ordem)` (brutos e centrais),
  assimetria e curtose em passada única numericamente estável (Welford).
- [ ] **C-07** `src/series.cpp` — `acf_cpp(x, lag)`, `pacf_cpp(x, lag)`
  (Durbin-Levinson), `media_movel_cpp(x, k)`, `ewma_cpp(x, alpha)`.
- [ ] **C-08** `src/cluster.cpp` — `silhueta_cpp(D, clusters)`, opcional
  `kmeans_lloyd_cpp` (se `stats::kmeans` virar gargalo).
- [ ] **C-09** `src/empirico.cpp` — `ecdf_cpp(x, pontos)`, `ranks_cpp(x)`,
  `binning_cpp(x, breaks)` para histogramas/frequências rápidas.
- [ ] **C-11** `src/markov.cpp` — `markov_npassos_cpp(P, n)` (potência de matriz),
  `markov_estacionaria_cpp(P)` (autovetor dominante via Armadillo).
- [ ] **C-12** `src/monte_carlo.cpp` — `monte_carlo_cpp(...)` (integração),
  `permutacao_cpp(x, y, n_perm)` (reamostragem de rótulos para teste de permutação).
- [ ] **C-13** `src/robusta.cpp` — `irls_cpp(X, y, psi, ...)` (M-estimadores Huber/bisquare).
- [ ] **C-14** `src/imputacao.cpp` — `knn_imputa_cpp(X, k)` (vizinhos por distância).
- [ ] **C-15** Para CADA backend: teste `test-cpp-<arquivo>.R` comparando com
  `stats::dist`, `stats::cov`, `lm`, `stats::acf`, `MASS::rlm` (referência) etc.
  (tol. `1e-8`). Validar também estabilidade numérica e casos-limite (n pequeno, NA).

---

## 5. Novas funções — Bloco DESCRITIVA / PROBABILIDADE (1º–2º ano)

> Backend C++ marcado com **[C++]**; gráfico **[ggplot]**.

- [ ] **F-01** `rnp_momentos(x, ordem = 4)` — momentos brutos/centrais, assimetria,
  curtose. **[C++ C-06]**
- [ ] **F-02** `rnp_intervalo_classes(x, regra = c("sturges","scott","fd"))` —
  classes ótimas; usado por `rnp_freq`/histograma. **[C++ C-09]**
- [ ] **F-03** `rnp_tabela_contingencia(x, y)` — tabela dupla correta (substitui o
  comportamento bugado de `rnp_2freq`): fa, fr, fr_linha, fr_coluna, marginais.
- [ ] **F-04** `rnp_bayes(p_a, p_b_dado_a, p_b_dado_nao_a)` — Teorema de Bayes
  (forma 2 eventos) e versão partição `rnp_bayes_particao(priori, veross)`.
- [ ] **F-05** `rnp_distribuicao_conjunta(tabela)` — marginais, condicionais,
  E[X], E[Y], Cov(X,Y), Cor(X,Y) a partir de tabela de probabilidade conjunta.
- [ ] **F-06** `rnp_esperanca_condicional(tabela)` — E[Y|X] e Var(Y|X).
- [ ] **F-07** `rnp_lei_grandes_numeros(dist, n_max, ...)` — simula convergência da
  média amostral. **[ggplot]**
- [ ] **F-08** `rnp_tcl_simulacao(dist, n, n_amostras, ...)` — demonstra o TCL com
  histograma das médias + curva normal teórica. **[ggplot]**
- [ ] **F-09** `rnp_distribuicao_normal(fun, media, dp, ...)` — wrapper d/p/q/r + plot.
  **[ggplot]**
- [ ] **F-10** `rnp_distribuicao_exponencial(fun, taxa, ...)`.
- [ ] **F-11** `rnp_distribuicao_gama(fun, forma, taxa, ...)`.
- [ ] **F-12** `rnp_distribuicao_beta(fun, a, b, ...)`.
- [ ] **F-13** `rnp_distribuicao_uniforme(fun, min, max, ...)`.
- [ ] **F-14** `rnp_distribuicao_t(fun, gl, ...)`.
- [ ] **F-15** `rnp_distribuicao_qui_quadrado(fun, gl, ...)`.
- [ ] **F-16** `rnp_distribuicao_f(fun, gl1, gl2, ...)`.
- [ ] **F-17** `rnp_grafico_distribuicao(dist, params)` — densidade/massa + acumulada
  lado a lado para qualquer distribuição. **[ggplot]**

---

## 6. Novas funções — Bloco INFERÊNCIA (3º ano, o coração)

- [ ] **F-18** `rnp_emv(log_veross, inicio, ...)` — estimador de máxima verossimilhança
  genérico via `optim`, com erros-padrão pela Hessiana (informação de Fisher obs.).
- [ ] **F-19** `rnp_metodo_momentos(x, dist)` — estimação por momentos para famílias
  comuns (normal, gama, beta, poisson...).
- [ ] **F-20** `rnp_log_verossimilhanca(x, dist)` — curva/superfície de log-veross. **[ggplot]**
- [ ] **F-21** `rnp_informacao_fisher(log_veross, theta)` — Fisher observada/esperada (numérica).
- [ ] **F-22** `rnp_bootstrap(x, estatistica, B, tipo = c("percentil","bca","normal"))`.
  **[C++ C-05]**
- [ ] **F-23** `rnp_ic_bootstrap(x, estatistica, conf, B)` — IC por reamostragem. **[C++ C-05]**
- [ ] **F-24** `rnp_jackknife(x, estatistica)` — viés e EP por jackknife. **[C++ C-05]**
- [ ] **F-25** `rnp_teste_qui_quadrado(x, y = NULL, p = NULL)` — independência E
  aderência num só lugar (substitui `rnp_associacao`), com V de Cramér e resíduos.
- [ ] **F-26** `rnp_teste_aderencia(x, dist, params)` — qui-quadrado de aderência.
- [ ] **F-27** `rnp_teste_ks(x, y = NULL, dist = NULL)` — Kolmogorov-Smirnov (1 e 2 amostras). **[C++ C-09]**
- [ ] **F-28** `rnp_teste_proporcoes(sucessos, n)` — teste de k proporções.
- [ ] **F-29** `rnp_teste_binomial(x, n, p0)` — binomial exato.
- [ ] **F-30** `rnp_poder_teste(efeito, n, alpha, tipo)` — poder e curva de poder. **[ggplot]**
- [ ] **F-31** `rnp_tamanho_amostra_teste(efeito, poder, alpha, tipo)` — n por poder
  (substitui dependência `pwr`).
- [ ] **F-32** `rnp_tamanho_efeito(...)` — consolidar/expandir o existente em `misc.R`
  (Cohen's d, eta², omega², g de Hedges) sem `effectsize`.

---

## 7. Novas funções — Bloco REGRESSÃO / MODELAGEM (3º ano)

- [ ] **F-33** `rnp_regressao_polinomial(formula, data, grau)` — ajuste polinomial tidy. **[C++ C-03]**
- [ ] **F-34** `rnp_regressao_ridge(formula, data, lambda)` — ridge. **[C++ C-04]**
- [ ] **F-35** `rnp_regressao_lasso(formula, data, lambda)` — lasso (coordinate descent). **[C++ C-04]**
- [ ] **F-36** `rnp_regressao_ponderada(formula, data, pesos)` — WLS. **[C++ C-03]**
- [ ] **F-37** `rnp_regressao_stepwise(formula, data, direcao, criterio)` — seleção AIC/BIC.
- [ ] **F-38** `rnp_regressao_poisson(formula, data)` — GLM Poisson tidy (contagens).
- [ ] **F-39** `rnp_vif(modelo)` — fator de inflação de variância (multicolinearidade).
- [ ] **F-40** `rnp_anova_modelos(modelo1, modelo2, ...)` — comparação de modelos aninhados (teste F).
- [ ] **F-41** `rnp_predicao(modelo, novos_dados, tipo)` — predição + IC/intervalo
  de predição em `tibble`.
- [ ] **F-42** `rnp_grafico_residuos(modelo)` — painel de diagnóstico (resíduos×ajustado,
  QQ, escala-locação, leverage). **[ggplot]**

---

## 8. Novas funções — Bloco MULTIVARIADA (3º ano)

- [ ] **F-43** `rnp_matriz_correlacao(base, metodo)` — matriz de correlação tidy +
  p-valores. **[C++ C-02]**
- [ ] **F-44** `rnp_grafico_correlograma(base)` — correlograma. **[ggplot]** (substitui `corrplot`).
- [ ] **F-45** `rnp_cluster_hierarquico(base, metodo, k)` — `hclust` tidy + corte. **[C++ C-01]**
- [ ] **F-46** `rnp_grafico_dendrograma(cluster_hier)` — dendrograma. **[ggplot]**
- [ ] **F-47** `rnp_silhueta(base, clusters)` — análise de silhueta. **[C++ C-08]**
- [ ] **F-48** `rnp_lda(formula, data)` — análise discriminante linear. **[C++ C-03]**
- [ ] **F-49** `rnp_biplot(rnp_pca_obj)` — biplot de PCA. **[ggplot]**
- [ ] **F-50** `rnp_grafico_dispersao_matriz(base)` — matriz de dispersão (pairs). **[ggplot]**

---

## 9. Novas funções — Bloco SÉRIES TEMPORAIS (sem `forecast`/`tseries`)

- [ ] **F-51** `rnp_media_movel(x, k, tipo)` — média móvel simples/centrada. **[C++ C-07]**
- [ ] **F-52** `rnp_suavizacao_exponencial(x, alpha)` — EWMA / Holt. **[C++ C-07]**
- [ ] **F-53** `rnp_ts_acf(x, lag)` / `rnp_ts_pacf(x, lag)` — reimplementar sem `forecast`. **[C++ C-07]**
- [ ] **F-54** `rnp_ts_diferenciacao(x, d, D, s)` — diferenciação regular e sazonal.
- [ ] **F-55** `rnp_grafico_serie(x)` / `rnp_grafico_acf(...)` — visualização. **[ggplot]**

---

## 9.5. Novas funções — APROFUNDAMENTO da ementa (lacunas identificadas)

> Análise profunda da grade UFPR revelou tópicos ainda descobertos. Blocos abaixo
> completam o "canivete suíço". Backends C++ adicionais em **§4 (C-11…C-14)**.

### Processos estocásticos & simulação (2º–3º ano)
- [ ] **F-56** `rnp_cadeia_markov(P, estado_inicial, n)` — distribuição em n passos,
  distribuição estacionária, classificação de estados. **[C++ C-11]**
- [ ] **F-57** `rnp_passeio_aleatorio(n, p)` — simulação + trajetória. **[ggplot]**
- [ ] **F-58** `rnp_processo_poisson(taxa, t)` — simula processo de Poisson homogêneo.
- [ ] **F-59** `rnp_simula_inversao(f_inv, n)` — método da transformação inversa.
- [ ] **F-60** `rnp_simula_aceitacao_rejeicao(f, g, r_g, M)` — aceitação-rejeição.
- [ ] **F-61** `rnp_monte_carlo(integrando, limites, n)` — integração Monte Carlo + erro. **[C++ C-12]**

### Distribuições adicionais
- [ ] **F-62** `rnp_distribuicao_lognormal(fun, ...)`.
- [ ] **F-63** `rnp_distribuicao_weibull(fun, forma, escala, ...)`.
- [ ] **F-64** `rnp_distribuicao_multinomial(fun, tamanho, probs, ...)`.
- [ ] **F-65** `rnp_ajuste_distribuicao(x, dist)` — ajuste por EMV + qualidade (AIC/BIC/KS).

### Inferência avançada (3º ano)
- [ ] **F-66** `rnp_teste_razao_veross(modelo_completo, modelo_reduzido)` — LRT.
- [ ] **F-67** `rnp_teste_wald(modelo, hipotese)` — teste de Wald.
- [ ] **F-68** `rnp_teste_score(log_veross, theta0)` — teste escore (Rao).
- [ ] **F-69** `rnp_ic_verossimilhanca(log_veross, conf)` — IC por verossimilhança perfilada.
- [ ] **F-70** `rnp_bootstrap_parametrico(x, dist, estatistica, B)` — bootstrap paramétrico. **[C++ C-05]**
- [ ] **F-71** `rnp_teste_permutacao(x, y, estatistica, n_perm)` — teste de permutação. **[C++ C-12]**
- [ ] **F-72** `rnp_bayes_conjugada(familia, priori, dados)` — beta-binomial, gama-poisson,
  normal-normal; posteriori + IC de credibilidade. **[ggplot]**
- [ ] **F-73** `rnp_teste_normalidade(x, metodo)` — unifica Shapiro/Lilliefors/Jarque-Bera/
  Anderson-Darling.
- [ ] **F-74** `rnp_teste_grubbs(x)` — detecção de outlier (Grubbs).
- [ ] **F-75** `rnp_teste_runs(x)` — teste de aleatoriedade (sequências/runs).
- [ ] **F-76** `rnp_teste_sinais(x, y)` — teste dos sinais (não-paramétrico pareado).

### Dados categóricos & concordância
- [ ] **F-77** `rnp_teste_fisher(tabela)` — teste exato de Fisher.
- [ ] **F-78** `rnp_odds_ratio(tabela)` — razão de chances + IC (log).
- [ ] **F-79** `rnp_risco_relativo(tabela)` — risco relativo + IC.
- [ ] **F-80** `rnp_kappa(avaliador1, avaliador2)` — Kappa de Cohen (concordância).

### Delineamento experimental / ANOVA avançada
- [ ] **F-81** `rnp_ancova(formula, data)` — análise de covariância.
- [ ] **F-82** `rnp_anova_medidas_repetidas(formula, data, sujeito)`.
- [ ] **F-83** `rnp_dbc(resposta, tratamento, bloco)` — delineamento em blocos casualizados.
- [ ] **F-84** `rnp_contrastes(modelo, contrastes)` — contrastes ortogonais.

### Regressão avançada
- [ ] **F-85** `rnp_regressao_robusta(formula, data)` — M-estimadores via IRLS. **[C++ C-13]**
- [ ] **F-86** `rnp_regressao_nao_linear(formula, data, inicio)` — `nls` tidy.
- [ ] **F-87** `rnp_elastic_net(formula, data, alpha, lambda)` — elastic net. **[C++ C-04]**
- [ ] **F-88** `rnp_box_cox(formula, data)` — transformação ótima de Box-Cox. **[ggplot]**
- [ ] **F-89** `rnp_regressao_multinomial(formula, data)` — logística multinomial.

### Multivariada avançada
- [ ] **F-90** `rnp_analise_fatorial(base, n_fatores, rotacao)` — análise fatorial.
- [ ] **F-91** `rnp_correspondencia(tabela)` — análise de correspondência (CA). **[C++ C-03]**
- [ ] **F-92** `rnp_hotelling(X, Y)` — T² de Hotelling (médias multivariadas). **[C++ C-03]**
- [ ] **F-93** `rnp_manova(formula, data)` — MANOVA (Wilks/Pillai).
- [ ] **F-94** `rnp_kmedoids(base, k)` — PAM (k-medoids robusto). **[C++ C-08]**
- [ ] **F-95** `rnp_normalidade_multivariada(X)` — teste de Mardia.
- [ ] **F-96** `rnp_correlacao_canonica(X, Y)` — correlação canônica. **[C++ C-03]**

### Séries temporais adicionais
- [ ] **F-97** `rnp_ts_ljung_box(x, lag)` — teste de autocorrelação (Ljung-Box). **[C++ C-07]**
- [ ] **F-98** `rnp_ts_holt_winters(x, sazonalidade)` — suavização sazonal. **[C++ C-07]**
- [ ] **F-99** `rnp_ts_periodograma(x)` — análise espectral. **[ggplot]**

### Pré-processamento / utilitários (substituem `recipes` pontual e `mice`)
- [ ] **F-100** `rnp_padroniza(x)` — z-score; `rnp_normaliza(x)` — min-max.
- [ ] **F-101** `rnp_winsoriza(x, p)` — winsorização de outliers.
- [ ] **F-102** `rnp_imputa(base, metodo)` — média/mediana/moda/kNN (substitui `mice`). **[C++ C-14]**
- [ ] **F-103** `rnp_discretiza(x, k, metodo)` — binning (igual largura/frequência/k-means).
- [ ] **F-104** `rnp_dummy(base)` — codificação one-hot de fatores.

---

## 10. Aprimoramento das funções existentes (FASE 3)

- [ ] **R-01** Migrar `rnp_distancia`, `rnp_correlacao_teste`, `rnp_pca` para usar
  backends C++ onde houver ganho.
- [ ] **R-02** (DECISÃO: quebra limpa em 3.0.0) Remover de vez as funções legadas
  redundantes — ver lista em **§3.5**. Sem `.Deprecated`: como é major, o `NEWS.md`
  documenta a migração. Nada de aliases que carregam dívida técnica.
- [ ] **R-03** Unificar tema/paleta gráfica e revisar os 8 gráficos atuais para
  consistência visual e rótulos PT-BR.
- [ ] **R-04** `rnp_options()` — expandir opções globais (dígitos, tema, idioma de msgs).
- [ ] **R-05** Revisar `R/inep.R` para usar `readr`/`utils` e tratar timeout/erros de rede.
- [ ] **R-06** Reescrever a vinheta `vignettes/rnp.Rmd` como tour didático organizado
  pela progressão 1º→3º ano, exibindo as novas funções.
- [ ] **R-07** Atualizar `README.md` com índice de funções por ementa e badge de cobertura.

---

## 11. Critérios de aceite finais (Definition of Done do projeto)

- [ ] **A-01** `devtools::check()` → **0 ERROS, 0 WARNINGS**, NOTES justificadas.
- [ ] **A-02** `≥ 100` funções novas exportadas (contadas em F-01…F-104), bem além
  das 50 exigidas.
- [ ] **A-03** `src/` compila em Linux/Win/Mac; backends testados vs. referência.
- [ ] **A-04** Cobertura de testes `≥ 90%` (`covr::package_coverage()`).
- [ ] **A-05** Nenhuma dependência fora da lista do §0.3 (`grep` limpo).
- [ ] **A-06** Toda documentação e mensagens em PT-BR; `R CMD check` roda todos os exemplos.
- [ ] **A-07** Vinheta compila e README atualizado.
- [ ] **A-08** Todas as caixas `[ ]` deste arquivo marcadas `[x]`.

---

## 12. Ordem de execução — FATIAS VERTICAIS por ementa (DECISÃO do usuário)

Em vez de fases horizontais, cada **fatia** entrega uma área 100% pronta
(funções + backend C++ + testes + docs + exemplos), já utilizável e checável.

```
FATIA 0 — Fundação (pré-requisito)
   §1 infra Rcpp/DESCRIPTION · §2 remover deps · §3 bugs · §3.5 remoções
   Backends transversais: C-01 distâncias, C-02 cov/cor, C-03 OLS, C-06 momentos, C-09 empírico

FATIA 1 — Descritiva & Probabilidade   → F-01..F-17, F-56..F-65   (+ C-11, C-12)
FATIA 2 — Inferência                    → F-18..F-32, F-66..F-76   (+ C-05)
FATIA 3 — Regressão & Modelagem         → F-33..F-42, F-85..F-89   (+ C-04, C-13)
FATIA 4 — Multivariada                  → F-43..F-50, F-90..F-96   (+ C-08)
FATIA 5 — Categóricos & Experimental    → F-77..F-84
FATIA 6 — Séries Temporais              → F-51..F-55, F-97..F-99   (+ C-07)
FATIA 7 — Pré-processamento & Utilitários → F-100..F-104           (+ C-14)

FECHAMENTO
   §10 aprimorar existentes (R-01..R-07) · §11 check final · cobertura · vinheta · README · NEWS.md
```

Regra: **uma fatia só fecha quando passa em `devtools::check()` sem ERROR/WARNING**
e todos os seus testes verdes. Só então abre a próxima.

**Totais planejados:**
- **104 funções novas** (F-01…F-104) — alvo de ≥100 exportadas.
- **14 backends C++** (`src/*.cpp`, C-01…C-14).
- **~12 funções legadas removidas/consolidadas** (D-01…D-08).

> Próximo passo após aprovação: iniciar **FATIA 0 (Fundação)**.
