# Aprendizado de maquina com tidymodels (FATIA 11).
# Os pacotes do tidymodels e os engines sao Suggests, carregados sob demanda.

.guarda_tidymodels <- function(funcao) {
  for (p in c("rsample", "recipes", "parsnip", "workflows", "tune", "yardstick")) {
    precisa_pacote(p, funcao)
  }
}

.modo_parsnip <- function(modo) {
  modo <- rlang::arg_match(modo, c("classificacao", "regressao"))
  if (modo == "classificacao") "classification" else "regression"
}

#' Particao treino/teste
#'
#' Divide os dados em conjuntos de treino e teste ([rsample::initial_split()]).
#'
#' @param data data.frame.
#' @param prop Proporcao para treino.
#' @param estrato Nome (string) da variavel de estratificacao. Opcional.
#' @param seed Inteiro. Semente.
#'
#' @return Um objeto de particao `rsplit` (use [rnp_ml_ajustar()] ou
#'   `rsample::training()`/`testing()`).
#'
#' @examples
#' \donttest{
#' rnp_ml_particao(iris, prop = 0.75, estrato = "Species")
#' }
#' @family ml
#' @export
rnp_ml_particao <- function(data, prop = 0.75, estrato = NULL, seed = 42L) {
  .guarda_tidymodels("rnp_ml_particao")
  set.seed(seed)
  args <- list(data = data, prop = prop)
  if (!is.null(estrato)) args$strata <- estrato
  do.call(rsample::initial_split, args)
}

#' Reamostras de validacao cruzada
#'
#' Cria as particoes de validacao cruzada k-fold ([rsample::vfold_cv()]).
#'
#' @param data data.frame.
#' @param v Numero de folds.
#' @param repeticoes Numero de repeticoes.
#' @param estrato Nome (string) da variavel de estratificacao. Opcional.
#' @param seed Inteiro.
#'
#' @return Um objeto `rset` de reamostras.
#'
#' @examples
#' \donttest{
#' rnp_ml_cv(iris, v = 5, estrato = "Species")
#' }
#' @family ml
#' @export
rnp_ml_cv <- function(data, v = 10, repeticoes = 1, estrato = NULL, seed = 42L) {
  .guarda_tidymodels("rnp_ml_cv")
  set.seed(seed)
  args <- list(data = data, v = v, repeats = repeticoes)
  if (!is.null(estrato)) args$strata <- estrato
  do.call(rsample::vfold_cv, args)
}

#' Receita de pre-processamento
#'
#' Constroi uma `recipe` com passos comuns, indicados por nomes em portugues.
#'
#' @param formula Formula `y ~ .`.
#' @param data data.frame.
#' @param passos Vetor de passos: `"normalizar"`, `"dummy"`, `"imputar_media"`,
#'   `"imputar_moda"`, `"zero_var"`, `"correlacao"`, `"boxcox"`.
#'
#' @return Objeto `recipe`.
#'
#' @examples
#' \donttest{
#' rnp_ml_receita(Species ~ ., iris, passos = c("normalizar"))
#' }
#' @family ml
#' @export
rnp_ml_receita <- function(formula, data, passos = NULL) {
  .guarda_tidymodels("rnp_ml_receita")
  rec <- recipes::recipe(formula, data = data)
  for (p in passos) {
    rec <- switch(p,
      normalizar    = recipes::step_normalize(rec, recipes::all_numeric_predictors()),
      dummy         = recipes::step_dummy(rec, recipes::all_nominal_predictors()),
      imputar_media = recipes::step_impute_mean(rec, recipes::all_numeric_predictors()),
      imputar_moda  = recipes::step_impute_mode(rec, recipes::all_nominal_predictors()),
      zero_var      = recipes::step_zv(rec, recipes::all_predictors()),
      correlacao    = recipes::step_corr(rec, recipes::all_numeric_predictors()),
      boxcox        = recipes::step_BoxCox(rec, recipes::all_numeric_predictors()),
      rlang::abort("Passo '{p}' desconhecido."))
  }
  rec
}

#' Especificacao de arvore de decisao
#'
#' @param modo `"classificacao"` ou `"regressao"`.
#' @param custo_complexidade Parametro de poda (cp). Pode ser `tune()`.
#' @param profundidade_max Profundidade maxima.
#' @param min_n Minimo de observacoes por no.
#' @return Especificacao `model_spec` (engine rpart).
#' @examples
#' \donttest{ rnp_ml_arvore("classificacao") }
#' @family ml
#' @export
rnp_ml_arvore <- function(modo = c("classificacao", "regressao"),
                          custo_complexidade = 0.01, profundidade_max = 30,
                          min_n = 2) {
  .guarda_tidymodels("rnp_ml_arvore"); precisa_pacote("rpart", "rnp_ml_arvore")
  spec <- rlang::inject(parsnip::decision_tree(
    cost_complexity = !!custo_complexidade,
    tree_depth = !!profundidade_max, min_n = !!min_n))
  parsnip::set_mode(parsnip::set_engine(spec, "rpart"), .modo_parsnip(modo))
}

#' Especificacao de floresta aleatoria
#'
#' @inheritParams rnp_ml_arvore
#' @param arvores Numero de arvores.
#' @param mtry Variaveis sorteadas por divisao. `NULL` usa o padrao.
#' @return Especificacao `model_spec` (engine ranger).
#' @examples
#' \donttest{ rnp_ml_floresta("classificacao") }
#' @family ml
#' @export
rnp_ml_floresta <- function(modo = c("classificacao", "regressao"),
                            arvores = 500, mtry = NULL, min_n = 5) {
  .guarda_tidymodels("rnp_ml_floresta"); precisa_pacote("ranger", "rnp_ml_floresta")
  spec <- rlang::inject(parsnip::rand_forest(
    trees = !!arvores, mtry = !!mtry, min_n = !!min_n))
  parsnip::set_mode(
    parsnip::set_engine(spec, "ranger", importance = "impurity"),
    .modo_parsnip(modo))
}

#' Especificacao de gradient boosting
#'
#' @inheritParams rnp_ml_arvore
#' @param arvores Numero de arvores.
#' @param profundidade Profundidade das arvores.
#' @param taxa_aprendizado Taxa de aprendizado.
#' @return Especificacao `model_spec` (engine xgboost).
#' @examples
#' \donttest{ rnp_ml_boosting("regressao") }
#' @family ml
#' @export
rnp_ml_boosting <- function(modo = c("classificacao", "regressao"),
                            arvores = 500, profundidade = 6,
                            taxa_aprendizado = 0.1) {
  .guarda_tidymodels("rnp_ml_boosting"); precisa_pacote("xgboost", "rnp_ml_boosting")
  spec <- rlang::inject(parsnip::boost_tree(
    trees = !!arvores, tree_depth = !!profundidade,
    learn_rate = !!taxa_aprendizado))
  parsnip::set_mode(parsnip::set_engine(spec, "xgboost"), .modo_parsnip(modo))
}

#' Especificacao de k-vizinhos
#'
#' @inheritParams rnp_ml_arvore
#' @param vizinhos Numero de vizinhos.
#' @return Especificacao `model_spec` (engine kknn).
#' @examples
#' \donttest{ rnp_ml_knn("classificacao") }
#' @family ml
#' @export
rnp_ml_knn <- function(modo = c("classificacao", "regressao"), vizinhos = 5) {
  .guarda_tidymodels("rnp_ml_knn"); precisa_pacote("kknn", "rnp_ml_knn")
  spec <- rlang::inject(parsnip::nearest_neighbor(neighbors = !!vizinhos))
  parsnip::set_mode(parsnip::set_engine(spec, "kknn"), .modo_parsnip(modo))
}

#' Especificacao de SVM (kernel radial)
#'
#' @inheritParams rnp_ml_arvore
#' @param custo Parametro de custo C.
#' @param sigma Parametro do kernel RBF. `NULL` usa o padrao.
#' @return Especificacao `model_spec` (engine kernlab).
#' @examples
#' \donttest{ rnp_ml_svm("classificacao") }
#' @family ml
#' @export
rnp_ml_svm <- function(modo = c("classificacao", "regressao"), custo = 1,
                       sigma = NULL) {
  .guarda_tidymodels("rnp_ml_svm"); precisa_pacote("kernlab", "rnp_ml_svm")
  spec <- rlang::inject(parsnip::svm_rbf(cost = !!custo, rbf_sigma = !!sigma))
  parsnip::set_mode(parsnip::set_engine(spec, "kernlab"), .modo_parsnip(modo))
}

#' Especificacao de modelo linear regularizado (glmnet)
#'
#' Lasso/ridge/elastic net via glmnet (`mistura = 1` lasso; `0` ridge).
#'
#' @inheritParams rnp_ml_arvore
#' @param penalidade Forca da penalizacao (lambda). Pode ser `tune()`.
#' @param mistura Mistura L1/L2 (alpha) em \[0, 1\].
#' @return Especificacao `model_spec` (engine glmnet).
#' @examples
#' \donttest{ rnp_ml_regularizada("regressao", penalidade = 0.1) }
#' @family ml
#' @export
rnp_ml_regularizada <- function(modo = c("classificacao", "regressao"),
                                penalidade = 0.1, mistura = 1) {
  .guarda_tidymodels("rnp_ml_regularizada")
  precisa_pacote("glmnet", "rnp_ml_regularizada")
  modo_p <- .modo_parsnip(modo)
  spec <- if (modo_p == "regression") {
    rlang::inject(parsnip::linear_reg(penalty = !!penalidade, mixture = !!mistura))
  } else {
    rlang::inject(parsnip::logistic_reg(penalty = !!penalidade, mixture = !!mistura))
  }
  parsnip::set_mode(parsnip::set_engine(spec, "glmnet"), modo_p)
}

#' Ajusta e avalia um modelo na particao
#'
#' Treina o modelo no conjunto de treino e avalia no teste
#' ([tune::last_fit()]).
#'
#' @param spec Especificacao de modelo (`rnp_ml_*`) ou `recipe`/`workflow`.
#' @param formula Formula do modelo.
#' @param split Particao de [rnp_ml_particao()].
#' @param digits Inteiro.
#'
#' @return Uma lista com `metricas` (no teste), `predicoes`, `modelo` (workflow
#'   ajustado) e `resultado`.
#'
#' @examples
#' \donttest{
#' sp <- rnp_ml_particao(iris, estrato = "Species")
#' rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)$metricas
#' }
#' @family ml
#' @export
rnp_ml_ajustar <- function(spec, formula, split, digits = 4L) {
  .guarda_tidymodels("rnp_ml_ajustar")
  wf <- workflows::add_formula(workflows::add_model(workflows::workflow(), spec),
                               formula)
  res <- tune::last_fit(wf, split)
  list(
    metricas  = dplyr::mutate(tune::collect_metrics(res),
                              dplyr::across(where(is.numeric), ~ arredonda(.x, digits))),
    predicoes = tune::collect_predictions(res),
    modelo    = tune::extract_workflow(res),
    resultado = res)
}

#' Tunagem de hiperparametros
#'
#' Busca em grade os melhores hiperparametros por validacao cruzada
#' ([tune::tune_grid()]). A `spec` deve ter parametros marcados com
#' `hardhat::tune()`.
#'
#' @param spec Especificacao com parametros a tunar.
#' @param formula Formula.
#' @param reamostras Reamostras de [rnp_ml_cv()].
#' @param grade Tamanho da grade (inteiro) ou `data.frame` de candidatos.
#' @param metrica Metrica para ordenar (`NULL` usa a primeira disponivel).
#' @param digits Inteiro.
#'
#' @return Uma lista com `melhores` (tibble), `melhor_param` e `resultado`.
#'
#' @examples
#' \donttest{
#' sp <- rnp_ml_arvore("classificacao", custo_complexidade = hardhat::tune())
#' rnp_ml_tunagem(sp, Species ~ ., rnp_ml_cv(iris, v = 3), grade = 5)$melhores
#' }
#' @family ml
#' @export
rnp_ml_tunagem <- function(spec, formula, reamostras, grade = 10,
                           metrica = NULL, digits = 4L) {
  .guarda_tidymodels("rnp_ml_tunagem")
  wf <- workflows::add_formula(workflows::add_model(workflows::workflow(), spec),
                               formula)
  res <- tune::tune_grid(wf, resamples = reamostras, grid = grade)
  m <- tune::collect_metrics(res)
  metrica <- metrica %||% m$.metric[1L]
  melhores <- tune::show_best(res, metric = metrica, n = 5)
  list(
    melhores = dplyr::mutate(melhores, dplyr::across(where(is.numeric),
                                                     ~ arredonda(.x, digits))),
    melhor_param = tune::select_best(res, metric = metrica),
    resultado = res)
}

#' Compara modelos por validacao cruzada
#'
#' Avalia varias especificacoes por reamostragem e compara as metricas.
#'
#' @param specs Lista nomeada de especificacoes de modelo.
#' @param formula Formula.
#' @param reamostras Reamostras de [rnp_ml_cv()].
#' @param digits Inteiro.
#'
#' @return Uma lista com `tabela` (tibble) e `grafico` (`ggplot`).
#'
#' @examples
#' \donttest{
#' specs <- list(arvore = rnp_ml_arvore("classificacao"))
#' rnp_ml_comparar(specs, Species ~ ., rnp_ml_cv(iris, v = 3))$tabela
#' }
#' @family ml
#' @export
rnp_ml_comparar <- function(specs, formula, reamostras, digits = 4L) {
  .guarda_tidymodels("rnp_ml_comparar")
  tab <- purrr::imap_dfr(specs, function(spec, nome) {
    wf <- workflows::add_formula(workflows::add_model(workflows::workflow(), spec),
                                 formula)
    r <- tune::fit_resamples(wf, resamples = reamostras)
    m <- tune::collect_metrics(r)
    m$modelo <- nome
    m
  })
  tab <- dplyr::mutate(tab, dplyr::across(where(is.numeric),
                                          ~ arredonda(.x, digits)))
  g <- ggplot2::ggplot(tab, ggplot2::aes(.data$modelo, .data$mean,
                                         fill = .data$modelo)) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::facet_wrap(~ .data$.metric, scales = "free_y") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Comparacao de modelos", x = NULL, y = "Media (CV)") +
    ggplot2::guides(fill = "none")
  list(tabela = tab, grafico = g)
}

#' Predicao com modelo ajustado
#'
#' @param modelo Workflow ajustado (de [rnp_ml_ajustar()]) ou parsnip fit.
#' @param novos_dados data.frame.
#' @param tipo `"classe"`, `"probabilidade"` ou `"numerico"`.
#'
#' @return tibble de predicoes.
#'
#' @examples
#' \donttest{
#' sp <- rnp_ml_particao(iris, estrato = "Species")
#' fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
#' rnp_ml_prever(fit$modelo, iris[1:5, ], tipo = "classe")
#' }
#' @family ml
#' @export
rnp_ml_prever <- function(modelo, novos_dados,
                          tipo = c("classe", "probabilidade", "numerico")) {
  tipo <- rlang::arg_match(tipo)
  type <- switch(tipo, classe = "class", probabilidade = "prob", numerico = "numeric")
  stats::predict(modelo, new_data = novos_dados, type = type)
}

#' Importancia de variaveis
#'
#' Extrai a importancia das variaveis do modelo (engines rpart e ranger).
#'
#' @param modelo Workflow ajustado ou parsnip fit.
#' @param digits Inteiro.
#'
#' @return tibble com `variavel` e `importancia`, em ordem decrescente.
#'
#' @examples
#' \donttest{
#' sp <- rnp_ml_particao(iris, estrato = "Species")
#' fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
#' rnp_ml_importancia(fit$modelo)
#' }
#' @family ml
#' @export
rnp_ml_importancia <- function(modelo, digits = 4L) {
  pf <- if (inherits(modelo, "workflow")) hardhat::extract_fit_parsnip(modelo) else modelo
  eng <- pf$fit
  imp <- if (!is.null(eng$variable.importance)) {
    eng$variable.importance                       # rpart
  } else if (!is.null(eng$importance)) {
    eng$importance                                # ranger (variable.importance) fallback
  } else {
    rlang::abort("Importancia nao disponivel para este engine.")
  }
  tibble::tibble(variavel = names(imp),
                 importancia = arredonda(unname(as.numeric(imp)), digits)) |>
    dplyr::arrange(dplyr::desc(.data$importancia))
}
