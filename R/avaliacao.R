# Avaliacao de modelos preditivos (FATIA 12). Implementacao propria (base R).

# Ordena escores e rotulos positivos por escore decrescente.
.ordena_escore <- function(observado, escore, positivo) {
  obs <- if (is.null(positivo)) as.numeric(observado) > 0 else
    as.character(observado) == as.character(positivo)
  ord <- order(escore, decreasing = TRUE)
  list(y = obs[ord], escore = escore[ord], P = sum(obs), N = sum(!obs))
}

#' Metricas de classificacao
#'
#' Calcula metricas a partir de classes preditas. Para problemas binarios,
#' reporta acuracia, precisao, revocacao (sensibilidade), especificidade,
#' F1/F-beta, coeficiente de correlacao de Matthews (MCC) e acuracia balanceada.
#' Para multiclasse, usa media macro.
#'
#' @param observado Vetor de classes observadas.
#' @param predito Vetor de classes preditas.
#' @param positivo Classe positiva (binario). Default: primeiro nivel.
#' @param beta Peso da revocacao no F-beta.
#' @param digits Inteiro.
#'
#' @return tibble com `metrica` e `valor`.
#'
#' @examples
#' obs <- factor(c("sim","sim","nao","nao","sim","nao"))
#' pred <- factor(c("sim","nao","nao","nao","sim","sim"))
#' rnp_metricas_classificacao(obs, pred, positivo = "sim")
#' @family avaliacao
#' @export
rnp_metricas_classificacao <- function(observado, predito, positivo = NULL,
                                       beta = 1, digits = 4L) {
  obs <- as.factor(observado)
  pred <- factor(predito, levels = levels(obs))
  niveis <- levels(obs)
  N <- length(obs)
  if (length(niveis) == 2L) {
    pos <- positivo %||% niveis[1L]
    neg <- setdiff(niveis, pos)
    TP <- sum(obs == pos & pred == pos); FP <- sum(obs == neg & pred == pos)
    FN <- sum(obs == pos & pred == neg); TN <- sum(obs == neg & pred == neg)
    prec <- TP / (TP + FP); rec <- TP / (TP + FN); spec <- TN / (TN + FP)
    fbeta <- (1 + beta^2) * prec * rec / (beta^2 * prec + rec)
    mcc_den <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    mcc <- if (mcc_den == 0) 0 else (TP * TN - FP * FN) / mcc_den
    vals <- c(acuracia = (TP + TN) / N, precisao = prec, revocacao = rec,
              especificidade = spec, f1 = 2 * prec * rec / (prec + rec),
              f_beta = fbeta, mcc = mcc,
              acuracia_balanceada = (rec + spec) / 2)
  } else {
    prec_c <- rec_c <- numeric(length(niveis))
    for (i in seq_along(niveis)) {
      cl <- niveis[i]
      TP <- sum(obs == cl & pred == cl); FP <- sum(obs != cl & pred == cl)
      FN <- sum(obs == cl & pred != cl)
      prec_c[i] <- if (TP + FP == 0) 0 else TP / (TP + FP)
      rec_c[i]  <- if (TP + FN == 0) 0 else TP / (TP + FN)
    }
    f1_c <- ifelse(prec_c + rec_c == 0, 0, 2 * prec_c * rec_c / (prec_c + rec_c))
    vals <- c(acuracia = mean(obs == pred),
              precisao_macro = mean(prec_c), revocacao_macro = mean(rec_c),
              f1_macro = mean(f1_c))
  }
  tibble::tibble(metrica = names(vals), valor = arredonda(unname(vals), digits))
}

#' Metricas de regressao
#'
#' @param observado Vetor numerico observado.
#' @param predito Vetor numerico predito.
#' @param digits Inteiro.
#'
#' @return tibble com `rmse`, `mae`, `mape`, `r2`, `rmse_relativo`.
#'
#' @examples
#' rnp_metricas_regressao(mtcars$mpg, predict(lm(mpg ~ wt, mtcars)))
#' @family avaliacao
#' @export
rnp_metricas_regressao <- function(observado, predito, digits = 4L) {
  e <- observado - predito
  rmse <- sqrt(mean(e^2)); mae <- mean(abs(e))
  mape <- mean(abs(e / observado)[is.finite(e / observado)]) * 100
  r2 <- 1 - sum(e^2) / sum((observado - mean(observado))^2)
  tibble::tibble(
    metrica = c("rmse", "mae", "mape", "r2", "rmse_relativo"),
    valor = arredonda(c(rmse, mae, mape, r2, rmse / mean(observado)), digits))
}

#' Curva de lift
#'
#' @param observado Vetor binario (ou fator de 2 niveis).
#' @param escore Probabilidades/escores preditos.
#' @param positivo Classe positiva.
#' @param n_grupos Numero de faixas (deciles por padrao).
#' @param digits Inteiro.
#'
#' @return Uma lista com `tabela` (tibble) e `grafico` (`ggplot`).
#'
#' @examples
#' set.seed(1); y <- rbinom(200, 1, 0.3); s <- y * 0.5 + runif(200)
#' rnp_curva_lift(y, s, positivo = 1)$tabela
#' @family avaliacao
#' @export
rnp_curva_lift <- function(observado, escore, positivo = NULL, n_grupos = 10,
                           digits = 4L) {
  o <- .ordena_escore(observado, escore, positivo)
  n <- length(o$y); taxa_base <- o$P / n
  idx <- ceiling(seq_len(n) / n * n_grupos)
  taxa <- vapply(seq_len(n_grupos), function(g) mean(o$y[idx == g]), numeric(1))
  tab <- tibble::tibble(grupo = seq_len(n_grupos), taxa_grupo = taxa)
  tab$lift <- arredonda(tab$taxa_grupo / taxa_base, digits)
  tab$taxa_grupo <- arredonda(tab$taxa_grupo, digits)
  g <- ggplot2::ggplot(tab, ggplot2::aes(.data$grupo, .data$lift)) +
    ggplot2::geom_col(fill = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Curva de lift", x = "Faixa (escore decrescente)", y = "Lift")
  list(tabela = tab, grafico = g)
}

#' Curva de ganho acumulado
#'
#' @inheritParams rnp_curva_lift
#' @return Uma lista com `tabela` e `grafico` (`ggplot`).
#' @examples
#' set.seed(1); y <- rbinom(200, 1, 0.3); s <- y * 0.5 + runif(200)
#' rnp_curva_ganho(y, s, positivo = 1)$grafico
#' @family avaliacao
#' @export
rnp_curva_ganho <- function(observado, escore, positivo = NULL, digits = 4L) {
  o <- .ordena_escore(observado, escore, positivo)
  n <- length(o$y)
  tab <- tibble::tibble(
    prop_populacao = arredonda(seq_len(n) / n, digits),
    prop_positivos = arredonda(cumsum(o$y) / o$P, digits))
  g <- ggplot2::ggplot(tab, ggplot2::aes(.data$prop_populacao, .data$prop_positivos)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 0.9) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Curva de ganho acumulado",
                  x = "Proporcao da populacao", y = "Proporcao de positivos captados")
  list(tabela = tab, grafico = g)
}

#' Calibracao de probabilidades
#'
#' Compara probabilidades preditas com frequencias observadas (por faixa) e
#' aplica o teste de Hosmer-Lemeshow.
#'
#' @param observado Vetor binario.
#' @param prob Probabilidades preditas em \[0, 1\].
#' @param positivo Classe positiva.
#' @param n_grupos Numero de faixas.
#' @param digits Inteiro.
#'
#' @return Uma lista com `tabela`, `hosmer_lemeshow` (tibble) e `grafico`.
#'
#' @examples
#' set.seed(1); p <- runif(300); y <- rbinom(300, 1, p)
#' rnp_calibracao(y, p, positivo = 1)$hosmer_lemeshow
#' @family avaliacao
#' @export
rnp_calibracao <- function(observado, prob, positivo = NULL, n_grupos = 10,
                           digits = 4L) {
  y <- if (is.null(positivo)) as.numeric(observado) > 0 else
    as.character(observado) == as.character(positivo)
  faixa <- cut(prob, breaks = stats::quantile(prob, seq(0, 1, length.out = n_grupos + 1)),
               include.lowest = TRUE, labels = FALSE)
  tab <- tibble::tibble(
    grupo = sort(unique(faixa)),
    prob_media = vapply(sort(unique(faixa)), function(g) mean(prob[faixa == g]), numeric(1)),
    freq_obs   = vapply(sort(unique(faixa)), function(g) mean(y[faixa == g]), numeric(1)),
    n          = vapply(sort(unique(faixa)), function(g) sum(faixa == g), integer(1)))
  # Hosmer-Lemeshow
  O1 <- vapply(tab$grupo, function(g) sum(y[faixa == g]), numeric(1))
  E1 <- vapply(tab$grupo, function(g) sum(prob[faixa == g]), numeric(1))
  hl <- sum((O1 - E1)^2 / (E1 * (1 - E1 / tab$n)))
  gl <- length(tab$grupo) - 2L
  hl_tb <- tibble::tibble(
    estatistica = arredonda(hl, digits), gl = gl,
    p_valor = arredonda(stats::pchisq(hl, gl, lower.tail = FALSE), digits))
  tab <- dplyr::mutate(tab, dplyr::across(c("prob_media", "freq_obs"),
                                          ~ arredonda(.x, digits)))
  g <- ggplot2::ggplot(tab, ggplot2::aes(.data$prob_media, .data$freq_obs)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1)) +
    ggplot2::geom_point(color = rnp_paleta_rnp("rnp_qual", 1), size = 2) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Calibracao", x = "Probabilidade media predita",
                  y = "Frequencia observada")
  list(tabela = tab, hosmer_lemeshow = hl_tb, grafico = g)
}

#' Escore de Brier
#'
#' @param observado Vetor binario.
#' @param prob Probabilidades preditas.
#' @param positivo Classe positiva.
#' @param digits Inteiro.
#'
#' @return tibble com `brier` e `brier_referencia` (modelo que prediz a
#'   prevalencia).
#'
#' @examples
#' set.seed(1); p <- runif(300); y <- rbinom(300, 1, p)
#' rnp_brier(y, p, positivo = 1)
#' @family avaliacao
#' @export
rnp_brier <- function(observado, prob, positivo = NULL, digits = 4L) {
  y <- if (is.null(positivo)) as.numeric(observado) > 0 else
    as.character(observado) == as.character(positivo)
  brier <- mean((prob - y)^2)
  ref <- mean((mean(y) - y)^2)
  tibble::tibble(
    brier = arredonda(brier, digits),
    brier_referencia = arredonda(ref, digits),
    escore_habilidade = arredonda(1 - brier / ref, digits))
}

#' Estatistica KS de classificador
#'
#' Maxima distancia entre as distribuicoes acumuladas dos escores das classes
#' positiva e negativa.
#'
#' @inheritParams rnp_brier
#' @param escore Escores/probabilidades.
#' @return Uma lista com `ks` (escalar) e `grafico` (`ggplot`).
#' @examples
#' set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.4 + runif(300)
#' rnp_ks_classificador(y, s, positivo = 1)$ks
#' @family avaliacao
#' @export
rnp_ks_classificador <- function(observado, escore, positivo = NULL, digits = 4L) {
  y <- if (is.null(positivo)) as.numeric(observado) > 0 else
    as.character(observado) == as.character(positivo)
  grade <- sort(unique(escore))
  Fpos <- stats::ecdf(escore[y])(grade)
  Fneg <- stats::ecdf(escore[!y])(grade)
  dif <- abs(Fneg - Fpos)
  ks <- max(dif)
  d <- tibble::tibble(escore = rep(grade, 2),
                      cdf = c(Fpos, Fneg),
                      classe = rep(c("positivo", "negativo"), each = length(grade)))
  g <- ggplot2::ggplot(d, ggplot2::aes(.data$escore, .data$cdf, color = .data$classe)) +
    ggplot2::geom_line(linewidth = 0.9) +
    rnp_tema_rnp() +
    ggplot2::labs(title = glue::glue("KS = {round(ks, 3)}"),
                  x = "Escore", y = "Distribuicao acumulada", color = NULL)
  list(ks = arredonda(ks, digits), grafico = g)
}

#' Curva precisao-revocacao
#'
#' @inheritParams rnp_curva_lift
#' @return Uma lista com `curva` (tibble), `auc_pr` (precisao media) e `grafico`.
#' @examples
#' set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.4 + runif(300)
#' rnp_curva_precisao_revocacao(y, s, positivo = 1)$auc_pr
#' @family avaliacao
#' @export
rnp_curva_precisao_revocacao <- function(observado, escore, positivo = NULL,
                                         digits = 4L) {
  o <- .ordena_escore(observado, escore, positivo)
  tp <- cumsum(o$y); fp <- cumsum(!o$y)
  precisao <- tp / (tp + fp)
  revocacao <- tp / o$P
  auc_pr <- sum(diff(c(0, revocacao)) * precisao)   # precisao media
  curva <- tibble::tibble(
    revocacao = arredonda(revocacao, digits),
    precisao = arredonda(precisao, digits))
  g <- ggplot2::ggplot(curva, ggplot2::aes(.data$revocacao, .data$precisao)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = o$P / length(o$y), linetype = "dashed", color = "grey50") +
    ggplot2::ylim(0, 1) + rnp_tema_rnp() +
    ggplot2::labs(title = glue::glue("AUC-PR = {round(auc_pr, 3)}"),
                  x = "Revocacao", y = "Precisao")
  list(curva = curva, auc_pr = arredonda(auc_pr, digits), grafico = g)
}

# AUC e valores de colocacao (placement values) de DeLong para um escore.
.delong_componentes <- function(y, escore) {
  x <- escore[y]; z <- escore[!y]
  m <- length(x); n <- length(z)
  # V10[i] = (1/n) sum_j psi(x_i, z_j); V01[j] = (1/m) sum_i psi(x_i, z_j)
  V10 <- vapply(x, function(xi) mean((xi > z) + 0.5 * (xi == z)), numeric(1))
  V01 <- vapply(z, function(zj) mean((x > zj) + 0.5 * (x == zj)), numeric(1))
  auc <- mean(V10)
  list(auc = auc, V10 = V10, V01 = V01, m = m, n = n)
}

#' Comparacao de duas curvas ROC (teste de DeLong)
#'
#' Compara as areas sob duas curvas ROC obtidas nos mesmos individuos (modelos
#' correlacionados), pelo teste de DeLong.
#'
#' @param observado Vetor binario.
#' @param escore1,escore2 Escores dos dois modelos.
#' @param positivo Classe positiva.
#' @param digits Inteiro.
#'
#' @return tibble com `auc1`, `auc2`, `diferenca`, `z`, `p_valor`.
#'
#' @examples
#' set.seed(1); y <- rbinom(300, 1, 0.4)
#' s1 <- y * 0.6 + runif(300); s2 <- y * 0.2 + runif(300)
#' rnp_comparar_roc(y, s1, s2, positivo = 1)
#' @family avaliacao
#' @export
rnp_comparar_roc <- function(observado, escore1, escore2, positivo = NULL,
                             digits = 4L) {
  y <- if (is.null(positivo)) as.numeric(observado) > 0 else
    as.character(observado) == as.character(positivo)
  c1 <- .delong_componentes(y, escore1)
  c2 <- .delong_componentes(y, escore2)
  m <- c1$m; n <- c1$n
  V10 <- cbind(c1$V10, c2$V10); V01 <- cbind(c1$V01, c2$V01)
  S10 <- stats::cov(V10); S01 <- stats::cov(V01)
  S <- S10 / m + S01 / n
  dif <- c1$auc - c2$auc
  var_dif <- S[1, 1] + S[2, 2] - 2 * S[1, 2]
  z <- dif / sqrt(var_dif)
  tibble::tibble(
    auc1 = arredonda(c1$auc, digits), auc2 = arredonda(c2$auc, digits),
    diferenca = arredonda(dif, digits), z = arredonda(z, digits),
    p_valor = arredonda(2 * stats::pnorm(-abs(z)), digits))
}

#' Acuracia diagnostica
#'
#' Metricas de avaliacao de testes diagnosticos: sensibilidade, especificidade,
#' valores preditivos e razoes de verossimilhanca.
#'
#' @param observado Condicao verdadeira (binaria).
#' @param predito Resultado do teste (binario).
#' @param positivo Nivel "doente"/positivo.
#' @param digits Inteiro.
#'
#' @return tibble com `metrica` e `valor`.
#'
#' @examples
#' obs <- c("D","D","D","S","S","S","S","D")
#' tst <- c("+","+","-","-","-","+","-","+")
#' rnp_acuracia_diagnostica(obs, tst, positivo = "D")
#' @family avaliacao
#' @export
rnp_acuracia_diagnostica <- function(observado, predito, positivo = NULL,
                                     digits = 4L) {
  obs <- as.factor(observado)
  pos_obs <- positivo %||% levels(obs)[1L]
  d <- as.character(observado) == as.character(pos_obs)   # doente
  niveis_t <- unique(as.character(predito))
  pos_t <- niveis_t[1L]
  if (!is.null(positivo) && positivo %in% niveis_t) pos_t <- positivo
  t <- as.character(predito) == pos_t                     # teste positivo
  TP <- sum(d & t); FP <- sum(!d & t); FN <- sum(d & !t); TN <- sum(!d & !t)
  sens <- TP / (TP + FN); espec <- TN / (TN + FP)
  vals <- c(sensibilidade = sens, especificidade = espec,
            vpp = TP / (TP + FP), vpn = TN / (TN + FN),
            razao_veross_pos = sens / (1 - espec),
            razao_veross_neg = (1 - sens) / espec,
            acuracia = (TP + TN) / length(d),
            prevalencia = (TP + FN) / length(d))
  tibble::tibble(metrica = names(vals), valor = arredonda(unname(vals), digits))
}
