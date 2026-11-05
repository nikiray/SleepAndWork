# =============================================================================
# 00_functions.R – helpers (base R + splines; nnet and mice optional)
# Adapted from the Fox Insight analysis code (R/00_functions.R)
# =============================================================================

num <- function(x) suppressWarnings(as.numeric(x))
neg_na <- function(x) { x <- num(x); x[!is.na(x) & x < 0] <- NA; x }      # ELSA negative codes -> NA
keep_codes <- function(x, ok) { x <- num(x); x[!(x %in% ok)] <- NA; x }

# ---- Files -------------------------------------------------------------------
find_file <- function(pattern, dir = cfg$data_dir, required = FALSE) {
  f <- list.files(dir)
  hit <- f[grepl(pattern, f, ignore.case = TRUE, perl = TRUE)]
  hit <- hit[!grepl("^h_elsa|harmon", hit, ignore.case = TRUE)]
  if (length(hit) == 0) {
    if (required) stop(sprintf("No file matching '%s' in %s", pattern, dir), call. = FALSE)
    return(NA_character_)
  }
  if (length(hit) > 1 && !grepl("eol|endoflife", pattern))
    stop(sprintf("Pattern '%s' matched several files (%s). Put the exact name in config.R.",
                 pattern, paste(hit, collapse = ", ")), call. = FALSE)
  file.path(dir, hit)
}

read_elsa <- function(path) {
  sep <- if (grepl("\\.csv$", path, ignore.case = TRUE)) "," else "\t"
  d <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::fread(path, sep = sep, showProgress = FALSE))
  } else utils::read.delim(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- tolower(trimws(names(d)))
  if (!cfg$id %in% names(d)) stop(sprintf("%s has no %s", basename(path), cfg$id), call. = FALSE)
  d[[cfg$id]] <- as.character(d[[cfg$id]])
  d
}

# Return columns, creating all-NA columns for any that are absent
cols <- function(d, vars) { for (v in setdiff(vars, names(d))) d[[v]] <- NA_real_; d[, vars, drop = FALSE] }
getv <- function(d, v) if (v %in% names(d)) d[[v]] else rep(NA_real_, nrow(d))

# ---- Models ------------------------------------------------------------------
complete_for <- function(d, f, extra = character(0)) {
  v <- unique(c(all.vars(f), extra))
  d[stats::complete.cases(d[, intersect(v, names(d)), drop = FALSE]), , drop = FALSE]
}

fit_logit <- function(f, d, w = NULL) {
  if (is.null(w)) stats::glm(f, data = d, family = stats::binomial())
  else { d$.w <- w; stats::glm(f, data = d, family = stats::quasibinomial(), weights = .w) }
}

# Cluster-robust (sandwich) covariance, HC0 with G/(G-1)
vcov_cluster <- function(fit, cluster) {
  b <- stats::coef(fit); keep <- !is.na(b)
  X <- stats::model.matrix(fit)[, keep, drop = FALSE]
  bu <- summary(fit)$cov.unscaled
  wres <- as.vector(stats::residuals(fit, type = "working")) * fit$weights
  S <- rowsum(X * wres, cluster, reorder = FALSE)
  G <- nrow(S)
  V <- bu %*% crossprod(S) %*% bu * G / (G - 1)
  dimnames(V) <- list(names(b)[keep], names(b)[keep])
  V
}

coef_table <- function(fit, cluster, terms, V = NULL) {
  if (is.null(V)) V <- vcov_cluster(fit, cluster)
  b <- stats::coef(fit)[rownames(V)]; se <- sqrt(diag(V)); z <- b / se
  out <- data.frame(term = names(b), log_or = b, se = se, or = exp(b), or_lo = exp(b - 1.96 * se),
                    or_hi = exp(b + 1.96 * se), p = 2 * stats::pnorm(-abs(z)), row.names = NULL)
  out[match(terms, out$term), ]
}

lincom <- function(fit, V, a, b) {   # log OR of coefficient a minus coefficient b
  bb <- stats::coef(fit)
  e <- bb[[a]] - bb[[b]]; s <- sqrt(V[a, a] + V[b, b] - 2 * V[a, b])
  c(or = exp(e), or_lo = exp(e - 1.96 * s), or_hi = exp(e + 1.96 * s), p = 2 * stats::pnorm(-abs(e / s)))
}

wald_test <- function(fit, V, pattern) {
  b <- stats::coef(fit)[rownames(V)]; k <- grep(pattern, names(b))
  if (!length(k)) return(c(chisq = NA, df = NA, p = NA))
  chi <- as.numeric(t(b[k]) %*% solve(V[k, k, drop = FALSE]) %*% b[k])
  c(chisq = chi, df = length(k), p = stats::pchisq(chi, length(k), lower.tail = FALSE))
}

# Standardised risks (g-computation) for each level of an exposure
gcomp_levels <- function(fit, d, expo, levels, w = NULL) {
  if (is.null(w)) w <- rep(1, nrow(d))
  vapply(levels, function(l) {
    z <- d; z[[expo]] <- if (is.factor(d[[expo]])) factor(l, levels = levels(d[[expo]])) else l
    stats::weighted.mean(stats::predict(fit, z, type = "response"), w)
  }, 0)
}

cluster_resample <- function(d, idvar) {
  ids <- unique(d[[idvar]]); pick <- sample(ids, length(ids), replace = TRUE)
  idx <- split(seq_len(nrow(d)), d[[idvar]])
  out <- d[unlist(idx[as.character(pick)], use.names = FALSE), , drop = FALSE]
  out[[idvar]] <- rep(seq_along(pick), vapply(idx[as.character(pick)], length, 1L))   # new cluster ids
  out
}

boot_ci <- function(est_fun, d, idvar, B) {
  ref <- est_fun(d)
  res <- replicate(B, tryCatch(est_fun(cluster_resample(d, idvar)),
                               error = function(e) rep(NA_real_, length(ref))))
  res <- matrix(res, ncol = B)
  out <- t(apply(res, 1, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE))
  rownames(out) <- names(ref); out
}

evalue <- function(rr) { if (is.na(rr)) return(NA_real_); if (rr < 1) rr <- 1 / rr; rr + sqrt(rr * (rr - 1)) }

# Multinomial logit with cluster-robust covariance (requires nnet)
multinom_cluster <- function(f, d, cluster) {
  fit <- nnet::multinom(f, data = d, trace = FALSE, Hess = TRUE)
  X <- stats::model.matrix(f, d)
  P <- stats::fitted(fit); if (is.null(dim(P))) P <- cbind(1 - P, P)
  Y <- stats::model.matrix(~ 0 + d[[all.vars(f)[1]]])
  K <- ncol(P)
  S <- do.call(cbind, lapply(2:K, function(k) X * (Y[, k] - P[, k])))
  B <- solve(fit$Hessian)
  Sg <- rowsum(S, cluster, reorder = FALSE); G <- nrow(Sg)
  V <- B %*% crossprod(Sg) %*% B * G / (G - 1)
  b <- as.vector(t(stats::coef(fit)))
  names(b) <- colnames(B)
  dimnames(V) <- list(colnames(B), colnames(B))
  list(fit = fit, b = b, V = V)
}

# ---- Disclosure control and output ---------------------------------------------
suppress_counts <- function(df, count_cols, min_cell = cfg$min_cell) {
  for (cc in intersect(count_cols, names(df))) {
    x <- num(df[[cc]])
    df[[cc]] <- ifelse(!is.na(x) & x > 0 & x < min_cell, paste0("<", min_cell), as.character(df[[cc]]))
  }
  df
}

write_out <- function(df, name) {
  dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, file.path(cfg$output_dir, name), row.names = FALSE)
  message("  wrote ", file.path(cfg$output_dir, name))
}

bind <- function(lst) {
  lst <- lst[!vapply(lst, is.null, TRUE)]
  if (!length(lst)) return(data.frame())
  cols_all <- unique(unlist(lapply(lst, names)))
  do.call(rbind, lapply(lst, function(x) { for (k in setdiff(cols_all, names(x))) x[[k]] <- NA; x[cols_all] }))
}
