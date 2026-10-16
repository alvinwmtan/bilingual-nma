bivoc_scale <- c(
  "single-language" = "#de64bb",
  "conceptual" = "#9452c7",
  "total" = "#4a39a8"
)

print_num <- function(x, digits = 2) {
  x |> round(digits) |> format(nsmall = digits)
}

ceiling_dec <- function(x, digits = 2) {
  format(ceiling(x * 10^digits) / 10^digits, nsmall = digits)
}

print_res <- function(df, term) {
  df_res <- df |> 
    filter(model == "full",
           variable == term)
  glue("{print_num(df_res$median)} [{print_num(df_res$q2.5)}, {print_num(df_res$q97.5)}]")
}

make_grid <- function(newdata = NULL) {
  coded <- c("age_c", "vocab_c", "typical_c", "proc_c", "asjp_c")
  x <- tibble(age_c = 0, vocab_c = 0, typical_c = 0, proc_c = 0, asjp_c = 0)
  if (is.null(newdata)) return(x)
  nd <- as_tibble(newdata)
  x <- x[rep(1, nrow(nd)), ]
  if ("age_years" %in% names(nd)) {
    grand_age <- mean(df_contrasts$age_trt - 12 * df_contrasts$age_c, na.rm = TRUE)
    x$age_c <- (nd$age_years) - grand_age / 12
  }
  if ("age_c" %in% names(nd)) x$age_c <- nd$age_c
  if ("vocab_type" %in% names(nd))
    x$vocab_c <- if_else(as.character(nd$vocab_type) == "expressive", 0.5, -0.5)
  if ("vocab_c" %in% names(nd)) x$vocab_c <- nd$vocab_c
  if ("infant_typical" %in% names(nd))
    x$typical_c <- if_else(as.logical(nd$infant_typical), 0.5, -0.5)
  if ("typical_c" %in% names(nd)) x$typical_c <- nd$typical_c
  if ("assessment_procedure" %in% names(nd))
    x$proc_c <- if_else(as.character(nd$assessment_procedure) == "direct", 0.5, -0.5)
  if ("proc_c" %in% names(nd)) x$proc_c <- nd$proc_c
  if ("asjp_dist" %in% names(nd))
    x$asjp_c <- nd$asjp_dist - mean(df_contrasts$asjp_dist, na.rm = TRUE)
  if ("asjp_c" %in% names(nd)) x$asjp_c <- nd$asjp_c
  bind_cols(nd |> select(-any_of(coded)), x)
}

arm_draws <- function(fit, x) {
  mu <- as.matrix(fit$draws("mu_d", format = "draws_matrix"))
  abs_d <- cbind(0, mu)
  X <- unlist(x[c("age_c", "vocab_c", "typical_c", "proc_c", "asjp_c")])
  beta <- as.matrix(fit$draws("beta", format = "draws_matrix"))
  for (k in 1:3) {
    bk <- beta[, paste0("beta[", k, ",", seq_along(X), "]"), drop = FALSE]
    abs_d[, k + 1] <- abs_d[, k + 1] + as.vector(bk %*% X)
  }
  abs_d
}

plot_network <- function(fit, newdata = NULL, title = NULL) {
  abs_d <- arm_draws(fit, make_grid(newdata))
  node_fill <- c(
    Mono = "white",
    `Bi-SL` = unname(bivoc_scale["single-language"]),
    `Bi-TV` = unname(bivoc_scale["total"]),
    `Bi-CV` = unname(bivoc_scale["conceptual"])
  )
  nodes <- tibble(
    arm = 1:4,
    name = names(node_fill),
    fill = unname(node_fill),
    lab_col = if_else(name == "Mono", "grey20", "white"),
    x = c(0, 1, 0, 1),
    y = c(1, 1, 0, 0)
  )
  edges <- list(c(1L, 2L), c(1L, 3L), c(1L, 4L), c(2L, 3L), c(2L, 4L), c(3L, 4L)) |>
    map(\(pr) {
      d <- abs_d[, pr[2]] - abs_d[, pr[1]]
      tibble(
        from = pr[1], to = pr[2],
        median = median(d),
        q2.5 = unname(quantile(d, 0.025)),
        q97.5 = unname(quantile(d, 0.975))
      )
    }) |> list_rbind() |>
    left_join(nodes |> select(from = arm, x1 = x, y1 = y), by = "from") |>
    left_join(nodes |> select(to = arm, x2 = x, y2 = y), by = "to") |>
    mutate(
      crosses0 = sign(q2.5) != sign(q97.5),
      lab = sprintf("%.2f", abs(median)),
      edge_col = case_when(
        from == 1 & to == 2 ~ unname(bivoc_scale["single-language"]),
        from == 1 & to == 3 ~ unname(bivoc_scale["total"]),
        from == 1 & to == 4 ~ unname(bivoc_scale["conceptual"]),
        .default = "grey60"
      ),
      xm = (x1 + x2) / 2 + case_when(from == 1 & to == 4 ~ -0.12, from == 2 & to == 3 ~ -0.12, .default = 0),
      ym = (y1 + y2) / 2 + case_when(from == 1 & to == 4 ~  0.12, from == 2 & to == 3 ~ -0.12, .default = 0)
    )

  ggplot() +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2,
                     linewidth = abs(median), linetype = crosses0, col = edge_col),
                 data = edges) +
    geom_label(aes(xm, ym, label = lab, col = edge_col), data = edges, size = 3, 
               linewidth = .25, fill = "white") +
    geom_point(aes(x, y, fill = fill), data = nodes,
               size = 12, shape = 21, col = "grey20") +
    geom_text(aes(x, y, label = name, col = lab_col), data = nodes, size = 3.2) +
    scale_linewidth(range = c(0.4, 4), guide = "none") +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_equal(xlim = c(-0.15, 1.15), ylim = c(-0.15, 1.15)) +
    theme_void(base_family = "Open Sans") +
    labs(subtitle = title)
}

pair_lab <- function(trt, ref) {
  arm_lab <- c(`2` = "single-language", `3` = "total", `4` = "conceptual")
  if (ref == 1L) unname(arm_lab[as.character(trt)])
  else paste0(arm_lab[as.character(trt)], " vs ", arm_lab[as.character(ref)])
}

model_band <- function(fit, formula, grid, pairs) {
  X <- model.matrix(formula, grid)
  X <- X[, setdiff(colnames(X), "(Intercept)"), drop = FALSE]
  mu   <- as.matrix(fit$draws("mu_d", format = "draws_matrix"))
  beta <- as.matrix(fit$draws("beta", format = "draws_matrix"))
  P <- ncol(X)
  map(pairs, function(pr) {
    k_trt <- pr$trt - 1L
    k_ref <- pr$ref - 1L
    mu_eta <- if (k_trt > 0) mu[, k_trt] else 0
    if (k_ref > 0) mu_eta <- mu_eta - mu[, k_ref]
    b <- matrix(0, nrow(mu), P)
    if (k_trt > 0) b <- b + beta[, paste0("beta[", k_trt, ",", seq_len(P), "]"), drop = FALSE]
    if (k_ref > 0) b <- b - beta[, paste0("beta[", k_ref, ",", seq_len(P), "]"), drop = FALSE]
    eta <- sweep(b %*% t(X), 1, mu_eta, "+")
    grid |>
      mutate(
        comparison = pair_lab(pr$trt, pr$ref),
        mean = colMeans(eta),
        q2.5 = apply(eta, 2, quantile, 0.025),
        q97.5 = apply(eta, 2, quantile, 0.975)
      )
  }) |> list_rbind()
}

plot_moderator <- function(mod_full, xvar, xlab, tag = NULL) {
  pairs_mono <- list(
    list(trt = 2L, ref = 1L),
    list(trt = 4L, ref = 1L),
    list(trt = 3L, ref = 1L)
  )
  pairs_bi <- list(
    list(trt = 3L, ref = 2L),
    list(trt = 4L, ref = 2L),
    list(trt = 4L, ref = 3L)
  )
  
  df_fit <- df_contrasts |> 
    drop_na(all_of(c("age_c", "vocab_type", "infant_typical", 
                     "assessment_procedure", "asjp_dist")))
  xs <- if (is.numeric(df_fit[[xvar]])) {
    seq(min(df_fit[[xvar]]), max(df_fit[[xvar]]), length.out = 80)
  } else {
    sort(unique(df_fit[[xvar]]))
  }
  grid <- make_grid(tibble(!!xvar := xs))
  band <- model_band(mod_full, 
                     formula(~ age_c + vocab_c + typical_c + proc_c + asjp_c), 
                     grid, pairs_mono)
  
  keys <- map_chr(pairs_mono, \(p) paste(p$trt, p$ref))
  comps <- unique(map_chr(pairs_mono, \(p) pair_lab(p$trt, p$ref)))
  pts <- df_fit |>
    filter(paste(arm_trt, arm_ref) %in% keys) |>
    mutate(comparison = map2_chr(arm_trt, arm_ref, pair_lab) |> factor(levels = comps))
  band <- band |> mutate(comparison = factor(comparison, levels = comps))
  
  pretty_x <- function(d) {
    if (xvar == "infant_typical")
      d[[xvar]] <- factor(d[[xvar]], levels = c(FALSE, TRUE),
                          labels = c("atypical", "typical"))
    d
  }
  band <- pretty_x(band)
  pts  <- pretty_x(pts)
  
  if (xvar == "age_c") {
    grand_age <- mean(df_fit$age_trt - 12 * df_fit$age_c, na.rm = TRUE)
    pts  <- pts  |> mutate(x_plot = age_trt / 12)
    band <- band |> mutate(x_plot = age_c + grand_age / 12)
  } else {
    pts  <- pts  |> mutate(x_plot = .data[[xvar]])
    band <- band |> mutate(x_plot = .data[[xvar]])
  }
  
  bi_keys <- map_chr(pairs_bi, \(p) paste(p$trt, p$ref))
  hist_df <- df_fit |>
    filter(paste(arm_trt, arm_ref) %in% bi_keys) |>
    pretty_x()
  if (xvar == "age_c") {
    hist_df <- hist_df |> mutate(x_plot = age_trt / 12)
  } else {
    hist_df <- hist_df |> mutate(x_plot = .data[[xvar]])
  }
  
  is_cat <- !is.numeric(pts$x_plot)
  p <- ggplot() + geom_hline(yintercept = 0, lty = "dashed")
  
  if (is_cat) {
    x_lvls <- if (is.factor(pts$x_plot)) levels(pts$x_plot) else sort(unique(as.character(pts$x_plot)))
    pts     <- pts     |> mutate(x_plot = factor(x_plot, levels = x_lvls))
    band    <- band    |> mutate(x_plot = factor(x_plot, levels = x_lvls))
    hist_df <- hist_df |> mutate(x_plot = factor(as.character(x_plot), levels = x_lvls))
    n_comp <- nlevels(pts$comparison)
    dw <- 0.6
    dodge_off <- \(g) (as.integer(g) - (n_comp + 1) / 2) * (dw / n_comp)
    pts  <- pts  |> mutate(x_num = as.numeric(x_plot) + dodge_off(comparison) +
                             runif(n(), -0.06, 0.06))
    band <- band |> mutate(x_num = as.numeric(x_plot) + dodge_off(comparison))
    p <- p +
      geom_point(aes(x_num, y, col = comparison, fill = comparison),
                 data = pts, alpha = 0.1) +
      geom_pointrange(aes(x_num, y = mean, ymin = q2.5, ymax = q97.5, col = comparison),
                      data = band, show.legend = FALSE) +
      scale_x_continuous(breaks = seq_along(x_lvls), labels = str_to_sentence(x_lvls), expand = c(0, 0)) +
      coord_cartesian(ylim = c(-7, 7), xlim = c(0.4, length(x_lvls) + 0.6))
    p_hist <- ggplot(hist_df, aes(as.numeric(x_plot))) +
      geom_bar(fill = "grey55", width = 0.2) +
      scale_x_continuous(breaks = seq_along(x_lvls), expand = c(0, 0)) +
      coord_cartesian(xlim = c(0.4, length(x_lvls) + 0.6))
  } else {
    p <- p +
      geom_point(aes(x_plot, y, col = comparison, fill = comparison),
                 data = pts, alpha = 0.15) +
      geom_ribbon(aes(x_plot, ymin = q2.5, ymax = q97.5, fill = comparison, group = comparison),
                  data = band, alpha = 0.2, col = NA, show.legend = FALSE) +
      geom_line(aes(x_plot, mean, col = comparison, group = comparison),
                data = band, show.legend = FALSE)
    x_rng <- range(c(pts$x_plot, band$x_plot, hist_df$x_plot), na.rm = TRUE)
    p_hist <- ggplot(hist_df, aes(x_plot)) +
      geom_histogram(fill = "grey55", bins = 20, col = NA) +
      coord_cartesian(xlim = x_rng)
    p <- p + coord_cartesian(ylim = c(-7, 7), xlim = x_rng)
  }
  
  p <- p +
    scale_fill_discrete(palette = bivoc_scale, guide = "none") +
    scale_colour_discrete(palette = bivoc_scale) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = "bottom") +
    labs(x = xlab, y = "Hedges' g",
         col = "Vocab measure", fill = NULL)
  
  p_hist <- p_hist +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(5.5, 5.5, 0, 5.5)) +
    labs(y = "N", tag = tag)
  
  p_hist / p + plot_layout(heights = c(0.22, 1), axes = "collect_x", tag_level = "new")
}
