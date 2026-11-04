# =============================================================================
# 05_figure.R – forest plot of adjusted odds ratios (Model 2), PD cohort
# =============================================================================

message("05: figure")
labs <- c(sleepiness = "Daytime sleepiness", insomnia = "Insomnia symptoms",
          rls = "Restless legs symptoms", nocturia = "Getting up at night to urinate",
          rbd = "Dream enactment (NMS-Quest)", rbd1q = "Dream enactment (RBD1Q)",
          pdprop_sleep = "Sleep among most bothersome problems",
          smell = "Negative control: loss of smell/taste")
fp <- bind(list(
  prim[prim$analysis == "Model 2 (primary)", ],
  bind(sec)[bind(sec)$analysis == "Exploratory exposure", ],
  bind(sens)[bind(sens)$analysis == "S8: negative control (smell/taste)", ]))
fp <- fp[!is.na(fp$or), ]
if (nrow(fp)) {
  fp$label <- ifelse(fp$exposure %in% names(labs), labs[fp$exposure], fp$exposure)
  fp <- fp[nrow(fp):1, ]
  png(file.path(cfg$output_dir, "05_forest_model2.png"), width = 2000, height = 300 + 120 * nrow(fp), res = 300)
  op <- par(mar = c(4, 14, 1, 1))
  rng <- range(c(fp$or_lo, fp$or_hi, 1), na.rm = TRUE)
  plot(fp$or, seq_len(nrow(fp)), log = "x", xlim = rng, pch = 15, yaxt = "n",
       xlab = "Adjusted odds ratio for leaving work (95% CI)", ylab = "", ylim = c(0.5, nrow(fp) + 0.5))
  segments(fp$or_lo, seq_len(nrow(fp)), fp$or_hi, seq_len(nrow(fp)))
  abline(v = 1, lty = 2, col = "grey50")
  axis(2, at = seq_len(nrow(fp)), labels = fp$label, las = 1, cex.axis = 0.8)
  par(op); dev.off()
  message("  wrote ", file.path(cfg$output_dir, "05_forest_model2.png"))
}
