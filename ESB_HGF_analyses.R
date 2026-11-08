# Hierarchical Gaussian Filter (HGF) parameter analyses for Phase 2 (n = 220)
# Supplementary Tables S1-S2 and the HGF paragraph of the Phase 2 Results.
# The HGF (3-level AR1 multi-arm bandit, softmax-mu03) was fit in MATLAB/TAPAS separately to
# trials 1-80 and 81-160; this script reads the per-participant output files.
# Run from the repository folder: it builds dataPRL_full from ESB_manuscript_analyses.Rmd.

library(tidyverse)
source(knitr::purl("ESB_manuscript_analyses.Rmd", output = tempfile(), quiet = TRUE))

HGF_DIR <- "C:/Users/SAgno/Desktop/HGF_check/ESB_PRLModeling/"

# One file per participant, 15 rows each (one per simulation).
# true_*  = parameters estimated from the participant's choices (same on all 15 rows)
# recov_* = parameters re-estimated from choices simulated with true_* (parameter recovery)
raw <- map_dfr(list.files(HGF_DIR, pattern = "recoveryParameters_OG.*csv", full.names = TRUE),
               read_csv, show_col_types = FALSE)
pars <- c("mu02", "mu03", "omega2", "omega3", "kappa2")
cols <- c(outer(pars, 1:2, paste, sep = "_"))   # _1 = trials 1-80, _2 = trials 81-160

hgf <- raw %>% distinct(subject_id, across(paste0("true_", cols))) %>%
  rename(prolific_id = subject_id) %>% rename_with(~ sub("true_", "", .x))
d <- dataPRL_full %>% inner_join(hgf, by = "prolific_id")
nrow(d)

# Table S1: descriptives, group differences (Welch, FDR across 10), parameter recovery -------
msd <- function(x) sprintf("%.2f (%.2f)", mean(x), sd(x))
rec <- raw %>% filter(subject_id %in% d$prolific_id) %>% group_by(subject_id) %>%
  summarise(across(starts_with("true_"), first), across(starts_with("recov_"), ~ mean(.x, na.rm = TRUE)))

tableS1 <- map_dfr(cols, function(v) {
  tt <- t.test(d[[v]] ~ d$ESB_group)
  tibble(param = v, Full = msd(d[[v]]),
         Conspiracy = msd(d[[v]][d$ESB_group == "Conspiracy"]),
         Non_conspiracy = msd(d[[v]][d$ESB_group == "Non-conspiracy"]),
         t = tt$statistic, df = tt$parameter, p = tt$p.value,
         recovery_r = cor(rec[[paste0("true_", v)]], rec[[paste0("recov_", v)]]))
}) %>% mutate(q = p.adjust(p, "BH"))
tableS1 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

# Table S2: Spearman correlations, FDR across all 60 ---------------------------------------
vs <- c(BCTI = "BCTI_total", VCB = "vaxConspiracyBelief_total", ES = "ERQ_ES",
        TAS = "TAS_total", WS = "w_sw_r", LS = "l_st_r")
s2 <- expand_grid(param = cols, var = names(vs)) %>% rowwise() %>%
  mutate(ct = list(suppressWarnings(cor.test(d[[param]], d[[vs[[var]]]], method = "spearman"))),
         rho = unname(ct$estimate), p = ct$p.value) %>%
  ungroup() %>% select(-ct) %>% mutate(q = p.adjust(p, "BH"))
s2 %>% select(param, var, rho) %>% pivot_wider(names_from = var, values_from = rho) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
s2 %>% select(param, var, q) %>% pivot_wider(names_from = var, values_from = q) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# mu03 alongside expressive suppression and win-switch --------------------------------------
z <- d %>% mutate(across(c(BCTI_total, vaxConspiracyBelief_total, ERQ_ES, w_sw_r, all_of(cols)),
                         ~ scale(.x)[, 1]))
for (o in c("BCTI_total", "vaxConspiracyBelief_total")) for (p in c("mu03_1", "mu03_2")) {
  m0 <- lm(as.formula(paste(o, "~ ERQ_ES + w_sw_r")), z)
  m1 <- update(m0, paste(". ~ . +", p))
  cat("\n", o, "~ ES + WS +", p, "| R2 change =", round(summary(m1)$r.squared - summary(m0)$r.squared, 3), "\n")
  print(round(coef(summary(m1)), 3))
}

# Expressive suppression x HGF parameter interactions, FDR across 20 -------------------------
int <- expand_grid(o = c("BCTI_total", "vaxConspiracyBelief_total"), p = cols) %>% rowwise() %>%
  mutate(beta = coef(lm(as.formula(paste(o, "~ ERQ_ES *", p)), z))[paste0("ERQ_ES:", p)],
         pv = coef(summary(lm(as.formula(paste(o, "~ ERQ_ES *", p)), z)))[paste0("ERQ_ES:", p), 4]) %>%
  ungroup() %>% mutate(q = p.adjust(pv, "BH"))
int %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% print(n = Inf)
