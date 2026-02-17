####****************************************************************************
##### 0. LIBRARIES #####
####****************************************************************************

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)



####****************************************************************************
##### 1. INPUT #####
####****************************************************************************

step_1 = read_excel("~/Desktop/food-security/income/data/step_1_income_diet_c_and_a.xlsx")
step_2 = read_excel("~/Desktop/food-security/income/data/step_2_income_diet_c_and_a.xlsx")
step_3 = read_excel("~/Desktop/food-security/income/data/step_3_income_diet_c_and_a.xlsx")



####****************************************************************************
##### 2. ADDING INTERPRETABEL ORs #####
####****************************************************************************

################################################################################
# Create new ORs scaled to a 100-unit increase
# The transformation is applied ONLY when effect_unit == "OR".
################################################################################

step_1 <- step_1 %>%
  dplyr::mutate(
    # OR estimate for a 100-unit increase
    estimate_interpretable_100 = dplyr::if_else(
      effect_unit == "OR",
      estimate_interpretable^1000,
      NA_real_
    ),
    
    # Lower confidence interval (100-unit increase)
    ci_low_interpretable_100 = dplyr::if_else(
      effect_unit == "OR",
      ci_low_interpretable^1000,
      NA_real_
    ),
    
    # Upper confidence interval (100-unit increase)
    ci_high_interpretable_100 = dplyr::if_else(
      effect_unit == "OR",
      ci_high_interpretable^1000,
      NA_real_
    )
  )





####****************************************************************************
##### 3. DAIL PLOT STEP 1 #####
####****************************************************************************

################################################################################
# Heatmap of regression effects by INCOME GROUP (using `step_1`)
# - 4 magnitude levels (Lowest, Low, High, Highest)  ✅
# - Same colors as before (RdBu, direction = -1)     ✅
# - Effect direction rule:
#     * OR  : Positive if > 1 ; otherwise Negative (<= 1) ✅
#     * β   : Positive if >= 0 ; otherwise Negative       ✅
#     * NA  : treated like β by default (editable)        ✅
################################################################################

#------------------------------------------------------------------------------
# 0) SETTINGS
#------------------------------------------------------------------------------

income_abbrev_map <- c(
  "High income"         = "HIC",
  "Upper middle income" = "UMIC",
  "Lower middle income" = "LMIC",
  "Low income"          = "LIC"
)
income_levels <- c("HIC", "UMIC", "LMIC", "LIC")

category_labels <- c(
  "diet_cost_and_affordability"              = "Diet cost and affordability"
)
category_order <- unname(category_labels)

#------------------------------------------------------------------------------
# 1) PREPARE DATA + 4-LEVEL MAGNITUDE RANKING
#------------------------------------------------------------------------------

plot_data <- step_1 %>%
  filter(independent_var == "total_publications") %>%
  mutate(
    effect_size = suppressWarnings(as.numeric(estimate_interpretable)),
    income_abbrev = unname(income_abbrev_map[income_group]),
    income_abbrev = factor(income_abbrev, levels = income_levels),
    category_facet = ifelse(category %in% names(category_labels),
                            unname(category_labels[category]),
                            category)
  ) %>%
  filter(!is.na(effect_size), !is.na(income_abbrev), !is.na(dependent_var), !is.na(category_facet)) %>%
  group_by(dependent_var) %>%
  mutate(
    # Rank by actual value (NOT absolute)
    coef_rank = rank(effect_size, ties.method = "min"),
    
    # Convert to 4 bins (Lowest -> Highest)
    rank_bin = ntile(coef_rank, 4),
    rank_category = factor(
      rank_bin,
      levels = 1:4,
      labels = c("Lowest", "Low", "High", "Highest")
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Significance stars (swap to p_value_adj_holm if desired)
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    
    # Direction rule (your requirement)
    effect_direction = case_when(
      effect_unit == "OR" ~ ifelse(effect_size > 1, "Positive", "Negative"),  # >1 only
      effect_unit == "β"  ~ ifelse(effect_size >= 0, "Positive", "Negative"),
      TRUE                ~ ifelse(effect_size >= 0, "Positive", "Negative")  # NA fallback
    )
  )

#------------------------------------------------------------------------------
# 2) ORDERING (facets + y-axis)
#------------------------------------------------------------------------------

plot_data <- plot_data %>%
  mutate(category_facet = factor(category_facet, levels = category_order))

y_levels <- plot_data %>%
  arrange(category_facet) %>%
  group_by(category_facet) %>%
  summarise(outcomes = list(unique(dependent_var)), .groups = "drop") %>%
  pull(outcomes) %>%
  unlist(use.names = FALSE)

plot_data <- plot_data %>%
  mutate(dependent_var = factor(dependent_var, levels = y_levels))

# Optional: show blank cells where a model is missing for some income group
plot_data <- plot_data %>%
  group_by(category_facet, dependent_var) %>%
  tidyr::complete(income_abbrev = factor(income_levels, levels = income_levels)) %>%
  ungroup()

#------------------------------------------------------------------------------
# 3) PLOT (same look as your original: RdBu palette + squares vs circles)
#------------------------------------------------------------------------------

plot <- ggplot(plot_data, aes(x = income_abbrev, y = dependent_var)) +
  # Positive effects => squares (tiles)
  geom_tile(
    data = subset(plot_data, !is.na(effect_direction) & effect_direction == "Positive"),
    aes(fill = rank_category),
    color = "white",
    linewidth = 0.5
  ) +
  # Negative effects => circles
  geom_point(
    data = subset(plot_data, !is.na(effect_direction) & effect_direction == "Negative"),
    aes(fill = rank_category),
    color = "white",
    size = 6,
    shape = 21
  ) +
  # Significance stars
  geom_text(
    data = subset(plot_data, !is.na(effect_size)),
    aes(label = significance),
    size = 2.6
  ) +
  # Same palette as your original script
  scale_fill_brewer(palette = "RdBu", direction = -1, drop = FALSE) +
  facet_grid(category_facet ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Country Income Classification",
    y = "",
    fill = "Coefficient\nMagnitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(plot)









####****************************************************************************
##### 4. DAIL PLOT STEP 2 #####
####****************************************************************************

################################################################################
# What this script does (like your original figure):
# 1) Builds a clean label: "Publications → <dependent_var>"
# 2) Uses pooled hierarchical estimates (no income-group split)
# 3) Uses:
#      - estimate_interpretable, ci_low_interpretable, ci_high_interpretable
#      - p_adj_fdr_bh for significance
#      - effect_unit in { "β", "OR" }  (drops NA as requested)
# 4) Creates:
#      - a forest plot (symlog x-scale)
#      - a right-side text table with "β / OR (95% CI)"
#      - combines both into one figure
################################################################################



#------------------------------------------------------------------------------
# 0) OPTIONAL: nicer category titles (keeps your dependent_var text intact)
#------------------------------------------------------------------------------

category_labels <- c(
  "diet_cost_and_affordability"              = "Diet cost and affordability"
)

# Facet order (top to bottom)
category_order <- unname(category_labels)

#------------------------------------------------------------------------------
# 1) PREPARE DATA
#------------------------------------------------------------------------------

hierarchical_results <- step_2 %>%
  # Keep only valid effect units (drop NA as requested)
  filter(effect_unit %in% c("β", "OR")) %>%
  mutate(
    # Coerce to numeric (interpretable scale)
    effect_size      = suppressWarnings(as.numeric(estimate_interpretable)),
    effect_ci_lower  = suppressWarnings(as.numeric(ci_low_interpretable)),
    effect_ci_upper  = suppressWarnings(as.numeric(ci_high_interpretable)),
    
    # Clean predictor label (your independent var is always total_publications)
    indep_clean = case_when(
      independent_var == "total_publications" ~ "Publications",
      TRUE ~ independent_var
    ),
    
    # Use dependent_var as-is (already human-readable)
    dep_clean = dependent_var,
    
    # Model label: Publications → outcome
    model_label = paste0(indep_clean, " \u2192 ", dep_clean),
    
    # Category for faceting:
    # Prefer step_2$category if present; map to nicer titles when possible
    category_raw = category,
    category = ifelse(category_raw %in% names(category_labels),
                      unname(category_labels[category_raw]),
                      category_raw),
    
    # Significance based on FDR BH adjusted p-values
    significance = case_when(
      p_adj_fdr_bh < 0.001 ~ "p < 0.001",
      p_adj_fdr_bh < 0.01  ~ "p < 0.01",
      p_adj_fdr_bh < 0.05  ~ "p < 0.05",
      TRUE                 ~ "Not significant"
    ),
    
    # Label for the model family / distribution on the plot
    # (use model_type if you like that wording better)
    distribution = model_type
  ) %>%
  filter(
    !is.na(effect_size),
    !is.na(effect_ci_lower),
    !is.na(effect_ci_upper),
    !is.na(model_label),
    !is.na(category)
  ) %>%
  mutate(
    # Set a stable category order
    category = factor(category, levels = category_order)
  )

#------------------------------------------------------------------------------
# 2) ORDER ROWS (like your original: by category, then by absolute magnitude)
#------------------------------------------------------------------------------

hierarchical_results <- hierarchical_results %>%
  arrange(category, desc(abs(effect_size))) %>%
  mutate(
    # Factor levels so the first row appears at the TOP within each facet block
    model_label = factor(model_label, levels = rev(unique(model_label)))
  )

#------------------------------------------------------------------------------
# 3) FORMAT EFFECT SIZE + CI FOR THE RIGHT-SIDE TABLE
#------------------------------------------------------------------------------

# Helper: format numbers with sensible precision (keeps OR around 1 nicely)
format_num <- function(x, unit = "β") {
  if (is.na(x)) return(NA_character_)
  
  # For OR, a compact, readable default works well (unless extremely large/small)
  if (unit == "OR") {
    if (abs(x) >= 1000 | abs(x) < 0.001) return(sprintf("%.2e", x))
    return(sprintf("%.3f", x))
  }
  
  # For beta, keep your original magnitude-based formatting style
  ax <- abs(x)
  if (ax < 0.001) return(sprintf("%.2e", x))
  if (ax < 0.01)  return(sprintf("%.5f", x))
  if (ax < 1)     return(sprintf("%.4f", x))
  if (ax < 10)    return(sprintf("%.3f", x))
  if (ax < 1000)  return(sprintf("%.2f", x))
  if (ax < 1e6)   return(sprintf("%.1f", x))
  sprintf("%.2e", x)
}

hierarchical_results <- hierarchical_results %>%
  rowwise() %>%
  mutate(
    effect_text = format_num(effect_size, effect_unit),
    ci_text     = paste0("(", format_num(effect_ci_lower, effect_unit), ", ",
                         format_num(effect_ci_upper, effect_unit), ")"),
    display_text = paste(effect_text, ci_text)
  ) %>%
  ungroup()

#------------------------------------------------------------------------------
# 4) SYMMETRIC LOG TRANSFORM (handles both negative β and large magnitudes)
#------------------------------------------------------------------------------

symlog_trans <- function(base = 10, threshold = 1, scale = 1) {
  trans <- function(x) sign(x) * log10(1 + abs(x) / threshold) * scale
  inv   <- function(x) sign(x) * (base^(abs(x) / scale) - 1) * threshold
  
  scales::trans_new(
    name = paste0("symlog-", format(threshold)),
    transform = trans,
    inverse = inv,
    domain = c(-Inf, Inf),
    breaks = scales::extended_breaks(),
    format = scales::format_format(scientific = FALSE)
  )
}

# Choose a safe threshold (avoid ultra-tiny thresholds if some β are ~0)
abs_nonzero <- abs(hierarchical_results$effect_size[hierarchical_results$effect_size != 0])
abs_nonzero <- abs_nonzero[is.finite(abs_nonzero)]

threshold <- if (length(abs_nonzero) == 0) {
  1
} else {
  max(quantile(abs_nonzero, probs = 0.05, names = FALSE) / 10, 1e-6)
}

#------------------------------------------------------------------------------
# 5) MAIN FOREST PLOT
#------------------------------------------------------------------------------

p <- ggplot(
  hierarchical_results,
  aes(
    y = model_label,
    x = effect_size,
    xmin = effect_ci_lower,
    xmax = effect_ci_upper
  )
) +
  # Reference line at 0 (kept like your original)
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  
  # Light reference lines aligned to the breaks (same idea as original)
  geom_vline(
    xintercept = c(-1e7, -1e5, -1e3, -10, -1, -0.1, 0.1, 1, 10, 1e3, 1e5, 1e7),
    color = "gray90", linewidth = 0.3, linetype = "solid", alpha = 0.5
  ) +
  
  # CIs (no end caps)
  geom_errorbarh(height = 0, color = "black", linewidth = 0.3) +
  
  # Points (colored by significance, black border)
  geom_point(
    aes(fill = significance),
    shape = 21, color = "black", stroke = 0.4, size = 3
  ) +
  
  # Small label for model distribution/family
  geom_text(
    aes(label = distribution),
    hjust = -0.2, vjust = -0.5, size = 2.8, color = "gray50"
  ) +
  
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  
  # Same palette as your original hierarchical plot
  scale_fill_manual(values = c(
    "p < 0.001"       = "#00a6fb",
    "p < 0.01"        = "#009E73",
    "p < 0.05"        = "#D55E00",
    "Not significant" = "#CCCCCC"
  )) +
  
  # Symmetric log x-axis
  scale_x_continuous(
    trans = symlog_trans(threshold = threshold),
    breaks = c(-1e7, -1e5, -1e3, -10, -1, -0.1, 0, 0.1, 1, 10, 1e3, 1e5, 1e7),
    labels = function(x) {
      ifelse(
        x == 0, "0",
        ifelse(abs(x) < 0.001 | abs(x) > 999,
               scales::scientific(x),
               scales::comma(x))
      )
    }
  ) +
  
  labs(
    x = "Effect size (symmetric log scale)",
    y = NULL,
    fill = "Significance"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

#------------------------------------------------------------------------------
# 6) RIGHT-SIDE TEXT TABLE (β / OR (95% CI))
#------------------------------------------------------------------------------

table_data <- hierarchical_results %>%
  select(model_label, display_text, category) %>%
  arrange(match(model_label, levels(hierarchical_results$model_label)))

p_table <- ggplot(table_data, aes(x = 1, y = model_label)) +
  geom_text(aes(label = display_text), hjust = 0, size = 2.8) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 0),
    axis.text.y = element_blank()
  )

p_table_title <- ggplot() +
  annotate("text", x = 1, y = 0.5, label = "\u03B2 / OR (95% CI)", size = 3.5, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(t = 18, r = 5, b = 0, l = 0))

# Combine title + table
table_combined <- gridExtra::grid.arrange(
  p_table_title, p_table,
  ncol = 1, heights = c(0.05, 0.95)
)

# Combine main plot + table
final_plot <- gridExtra::grid.arrange(
  p, table_combined,
  ncol = 2, widths = c(0.8, 0.2)
)

print(final_plot)

# Save if needed:
# ggsave("forest_step2_hierarchical.png", final_plot, width = 14, height = 8, dpi = 300)









####****************************************************************************
##### 5. SCALING STEP 3 ORs #####
####****************************************************************************

#*******************************************************************************
#### 8. POST-PROCESSING: ADD SCALED OR COLUMNS DIRECTLY INTO step_3 ####
#*******************************************************************************

# This script MODIFIES step_3 in-place (i.e., it adds new columns to step_3).
# It only computes scaled quantities for rows where effect_unit == "OR".

X_SCALE <- 1000  # "per +100 units of predictor_x" (e.g., per +100 publications)

step_3 <- step_3 %>%
  mutate(
    # Flag rows eligible for OR scaling
    is_or_model = !is.na(effect_unit) & effect_unit == "OR",
    
    # --- A) Scaled interaction effect (per +100 units in X) ---
    interaction_OR_per100 = if_else(
      is_or_model & is.finite(estimate_raw),
      exp(X_SCALE * estimate_raw),
      NA_real_
    ),
    interaction_OR_per100_ci_low = if_else(
      is_or_model & is.finite(ci_low_raw),
      exp(X_SCALE * ci_low_raw),
      NA_real_
    ),
    interaction_OR_per100_ci_high = if_else(
      is_or_model & is.finite(ci_high_raw),
      exp(X_SCALE * ci_high_raw),
      NA_real_
    ),
    
    # --- B) Simple slopes: effect of +100 X at low vs high Z ---
    OR_X_per100_at_Zp25 = if_else(
      is_or_model & is.finite(x_slope_p25_raw),
      exp(X_SCALE * x_slope_p25_raw),
      NA_real_
    ),
    OR_X_per100_at_Zp25_ci_low = if_else(
      is_or_model & is.finite(x_slope_p25_ci_low_raw),
      exp(X_SCALE * x_slope_p25_ci_low_raw),
      NA_real_
    ),
    OR_X_per100_at_Zp25_ci_high = if_else(
      is_or_model & is.finite(x_slope_p25_ci_high_raw),
      exp(X_SCALE * x_slope_p25_ci_high_raw),
      NA_real_
    ),
    
    OR_X_per100_at_Zp75 = if_else(
      is_or_model & is.finite(x_slope_p75_raw),
      exp(X_SCALE * x_slope_p75_raw),
      NA_real_
    ),
    OR_X_per100_at_Zp75_ci_low = if_else(
      is_or_model & is.finite(x_slope_p75_ci_low_raw),
      exp(X_SCALE * x_slope_p75_ci_low_raw),
      NA_real_
    ),
    OR_X_per100_at_Zp75_ci_high = if_else(
      is_or_model & is.finite(x_slope_p75_ci_high_raw),
      exp(X_SCALE * x_slope_p75_ci_high_raw),
      NA_real_
    ),
    
    # --- C) Contrast: high vs low moderator (ratio of ORs) ---
    slope_diff_raw_p75_minus_p25 = if_else(
      is_or_model & is.finite(x_slope_p75_raw) & is.finite(x_slope_p25_raw),
      x_slope_p75_raw - x_slope_p25_raw,
      NA_real_
    ),
    OR_ratio_X_per100_high_vs_lowZ = if_else(
      is_or_model & is.finite(slope_diff_raw_p75_minus_p25),
      exp(X_SCALE * slope_diff_raw_p75_minus_p25),
      NA_real_
    ),
    
    # --- Optional: ready-to-paste strings for reporting ---
    OR_X_per100_at_Zp25_text = if_else(
      is_or_model & is.finite(OR_X_per100_at_Zp25),
      sprintf("OR(+%d X) @ Zp25: %.4f [%.4f–%.4f]",
              X_SCALE, OR_X_per100_at_Zp25, OR_X_per100_at_Zp25_ci_low, OR_X_per100_at_Zp25_ci_high),
      NA_character_
    ),
    OR_X_per100_at_Zp75_text = if_else(
      is_or_model & is.finite(OR_X_per100_at_Zp75),
      sprintf("OR(+%d X) @ Zp75: %.4f [%.4f–%.4f]",
              X_SCALE, OR_X_per100_at_Zp75, OR_X_per100_at_Zp75_ci_low, OR_X_per100_at_Zp75_ci_high),
      NA_character_
    ),
    OR_ratio_X_per100_text = if_else(
      is_or_model & is.finite(OR_ratio_X_per100_high_vs_lowZ),
      sprintf("OR ratio (+%d X): %.4f (Zp75 vs Zp25)",
              X_SCALE, OR_ratio_X_per100_high_vs_lowZ),
      NA_character_
    )
  )







