# ============================================================
# TANZANIA – OBJECTIVE 4
# School-meal micronutrient content vs Allen et al. (2020) H-AR
#
# Scenarios
#   Unfortified             : baseline meal (FCT values only)
#   Fortified/biofortified  : fortified maize flour
#                             + biofortified beans
#                             + vitamin A-fortified sunflower oil
#
# FCT values and intervention increments are per 100 g.
# Increments are added to the FCT values; retention factors
# are then applied to the whole meal.
# ============================================================

library(tidyverse)


# ---- 0. Path -----------------------------------------------

fct_path <- paste0("processed_data/tza_hbs1718_fct.csv")


# ============================================================
# 1. INPUTS  (all modelling assumptions live in this section)
# ============================================================

# ---- 1a. Foods (HBS 2017/18 FCT item codes) ----------------

foods <- tribble(
  ~food,           ~item_code, ~food_label,
  "maize_flour",   "111504",   "Maize flour",
  "beans",        "117314",   "Beans, dry",
  "sunflower_oil", "115401",   "Sunflower oil"
)


# ---- 1b. Nutrients and cooking retention -------------------

nutrients <- tribble(
  ~nutrient,      ~nutrient_name, ~unit,     ~retention,
  "vita_rae_mcg", "Vitamin A",    "mcg RAE", 0.70,
  "folate_mcg",   "Folate",       "mcg",     0.75,
  "vitb12_mcg",   "Vitamin B12",  "mcg",     0.80,
  "fe_mg",        "Iron",         "mg",      1.00,
  "zn_mg",        "Zinc",         "mg",      0.95
)


# ---- 1c. Rations (g per child per meal) --------------------
# Boys and girls 11-14 y share a ration but differ in H-AR.

rations <- tribble(
  ~ration_group, ~maize_flour, ~beans, ~sunflower_oil,
  "7-10",        100,          30,      5,
  "11-14",       120,          45,      5
) %>%
  pivot_longer(-ration_group, names_to = "food", values_to = "ration_g")


# ---- 1d. Allen et al. (2020) H-AR, per day -----------------

har <- tribble(
  ~age_sex_group,        ~ration_group, ~vita_rae_mcg, ~folate_mcg, ~vitb12_mcg, ~fe_mg, ~zn_mg,
  "Children 7-10 years", "7-10",        320,           160,         1.0,          8.0,   6.2,
  "Boys 11-14 years",    "11-14",       480,           210,         1.5,         12.8,   8.9,
  "Girls 11-14 years",   "11-14",       480,           210,         1.5,         11.2,   8.9
) %>%
  pivot_longer(
    all_of(nutrients$nutrient),
    names_to  = "nutrient",
    values_to = "har"
  )


# ---- 1e. Intervention increments (added per 100 g) ---------
# Units match the FCT. Nutrients not listed get no increment.
# Use 0, not NA, where an intervention adds nothing.

interventions <- tribble(
  ~food,           ~nutrient,      ~added_100g, ~source,

  # Fortified maize flour
  "maize_flour",   "folate_mcg",   150,         "Fortified maize flour",
  "maize_flour",   "vitb12_mcg",   0.5,         "Fortified maize flour",
  "maize_flour",   "fe_mg",        1.5,         "Fortified maize flour",
  "maize_flour",   "zn_mg",        2.25,        "Fortified maize flour",

  # Biofortified beans
  "beans",        "folate_mcg",   0,         "Biofortified beans",
  "beans",        "vitb12_mcg",   0,         "Biofortified beans",
  "beans",        "fe_mg",        2.4,         "Biofortified beans",
  "beans",        "zn_mg",        1.76,        "Biofortified beans",

  # Vitamin A-fortified sunflower oil
  "sunflower_oil", "vita_rae_mcg", 1700,        "Vitamin A-fortified oil"
)


# ---- 1f. Plot settings -------------------------------------

scenario_levels  <- c("Unfortified", "Fortified/biofortified")
scenario_colours <- c("Unfortified" = "#2176AE", "Fortified/biofortified" = "#D96B1B")
nutrient_order   <- c("Folate", "Iron", "Zinc", "Vitamin A", "Vitamin B12")
age_order <- c(
  "Children 7-10 years",
  "Girls 11-14 years",
  "Boys 11-14 years"
)
target_range     <- c(30, 45)


# ============================================================
# 2. INPUT CHECKS
# ============================================================

stop_if <- function(condition, ...) {
  if (condition) stop(..., call. = FALSE)
}

stop_if(anyNA(interventions$added_100g),
        "Some intervention increments are missing. Enter 0 where nothing is added.")
stop_if(anyDuplicated(interventions[c("food", "nutrient")]) > 0,
        "Duplicate food x nutrient rows in 'interventions'.")
stop_if(!all(interventions$nutrient %in% nutrients$nutrient),
        "Unknown nutrient in 'interventions'.")
stop_if(!all(interventions$food %in% foods$food),
        "Unknown food in 'interventions'.")
stop_if(any(interventions$nutrient == "vita_rae_mcg" & interventions$food != "sunflower_oil"),
        "Vitamin A should be added only through fortified sunflower oil.")
stop_if(any(interventions$food == "sunflower_oil" & interventions$nutrient != "vita_rae_mcg"),
        "A nutrient other than vitamin A was entered for sunflower oil.")


# ============================================================
# 3. FOOD COMPOSITION TABLE
# ============================================================

fct <- read_csv(
  fct_path,
  col_types = cols(item_code = col_character(), .default = col_guess())
) %>%
  filter(item_code %in% foods$item_code) %>%
  select(item_code, all_of(nutrients$nutrient))

stop_if(anyDuplicated(fct$item_code) > 0,
        "Duplicate item_code in the FCT: ",
        paste(unique(fct$item_code[duplicated(fct$item_code)]), collapse = ", "))
stop_if(!all(foods$item_code %in% fct$item_code),
        "Item codes not found in the FCT: ",
        paste(setdiff(foods$item_code, fct$item_code), collapse = ", "))

fct_long <- fct %>%
  pivot_longer(-item_code, names_to = "nutrient", values_to = "fct_100g")

missing_fct <- filter(fct_long, is.na(fct_100g))
if (nrow(missing_fct) > 0) {
  warning("Missing FCT values are treated as zero:", call. = FALSE)
  print(missing_fct)
}

fct_long <- mutate(fct_long, fct_100g = replace_na(fct_100g, 0))


# ============================================================
# 4. FOOD-LEVEL CONTRIBUTIONS
# ============================================================
# One row per ration group x food x nutrient.
# 'baseline' = FCT content; 'added' = intervention increment.
# Both are after cooking retention.

food_level <- rations %>%
  left_join(foods, by = "food") %>%
  left_join(fct_long, by = "item_code", relationship = "many-to-many") %>%
  left_join(interventions, by = c("food", "nutrient")) %>%
  left_join(nutrients, by = "nutrient") %>%
  mutate(
    added_100g = replace_na(added_100g, 0),
    baseline   = fct_100g   * ration_g / 100 * retention,
    added      = added_100g * ration_g / 100 * retention
  )


# ============================================================
# 5. MEAL TOTALS VS H-AR
# ============================================================

results <- food_level %>%
  group_by(ration_group, nutrient, nutrient_name, unit) %>%
  summarise(
    Unfortified = sum(baseline),
    added       = sum(added),
    .groups     = "drop"
  ) %>%
  mutate(`Fortified/biofortified` = Unfortified + added) %>%
  pivot_longer(all_of(scenario_levels),
               names_to = "scenario", values_to = "amount") %>%
  inner_join(har, by = c("ration_group", "nutrient"),
             relationship = "many-to-many") %>%
  mutate(
    pct_har = amount / har * 100,
    gap_30  = amount - har * target_range[1] / 100,
    gap_45  = amount - har * target_range[2] / 100,
    status  = case_when(
      round(pct_har, 1) <  target_range[1] ~ "Below target",
      round(pct_har, 1) <= target_range[2] ~ "Within target",
      TRUE                                 ~ "Above target"
    ),
    scenario      = factor(scenario, scenario_levels),
    age_sex_group = factor(age_sex_group, age_order),
    nutrient_name = factor(nutrient_name, nutrient_order)
  ) %>%
  select(scenario, age_sex_group, nutrient_name, unit,
         amount, har, pct_har, gap_30, gap_45, status) %>%
  arrange(scenario, age_sex_group, nutrient_name)


# ---- 5a. Side-by-side comparison ---------------------------

comparison <- results %>%
  select(age_sex_group, nutrient_name, unit, har,
         scenario, amount, pct_har, status) %>%
  pivot_wider(
    names_from  = scenario,
    values_from = c(amount, pct_har, status)
  ) %>%
  mutate(
    absolute_change = `amount_Fortified/biofortified` - amount_Unfortified,
    pct_change      = if_else(amount_Unfortified > 0,
                              absolute_change / amount_Unfortified * 100,
                              NA_real_),
    change_pp       = `pct_har_Fortified/biofortified` - pct_har_Unfortified
  ) %>%
  arrange(age_sex_group, nutrient_name)


# ---- 5b. Retained increment by intervention ----------------

contribution <- food_level %>%
  filter(added > 0) %>%
  select(ration_group, source, nutrient_name, unit,
         fct_100g, added_100g, ration_g, added) %>%
  arrange(ration_group, source, nutrient_name)


print(mutate(results,      across(where(is.numeric), ~ round(.x, 1))), n = Inf, width = Inf)
print(mutate(comparison,   across(where(is.numeric), ~ round(.x, 1))), n = Inf, width = Inf)
print(mutate(contribution, across(where(is.numeric), ~ round(.x, 1))), n = Inf, width = Inf)


# ============================================================
# 6. PLOTS
# ============================================================

x_max_unfortified <- 50

x_max_comparison <- max(
  50,
  ceiling((max(results$pct_har, na.rm = TRUE) + 10) / 10) * 10
)

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    plot.caption       = element_blank(),
    
    strip.text         = element_text(
      size = 12,
      face = "bold",
      colour = "#1F2933"
    ),
    
    strip.background   = element_rect(
      fill = "#EEF2F5",
      colour = NA
    ),
    
    axis.text.y        = element_text(
      size = 11,
      face = "bold",
      colour = "#1F2933",
      margin = margin(r = 8)
    ),
    
    axis.text.x.top    = element_text(
      size = 10,
      colour = "#374151"
    ),
    
    axis.ticks.x.top   = element_line(
      colour = "#374151",
      linewidth = 0.6
    ),
    
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    
    panel.spacing      = unit(1.2, "lines"),
    
    legend.position    = "bottom",
    legend.title       = element_blank(),
    
    legend.text        = element_text(
      size = 11
    ),
    
    legend.key.width   = unit(
      1.4,
      "cm"
    ),
    
    plot.margin        = margin(
      15,
      50,
      15,
      15
    )
  )

plot_har <- function(data, x_max) {
  
  data <- data %>%
    mutate(
      nutrient_name = fct_rev(
        fct_drop(nutrient_name)
      )
    )
  
  dodge <- position_dodge2(
    width = 0.82,
    preserve = "single",
    padding = 0.08,
    reverse = TRUE
  )
  
  legend <- if (
    n_distinct(data$scenario) > 1
  ) "bottom" else "none"
  
  ggplot(
    data,
    aes(
      pct_har,
      nutrient_name,
      fill = scenario
    )
  ) +
    annotate(
      "rect",
      xmin = target_range[1],
      xmax = target_range[2],
      ymin = -Inf,
      ymax = Inf,
      fill = "#D9EAD3",
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = target_range,
      colour = "#568A4B",
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    geom_col(
      position = dodge,
      width = 0.72
    ) +
    geom_text(
      aes(
        label = paste0(
          round(pct_har),
          "%"
        )
      ),
      position = dodge,
      hjust = -0.15,
      size = 3.4,
      fontface = "bold",
      colour = "#1F2933"
    ) +
    facet_wrap(
      ~age_sex_group,
      ncol = 1
    ) +
    scale_fill_manual(
      values = scenario_colours
    ) +
    scale_x_continuous(
      position = "top",
      breaks = seq(
        0,
        x_max,
        10
      ),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = c(
        0,
        x_max
      ),
      clip = "off"
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    plot_theme +
    theme(
      legend.position = legend
    )
}

# ============================================================
# Unfortified plot
# ============================================================

unfortified_plot <- results %>%
  filter(
    scenario == "Unfortified",
    nutrient_name %in% c(
      "Folate",
      "Iron",
      "Zinc"
    )
  ) %>%
  plot_har(
    x_max = x_max_unfortified
  )

# ============================================================
# Comparison plot
# ============================================================

comparison_plot <- results %>%
  plot_har(
    x_max = x_max_comparison
  )

unfortified_plot
comparison_plot


ggsave(filename = "figures/others/unfortified_plot_small.png",
       unfortified_plot,
       width = 8, height = 5, units = "in", dpi = 300,
       bg = "white")

ggsave(filename = "figures/others/fortified_biofortified_plot.png",
       comparison_plot,
       width = 12, height = 6, units = "in", dpi = 300,
       bg = "white")
