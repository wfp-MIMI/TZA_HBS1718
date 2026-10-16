# =============================================================================
# Author: Uchenna Agu
# Date: September 2026
# Tanzania – bivariate maps of REACH (% households consuming) x INTAKE (mean g/AFE/day among consumers) for fortification vehicles:
# wheat flour, edible oil, maize flour
# =============================================================================

pacman::p_load(tidyverse, sf, survey, srvyr, biscale, cowplot)

options(survey.lonely.psu = "adjust")   # avoid errors from single-PSU strata

# ---- 1. Paths ----------------------------------------------------------------

gis_path <- "C:/Users/uchenna.agu/Downloads/OneDrive_2026-03-24/MIMI GIS files/tza_admbnda_adm1_20181019.shp"
out_dir  <- "."
hbs_dir      <- "C:/Users/uchenna.agu/OneDrive - World Food Programme/MIMI Project - HBS17-18/processed_data"
hh_info_path <- file.path(hbs_dir, "tza_hbs1718_hh_information.csv")
cons_path    <- file.path(hbs_dir, "tza_hbs1718_food_consumption.csv")

# ---- 2. Vehicle configuration -----------------------------------------------
# `codes` = named vector: item_code -> conversion factor to vehicle equivalent
# (1 = item is the vehicle itself)

reach_breaks  <- c(0, 25, 50, 75, 100)
intake_breaks <- c(-Inf, 75, 149, 300, Inf)
intake_labels <- c("<75", "75–149", "150–300", ">300")

vehicles <- list(
  wheat = list(
    codes = c(
      `111506` = 1.000,  # Wheat flour
      `111201` = 0.750,  # Flat bread
      `111202` = 0.750,  # Loaf of white bread
      `111203` = 0.750,  # Round bread
      `111204` = 0.750,  # Sliced bread
      `111205` = 0.750,  # Sliced brown bread
      `111206` = 0.750,  # Sweet bread
      `111207` = 0.750,  # Traditional bread
      `111299` = 0.750,  # Other bread varieties
      `111402` = 0.330,  # Wheat buns / scones
      `111401` = 0.635,  # Cake / half cake
      `111405` = 0.277,  # Kalimati / pie / sambusa
      `111208` = 0.630   # Biscuits
    ),
    intake_breaks = intake_breaks,
    intake_labels = intake_labels
  ),
  oil = list(
    codes = c(`115101` = 1, `115401` = 1, `115402` = 1,
              `115403` = 1, `115404` = 1, `115405` = 1),
    intake_breaks = c(-Inf, 25, 49, 75, Inf),
    intake_labels = c("<25", "25–49", "50–75", ">75")
  ),
  maize = list(
    codes = c(`111504` = 1),         # Maize flour
    intake_breaks = intake_breaks,
    intake_labels = intake_labels
  )
)

# ---- 3. Functions ------------------------------------------------------------

# Read consumption data, attach household info, express per AFE
read_survey <- function(hh_info_path, cons_path) {
  hh_info <- read.csv(hh_info_path)

  read.csv(cons_path) %>%
    left_join(hh_info, by = "hhid") %>%
    mutate(quantity_g = quantity_g / afe) %>%
    select(hhid, item_code, quantity_g, adm1, survey_wgt, res)
}

# Household-level consumption flag + vehicle-equivalent grams,
# then survey-weighted reach and intake by adm1
calc_reach_intake <- function(cons, codes) {

  hh <- cons %>%
    mutate(
      conv  = unname(codes[as.character(item_code)]),
      veh_g = if_else(is.na(conv), 0, quantity_g * conv)
    ) %>%
    group_by(hhid, adm1, survey_wgt, res) %>%
    summarise(
      consumed = as.integer(any(!is.na(conv))),
      veh_g    = sum(veh_g, na.rm = TRUE),
      .groups  = "drop"
    )

  # No cluster (PSU) variable available: stratify by urban/rural only.
  # Point estimates depend only on the weights.
  des <- hh %>%
    mutate(res = factor(res)) %>%
    as_survey_design(ids = 1, strata = res, weights = survey_wgt)

  reach <- des %>%
    group_by(adm1) %>%
    summarise(reach_pct = survey_mean(consumed, vartype = NULL) * 100)

  # Intake among consumers (domain estimate keeps the full design)
  intake <- des %>%
    filter(consumed == 1) %>%
    group_by(adm1) %>%
    summarise(mean_g = survey_mean(veh_g, na.rm = TRUE, vartype = NULL))

  left_join(reach, intake, by = "adm1")
}

# Bin, join to boundaries and draw the bivariate map (+ legend version)
make_bivariate_map <- function(est, shp, cfg, name) {

  dat <- est %>%
    mutate(
      adm1        = as.character(adm1),
      across(c(reach_pct, mean_g), ~ replace_na(.x, 0)),
      reach_bins  = cut(reach_pct, reach_breaks, include.lowest = TRUE),
      intake_bins = cut(mean_g, cfg$intake_breaks,
                        labels = cfg$intake_labels, include.lowest = TRUE)
    )

  unmatched <- setdiff(dat$adm1, shp$adm1)
  if (length(unmatched) > 0)
    warning(name, ": regions not found in shapefile: ",
            paste(unmatched, collapse = ", "))

  dat <- shp %>%
    inner_join(dat, by = "adm1") %>%
    bi_class(x = reach_bins, y = intake_bins, dim = 4)

  breaks <- bi_class_breaks(dat, x = reach_bins, y = intake_bins, dim = 4)

  bi_map <- ggplot() +
    geom_sf(data = dat, aes(fill = bi_class), color = NA, show.legend = FALSE) +
    bi_scale_fill(pal = "BlueGold", dim = 4) +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = 0.5) +
    bi_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          axis.line = element_blank())

  legend <- bi_legend(pal = "BlueGold", dim = 4,
                      xlab = "Higher Reach (%) ",
                      ylab = "Higher Consumption (g) ",
                      size = 8, breaks = breaks)

  full <- ggdraw() +
    draw_plot(bi_map, 0, 0, 1, 1) +
    draw_plot(legend, 0.65, 0.2, 0.45, 0.2)

  list(data = dat, map = bi_map, map_legend = full)
}

# ---- 4. Run ------------------------------------------------------------------

tza_adm1 <- st_read(gis_path, quiet = TRUE) %>%
  rename(adm1 = ADM1_EN) %>%
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) %>%
  mutate(adm1 = recode(adm1, "Dar-es-salaam" = "Dar Es Salaam"))

tza_food_cons <- read_survey(hh_info_path, cons_path)

results <- imap(vehicles, function(cfg, name) {

  est   <- calc_reach_intake(tza_food_cons, cfg$codes)
  plots <- make_bivariate_map(est, tza_adm1, cfg, name)

  ggsave(file.path(out_dir, paste0("tza ", name, " no legend.png")),
         plots$map, width = 10, height = 5, dpi = 300, bg = "white")
  ggsave(file.path(out_dir, paste0("tza ", name, " bivariate.png")),
         plots$map_legend, width = 10, height = 5, dpi = 300, bg = "white")

  c(list(estimates = est), plots)
})

# Inspect results, e.g.:
# results$wheat$estimates
# results$oil$map_legend
# bind_rows(map(results, "estimates"), .id = "vehicle")
