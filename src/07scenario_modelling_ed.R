# Author: Uchenna Agu and Mo Osman
# Date created: May 2026
# Last edited: July 2026
#
# Purpose: Fortification scenario modelling

# ==============================================================================
# INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================

# List of required packages
rq_packages <- c(
  "readr", "tidyverse", "ggplot2", "spdep",
  "sf", "wesanderson", "srvyr", "plotly"
)

# Install missing packages
installed_packages <- rq_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

# Load packages into session
lapply(rq_packages, require, character.only = TRUE)

# Clean workspace
rm(list = c("rq_packages", "installed_packages"))


# ==============================================================================
# SOURCE CUSTOM FUNCTIONS
# ==============================================================================

# Load user-defined functions used throughout the analysis
source("src/00functions_ed.R")
# ==============================================================================
# DEFINE FOOD FORTIFICATION VEHICLES AND FORTIFICATION STANDARDS
# ==============================================================================

# ------------------------------------------------------------------------------
# MAIZE FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Food items assumed to be industrially fortified maize products
maize_products <- tibble(
  item_code = c(
    111504  # Maize Flour White - Loose
  ),
  fraction = c(
    1       # Assumed 100% maize flour content
  )
)

# Tanzanian maize flour fortification standard
# Units:
#   Vitamin B12 (mcg/kg)
#   Iron (mg/kg)
#   Zinc (mg/kg)
#   Folate (mcg/kg)
maize_standard <- list(
  vitb12_mcg = 0.5,
  fe_mg      = 1.5,
  zn_mg      = 2.25,
  folate_mcg = 150
)

# Expected micronutrient degradation (losses): 
maize_mn_degradation <- list(
  vitb12_mcg = 0.15,
  fe_mg      = 0,
  zn_mg      = 0,
  folate_mcg = 0.17
)


# ------------------------------------------------------------------------------
# WHEAT FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Wheat flour and wheat-derived products
# Fractions represent assumed wheat flour contribution
wheat_products <- tibble(
  item_code = c(
    111506, # Wheat Flour
    111201, # Flat bread
    111202, # Loaf of white bread
    111203, # Round bread
    111204, # Sliced bread
    111205, # Sliced brown bread
    111206, # Sweet bread
    111207, # Traditional bread
    111299, # Other bread varieties
    111402, # Wheat buns / scones
    111401, # Cake / half cake
    111405, # Kalimati / Pie / Sambusa
    111208  # Biscuits
  ),
  fraction = c(
    1.000,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.330,
    0.635,
    0.277,
    0.630
  )
)

# Tanzanian wheat flour fortification standard
wheat_standard <- list(
  vitb12_mcg = 1.5,
  fe_mg      = 4,
  zn_mg      = 4,
  folate_mcg = 300
)

# Expected micronutrient degradation (losses):
wheat_mn_degradation <- list(
  vitb12_mcg = 0.15,
  fe_mg      = 0,
  zn_mg      = 0,
  folate_mcg = 0.17
)

# ------------------------------------------------------------------------------
# EDIBLE OILS
# ------------------------------------------------------------------------------

# Oils assumed eligible for vitamin A fortification
oil_products <- tibble(
  item_code = c(
    115101, # Butter oil
    #115301, # Olive oil
    115401, # Sunflower oil
    115402, # Cottonseed oil
    115403, # Groundnut oil
    115404, # Sesame oil
    115405  # Coconut cooking oil
  )
)

# Vitamin A fortification standard
# Units: mcg RAE/kg
oil_standard <- list(
  vita_rae_mcg = 1700
)

# Expected micronutrient degradation (losses):
oil_mn_degradation <- list(
  vita_rae_mcg = 0.32
)


# ==============================================================================
# LOAD PROCESSED DATA
# ==============================================================================

# Compliance scenarios modelled - matches tza_maize_scenario.R /
# tza_wheat_scenario.R / tza_oil_scenario.R
compliance_scenarios <- c(0.00, 0.65, 0.85, 1.00)

# Create fortification-adjusted nutrient intake dataset, stacked across every
# compliance scenario (long format, with "compliance" and "compliance_label"
# columns identifying each scenario)
fortification_ai <- fortification_scenario(
  "tza_hbs1718",
  compliance = compliance_scenarios
) |>
  mutate(
    compliance_label = factor(
      compliance_label,
      levels = paste0(compliance_scenarios * 100, "%"),
      ordered = TRUE
    )
  )

# Household-level information including weights and geography
hh_information <- read_csv(
  "processed_data/tza_hbs1718_hh_information.csv"
)


# ==============================================================================
# LOAD AND PREPARE ADMINISTRATIVE BOUNDARIES
# ==============================================================================

# ADM1 shapefile used for regional mapping
tanzania_1 <- st_read(
  "shapefiles/tza_admbnda_adm1_20181019.shp"
) |>
  dplyr::select(
    adm1 = ADM1_EN,
    geometry
  ) |>
  # Exclude Zanzibar regions from mainland analysis
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  # Harmonize naming with survey dataset
  mutate(
    adm1 = recode(
      adm1,
      "Dar-es-salaam" = "Dar Es Salaam"
    )
  )


# ==============================================================================
# CREATE BINARY INDICATORS OF NUTRIENT INADEQUACY
# ==============================================================================

# Nutrients evaluated against Allen EAR values
micronutrients <- c(
  "vita_rae_mcg",
  "folate_mcg",
  "vitb12_mcg",
  "fe_mg",
  "zn_mg"
)

# Create indicator:
# 1 = intake below EAR
# 0 = intake at or above EAR
for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[
    allen_ear$nutrient == i
  ]
  
  new_col <- paste0(i, "_inadequate")
  
  fortification_ai[[new_col]] <-
    ifelse(
      fortification_ai[[i]] < ear_value,
      1,
      0
    )
}

rm(ear_value, i, new_col)


# ==============================================================================
# ESTIMATE PREVALENCE OF INADEQUATE INTAKE, BY COMPLIANCE SCENARIO
# ==============================================================================

# Merge household weights and geographic/socioeconomic identifiers.
# "compliance" and "compliance_label" already exist on fortification_ai
analysis_df <- fortification_ai |>
  left_join(
    hh_information |>
      dplyr::select(
        hhid,
        survey_wgt,
        adm1,
        adm2,
        res,
        sep_quintile
      ),
    by = "hhid"
  ) |>
  filter(
    !is.na(survey_wgt),
    survey_wgt > 0
  ) |>
  as_survey_design(weights = survey_wgt)

# Reusable function for EAR cut-point based inadequacy estimates -----------
estimate_mn_inadequacy <- function(survey_object, group_vars = NULL) {
  
  if (!is.null(group_vars)) {
    survey_object <- survey_object |>
      group_by(across(all_of(group_vars)))
  }
  
  survey_object |>
    summarise(
      vita_inadequacy =
        survey_mean(vita_rae_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      
      folate_inadequacy =
        survey_mean(folate_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      
      vitb12_inadequacy =
        survey_mean(vitb12_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      
      zn_inadequacy =
        survey_mean(zn_mg_inadequate, na.rm = TRUE, vartype = NULL)
    ) |>
    mutate(
      across(
        c(vita_inadequacy, folate_inadequacy, vitb12_inadequacy, zn_inadequacy),
        ~ round(.x * 100, 0)
      )
    )
}

# National, ADM1, residence, and SEP quintile tables, for every compliance level
mn_inadequacy_national <- estimate_mn_inadequacy(
  analysis_df,
  group_vars = c("compliance", "compliance_label")
) |>
  arrange(compliance)

mn_inadequacy_adm1 <- estimate_mn_inadequacy(
  analysis_df,
  group_vars = c("compliance", "compliance_label", "adm1")
) |>
  arrange(compliance, adm1)


# ==============================================================================
# ESTIMATE IRON INADEQUACY USING FULL PROBABILITY APPROACH, BY COMPLIANCE
# ==============================================================================

# Iron inadequacy cannot be estimated using simple EAR cut-point methods
# Therefore the full probability method is used assuming 10% iron bioavailability

tza_fortification_ai_fe <- fortification_ai |>
  left_join(hh_information, by = "hhid") |>
  select(
    hhid,
    compliance,
    compliance_label,
    adm1,
    survey_wgt,
    fe_mg
  )

# Run the full-probability method separately for each compliance scenario,
# at national, ADM1, residence, and SEP quintile level
fe_inadequacy_national <- purrr::map_dfr(
  compliance_scenarios,
  function(comp) {
    
    tza_fortification_ai_fe |>
      filter(compliance == comp) |>
      fe_full_prob(bio_avail = 10, hh_weight = "survey_wgt") |>
      mutate(
        compliance = comp,
        compliance_label = paste0(comp * 100, "%")
      )
  }
) |>
  rename(fe_inadequacy = prev_inad) |>
  mutate(fe_inadequacy = round(fe_inadequacy, 0)) |>
  select(compliance, compliance_label, fe_inadequacy)

fe_inadequacy_adm1 <- purrr::map_dfr(
  compliance_scenarios,
  function(comp) {
    
    tza_fortification_ai_fe |>
      filter(compliance == comp) |>
      fe_full_prob(group1 = adm1, bio_avail = 10, hh_weight = "survey_wgt") |>
      rename(adm1 = subpopulation, fe_inadequacy = fe_mg_prop) |>
      mutate(
        compliance = comp,
        compliance_label = paste0(comp * 100, "%"),
        fe_inadequacy = round(fe_inadequacy, 0)
      )
  }
)


# Merge iron results into the corresponding EAR-based inadequacy tables
mn_inadequacy_national <- mn_inadequacy_national |>
  left_join(fe_inadequacy_national, by = c("compliance", "compliance_label"))

mn_inadequacy_adm1 <- mn_inadequacy_adm1 |>
  left_join(fe_inadequacy_adm1, by = c("compliance", "compliance_label", "adm1"))


# ==============================================================================
# PRODUCE CHOROPLETH MAPS - 65%, 85%, AND 100% COMPLIANCE SCENARIOS
# ==============================================================================

# Indicators to map
micronutrients <- c(
  "vita_inadequacy",
  "folate_inadequacy",
  "vitb12_inadequacy",
  "fe_inadequacy",
  "zn_inadequacy"
)

# Compliance scenarios to map
map_compliance <- c("65%", "85%", "100%")

# Generate and save ADM1-level maps for each compliance scenario
for (comp in map_compliance) {
  
  # Prepare spatial dataset for the selected compliance scenario
  mn_inadequacy_sc <- mn_inadequacy_adm1 |>
    filter(compliance_label == comp) |>
    left_join(tanzania_1, by = "adm1") |>
    st_as_sf()
  
  # Generate a map for each micronutrient
  for (i in micronutrients) {
    
    p <- plot_map(
      data = mn_inadequacy_sc,
      col = i,
      title = "",
      metric = "Risk of inadequate intake (%)",
      outline_sf = tanzania_1
    ) +
      theme(legend.position = "none")
    
    print(p)
    comp_file <- gsub("%", "pct", comp)
    
    ggsave(
      filename = paste0(
        "figures/fortification_maps/",
        i, "_", comp_file, "_map.png"
      ),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}


################################################################################
################################# END OF SCRIPT ################################
