# Author: Uchenna Agu 
# Date created: August 2026
#===============================================================================
# Load Required Packages
#===============================================================================

# Packages used for:
# readr      - import/export data
# tidyverse  - data manipulation
# ggplot2    - graphics
# spdep      - spatial dependence tools
# sf         - spatial vector data handling
# wesanderson- colour palettes
# srvyr      - survey-weighted analyses
# plotly     - interactive graphics

rq_packages <- c(
  "readr", "tidyverse", "ggplot2", "spdep",
  "sf", "wesanderson", "srvyr", "plotly"
)

installed_packages <- rq_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = TRUE)

rm(list = c("rq_packages", "installed_packages"))

#===============================================================================
# Load User-Defined Functions
#===============================================================================

source("src/00functions_ed.R")


#===============================================================================
# Estimate Household Apparent Nutrient Intakes
#===============================================================================

# Generates household-level apparent nutrient intake estimates from
# the Tanzania Household Budget Survey (HBS) 2017/18.

base_ai <- apparent_intake("tza_hbs1718")


#===============================================================================
# Estimated Average Requirements (EARs)
#===============================================================================

# Extract nutrient-specific EAR values from reference table

ear <- setNames(
  allen_ear$ear_value,
  allen_ear$nutrient
)


#===============================================================================
# Coefficients of Variation (CVs) of Requirement Distributions
#===============================================================================

# Assumed requirement variability used in the Probability of Adequacy (PA)
# calculations. Standard deviation is computed as:
#
# SD = EAR × CV

cv <- c(
  vita_rae_mcg = 0.20,
  folate_mcg   = 0.10,
  vitb12_mcg   = 0.10,
  zn_mg        = 0.125
)


#===============================================================================
# Iron Probability of Adequacy
#===============================================================================

# Iron requirements are not normally distributed.
# Therefore iron PA is estimated separately using the full probability
# approach rather than the standard normal approximation.

fe_probs <- fe_full_prob_mpa(
  data = base_ai %>% select(hhid, fe_mg),
  bio_avail = 10
)


#===============================================================================
# Calculate Nutrient-Specific Probability of Adequacy (PA)
#===============================================================================

# For vitamin A, folate, vitamin B12, and zinc:
#
# PA = Φ((Intake − EAR)/(EAR × CV))
#
# where:
#   Φ  = cumulative standard normal distribution (pnorm)
#   EAR = Estimated Average Requirement
#   CV  = coefficient of variation of requirements
#
# PA represents the probability that intake satisfies physiological
# nutrient requirements.

mpa_result <- base_ai %>%
  rowwise() %>%
  mutate(
    
    pa_vita = pnorm(
      (vita_rae_mcg - ear["vita_rae_mcg"]) /
        (ear["vita_rae_mcg"] * cv["vita_rae_mcg"])
    ),
    
    pa_folate = pnorm(
      (folate_mcg - ear["folate_mcg"]) /
        (ear["folate_mcg"] * cv["folate_mcg"])
    ),
    
    pa_b12 = pnorm(
      (vitb12_mcg - ear["vitb12_mcg"]) /
        (ear["vitb12_mcg"] * cv["vitb12_mcg"])
    ),
    
    pa_zn = pnorm(
      (zn_mg - ear["zn_mg"]) /
        (ear["zn_mg"] * cv["zn_mg"])
    )
    
  ) %>%
  ungroup() %>%
  
  # Append iron probability of adequacy
  left_join(fe_probs, by = "hhid") %>%
  
  mutate(
    
    #---------------------------------------------------------
    # Mean Probability of Adequacy (MPA)
    #---------------------------------------------------------
    #
    # Average adequacy probability across five micronutrients:
    # Vitamin A, Folate, Vitamin B12, Zinc and Iron
    #
    # Higher values indicate better overall micronutrient adequacy.
    #
    mpa = rowMeans(
      cbind(pa_vita, pa_folate, pa_b12, pa_zn, pa_fe),
      na.rm = TRUE
    ),
    
    # Household classified as micronutrient-inadequate
    flag = if_else(mpa < 0.5, 1L, 0L)
    
  ) %>%
  select(hhid, mpa, flag)


#===============================================================================
# Micronutrient Probability of Inadequacy (MPI)
#===============================================================================

# Convert adequacy into inadequacy:
#
# MPI = 1 − MPA
#
# MPI ranges from 0 to 1:
# 0 = complete adequacy
# 1 = complete inadequacy

mpi_result <- mpa_result %>%
  mutate(mpi = 1 - mpa)

# Write CSV for use in other analyses: 
write_csv(mpi_result, "processed_data/tza_hbs1718_mpi.csv")


#===============================================================================
# Household Characteristics
#===============================================================================

tza_hh_info <- read_csv(
  "processed_data/tza_hbs1718_hh_information.csv"
) %>%
  select(
    hhid,
    adm1,
    res,
    sep_quintile,
    survey_wgt
  )


#===============================================================================
# Merge MPI with Household Characteristics
#===============================================================================

merged_tza_hh_mpi <- mpi_result %>%
  left_join(tza_hh_info, by = "hhid")


#===============================================================================
# Survey Design Object
#===============================================================================

# Apply sampling weights for nationally representative estimates.

survey_design <- merged_tza_hh_mpi %>%
  filter(
    !is.na(survey_wgt),
    !is.na(mpi),
    !is.na(adm1),
    !is.na(sep_quintile)
  ) %>%
  as_survey_design(weights = survey_wgt)


#===============================================================================
# National Mean MPI
#===============================================================================

national_mpi <- survey_design %>%
  summarise(
    mean_mpi = survey_mean(
      mpi,
      na.rm = TRUE,
      vartype = c("se", "ci")
    )
  ) %>%
  mutate(mean_mpi = round(mean_mpi, 3))


#===============================================================================
# Regional Mean MPI
#===============================================================================

mpi_by_region <- survey_design %>%
  group_by(adm1) %>%
  summarise(
    mean_mpi = survey_mean(
      mpi,
      na.rm = TRUE,
      vartype = c("se", "ci")
    )
  ) %>%
  mutate(mean_mpi = round(mean_mpi, 3)) %>%
  arrange(desc(mean_mpi))


#===============================================================================
# Display Summary Statistics
#===============================================================================

national_mpi
mpi_by_region


#===============================================================================
# Load Administrative Boundary Shapefile
#===============================================================================

tanzania_1 <- st_read(
  "shapefiles/tza_admbnda_adm1_20181019.shp"
) |>
  select(
    adm1 = ADM1_EN,
    geometry
  ) |>
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  mutate(
    adm1 = recode(
      adm1,
      "Dar-es-salaam" = "Dar Es Salaam"
    )
  )


#===============================================================================
# Merge Regional MPI Estimates with Spatial Boundaries
#===============================================================================

mpi_map <- tanzania_1 |>
  left_join(mpi_by_region, by = "adm1")

# Preserve spatial geometry
mpi_map <- st_as_sf(mpi_map)


#===============================================================================
# Choropleth Map of Regional Micronutrient Inadequacy
#===============================================================================

tza_mpi_map <- plot_map(
  data = mpi_map,
  col = "mean_mpi",
  title = paste0("Mean Probability of Inadequacy (", round(national_mpi$mean_mpi, 2), ")"),
  metric = "Mean MPI",
  outline_sf = tanzania_1, 
  limits = c(0, 1)
) + 
  # Add legend
  scale_fill_gradientn(
    colors = wes_palette(
      "Zissou1",
      n = 100,
      type = "continuous"
    ),
    limits = c(0, 1),
    name = "Prob. Inadequacy"
  ) +
  theme(
    legend.position  = "bottom")
  
  # ggplot() +
  # geom_sf(
  #   data = mpi_map,
  #   aes(fill = mean_mpi),
  #   color = NA
  # ) +
  # scale_fill_gradientn(
  #   colors = wes_palette(
  #     "Zissou1",
  #     n = 100,
  #     type = "continuous"
  #   ),
  #   limits = c(0, 1),
  #   name = "Prob. Inadequacy"
  # ) +
  # theme_minimal() +
  # theme(
  #   plot.title       = element_text(hjust = 0.5),
  #   plot.caption     = element_text(hjust = 0.5),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.title       = element_blank(),
  #   axis.text        = element_blank(),
  #   axis.ticks       = element_blank(),
  #   legend.position  = "bottom",
  #   legend.direction = "horizontal",
  #   legend.title     = element_text(hjust = 0.5),
  #   legend.key.width = unit(1.35, "cm"),
  #   legend.key.height = unit(0.5, "cm")
  # )

tza_mpi_map

ggsave(
  filename = "figures/maps/mpi_map.png",
  plot = tza_mpi_map,
  width = 8,
  height = 6,
  dpi = 300
)


#===============================================================================
# Mean MPI by Socioeconomic Position and Residence
#===============================================================================

# Urban-Rural estimates

mpi_sep_res <- survey_design %>%
  group_by(sep_quintile, res) %>%
  summarise(
    mean_mpi = survey_mean(
      mpi,
      na.rm = TRUE,
      vartype = NULL
    )
  )


#===============================================================================
# Mean MPI by Socioeconomic Position (Overall)
#===============================================================================

mpi_sep_Overall <- survey_design %>%
  group_by(sep_quintile) %>%
  summarise(
    mean_mpi = survey_mean(
      mpi,
      na.rm = TRUE,
      vartype = NULL
    )
  ) %>%
  mutate(res = "Overall")


#===============================================================================
# Combine Estimates
#===============================================================================

mpi_sep_plot <- bind_rows(
  mpi_sep_Overall,
  mpi_sep_res
)


#===============================================================================
# Plot Colours
#===============================================================================

my_cols <- c(
  Overall = "black",
  Urban = "#2C7FB8",
  Rural = "#41AB5D"
)


#===============================================================================
# Socioeconomic Gradient in Micronutrient Inadequacy
#===============================================================================

sep_quintile_line_plot <- ggplot(
  mpi_sep_plot,
  aes(
    x = sep_quintile,
    y = mean_mpi,
    colour = res,
    group = res
  )
) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = my_cols,
    breaks = c("Overall", "Urban", "Rural")
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c(
      "Poorest",
      "Poor",
      "Middle",
      "Wealthy",
      "Wealthiest"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    x = "Socioeconomic Position",
    y = "Mean Probability of Inadequacy",
    colour = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.2),
    legend.background = element_rect(
      fill = "white",
      colour = "grey80"
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )

sep_quintile_line_plot

ggsave(
  filename = "figures/others/sep_quintile_line_plot.png",
  plot = sep_quintile_line_plot,
  width = 8,
  height = 6,
  dpi = 300
)
