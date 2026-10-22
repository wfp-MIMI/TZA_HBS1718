################################################################################
##################### COMPILE BASE MODEL - TZA HBS 2017-2018 ###################
################################################################################

# Author: Mo Osman
# Date created: 11-Nov-2025
# Last edited: 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "spdep", "sf", "wesanderson",
                 "srvyr", "plotly", "cowplot")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
base_ai <- read_csv("processed_data/tza_hbs1718_base_ai.csv")
hh_information <- read_csv("processed_data/tza_hbs1718_hh_information.csv")

#-------------------------------------------------------------------------------

# SHAPEFILES: 
tanzania_1 <- st_read("shapefiles/tza_admbnda_adm1_20181019.shp") |> 
  dplyr::select(
    adm1 = ADM1_EN, 
    geometry
  ) |> 
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  mutate(adm1 = recode(
    adm1, 
    "Dar-es-salaam" = "Dar Es Salaam",
  ))

#-------------------------------------------------------------------------------

# GET EAR VALUES: 
source("src/00functions_ed.R")

rm(list = setdiff(ls(), c("base_ai", "hh_information", "allen_ear", "tanzania_1",
                         "fe_full_prob", "plot_map")))

#-------------------------------------------------------------------------------

# BINARISE RISK OF INADEQUATE MICRONUTRIENT INTAKE:

# Specify list of micronutrients: 
micronutrients <- c("vita_rae_mcg", "folate_mcg",  "vitb12_mcg",
                    "fe_mg",  "zn_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(ear_value, i, new_col)

#-------------------------------------------------------------------------------

# PREVALENCE OF INADEQUATE INTAKE: 
analysis_df <- base_ai |> 
  left_join(hh_information |> 
              dplyr::select(hhid, survey_wgt, adm1), 
            by = "hhid") |>
  as_survey_design(weights = survey_wgt)

# National level estimates: 
national_mn_inadequacy <- analysis_df |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  mutate(across(everything(), ~ .x * 100)) |> 
  mutate(across(everything(), ~ round(.x, digits = 0)))

mn_inadequacy <- analysis_df |> 
  group_by(adm1) |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(tanzania_1, by = "adm1")

# Multiply by 100 and round to 1 decimal place:
mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(adm1, geometry), ~ .x * 100)) |> 
  mutate(across(-c(adm1, geometry), ~ round(.x, digits = 0)))

tza_base_ai_fe <- base_ai |>
  left_join(hh_information, by = "hhid") |>
  select(hhid, adm1, adm2, survey_wgt, fe_mg)

national_fe_inadequacy <- fe_full_prob(tza_base_ai_fe, bio_avail = 10, hh_weight = "survey_wgt") 

tza_fe_inadequacy_adm1 <- fe_full_prob(
  tza_base_ai_fe,
  group1 = adm1,
  bio_avail = 10,
  hh_weight = "hh_weight"
) %>%
  rename(fe_inadequacy = fe_mg_prop) %>%
  mutate(fe_inadequacy = round(fe_inadequacy, 0))

mn_inadequacy <- mn_inadequacy |>
  left_join(tza_fe_inadequacy_adm1, by=c("adm1"="subpopulation"))

# Merge national iron inadequacy with other micronutrients:
national_mn_inadequacy <- national_mn_inadequacy |>
  cbind(
    round(national_fe_inadequacy |>
      dplyr::select(prev_inad) |> 
      rename(fe_inadequacy = prev_inad), digits = 0)
  ) 

#-------------------------------------------------------------------------------

# MEAN ADEQUACY RATIO (MAR):
# at the ADM1 level, 4 micronutrients (Vitamin A, Vitamin B12, Iron, Zinc)
mn_mar <- hh_information |> 
  dplyr::select(hhid, adm1, survey_wgt, res, sep_quintile) |> 
  left_join(base_ai, by = "hhid") |>
  # Nutrient adequacy ratio (NAR) for each micronutrient:
  mutate(va_nar = vita_rae_mcg /allen_ear[allen_ear$nutrient == "vita_rae_mcg", "ear_value"],
         vb12_nar = vitb12_mcg /allen_ear[allen_ear$nutrient == "vitb12_mcg", "ear_value"],
         fol_nar = folate_mcg /allen_ear[allen_ear$nutrient == "folate_mcg", "ear_value"],
         fe_nar = fe_mg /allen_ear[allen_ear$nutrient == "fe_mg", "ear_value"],
         zn_nar = zn_mg /allen_ear[allen_ear$nutrient == "zn_mg", "ear_value"]) |> 
  # Truncate NAR values to 1:
  mutate(across(c(va_nar:zn_nar), ~ ifelse(. > 1, 1, .))) |>
  # Average values to obtain MAR:
  mutate(mar = rowMeans(across(c(va_nar:zn_nar)), na.rm = TRUE))
  # FOLATE TO BE ADDED ONCE WE HAVE DONE FCT MATCHES FOR IT.

# Binarise MAR (inadequate if MAR < 0.75): 
mn_mar <- mn_mar |> 
  mutate(mar_inadequate = ifelse(mar < 0.75, 1, 0)) |> 
  dplyr::select(iso3, survey, hhid, res, sep_quintile, adm1, survey_wgt, mar_inadequate)

mar_analysis_df <- mn_mar |> 
  as_survey_design(weights = survey_wgt)

mar_inadequacy <- mar_analysis_df |> 
  group_by(adm1) |> 
  summarise(mar = survey_mean(mar_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(tanzania_1, by = "adm1")

# Multiply by 100 and round to 0 decimal place:
mar_inadequacy <- mar_inadequacy |> 
  mutate(mar = mar * 100) |> 
  mutate(mar = round(mar, digits = 0))

#-------------------------------------------------------------------------------


# MAP INADEQUACY:
micronutrients <- c("vita_inadequacy", "folate_inadequacy",
                    "vitb12_inadequacy", "fe_inadequacy", "zn_inadequacy")

figure_title <- c("Vitamin A", "Folate", "Vitamin B12", "Iron", "Zinc")

mn_inadequacy <- st_as_sf(mn_inadequacy)

for (i in micronutrients) {

  p <- plot_map(
    data = mn_inadequacy,
    col = i,
    title = paste0(figure_title[which(micronutrients == i)], " (", national_mn_inadequacy[[i]], "%)"),
    metric = "Risk of inadequate intake (%)",
    outline_sf = tanzania_1
  ) 

  print(p)

  ggsave(
    filename = paste0("figures/maps/", i, "_map.png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
}

# Get a standalone legend: 
legend <- get_legend(
  p + theme(legend.position = "bottom")
)

ggsave(
  filename = "figures/maps/legend_map.png",
  plot = legend,
  width = 8,
  height = 2,
  dpi = 300
)

# MAP MAR INADEQUACY:
# mar_inadequacy <- st_as_sf(mar_inadequacy)
# 
# p_mar <- plot_map(
#   data = mar_inadequacy,
#   col = "mar",
#   title = "",
#   metric = "Risk of inadequate intake (%)",
#   outline_sf = tanzania_1
# )
# 
# p_mar

# ggsave(filename = "figures/maps/mar_map.png",
#        plot = p_mar,
#        width = 8, height = 6, dpi = 300)


#-------------------------------------------------------------------------------

# Clear environment:
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################
