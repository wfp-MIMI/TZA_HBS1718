# ============================================================
# TANZANIA HBS 2017-18: SCHOOL-ATTENDANCE DATA FOR CHILDREN AGED 7-13
# Authors: UA & MO
# Date: August 2026
# ============================================================

#=============================================================
######################## DATA CLEANING #######################
#=============================================================

# ------------------------------------------------------------
# 1. Install and load the required packages
# ------------------------------------------------------------

# List the packages required for the analysis
rq_packages <- c(
  "readr",
  "readxl",
  "tidyverse",
  "haven", 
  "srvyr",
  "gt",
  "ggplot2",
  "scales",
  "sf",
  "wesanderson",
  "RColorBrewer", 
  "dplyr"
)

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# ------------------------------------------------------------
# 2. Import the Tanzania HBS individual-level dataset
# ------------------------------------------------------------

household_roster <- read_dta("raw_data/HBS 2017-18 _Final_Poverty+Individual_Data.dta")

# ------------------------------------------------------------
# 3. Select and prepare the relevant variables
# ------------------------------------------------------------

household_roster <- household_roster |>
  
  # Retain household ID, age, current school attendance,
  # reason for not currently attending, and reason for
  # never attending school
  select(
    hhid = HHID,
    age = calc_age,
    in_school = S5_4,
    S5_6,
    S5_2
  ) |>
  
  # Remove observations with missing age or the special
  # invalid age code -9998
  filter(
    !is.na(age),
    age != -9998
  ) |>
  
  mutate(
    
    # Assign 2 to in_school for household members outside
    # the selected school-age range of 7-13 years.
    # For people aged 7-13, retain their original S5_4 value,
    # including missing values.
    in_school = if_else(
      age < 7 | age > 13,
      2,
      as.numeric(in_school)
    ),
    
    # Create an indicator for school-aged children:
    # 1 = aged 7-13
    # 0 = younger than 7 or older than 13
    school_age_7_13 = if_else(
      age >= 7 & age <= 13,
      1L,
      0L
    )
  )

# ------------------------------------------------------------
# 4. Check the prepared dataset
# ------------------------------------------------------------

# Display the variable names
colnames(household_roster)

# Count missing values in each variable
colSums(
  is.na(household_roster)
)

# ------------------------------------------------------------
# 5. Identify school-aged children with missing attendance
# ------------------------------------------------------------

missing_attendance_records <- household_roster |>
  filter(
    school_age_7_13 == 1,
    is.na(in_school)
  )

# Display the number of school-aged children whose
# current school-attendance status is missing
nrow(missing_attendance_records)

# ------------------------------------------------------------
# 6. Inspect the response labels for S5_2
# ------------------------------------------------------------

# S5_2 asks why the person never attended school
print_labels(
  household_roster$S5_2
)

# ------------------------------------------------------------
# 7. Summarise S5_2 among children with missing attendance
# ------------------------------------------------------------

S5_2_summary <- missing_attendance_records |>
  
  # Retain children with a recorded S5_2 response
  filter(
    !is.na(S5_2)
  ) |>
  
  # Convert the labelled Stata variable into readable labels
  mutate(
    S5_2_response = as_factor(S5_2)
  ) |>
  
  # Count children in each S5_2 response category
  count(
    S5_2_response,
    sort = TRUE,
    name = "count"
  ) |>
  
  # Calculate each category's percentage among children
  # with a non-missing S5_2 response
  mutate(
    percentage = round(
      100 * count / sum(count),
      2
    )
  )

# Display the S5_2 frequency table
S5_2_summary

# Re-classify children aged 7 who rerported being "Too Young" as non-school-aged:
# (This is because the child is likely to have been younger than the official age
# of entry at the start of the school year, and has since turned 7)

household_roster <- household_roster |>
  mutate(
    school_age_7_13 = if_else(
      age == 7 & S5_2 == 1,
      0L,
      school_age_7_13
    )
  )


# ------------------------------------------------------------
# 8. Recover non-attendance information using S5_2
# ------------------------------------------------------------

household_roster <- household_roster |>
  mutate(
    
    # Assign 2 to in_school when:
    # 1. The original school-attendance value is missing, and
    # 2. S5_2 contains a reason why the person never attended.
    #
    # A substantive S5_2 response indicates that the child
    # has never attended school and therefore is not currently
    # attending school.
    #
    # All other in_school values are left unchanged.
    in_school = if_else(
      is.na(in_school) & !is.na(S5_2),
      2,
      in_school
    )
  )


# ------------------------------------------------------------
# 9. Retain the final variables required for household analysis
# ------------------------------------------------------------

household_roster <- household_roster |>
  select(
    hhid,
    age,
    school_age_7_13,
    in_school
  )

# ------------------------------------------------------------
# 10. Check remaining missing values
# ------------------------------------------------------------

colSums(is.na(household_roster))

# ------------------------------------------------------------
# 11. Remove observations whose attendance remains unresolved
# ------------------------------------------------------------

# At this stage, remaining missing values should primarily be
# school-aged children for whom neither S5_4 nor S5_2 provides
# usable attendance information.
#
# These observations are removed from the analytical dataset.
household_roster <- household_roster |>
  filter(
    !is.na(in_school)
  )

# ------------------------------------------------------------
# 12. Final data-quality checks
# ------------------------------------------------------------

# Confirm that the analytical variables contain no missing values
colSums(is.na(household_roster))

# Display the dimensions of the final individual-level dataset
dim(household_roster)

# Check attendance status among children aged 7-13
household_roster |>
  filter(
    school_age_7_13 == 1
  ) |>
  count(
    in_school,
    name = "number_children"
  ) |>
  mutate(
    percentage = round(
      100 * number_children / sum(number_children),
      2
    )
  )

# Create household-level school-attendance dataset
hh_level <- household_roster |>
  group_by(hhid) |>
  summarise(
    # Number of household members aged 7-13
    number_school_age = sum(
      school_age_7_13 == 1,
      na.rm = TRUE
    ),
    
    # Number of school-aged children attending school
    number_attending = sum(
      school_age_7_13 == 1 & in_school == 1,
      na.rm = TRUE
    ),
    
    # Assign household status:
    # 1 = No school-age children
    # 2 = School-age children, at least one attending
    # 3 = School-age children, none attending
    hhid_status = case_when(
      number_school_age == 0 ~ 1L,
      number_attending >= 1 ~ 2L,
      number_school_age >= 1 & number_attending == 0 ~ 3L
    ),
    
    .groups = "drop"
  ) |>
  
  # Keep only the two requested variables
  select(
    hhid,
    hhid_status
  )

# Save
write_csv(hh_level, file = "processed_data/hh_level_school_status.csv")

# Show count
hh_level |>
  count(hhid_status) |>
  mutate(
    percentage = round(
      100 * n / sum(n),
      2
    )
  )

#=============================================================
################ EXPLORATORY DATA ANALYSIS ###################
#=============================================================

#-------------------------------------------------------------
# 1. Create analysis data-frame
#-------------------------------------------------------------

# Retain only required objects: 
rm(list = setdiff(ls(), c("hh_level")))

# Read in other required data-frames: 
base_ai <- read_csv("processed_data/tza_hbs1718_base_ai.csv")
hh_information <- read_csv("processed_data/tza_hbs1718_hh_information.csv")
mpi <- read_csv("processed_data/tza_hbs1718_mpi.csv")

# Create analysis dataframe with required variables: 
analysis_df <- base_ai |> 
  left_join(
    hh_information |> dplyr::select(hhid, survey_wgt, adm1, res, sep_quintile), 
    by = "hhid"
  ) |>
  left_join(
    mpi |> dplyr::select(hhid, mpi), 
    by = "hhid"
  ) |>
  left_join(
    hh_level, 
    by = "hhid"
  ) |> 
  rename(
    school_attendance_status = hhid_status
  ) |> 
  # Label the school attendance status variable:
  mutate(
    school_attendance_status = factor(
      school_attendance_status,
      levels = c(1, 2, 3),
      labels = c(
        "No school-age children",
        "School-age children, at least one attending",
        "School-age children, none attending"
      )
    )
  )

#-------------------------------------------------------------
# 2. Prepare svy analysis object with required variables
#-------------------------------------------------------------

# Get E-AR values required to binarise intake inadequacy: 
source("src/00functions_ed.R")

rm(list = setdiff(ls(), c("analysis_df", "allen_ear", "fe_full_prob", "plot_map")))

# BINARISE RISK OF INADEQUATE MICRONUTRIENT INTAKE:

# Specify list of micronutrients: 
micronutrients <- c("vita_rae_mcg", "folate_mcg",  "vitb12_mcg", "zn_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  analysis_df[[new_col]] <- ifelse(analysis_df[[i]] < ear_value, 1, 0)
  
}

rm(ear_value, i, new_col)

# Retain required columns: 
analysis_df <- analysis_df |> 
  dplyr::select(hhid, survey_wgt, adm1, res, sep_quintile, school_attendance_status,
                vita_rae_mcg_inadequate, folate_mcg_inadequate, vitb12_mcg_inadequate,
                zn_mg_inadequate, fe_mg, mpi)

# # Create survey analysis object: 
svy_analysis_df <- analysis_df |> 
  as_survey_design(weights = survey_wgt)

#-------------------------------------------------------------
# 3. Compute school attendance by district, wealth quintile,
# and urban/rural residence
#-------------------------------------------------------------

school_attendance <- svy_analysis_df |> 
  group_by(adm1) |> 
  summarise(
    no_sac = survey_mean(
      school_attendance_status == "No school-age children",
      na.rm = TRUE,
      vartype = NULL
    ), 
    sac_non_attendance = survey_mean(
      school_attendance_status == "School-age children, none attending",
      na.rm = TRUE,
      vartype = NULL
    ),
    sac_attendance = survey_mean(
      school_attendance_status == "School-age children, at least one attending",
      na.rm = TRUE,
      vartype = NULL
    )
  )

school_attendance <- school_attendance |> 
  mutate(across(-c(adm1), ~ .x * 100)) |> 
  mutate(across(-c(adm1), ~ round(.x, digits = 1)))

school_attendance_table <- school_attendance |> gt() |> 
  cols_label(
    adm1 = "District",
    no_sac = "No school-age children",
    sac_non_attendance = "School-age children, none attending",
    sac_attendance = "School-age children, at least one attending"
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |> 
  tab_spanner(
    label = "School Attendance Status, by district (% of households)",
    columns = c(no_sac, sac_non_attendance, sac_attendance)
  )

school_attendance_table

# Save: 
gtsave(
  data = school_attendance_table,
  filename = "figures/school_meals/school_attendance_by_district.png"
)

# Create alternative analysis data-frame that filters out households with no school-age children:
sac_df <- analysis_df |> 
  filter(school_attendance_status != "No school-age children")

sac_df_svy <- sac_df |> 
  as_survey_design(weights = survey_wgt)

# Summarise school attendance by wealth quintile and urban/rural residence:
school_attendance_disaggregated <- sac_df_svy |> 
  group_by(sep_quintile, res) |> 
  summarise(
    sac_attendance = survey_mean(
      school_attendance_status == "School-age children, at least one attending",
      na.rm = TRUE,
      vartype = NULL
    )
  )

school_attendance_disaggregated <- school_attendance_disaggregated |> 
    mutate(
    sep_quintile = factor(
      sep_quintile,
      levels = 1:5,
      labels = c("Q1\n(poorest)", "Q2", "Q3", "Q4", "Q5\n(wealthiest)")
    ),
    res = factor(res, levels = c("Rural", "Urban"))
  )

p <- ggplot(school_attendance_disaggregated, aes(x = sep_quintile, y = sac_attendance,
                     colour = res, linetype = res, group = res)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("Rural" = "#2A78D6", "Urban" = "#5e5e5e")) +
  scale_linetype_manual(values = c("Rural" = "solid", "Urban" = "solid")) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0.78, 1),
    breaks = seq(0.78, 1, by = 0.02),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Socio-economic quintile",
    y = "% of households with school aged children, \nwith at least one child attending school",
    colour = "Residence",
    linetype = "Residence",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "plain", size = 12, hjust = 0),
    axis.title = element_text(size = 11)
  )
 
print(p)

ggsave(
  filename = "figures/school_meals/school_attendance_disaggregated_plot.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

# Map school attendance by district:
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

school_attendance_district <- sac_df_svy |> 
  group_by(adm1) |> 
  summarise(
    sac_attendance = survey_mean(
      school_attendance_status == "School-age children, at least one attending",
      na.rm = TRUE,
      vartype = NULL
    )
  ) |> 
  mutate(sac_attendance = round(sac_attendance * 100, digits = 1)) |> 
  left_join(tanzania_1, by = "adm1") |> 
  st_as_sf()

district_map <- ggplot() +
    geom_sf(
      data = school_attendance_district,
      aes_string(fill = "sac_attendance"),
      color = "black",
      size = 0.2
    ) +
    # geom_sf(
    #   data = tanzania_1,
    #   fill = NA,
    #   color = "black",
    #   size = 0.5
    # ) +
    scale_fill_gradientn(
      colours = RColorBrewer::brewer.pal(9, "Blues"),
      limits = c(75, 100),
      name = "Households with school-age children\n with at least one attending school (%)"
    ) +
    coord_sf(expand = FALSE) +
    labs(title = "") +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(
        fill = "transparent",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.position = "bottom"
    )

district_map

ggsave(
  filename = "figures/school_meals/school_attendance__map.png",
  plot = district_map,
  width = 8,
  height = 6,
  dpi = 300
)

#-------------------------------------------------------------
# 4. Compute risk of inadequacy for each group
#-------------------------------------------------------------

# Compute risk of inadequate intake for each MN & MPI (grouped by school attendance status): 
mn_inadequacy <- svy_analysis_df |> 
  group_by(school_attendance_status) |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL),
            mpi = survey_mean(mpi, na.rm = T, vartype = "ci"))

mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(school_attendance_status), ~ .x * 100)) |> 
  mutate(across(-c(school_attendance_status), ~ round(.x, digits = 1)))

# Probability of inadequate intake for Fe:
fe_inadequacy <- fe_full_prob(
  data = analysis_df,
  group1 = school_attendance_status,
  bio_avail = 10, 
  hh_weight = "survey_wgt"
) |> 
  rename(
    school_attendance_status = subpopulation,
    fe_inadequacy = fe_mg_prop
  ) |> 
  mutate(
    fe_inadequacy = round(fe_inadequacy, digits = 1)
  )

# Join: 
mn_inadequacy <- mn_inadequacy |> 
  left_join(fe_inadequacy, by = c("school_attendance_status")) |> 
  dplyr::select(school_attendance_status, vita_inadequacy, folate_inadequacy, vitb12_inadequacy,
                zn_inadequacy, fe_inadequacy, mpi, mpi_low, mpi_upp)

# Put confidence intervals in brackets: 
mn_inadequacy <- mn_inadequacy |>
  mutate(
    mpi = paste0(mpi, " (", mpi_low, ", ", mpi_upp, ")")
  ) |>
  dplyr::select(-c(mpi_low, mpi_upp))

# GT table: 
mn_inadequacy_table <- mn_inadequacy |> 
  gt() |> 
  cols_label(
    school_attendance_status = "School attendance status",
    vita_inadequacy = "Vitamin A",
    folate_inadequacy = "Folate",
    vitb12_inadequacy = "Vitamin B12",
    zn_inadequacy = "Zinc",
    fe_inadequacy = "Iron",
    mpi = "MPI (95% CI)"
  ) |>
  tab_spanner(
    label = "Risk of inadequate intake, by school attendance status (% of households)",
    columns = c(vita_inadequacy, folate_inadequacy, vitb12_inadequacy, zn_inadequacy, fe_inadequacy, mpi)
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |> 
  # Make column widths equal for vitamin A:MPI columns:
  cols_width(
    school_attendance_status ~ px(120),
    vita_inadequacy ~ px(80),
    folate_inadequacy ~ px(80),
    vitb12_inadequacy ~ px(80),
    zn_inadequacy ~ px(80),
    fe_inadequacy ~ px(80),
    mpi ~ px(120)
  ) |>  
  # centre align all columns:
  cols_align(
    align = "center",
    columns = everything()
  )

mn_inadequacy_table

gtsave(
  data = mn_inadequacy_table,
  filename = "figures/school_meals/mn_inadequacy_by_school_attendance.png",
)

# Testing