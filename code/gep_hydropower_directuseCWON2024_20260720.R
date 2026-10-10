################################################################################
# Script: gep_hydropower_01_calc_20260720.R
#
# Description:
#   Generate GEP estimates for hydropower using the resource-rent method and
#   resource-rent data from the Changing Wealth of Nations (CWON) 2024.
#
# Revision:
#   Update the hydropower production data using IRENA data so that the results
#   are compatible with the data and methods used for other renewable energy
#   sources.
#
# Author: Lifeng Ren
# Date:   2026-07-20
################################################################################


# ------------------------------------------------------------------------------
# Set the working directory to the location of this script
# ------------------------------------------------------------------------------

script_path <- paste0(
  "F:/Dropbox/400_research/430_projects/432_UMN/443_gep/gep_hydropower/scripts/gep_hydropower_directuseCWON2024_20260720.R"
)

script_dir <- dirname(script_path)

setwd(script_dir)


# ------------------------------------------------------------------------------
# Load packages
# ------------------------------------------------------------------------------

library(data.table)


# ------------------------------------------------------------------------------
# Read input data and GEP output template
# ------------------------------------------------------------------------------

intermediate_dir <- file.path("..", "intermediate")

gep_hydro <- fread(
  file.path("..", "..", "EXAMPLE_SERVICE_gep.csv")
)

cwonresrent <- fread("F:/Dropbox/400_research/430_projects/432_UMN/443_gep/gep_hydropower/intermediate/cwon_resrent_hydro_2019usd_20260720.csv")


# Ensure both objects are data.tables
setDT(cwonresrent)
setDT(gep_hydro)

# ------------------------------------------------------------------------------
# Harmonize CWON country names with the GEP country-name standard
# ------------------------------------------------------------------------------

country_name_correspondence <- data.table(
  cwon_name = c(
    "Congo, Dem. Rep.",
    "Congo, Rep.",
    "Cote d'Ivoire",
    "Egypt, Arab Rep.",
    "Eswatini",
    "Iran, Islamic Rep.",
    "Korea, Rep.",
    "Lao PDR",
    "Russian Federation",
    "Slovak Republic",
    "Turkiye",
    "Venezuela, RB"
  ),
  gep_name = c(
    "Democratic Republic of the Congo",
    "Republic of the Congo",
    "Côte d'lvoire",
    "Egypt",
    "Swaziland",
    "Iran",
    "South Korea",
    "Laos",
    "Russia",
    "Slovakia",
    "Turkey",
    "Venezuela"
  )
)

# Replace the 12 CWON names with their corresponding GEP names
cwonresrent[
  country_name_correspondence,
  on = .(iso3_r250_name = cwon_name),
  iso3_r250_name := i.gep_name
]

# ------------------------------------------------------------------------------
# Check that all CWON country names match the GEP country-name standard
# ------------------------------------------------------------------------------

unmatched_cwon_names <- cwonresrent[
  !gep_hydro,
  on = "iso3_r250_name",
  unique(iso3_r250_name)
]

if (length(unmatched_cwon_names) > 0L) {
  warning(
    "CWON country names not found in gep_hydro: ",
    paste(unmatched_cwon_names, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# Merge CWON resource-rent values into the GEP hydropower table
# ------------------------------------------------------------------------------

gep_hydro <- cwonresrent[
  gep_hydro,
  on = "iso3_r250_name"
]

# Put the original GEP columns first
setcolorder(
  gep_hydro,
  c(
    "iso3_r250_id",
    "iso3_r250_label",
    "iso3_r250_name",
    "EXAMPLE_SERVICE_gep",
    "gep_hydro_cwonresrent_2019usd"
  )
)


# ------------------------------------------------------------------------------
# Confirm that all cleaned CWON resource-rent observations match the GEP country names
# ------------------------------------------------------------------------------

unmatched_cwonresrent <- cwonresrent[
  !gep_hydro,
  on = "iso3_r250_name"
]

cat(
  "Number of CWON resource-rent observations without a GEP match:",
  nrow(unmatched_cwonresrent),
  "\n"
)

print(unmatched_cwonresrent)

stopifnot(nrow(unmatched_cwonresrent) == 0L)

# ==============================================================================
# Clean and reorder hydropower GEP data
# ==============================================================================

# Drop the example service column, if present
gep_hydro[
  ,
  EXAMPLE_SERVICE_gep := NULL
]

# Reorder columns
setcolorder(
  gep_hydro,
  c(
    "iso3_r250_id",
    "iso3_r250_label",
    "iso3_r250_name",
    "gep_hydro_cwonresrent_2019usd"
  )
)

# Sort rows alphabetically by ISO3 label
setorder(gep_hydro, iso3_r250_label)

# Clean formatted currency strings and convert to numeric
gep_hydro[
  ,
  gep_hydro_cwonresrent_2019usd :=
    as.numeric(
      gsub(
        ",",
        "",
        trimws(gep_hydro_cwonresrent_2019usd)
      )
    )
]

# Confirm conversion
str(gep_hydro$gep_hydro_cwonresrent_2019usd)

# ==============================================================================
# Summary
# ==============================================================================

n_non_na_gep_hydro <- gep_hydro[
  !is.na(gep_hydro_cwonresrent_2019usd),
  .N
]

global_gep_hydro_cwonresrent_2019usd <- gep_hydro[
  ,
  sum(gep_hydro_cwonresrent_2019usd, na.rm = TRUE)
]

cat(
  "Number of non-NA rows:",
  n_non_na_gep_hydro,
  "\n"
)

cat(
  "Global hydropower direct-use GEP based on CWON resource rent in 2019 USD:",
  format(
    global_gep_hydro_cwonresrent_2019usd,
    big.mark = ",",
    scientific = FALSE
  ),
  "\n"
)

gep_hydro_summary <- gep_hydro[
  ,
  .(
    n_total_rows = .N,
    n_non_na_rows = sum(!is.na(gep_hydro_cwonresrent_2019usd)),
    n_missing_rows = sum(is.na(gep_hydro_cwonresrent_2019usd)),
    global_gep_hydro_cwonresrent_2019usd =
      sum(gep_hydro_cwonresrent_2019usd, na.rm = TRUE)
  )
]

gep_hydro_summary


# ------------------------------------------------------------------------------
# Create summary data table
# ------------------------------------------------------------------------------

gep_hydro_summary <- gep_hydro[
  ,
  .(
    n_total_rows = .N,
    n_non_na_rows = sum(!is.na(gep_hydro_cwonresrent_2019usd)),
    n_missing_rows = sum(is.na(gep_hydro_cwonresrent_2019usd)),
    global_gep_hydro_cwonresrent_2019usd =
      sum(gep_hydro_cwonresrent_2019usd, na.rm = TRUE),
    mean_gep_hydro_cwonresrent_2019usd =
      mean(gep_hydro_cwonresrent_2019usd, na.rm = TRUE),
    median_gep_hydro_cwonresrent_2019usd =
      median(gep_hydro_cwonresrent_2019usd, na.rm = TRUE),
    min_gep_hydro_cwonresrent_2019usd =
      min(gep_hydro_cwonresrent_2019usd, na.rm = TRUE),
    max_gep_hydro_cwonresrent_2019usd =
      max(gep_hydro_cwonresrent_2019usd, na.rm = TRUE)
  )
]

print(gep_hydro_summary)


# ==============================================================================
# Save hydropower GEP outputs
# ==============================================================================

output_dir <- paste0(
  "F:/Dropbox/400_research/430_projects/432_UMN/443_gep/",
  "gep_hydropower/intermediate/gep_hydropower/2026"
)

output_file <- file.path(
  output_dir,
  "gep_hydro_directuse_CWONresrent_20260720.csv"
)

# Create the output directory if it does not already exist
dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Save country-level hydropower GEP data
fwrite(
  gep_hydro,
  output_file
)

cat(
  "Hydropower GEP data saved to:",
  output_file,
  "\n"
)