################################################################################
#-------------------- Script Information--------------------
# Project: Global GEP: Hydropower-Calculation
# Author: Lifeng Ren
# Date Created: 11/24/2024
# Last Modified: 11/24/2024
# Description: This script with takes in the raw csvs and calculate the GEP-hydropower
#----------------End Script Information--------------------# 
################################################################################



################################################################################
# 1. Setup----
################################################################################

# 1.1 Load Required Libraries
library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)
# library(gganimate)

# 1.2 Define Working Directory
setwd("D:/Users/lifengren/Dropbox/400_research/440_UMN/gep/gep_hydropower/scripts")
#setwd("C:/Users/lifengren/Dropbox/400_research/440_UMN/gep/gep_fisheries/scripts")

# Define output directory
output_dir <- "../intermediate/gep_hydropower/"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Output directory created:", output_dir, "\n")
} else {
  cat("Output directory already exists:", output_dir, "\n")
}

################################################################################
# 2. Data Import----
################################################################################

# Import the data using fread
hydro_elec_quant <- fread("../data/raw_data/eia/eia_net_hydroelectricity_generation.csv", header = TRUE)
hydro_elec_price <- read_excel(
  "../data/raw_data/worldbank/P_electricity_worldbank.xlsx",
  col_types = c("text", "text", "text", "text")
)

# Convert the electricity price column to numeric
hydro_elec_price$P_electricity_UScents_per_kWh <- as.numeric(hydro_elec_price$P_electricity_UScents_per_kWh)



################################################################################
# 3. Data Cleaning----
################################################################################

## 3.1 Change both Quant and Price Data into Panel----

# Assuming hydro_elec_quant is already a data.table
hydro_elec_quant <- melt(
  hydro_elec_quant,
  id.vars = "Country_Name",          # Keep this column as-is
  variable.name = "Year",            # Name for the new column containing years
  value.name = "hydro_net_gen_billion_kwh" # Name for the values column
)

# Convert Year column to numeric
hydro_elec_quant[, Year := as.numeric(as.character(Year))]

## 3.2 Rename the hydro_elec_price col Time into Year so we can merge later

# Rename the column in base R
colnames(hydro_elec_price)[colnames(hydro_elec_price) == "Time"] <- "Year"

setDT(hydro_elec_price)

## 3.3 Country Names in Quant and Price are not consistent

# Identify mismatched country names
mismatched_names <- setdiff(hydro_elec_price$Country_Name, hydro_elec_quant$Country_Name)

# View the mismatched names
print(mismatched_names)

# Proposed renaming dictionary
# Updated renaming map
renaming_map <- list(
  "Bahamas, The" = "The Bahamas",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Czech Republic" = "Czechia",
  "Egypt, Arab Rep." = "Egypt",
  "Gambia, The" = "        Gambia, The",
  "Iran, Islamic Rep." = "Iran",
  "Korea, Rep." = "South Korea",
  "Lao PDR" = "Laos",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Russian Federation" = "Russia",
  "São Tomé and Principe" = "Sao Tome and Principe",
  "Slovak Republic" = "Slovakia",
  "Syrian Arab Republic" = "Syria",
  "Taiwan, China" = "Taiwan",
  "Turkey" = "Turkiye",
  "Venezuela, RB" = "Venezuela",
  "Yemen, Rep." = "Yemen",
  "Brunei Darussalam" = "Brunei",
  "Congo, Dem. Rep." = "Congo-Kinshasa",
  "Congo, Rep." = "Congo-Brazzaville",
  "Hong Kong, China" = "Hong Kong",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Myanmar" = "Burma",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "St. Lucia" = "Saint Lucia",
  "St. Vincent and the Grenadines" = "Saint Vincent/Grenadines",
  "West Bank and Gaza" = "Palestinian Territories"
)

# Convert renaming map to a named vector
renaming_vector <- unlist(renaming_map)

# Apply renaming
hydro_elec_price[, Country_Name := ifelse(
  Country_Name %chin% names(renaming_vector),
  renaming_vector[Country_Name],
  Country_Name
)]

# Check for remaining mismatches
mismatched_names_after <- setdiff(hydro_elec_price$Country_Name, hydro_elec_quant$Country_Name)
print(mismatched_names_after)
# [1] "Liechtenstein"    "Marshall Islands" "Palau"            "San Marino"    
# We drop those island for now

################################################################################
# 4. Data Merging----
################################################################################

# Ensure both datasets are data.tables and Year is the same type
setDT(hydro_elec_price)
setDT(hydro_elec_quant)
hydro_elec_price[, Year := as.character(Year)]
hydro_elec_quant[, Year := as.character(Year)]

# Merge the datasets
merged_data <- merge(
  hydro_elec_price,
  hydro_elec_quant,
  by = c("Country_Name", "Year"),
  all.x = TRUE
)

# Get unmatched rows using anti-join logic
unmerged_data <- hydro_elec_price[!hydro_elec_quant, on = .(Country_Name, Year)]

# Drop rows in unmerged_data from merged_data
cleaned_data <- merged_data[!unmerged_data, on = .(Country_Name, Year)]

# Clean leading and trailing spaces in Country_Name
cleaned_data[, Country_Name := trimws(Country_Name)]

################################################################################
# 5. GEP Calculation----
################################################################################

# Filter the data for years 2014 and later as Price Data only avaible after 2014
cleaned_data <- cleaned_data[Year >= 2014]

# Drop rows where hydro_net_gen_billion_kwh is 0 or NA: there is not hydro Production
cleaned_data <- cleaned_data[hydro_net_gen_billion_kwh != 0 & !is.na(hydro_net_gen_billion_kwh)]


# Ensure columns are numeric
cleaned_data[, P_electricity_UScents_per_kWh := as.numeric(P_electricity_UScents_per_kWh)]
cleaned_data[, hydro_net_gen_billion_kwh := as.numeric(hydro_net_gen_billion_kwh)]

# Calculate GEP_hydroelectricity_usd1000
gep_hydro <- cleaned_data[, GEP_hydroelectricity_usd1000 := 
               (P_electricity_UScents_per_kWh / 100000) * hydro_net_gen_billion_kwh * 1e9
]

fwrite(gep_hydro, paste0(output_dir,"gep_hydro_year.csv"))


# # Subset the data for the year 2018
# gep_hydro_2018 <- gep_hydro[Year == 2018]
# 
# # Filter rows where GEP_hydroelectricity_usd1000 is 0 or NA
# zero_or_na_gep <- gep_hydro[GEP_hydroelectricity_usd1000 == 0 | is.na(GEP_hydroelectricity_usd1000)]
# 
# # Display the rows
# print(zero_or_na_gep)

################################################################################
# 5. GEP Hydropower Summary----
################################################################################

# Add global total GEP for each year (convert to USD billion)
global_gep_summary <- cleaned_data_no_na[, {
  num_countries = uniqueN(Country_Name)
  max_gep = max(GEP_hydroelectricity_usd1000, na.rm = TRUE) / 1e6  # Convert to billion
  min_gep = min(GEP_hydroelectricity_usd1000, na.rm = TRUE) / 1e6  # Convert to billion
  median_gep = median(GEP_hydroelectricity_usd1000, na.rm = TRUE) / 1e6  # Convert to billion
  total_gep = sum(GEP_hydroelectricity_usd1000, na.rm = TRUE) / 1e6  # Convert to billion
  
  # Identify countries with max and min GEP
  country_max = Country_Name[GEP_hydroelectricity_usd1000 == max_gep * 1e6]
  country_min = Country_Name[GEP_hydroelectricity_usd1000 == min_gep * 1e6]
  
  # Calculate absolute differences from median
  abs_diff = abs(GEP_hydroelectricity_usd1000 - median_gep * 1e6)
  min_abs_diff = min(abs_diff, na.rm = TRUE)
  
  # Identify country or countries closest to median GEP
  country_median = Country_Name[abs_diff == min_abs_diff]
  
  .(Year, num_countries, max_gep, min_gep, median_gep, total_gep,
    country_max = paste(country_max, collapse = ", "),
    country_min = paste(country_min, collapse = ", "),
    country_median = paste(country_median, collapse = ", "))
}, by = Year]

# View the updated summary
print(global_gep_summary)

fwrite(global_gep_summary, paste0(output_dir,"global_gep_hydro_summary.csv"))


library(ggplot2)

# Check and fix duplicate column names
if (anyDuplicated(names(global_gep_summary))) {
  global_gep_summary <- global_gep_summary[, !duplicated(names(global_gep_summary)), with = FALSE]
}

# Convert Year to numeric
global_gep_summary$Year <- as.numeric(global_gep_summary$Year)

# Create a line plot of global total GEP over time and assign it to a variable
gep_plot <- ggplot(global_gep_summary, aes(x = Year, y = total_gep)) +
  geom_line(color = "blue", linewidth = 1.2) +  # Use `linewidth` instead of `size`
  geom_point(color = "red", size = 3) +
  labs(
    title = "Global Total GEP from Hydroelectricity (2014-2019)",
    x = "Year",
    y = "Global GEP (USD Billion)"
  ) +
  theme_minimal()

# Ensure the output directory has a trailing slash
output_dir <- ifelse(grepl("/$", output_dir), output_dir, paste0(output_dir, "/"))

# Save the plot as a PNG file
ggsave(filename = paste0(output_dir, "global_gep_hydro_line_plot.png"), 
       plot = gep_plot,  # Use the created plot
       width = 6, 
       height = 4, 
       dpi = 300)
# # Install gganimate if not already installed
# if (!requireNamespace("gganimate", quietly = TRUE)) {
#   install.packages("gganimate")
# }

# # Calculate share of GEP for each country by year
# country_shares <- cleaned_data_no_na[, .(
#   total_gep = sum(GEP_hydroelectricity_usd1000, na.rm = TRUE)
# ), by = .(Year, Country_Name)]
# 
# # Add global total for each year
# global_totals <- cleaned_data_no_na[, .(
#   global_total_gep = sum(GEP_hydroelectricity_usd1000, na.rm = TRUE)
# ), by = Year]
# 
# # Merge and calculate share
# country_shares <- merge(country_shares, global_totals, by = "Year")
# country_shares[, share := total_gep / global_total_gep * 100]
# 
# # Ensure your country_shares data is a data.table
# setDT(country_shares)
# 
# # Function to process data for top 10 countries and group others
# process_top_countries <- function(data, year) {
#   # Filter data for the specific year
#   year_data <- data[Year == year]
#   
#   # Handle potential NAs
#   year_data <- year_data[!is.na(share)]
#   
#   # Sort by share in descending order
#   year_data <- year_data[order(-share)]
#   
#   # Select top 10 countries
#   top_countries <- head(year_data, 10)
#   
#   # Calculate share for "Others"
#   others_share <- 100 - sum(top_countries$share)
#   
#   # Append "Others" row if necessary
#   if (others_share > 0) {
#     top_countries <- rbind(
#       top_countries,
#       data.table(
#         Year = year,
#         Country_Name = "Others",
#         total_gep = NA,
#         global_total_gep = NA,
#         share = others_share
#       )
#     )
#   }
#   
#   return(top_countries)
# }
# 
# # Apply the function to all years
# top_country_data <- rbindlist(
#   lapply(unique(country_shares$Year), function(year) process_top_countries(country_shares, year))
# )
# 
# # Ensure Year is treated as a factor for proper animation
# top_country_data$Year <- as.factor(top_country_data$Year)
# 
# # Create animated pie chart
# animated_pie <- ggplot(top_country_data, aes(x = "", y = share, fill = Country_Name)) +
#   geom_bar(stat = "identity", width = 1, color = "white") +
#   coord_polar(theta = "y") +
#   labs(
#     title = "Share of GEP by Country: {closest_state}",
#     fill = "Country"
#   ) +
#   theme_void() +
#   theme(legend.position = "right") +
#   transition_states(Year, transition_length = 1, state_length = 2) +
#   ease_aes('cubic-in-out')
# 
# # Render animation
# animate(animated_pie, nframes = length(unique(top_country_data$Year)) * 20, fps = 10)
