# ==============================================================================
# ETC5543 Fish Hydroacoustics - Initial EDA
# ==============================================================================
# Purpose: Basic exploration of fish acoustic data
# Author: Dulitha Perera
# ==============================================================================

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(forcats)

# Create folders
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("outputs")) dir.create("outputs")

# ==============================================================================
# 1. LOAD AND INSPECT DATA
# ==============================================================================



# Load the data
fish_data <- read_rds("data/TSresponse_clean.RDS")



# ==============================================================================
# 2. CHECK KEY VARIABLES
# ==============================================================================


# Check some important columns
key_vars <- c("species", "totalLength", "weight", "Target_true_depth")

for(var in key_vars) {
  if(var %in% names(fish_data)) {
    cat(var, "- OK\n")
  } else {
    cat(var, "- MISSING\n")
  }
}

# Look at size differences (Average Length and Weights & Counts by Species)
cat("\nSize summary by species:\n")
size_summary <- fish_data |>
  group_by(species) |>
  summarise(
    count = n(),
    avg_length = round(mean(totalLength, na.rm = TRUE)),
    avg_weight = round(mean(weight, na.rm = TRUE))
  )
print(size_summary)

# ==============================================================================
# 3. SIMPLE PLOTS
# ==============================================================================



# 1. Size comparison
p1 <- ggplot(fish_data, aes(x = species, y = totalLength, fill = species)) +
  geom_boxplot() +
  labs(title = "Fish Length by Species",
       x = "Species", 
       y = "Total Length (mm)") +
  theme_minimal()

print(p1)
ggsave("figures/01_length_comparison.png", p1, width = 8, height = 6)

# 2. Weight comparison  
p2 <- ggplot(fish_data, aes(x = species, y = weight, fill = species)) +
  geom_boxplot() +
  labs(title = "Fish Weight by Species",
       x = "Species",
       y = "Weight (g)") +
  theme_minimal()

print(p2)
ggsave("figures/02_weight_comparison.png", p2, width = 8, height = 6)

# 3. Depth distribution
p3 <- ggplot(fish_data, aes(x = Target_true_depth, fill = species)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  labs(title = "Depth Distribution by Species",
       x = "Depth (m)",
       y = "Count") +
  theme_minimal()

print(p3)
ggsave("figures/03_depth_distribution.png", p3, width = 8, height = 6)

# Girth comparison
p4 <- ggplot(fish_data, aes(x = species, y = girth, fill = species)) +
  geom_boxplot() +
  labs(title = "Fish Girth by Species",
       x = "Species",
       y = "Girth (mm)") +
  theme_minimal()

print(p4)
ggsave("figures/04_girth_comparison.png", p4, width = 8, height = 6)


# Air bladder length comparison
p5 <- ggplot(fish_data, aes(x = species, y = airbladderTotalLength, fill = species)) +
  geom_boxplot() +
  labs(title = "Air Bladder Length by Species",
       x = "Species",
       y = "Air Bladder Length (mm)") +
  theme_minimal()

print(p5)
ggsave("figures/05_airbladder_length.png", p5, width = 8, height = 6)


# Air bladder width comparison
p6 <- ggplot(fish_data, aes(x = species, y = airBladderWidth, fill = species)) +
  geom_boxplot() +
  labs(title = "Air Bladder Width by Species",
       x = "Species",
       y = "Air Bladder Width (mm)") +
  theme_minimal()

print(p6)
ggsave("figures/06_airbladder_width.png", p6, width = 8, height = 6)


# Air bladder ratio (length/width)
fish_data <- fish_data %>%
  mutate(airbladder_ratio = airbladderTotalLength / airBladderWidth)

p7 <- ggplot(fish_data, aes(x = species, y = airbladder_ratio, fill = species)) +
  geom_boxplot() +
  labs(title = "Air Bladder Ratio (Length / Width) by Species",
       x = "Species", y = "Ratio") +
  theme_minimal()
print(p7)
ggsave("figures/07_airbladder_ratio.png", p7, width = 8, height = 6)


# Species proportion by region

# --- Clean region labels (keep just the numeric ID) ---
fish_data <- fish_data %>%
  mutate(
    region_short = gsub("^\\s*Region_\\s*", "", Region_name) |> trimws()
  )

# --- Counts and proportions by region × species ---
region_species <- fish_data %>%
  count(region_short, species, name = "n") %>%
  group_by(region_short) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# --- Force numeric ordering on the y-axis ---
region_species_num <- region_species %>%
  mutate(region_num = readr::parse_number(region_short)) %>%
  filter(!is.na(region_num))

num_levels <- sort(unique(region_species_num$region_num))

region_species_num <- region_species_num %>%
  mutate(region_id = factor(region_num, levels = num_levels))  # ordered factor 1..N

# --- Plot ---
p_region_species <- ggplot(region_species_num,
                           aes(x = prop, y = region_id, fill = species)) +
  geom_col(color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Species Proportion by Region (Region order)",
       x = "Proportion", y = "Region") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

print(p_region_species)
ggsave("figures/species_proportion_by_region.png",
       p_region_species, width = 9, height = 12, dpi = 200)
