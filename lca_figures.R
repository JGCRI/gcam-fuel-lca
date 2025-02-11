# This script generates figures from the upstream/LCA type data for the OPT-GPT analysis products
# Page Kyle, October 2024

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

fuels_lca_ghg <- read_csv("outputs/fuels_lca_ghg.csv")
fuels_primary_energy <- read_csv("outputs/fuels_primary_energy.csv")
fuels_prices <- read_csv("outputs/fuels_prices.csv")


#-------------------------------------------------------------------------------

pe_color_scheme <- c("Biomass" = "green",
                     "Soybean oil" = "brown",
                     "Coal" = "black",
                       "Nuclear" = "orange",
                       "Natural Gas" = "blue",
                       "Geothermal" = "red",
                       "Hydropower" = "blue4",
                       "Solar" = "yellow",
                       "Wind" = "seagreen",
                       "Oil" = "gray")

fuels_lca_ghg_base <- filter(fuels_lca_ghg, scenario == "h1r1b1n1c1p1")
p <- ggplot(fuels_lca_ghg_base, aes(x = kgCO2e_GJ, y = paste(fuel, year), fill = source)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("") +
  xlab("kg CO2e per GJ") +
  labs(fill = "") +
  theme_bw()

ggsave("figures/fuels_ghgs_base.png", height = 6, width = 8, units = "in")

# zoom in on hydrogen and ammonia across the scenarios
h2_nh3_lca_ghg <- filter(fuels_lca_ghg, fuel %in% c("hydrogen", "ammonia"))

p <- ggplot(h2_nh3_lca_ghg, aes(x = kgCO2e_GJ, y = scenario, fill = source)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(fuel ~ year) +
  ylab("") +
  xlab("kg CO2e per GJ") +
  labs(fill = "") +
  theme_bw()

ggsave("figures/h2_nh3_lca_ghg.png", height = 6, width = 8, units = "in")

# just show 2050 from the base scenario
p <- ggplot(filter(fuels_lca_ghg_base, year == 2050), aes(x = kgCO2e_GJ, y = fuel, fill = source)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("") +
  xlab("kg CO2e per GJ") +
  labs(fill = "") +
  theme_bw()

ggsave("figures/fuels_ghgs_2050.png", height = 6, width = 8, units = "in")

# primary energy in the base scenario
fuels_primary_energy_base <- filter(fuels_primary_energy, scenario == "h1r1b1n1c1p1")

p <- ggplot(filter(fuels_primary_energy_base, year == 2050), aes(x = IO, y = fuel, fill = Primary_fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("") +
  xlab("Input/Output") +
  labs(fill = "") +
  scale_fill_manual(values = pe_color_scheme) +
  theme_bw()

ggsave("figures/fuels_pe_base_2050.png", height = 6, width = 8, units = "in")

# show how the blue vs green changes the primary energy footprint of hydrogen and ammonia
p <- ggplot(filter(fuels_primary_energy, grepl("h2|h4", scenario), year == 2050,
                   fuel %in% c("hydrogen", "ammonia")),
            aes(x = IO, y = scenario, fill = Primary_fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~fuel) +
  ylab("") +
  xlab("Input/Output") +
  labs(fill = "") +
  scale_fill_manual(values = pe_color_scheme) +
  theme_bw()

ggsave("figures/h2_nh3_pe.png", height = 6, width = 8, units = "in")

#prices by scenario and year
p <- ggplot(fuels_prices, aes(x = year, y = price_USDperGJ, color = scenario)) +
  geom_line() +
  facet_wrap(~reporting_fuel) +
  ylab("") +
  xlab("$/GJ") +
  labs(fill = "") +
  theme_bw()

ggsave("figures/fuels_prices.png", height = 6, width = 8, units = "in")

