# This script generates the upstream/LCA type data for the OPT-GPT analysis products
# Page Kyle, October 2024

library(rgcam)
library(dplyr)
library(tidyr)
library(readr)

fuel_techs <- read_csv("inputs/fuel_techs.csv")
co2_ef <- read_csv("inputs/co2_ef.csv")

#-------------------------------------------------------------------------------

CONV_USD_1975_2020 <- 3.8
ANALYSIS_YEARS <- seq(2030, 2050, 10)
RECURSION_DEPTH <- 10
PRIMARY_COMMODITIES <- co2_ef$sector

#-------------------------------------------------------------------------------

gcam_data.proj <- loadProject("rgcam/gcam_data.proj")
REQUERY_OUTPUT <- FALSE
if(REQUERY_OUTPUT){
  conn <- localDBConn("/Users/D3P747/Desktop/stash/freight_fuels/output/", "database_basexdb")
  gcam_data.proj <- addScenario(conn, gcam_data.proj, c(), "rgcam/BatchQueries_gcam_lca.xml", clobber = TRUE)
}

# the method starts from a technology, so the queries are done at the technology level. however once the immediate inputs to
# the technology are known, all subsequent upstream steps take place at the sectoral level.

#outputs: drop all secondary outputs
outputs_tech <- getQuery(gcam_data.proj, "outputs by tech") %>%
  filter(output == sector, year %in% ANALYSIS_YEARS) %>%
  select(-region, -Units, -output)

outputs_sector <- outputs_tech %>%
  group_by(scenario, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#inputs: drop all water (not within scops of LCA), and oil-credits
inputs_tech <- getQuery(gcam_data.proj, "inputs by tech") %>%
  select(-Units, -region) %>%
  filter(!grepl("water", input),
         input != "oil-credits",
         year %in% ANALYSIS_YEARS)

# inputs to hydropower production are not picked up in the query of inputs
hydro_tech <- filter(outputs_tech,
                     technology =="hydro", year %in% ANALYSIS_YEARS) %>%
  mutate(input = "hydropower")

# carbon-storage is not picked up as an input to technologies. map it in
ccs_tech <- getQuery(gcam_data.proj, "CO2 sequestration by tech") %>%
  select(-region, -Units) %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(input = "carbon-storage")

# bind in ancillary data to inputs by technology data table
inputs_tech <- bind_rows(inputs_tech, hydro_tech, ccs_tech)

inputs_sector <- inputs_tech %>%
  group_by(scenario, sector, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# calculate the technoloy- and sector-level input-output coefficients that are used to move upstream
IOcoefs_tech <- left_join(inputs_tech, outputs_tech,
                     by = c("scenario", "sector", "subsector", "technology", "year"),
                     suffix = c(".input", ".output")) %>%
  mutate(value = value.input / value.output) %>%
  select(-value.input, -value.output)

IOcoefs_sector <- left_join(inputs_sector, outputs_sector,
                          by = c("scenario", "sector", "year"),
                          suffix = c(".input", ".output")) %>%
  mutate(value = value.input / value.output) %>%
  select(-value.input, -value.output)

# create the base data tables that scenario- and fuel-specific data will be binded onto
fuels_upstream_co2 <- tibble(fuel = character(0),
                             scenario = character(0),
                             year = numeric(),
                             MtCO2 = numeric(),
                             EJ_out = numeric(),
                             kgCO2_GJ = numeric())

fuels_primary_energy <- tibble(fuel = character(0),
                               scenario = character(0),
                               year = numeric(),
                               Primary_fuel = character(0),
                               EJ_in = numeric(),
                               EJ_out = numeric(),
                               IO = numeric())

# for each technology (fuel) analyzed, as indicated in fuel_techs.csv:
for(i in 1:nrow(fuel_techs)){

  # Start with the output of the target technology (fuel)
  fuel_production <- outputs_tech %>%
    inner_join(fuel_techs[i,], by = c("sector", "subsector", "technology")) %>%
    select(scenario, fuel, year, EJ_out = value)
  
  # Inputs one step upstream are determined as fuel production times each input's IO coefficient
  upstream_inputs <- outputs_tech %>%
    semi_join(fuel_techs[i,], by = c("sector", "subsector", "technology")) %>%
    left_join(IOcoefs_tech, by = c("scenario", "sector", "subsector", "technology", "year")) %>%
    mutate(value = value.x * value.y) %>%
    select(scenario, input, year, value) %>%
    rename(sector = input)
  
  # separate out any primary energy inputs, and pass the remaining data table to the next step
  primary_inputs <- filter(upstream_inputs, sector %in% PRIMARY_COMMODITIES)
  upstream_inputs <- filter(upstream_inputs, !sector %in% PRIMARY_COMMODITIES)
  
  # Remaining steps are at the sectoral level. relationship="many-to-many" in the join as there may be multiple paths to
  # the same upstream commodity at the same recursion depth (e.g., electricity may be an input to multiple intermediate inputs)
    for(j in 1:RECURSION_DEPTH){
      upstream_inputs <- upstream_inputs %>%
        left_join(IOcoefs_sector, by = c("scenario", "sector", "year"),
                  relationship = "many-to-many") %>%
        mutate(value = value.x * value.y) %>%
        select(scenario, input, year, value) %>%
        rename(sector = input)
      
      primary_inputs <- bind_rows(primary_inputs,
                                  filter(upstream_inputs, sector %in% PRIMARY_COMMODITIES))
      upstream_inputs <- filter(upstream_inputs, !sector %in% PRIMARY_COMMODITIES)
    }
  # aggregate by commodity type ("sector")
  primary_inputs <- group_by(primary_inputs, scenario, sector, year) %>%
    summarise(EJ_in = sum(value)) %>%
    ungroup()
  
  # calculate CO2 emissions (Mt CO2) by joining in emissions factors, multiplying, and adding
  # join in fuel output from the target technology to calculate the intensity (kg CO2 upstream per unit of fuel produced)
  upstream_co2 <- left_join(primary_inputs, co2_ef,
                            by = "sector") %>%
    mutate(MtCO2 = EJ_in * CO2_kgGJ) %>%
    group_by(scenario, year) %>%
    summarise(MtCO2 = sum(MtCO2)) %>%
    ungroup() %>%
    left_join(fuel_production, by = c("scenario", "year")) %>%
    mutate(kgCO2_GJ = MtCO2 / EJ_out) %>%
    select(fuel, scenario, year, MtCO2, EJ_out, kgCO2_GJ)
  
  fuels_upstream_co2 <- bind_rows(fuels_upstream_co2, upstream_co2)
  
  primary_energy <- left_join(primary_inputs, co2_ef, by = "sector") %>%
    filter(!is.na(Primary_fuel)) %>%
    group_by(scenario, Primary_fuel, year) %>%
    summarise(EJ_in = sum(EJ_in)) %>%
    ungroup() %>%
    left_join(fuel_production, by = c("scenario", "year")) %>%
    mutate(IO = EJ_in / EJ_out) %>%
    select(fuel, scenario, year, Primary_fuel, EJ_in, EJ_out, IO)
  
  fuels_primary_energy <- bind_rows(fuels_primary_energy, primary_energy)
  }

# in some cases, the fuel production technologies may be more granular than what we want for reporting.
# collapse to a small number of reporting categories
fuels_upstream_co2 <- fuels_upstream_co2 %>%
  left_join(select(fuel_techs, fuel, reporting_fuel),
            by = "fuel") %>%
  group_by(reporting_fuel, scenario, year) %>%
  summarise(MtCO2 = sum(MtCO2),
            EJ_out = sum(EJ_out)) %>%
  ungroup() %>%
  mutate(kgCO2_GJ = MtCO2 / EJ_out) %>%
  rename(fuel = reporting_fuel) %>%
  select(fuel, scenario, year, kgCO2_GJ)

fuels_primary_energy <- fuels_primary_energy %>%
  left_join(select(fuel_techs, fuel, reporting_fuel),
            by = "fuel") %>%
  group_by(reporting_fuel, scenario, year, Primary_fuel) %>%
  summarise(EJ_in = sum(EJ_in),
            EJ_out = sum(EJ_out)) %>%
  ungroup() %>%
  mutate(IO = EJ_in / EJ_out) %>%
  rename(fuel = reporting_fuel) %>%
  select(fuel, scenario, year, Primary_fuel, IO)

# write out the data tables
write_csv(fuels_upstream_co2, "outputs/fuels_upstream_co2.csv")
write_csv(fuels_primary_energy, "outputs/fuels_primary_energy.csv")

# todo
# add in tailpipe emissions (just output times exogenous CO2, CH4, N2O emissions factors)
# compile and write out price data
# assign CH4 and N2O emissions as "inputs by sector"
# assign them as primary commodities in co2_ef (EF = GWP)
# keep them separate for reporting purposes







