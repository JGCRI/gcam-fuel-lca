# Disclaimer
# This material was prepared as an account of work sponsored by an agency of the United States Government. Neither the
# United States Government nor the United States Department of Energy, nor Battelle, nor any of their employees, nor any
# jurisdiction or organization that has cooperated in the development of these materials, makes any warranty, express or
# implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness or any
# information, apparatus, product, software, or process disclosed, or represents that its use would not infringe
# privately owned rights.
#
# Reference herein to any specific commercial product, process, or service by trade name, trademark, manufacturer, or
# otherwise does not necessarily constitute or imply its endorsement, recommendation, or favoring by the United States
# Government or any agency thereof, or Battelle Memorial Institute. The views and opinions of authors expressed herein
# do not necessarily state or reflect those of the United States Government or any agency thereof.
#
# PACIFIC NORTHWEST NATIONAL LABORATORY
# operated by
# BATTELLE
# for the
# UNITED STATES DEPARTMENT OF ENERGY
# under Contract DE-AC05-76RL01830

library(rgcam)
library(dplyr)
library(tidyr)
library(readr)

fuel_techs <- read_csv("inputs/fuel_techs.csv")
co2_ef <- read_csv("inputs/co2_ef.csv")
nonco2_gwp <- read_csv("inputs/nonco2_gwp.csv")
fuel_delivery_cost <- read_csv("inputs/fuel_delivery_cost.csv")
scenario_definitions <- read_csv("inputs/scenario_definitions.csv")

#-------------------------------------------------------------------------------

CONV_USD_1975_2020 <- 3.8
CONV_USD_1990_2020 <- 1.772
CONV_C_CO2 <- 44/12
ANALYSIS_YEARS <- seq(2020, 2050, 5)
LUC_YEARS <- 2020:2050
RECURSION_DEPTH <- 10
PRIMARY_COMMODITIES <- co2_ef$sector
NONCO2_GHGS <- c("CH4", "H2", "N2O")
ROUNDING_DIGITS <- 5

# For GREET comparison we need to use the convention of assigning equal tailpipe and
# upstream CO2 from biofuel and e-fuel production and use, rather than leaving both as 0
# This is a non-standard convention in GHG emissions accounting so it is provided
# here as an option
USE_GREET_CO2_REPORTING <- TRUE
KGCO2GJ_FOR_GREET_REPORTING <- fuel_techs$kgCO2_GJ[fuel_techs$fuel == "petroleum diesel"]
FUELS_WITH_GREET_REPORTING <- unique(fuel_techs$reporting_fuel[fuel_techs$sector == "diesel" &
                                                        fuel_techs$kgCO2_GJ == 0])
#-------------------------------------------------------------------------------

approx_fun <- function(year, value, rule = 1) {
  stats::approx(as.vector(year), value, rule = rule, xout = year, ties = mean)$y
}

# Query output is saved in blocks of 48 scenarios differentiated by ccs and policy assumptions
# These files are in /rcfs/projects/intermodal/gpk-workspace
# to copy locally:
# scp USERNAME@deception.pnl.gov:/rcfs/projects/intermodal/gpk-workspace/query_out_*.proj ./rgcam

query_out_c1p1.proj <- loadProject("rgcam/query_out_c1p1.proj")
query_out_c2p1.proj <- loadProject("rgcam/query_out_c2p1.proj")
query_out_c1p2.proj <- loadProject("rgcam/query_out_c1p2.proj")
query_out_c2p2.proj <- loadProject("rgcam/query_out_c2p2.proj")
query_out_c1p3.proj <- loadProject("rgcam/query_out_c1p3.proj")
query_out_c2p3.proj <- loadProject("rgcam/query_out_c2p3.proj")

query_out_all.proj <- c(query_out_c1p1.proj, query_out_c2p1.proj,
                        query_out_c1p2.proj, query_out_c2p2.proj,
                        query_out_c1p3.proj, query_out_c2p3.proj)

query_out_luc.proj <- loadProject("rgcam/query_out_luc.proj")

# the method starts from a technology, so the queries are done at the technology level. however once the immediate inputs to
# the technology are known, all subsequent upstream steps take place at the sectoral level.

#outputs: drop all secondary outputs
outputs_tech <- getQuery(query_out_all.proj, "outputs by tech") %>%
  filter(output == sector, year %in% ANALYSIS_YEARS) %>%
  select(-region, -Units, -output)

outputs_sector <- outputs_tech %>%
  group_by(scenario, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#inputs: drop all water (not within scope of LCA), and oil-credits
inputs_tech <- getQuery(query_out_all.proj, "inputs by tech") %>%
  select(-Units, -region) %>%
  filter(!grepl("water", input),
         input != "oil-credits",
         year %in% ANALYSIS_YEARS)

# inputs to hydropower production are not picked up in the query of inputs
hydro_tech <- filter(outputs_tech,
                     technology =="hydro", year %in% ANALYSIS_YEARS) %>%
  mutate(input = "hydropower")

# non-co2 emissions and carbon storage are not technically inputs to a technology, but categorizing them as such
# allows for the upstream attribution to take place alongside energy and co2 emissions attribution
# mapping in carbon-storage
ccs_tech <- getQuery(query_out_all.proj, "CO2 sequestration by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(input = "carbon-storage") %>%
  select(scenario, sector, subsector, technology, input, year, value)

# nonco2 emissions are similarly treated as inputs to a technology. Drop the AGR suffix
nonco2_tech <- getQuery(query_out_all.proj, "NonCO2 GHG emissions by tech") %>%
  mutate(input = sub("_AGR", "", ghg)) %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  select(scenario, sector, subsector, technology, input, year, value)

# bind in ancillary data to inputs by technology data table
inputs_tech <- bind_rows(inputs_tech, hydro_tech, ccs_tech, nonco2_tech)

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

fuels_upstream_nonco2_co2e <- tibble(fuel = character(0),
                                      scenario = character(0),
                                      year = numeric(),
                                      GHG = character(0),
                                      MtCO2e = numeric(),
                                      EJ_out = numeric(),
                                      kgCO2e_GJ = numeric())

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
  upstream_nonco2 <- filter(upstream_inputs, sector %in% NONCO2_GHGS)
  upstream_inputs <- filter(upstream_inputs, !sector %in% c(PRIMARY_COMMODITIES, NONCO2_GHGS))

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
      upstream_nonco2 <- bind_rows(upstream_nonco2,
                                  filter(upstream_inputs, sector %in% NONCO2_GHGS))
      upstream_inputs <- filter(upstream_inputs, !sector %in% c(PRIMARY_COMMODITIES, NONCO2_GHGS))
    }
  # aggregate primary energy by commodity type ("sector")
  primary_inputs <- group_by(primary_inputs, scenario, sector, year) %>%
    summarise(EJ_in = sum(value)) %>%
    ungroup()

  # aggregate nonco2 emissions by GHG type ("sector")
  upstream_nonco2 <- group_by(upstream_nonco2, scenario, sector, year) %>%
    summarise(TG = sum(value)) %>%
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

  upstream_nonco2_co2e <- upstream_nonco2 %>%
    rename(GHG = sector) %>%
    left_join(nonco2_gwp, by = "GHG") %>%
    mutate(MtCO2e = TG * GWP) %>%
    left_join(fuel_production, by = c("scenario", "year")) %>%
    mutate(kgCO2e_GJ = MtCO2e / EJ_out) %>%
    select(fuel, scenario, year, GHG, MtCO2e, EJ_out, kgCO2e_GJ)

  fuels_upstream_nonco2_co2e <- bind_rows(fuels_upstream_nonco2_co2e, upstream_nonco2_co2e)
  }

# in some cases, the fuel production technologies may be more granular than what we want for reporting.
# collapse to a small number of reporting categories
# subtract fossil carbon in fuels that will be reported as tailpipe co2
fuels_upstream_co2 <- fuels_upstream_co2 %>%
  left_join(select(fuel_techs, fuel, reporting_fuel),
            by = "fuel") %>%
  group_by(reporting_fuel, scenario, year) %>%
  summarise(MtCO2 = sum(MtCO2),
            EJ_out = sum(EJ_out)) %>%
  ungroup() %>%
  left_join(distinct(fuel_techs, reporting_fuel, kgCO2_GJ), by = "reporting_fuel") %>%
  rename(kgCO2_GJ_tailpipe = kgCO2_GJ) %>%
  mutate(kgCO2_GJ = MtCO2 / EJ_out - kgCO2_GJ_tailpipe) %>%
  rename(fuel = reporting_fuel) %>%
  select(fuel, scenario, year, kgCO2_GJ)

fuels_primary_energy <- fuels_primary_energy %>%
  left_join(select(fuel_techs, fuel, reporting_fuel),
            by = "fuel") %>%
  group_by(reporting_fuel, scenario, year, Primary_fuel) %>%
  summarise(EJ_in = sum(EJ_in),
            EJ_out = sum(EJ_out)) %>%
  ungroup() %>%
  mutate(IO = round(EJ_in / EJ_out, digits = ROUNDING_DIGITS)) %>%
  rename(fuel = reporting_fuel) %>%
  select(fuel, scenario, year, Primary_fuel, IO)

fuels_upstream_nonco2_co2e <- fuels_upstream_nonco2_co2e %>%
  left_join(select(fuel_techs, fuel, reporting_fuel),
            by = "fuel") %>%
  group_by(reporting_fuel, scenario, year, GHG) %>%
  summarise(MtCO2e = sum(MtCO2e),
            EJ_out = sum(EJ_out)) %>%
  ungroup() %>%
  mutate(kgCO2e_GJ = MtCO2e / EJ_out) %>%
  rename(fuel = reporting_fuel) %>%
  select(fuel, scenario, year, GHG, kgCO2e_GJ)

# tailpipe co2: this data table does not require any GCAM output, other than the scenario list
fuels_tailpipe_co2 <- fuel_techs %>%
  select(reporting_fuel, kgCO2_GJ) %>%
  distinct() %>%
  mutate(year = NA_real_,
         scenario = NA_character_) %>%
  complete(nesting(reporting_fuel, kgCO2_GJ),
           year = ANALYSIS_YEARS,
           scenario = unique(outputs_tech$scenario)) %>%
  drop_na() %>%
  select(fuel = reporting_fuel, scenario, year, kgCO2_GJ)

#tailpipe nonco2s: reported by the downstream consumers
# determine the nonco2 emissions from downstream freight transportation consumers,
# aggregate by general fuel type, calculate emissions factors, and expand to
# full reporting set of fuels.
fuels_tailpipe_nonco2_co2e <- filter(inputs_tech,
                                input %in% c(fuel_techs$sector, fuel_delivery_cost$delivery_cost),
                                sector %in% c("trn_freight_road", "trn_freight", "trn_shipping_intl")) %>%
  # for fuels whose end-use commodities have a different name from the fuel production sector, make the replacement
  left_join(fuel_delivery_cost, by = c(input = "delivery_cost"), suffix = c("", ".revised")) %>%
  mutate(fuel_category = if_else(is.na(sector.revised), input, sector.revised)) %>%
  rename(fuel_EJ = value) %>%
  select(-sector.revised, -input) %>%
  # join the nonco2 emissions to the energy consumption. this is an expanding join for freight
  # technologies that produce multiple GHGs, and a filtering join for those that produce none.
  inner_join(nonco2_tech, by = c("scenario", "sector", "subsector", "technology", "year")) %>%
  rename(GHG = input) %>%
  left_join(nonco2_gwp, by = "GHG") %>%
  mutate(MtCO2e = value * GWP) %>%
  group_by(scenario, fuel_category, year, GHG) %>%
  summarise(fuel_EJ = sum(fuel_EJ),
            MtCO2e = sum(MtCO2e)) %>%
  ungroup() %>%
  mutate(kgCO2e_GJ = MtCO2e / fuel_EJ) %>%
  # expand the general fuels (e.g. diesel) to the reporting fuels (e.g., petroleum diesel, bio-based, e-diesel)
  left_join(distinct(fuel_techs, sector, reporting_fuel),
            by = c(fuel_category = "sector"),
            relationship = "many-to-many") %>%
  select(fuel = reporting_fuel, scenario, year, GHG, kgCO2e_GJ)

# prices: calculated from output-weighted average where multiple techs pipe to the same reporting fuel
# the first step is to add in the delivery costs where those are indicated in a separate sector
delivered_fuel_prices <- getQuery(query_out_all.proj, "prices by sector") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector %in% fuel_delivery_cost$delivery_cost)
delivery_costs <- getQuery(query_out_all.proj, "prices by sector") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector %in% fuel_delivery_cost$sector) %>%
  left_join(fuel_delivery_cost, by = "sector") %>%
  left_join(delivered_fuel_prices, by = c("scenario", delivery_cost = "sector", "year")) %>%
  mutate(delivery_cost = value.y - value.x) %>%
  select(scenario, sector, year, delivery_cost)

# co2 penalties are estimated from scenario-specific co2 prices, converted to 2020$ per kg of CO2
co2_penalties <- getQuery(query_out_all.proj, "CO2 prices") %>%
  filter(market == "USACO2") %>%
  mutate(USD_kgCO2 = value * CONV_USD_1990_2020 / CONV_C_CO2 / 1000) %>%
  select(scenario, year, USD_kgCO2)

fuels_prices <- getQuery(query_out_all.proj, "costs by tech") %>%
  inner_join(fuel_techs, by = c("sector", "subsector", "technology")) %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  left_join(delivery_costs, by = c("scenario", "sector", "year")) %>%
  left_join(co2_penalties, by = c("scenario", "year")) %>%
  left_join(outputs_tech, by = c("scenario", "sector", "subsector", "technology", "year"),
            suffix = c(".price", ".quantity")) %>%
  replace_na(list(delivery_cost = 0,
                  USD_kgCO2 = 0,
                  value.quantity = 0)) %>%
  mutate(total.price = (value.price + delivery_cost) * CONV_USD_1975_2020 + (kgCO2_GJ * USD_kgCO2),
         weighted.price = total.price * value.quantity) %>%
  group_by(scenario, reporting_fuel, year) %>%
  summarise(weighted.price = sum(weighted.price),
            value.quantity = sum(value.quantity)) %>%
  ungroup() %>%
  mutate(price_USDperGJ = round(weighted.price / value.quantity, ROUNDING_DIGITS)) %>%
  select(-weighted.price, -value.quantity)

# Add the emissions source information to the emissions data tables and bind
fuels_upstream_co2 <- mutate(fuels_upstream_co2,
                             GHG = "CO2", source =  "Upstream CO2") %>%
  rename(kgCO2e_GJ = kgCO2_GJ)
fuels_upstream_nonco2_co2e <- mutate(fuels_upstream_nonco2_co2e,
                                     source = paste("Upstream", GHG))
fuels_tailpipe_co2 <- mutate(fuels_tailpipe_co2,
                             GHG = "CO2", source =  "Tailpipe CO2") %>%
  rename(kgCO2e_GJ = kgCO2_GJ)
fuels_tailpipe_nonco2_co2e <- mutate(fuels_tailpipe_nonco2_co2e,
                                     source = paste("Tailpipe", GHG))

# Apply GREET convention on carbon-based fuels whose CO2 is not counted in standard accounting
if(USE_GREET_CO2_REPORTING){
  fuels_upstream_co2$kgCO2e_GJ[fuels_upstream_co2$fuel %in% FUELS_WITH_GREET_REPORTING] <-
    fuels_upstream_co2$kgCO2e_GJ[fuels_upstream_co2$fuel %in% FUELS_WITH_GREET_REPORTING] -
    KGCO2GJ_FOR_GREET_REPORTING

  fuels_tailpipe_co2$kgCO2e_GJ[fuels_tailpipe_co2$fuel %in% FUELS_WITH_GREET_REPORTING] <-
    fuels_tailpipe_co2$kgCO2e_GJ[fuels_tailpipe_co2$fuel %in% FUELS_WITH_GREET_REPORTING] +
    KGCO2GJ_FOR_GREET_REPORTING
}

# Calculate LUC CO2 as cumulative additional LUC_CO2 divided by cumulative additional biofuel production
luc_fuel_map <- filter(fuel_techs, !is.na(luc_fuel)) %>%
  distinct(reporting_fuel, luc_fuel)
LUC <- getQuery(query_out_luc.proj, "LUC emissions by region") %>%
  filter(year %in% LUC_YEARS) %>%
  group_by(scenario, year) %>%
  summarise(luc = sum(value)) %>%
  ungroup() %>%
  complete(nesting(scenario), year = LUC_YEARS) %>%
  group_by(scenario) %>%
  mutate(luc = approx_fun(year, luc)) %>%
  summarise(luc = sum(luc)) %>%
  ungroup() %>%
  separate(scenario, into = c("drop", "luc_fuel", "Policy"), sep = "_") %>%
  left_join(luc_fuel_map, by = "luc_fuel") %>%
  select(-drop)

baseLUC <- filter(LUC, luc_fuel == "base") %>%
  select(-reporting_fuel)
addLUC <- filter(LUC, luc_fuel != "base") %>%
  left_join(baseLUC, by = "Policy", suffix = c("", ".base")) %>%
  mutate(LUC_MtCO2e = (luc - luc.base) * CONV_C_CO2) %>%
  select(reporting_fuel, Policy, LUC_MtCO2e)

# Cumulative additional fuel production
fuel_production <- getQuery(query_out_luc.proj, "outputs by tech") %>%
  filter(output == sector, year %in% LUC_YEARS) %>%
  select(-region, -Units, -output) %>%
  inner_join(subset(fuel_techs, !is.na(luc_fuel)), by = c("sector", "subsector", "technology")) %>%
  group_by(scenario, reporting_fuel, luc_fuel, year) %>%
  summarise(Fuel_EJ = sum(value)) %>%
  ungroup() %>%
  complete(nesting(scenario, reporting_fuel, luc_fuel), year = LUC_YEARS) %>%
  group_by(scenario, reporting_fuel, luc_fuel) %>%
  mutate(Fuel_EJ = approx_fun(year, Fuel_EJ)) %>%
  summarise(Fuel_EJ = sum(Fuel_EJ)) %>%
  ungroup() %>%
  separate(scenario, into = c("drop", "luc_fuel", "Policy"), sep = "_") %>%
  select(-drop)

basefuelprod <- filter(fuel_production, luc_fuel == "base") %>%
  select(-luc_fuel)
addfuelprod <- filter(fuel_production, luc_fuel != "base") %>%
  semi_join(distinct(fuel_techs, reporting_fuel, luc_fuel)) %>%
  left_join(basefuelprod, by = c("Policy", "reporting_fuel"), suffix = c("", ".base")) %>%
  mutate(Fuel_EJ = Fuel_EJ - Fuel_EJ.base) %>%
  select(reporting_fuel, Policy, Fuel_EJ)

LUC_CO2_GJ = left_join(addLUC, addfuelprod, by = c("reporting_fuel", "Policy")) %>%
  mutate(LUC_kgCO2_GJ = LUC_MtCO2e / Fuel_EJ) %>%
  select(fuel = reporting_fuel, Policy, LUC_kgCO2_GJ)

#Add LUC CO2 to upstream CO2 (not disaggregating as a series)
fuels_upstream_co2 <- fuels_upstream_co2 %>%
  mutate(Policy = substr(scenario, nchar(scenario) - 1, nchar(scenario))) %>%
  left_join(LUC_CO2_GJ, by = c("fuel", "Policy")) %>%
  mutate(kgCO2e_GJ = if_else(is.na(LUC_kgCO2_GJ), kgCO2e_GJ, kgCO2e_GJ + LUC_kgCO2_GJ)) %>%
  select(-Policy, -LUC_kgCO2_GJ)

fuels_lca_ghg <- bind_rows(fuels_upstream_co2,
                           fuels_upstream_nonco2_co2e,
                           fuels_tailpipe_co2,
                           fuels_tailpipe_nonco2_co2e) %>%
  select(scenario, fuel, GHG, source, year, kgCO2e_GJ) %>%
  mutate(kgCO2e_GJ = round(kgCO2e_GJ, digits = ROUNDING_DIGITS))

# disambiguate the scenario names to their categories and levels/descriptions
disambiguate_scenario_name <- function(df){
  variable_names <- unique(scenario_definitions$variable_name)
  df_final <- separate(df, scenario, into = variable_names, sep = c(2,4,6,8,10), remove = FALSE)
  for(var in variable_names){
    df_final[[var]] <- scenario_definitions$description[match(df_final[[var]], scenario_definitions$string)]
  }
  return(df_final)
}

fuels_lca_ghg <- disambiguate_scenario_name(fuels_lca_ghg)
fuels_primary_energy <- disambiguate_scenario_name(fuels_primary_energy)
fuels_prices <- disambiguate_scenario_name(fuels_prices)

# write out the data tables
write_csv(fuels_lca_ghg, "outputs/fuels_lca_ghg.csv")
write_csv(fuels_primary_energy, "outputs/fuels_primary_energy.csv")
write_csv(fuels_prices, "outputs/fuels_prices.csv")








