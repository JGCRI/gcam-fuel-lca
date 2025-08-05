# gcam-fuel-lca
Tool for performing life cycle analysis of fuel technologies in GCAM

## Running the script
The core calculations are performed by the lca_estimation.R file, which requires several unversioned (i.e., not part of this repository) query output files, and one package ("rgcam") that is not part of the Comprehensive R Archive Network (CRAN). The rgcam package is maintained by JGCRI on GitHub, and can be downloaded and installed using the install_github() function of the devtools() package.
devtools::install_github('JGCRI/rgcam')
The unversioned scenario output files should be downloaded from https://doi.org/10.5281/zenodo.16748632 and placed in the rgcam folder of this repository. 

## Description

This repository contains code and several mappings files that performs full upstream analysis of the inputs to any energy technology, including primary energy, non CO2 emissions, and CO2 storage. After stepping up from the given technology to the commodities that are inputs to the technology, each model sector is effectively collapsed into a set of energy input-output coefficients, CO2 storage factors, and non CO2 emission factors, all expressed per unit of output of the sector. The user-specified recursion depth assigns the number of steps that will be taken upstream of any given technology; lower numbers may miss quantitatively relevant primary energy inputs to the technology, while higher numbers may take longer only to revise the estimates well below the reporting or visualization thresholds. Because of how traded commodities are handled--that the global trade is contained within the USA region--the method has structured right now will only return reliably correct results for the USA region. In Non-US regions the method will need to be revised for any commodities where imports account for a portion of the primary energy footprint.
