Code and data for ‘22 years of coal exposure’
================
Lucas Henneman
4/20/2022

## Data used in this work

This repository references three datasets: 1) US coal power plant
characteristics and annual emissions from US EPA Air Markets Program 2)
Gridded coal<sub>2.5</sub> exposure from the HyADS model 3) County-level
population data from the US census

#### Air Markets Program Data emissions from EPA

EPA hosts emissions and facility attribute information on its [AMPD
site](https://ampd.epa.gov/ampd/). R scripts to
[download](https://github.com/munshimdrasel/getting-raw-ampd-data) and
[wrangle](https://github.com/munshimdrasel/ampd-raw-data-processing)
this data are available on Github. We include pre-processed coal power
plant datasets with this repository used in this analysis described
below.

#### Coal PM<sub>2.5</sub> exposure

This data is stored on an [Open Science Framework project
site](https://osf.io/8gdau), and instructions for downloading and
manipulating the files is provided on the related [Github
repository](https://github.com/lhenneman/coal_unit_PM25).

Information on how the annual gridded exposure data is aggregated to
counties is described below. It is recommended to download the coal
PM<sub>2.5</sub> data to the `data/inputs/coal_pm25` directory.

#### County-level population data in the US

The analysis applies 2 datasets of Resident Population Estimates by Age,
Sex, Race, and Hispanic Origin
([CC-EST2019-ALLDATA-ST-FIPS](https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-alldata.pdf))
and
[CO-EST00INT-ALLDATA-ST](https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-alldata.pdf).
Data processing examples for these scripts is provided in
`code/census_data_wrangle.R`.

The census data used for this analysis is saved as
`data/inputs/population_county.fst`. For users planning to extend this
analysis, it is recommended that they re-download the census data and
recreate this file to use the most up-to-date census products.

## Datasets included in this repository

Datasets are included in both `.csv` and `.fst` (for large files)
formats.

1.  Input datasets (`data/inputs/`)
    1.  Coal facility data (`data/inputs/coal_facility_data`)
        -   `unit_fac_crosswalk.csv` provides a crosswalk between
            electricity generating unit IDs and facility IDs
        -   `facilities_by_year.csv` provides facility-specific
            information such as latitude/longitude, names, state, and
            SOx emissions (tons)
        -   `units_coal_1999_2020.fst` Annual SOx emissions (tons) and
            Heat input (MMBTU) for each unit
        -   `facility_operating_scrubbers_startyear.fst` years that each
            unit installed a scrubber or retired
    2.  `data/inputs/census_data/population_county.fst` County
        populations from census data summarized using
        `census_data_wrangle.R`
2.  Output datasets (`data/outputs/`)
    1.  `data/outputs/popwgt_hyads_race.fst` includes
        population-weighted coal PM<sub>2.5</sub> from all coal
        generating units. Created using `coal_pm25_to_county.R`.
    2.  `data/outputs/popwgt_hyads_units_race.fst` includes
        population-weighted coal PM<sub>2.5</sub> from each coal
        generating unit. Created using `coal_pm25_to_county.R`.
    3.  `data/outputs/popwgt_hyads_grids_idwe_race.fst` includes
        population-inverse distance weighted exposure from each grid
        cell on the 36-km resolution grid. These values are represent
        the PWe<sub>c,y,d</sub> described in the manuscript. Created in
        `coal_pm25_to_county.R`.

## R scripts in this repository

#### Utility functions for data wrangling

All code is stored in the `code/` directory. There are four R scripts.

1.  `census_data_wrangle.R` documents the source and manipulation of the
    census population data
2.  `coal_pm25_to_county.R` documents the spatial aggregation of annual
    gridded coal PM<sub>2.5</sub> to counties. In addition, this file
    includes code to calculate inverse-distance population-weighted
    exposure from each grid location, which lays the foundation for
    calculating region-specific relative expected population-weighted
    exposure.

#### Scripts that reproduce results in the manuscript

3.  `exposure_disparities.R` Population-weighted exposure disparity
    calculations, including absolute and relative differences. This
    script reproduces many of the figures in the manuscript and
    supplement
4.  `exposure_contributed_avoided.R` population-weighted exposure
    contributed and avoided by individual facilities
