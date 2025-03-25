## Function to calculate colony mean (old and new) mortality at the site and strata level

# Purpose:
# creates csv files with mean mortality.


## Tag: data analysis


# outputs created in this file --------------
# old_mortality_site
# new_mortality_site
# Domain estimates



# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Feb 2025


##############################################################################################################################

#' Creates colony mortality summary dataframes
#'
#' Calculates mean old and recent coral mortality at each site, strata, and
#' region from the NCRMP or NCRMP+DRM coral demographic data.
#' Means are also calculated for each species at each strata and for the region.
#' Regional estimates of mortality are weighted by the number of grid cells
#' of a stratum in the sample frame.
#'
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP ("NCRMP") or NCRMP and DRM combined ("NCRMO_DRM").
#' @param region A string indicating the region. Options include: "FLK", "SEFCRI", "Tortugas", "STX", "STTSTJ", and "FGB".
#' @param species_filter An optional string indicating whether to filter to a subset of species.
#' @return A list of dataframes including 1) mean old mortality at each site, 2)
#' mean recent mortality at each site, 3) mean old mortality in each strata, 4)
#' mean recent mortality in each strata, 5) mean old mortality for each species in
#' each strata, 6) mean recent mortality for each species in each strata, 7)
#' regional estimate for old mortality, 8) regional estimate for recent mortality,
#' 9) regional estimate for old mortality for each species, 10) regional
#' estimate for recent mortality for each species.
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#'

NCRMP_DRM_calculate_mortality <- function(project, region, species_filter = "NULL") {
  
  #### Load Data ####
  mortality_data <- load_NCRMP_DRM_demo_data(project = project, region = region, species_filter = species_filter)
  list2env(mortality_data, envir = environment())
  
  #### Helper Functions ####
  clean_mortality_data <- function(data) {
    data %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))
  }
  
  calculate_mean_mortality <- function(data, mortality_column, mortality_type) {
    data %>%
      dplyr::summarise(avg_site_mortality = mean({{mortality_column}}, na.rm = TRUE), .groups = "keep") %>%
      dplyr::mutate(MORTALITY_TYPE = mortality_type) %>%
      dplyr::ungroup()
  }
  
  #### Calculate Old Mortality ####
  if (project == "NCRMP_DRM" || (project == "NCRMP" && region %in% c("SEFCRI", "Tortugas"))) {
    old_mortality_stage1 <- dat_1stage %>%
      clean_mortality_data() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      calculate_mean_mortality(mortality_column = OLD_MORT, mortality_type = "Old")
    
    old_mortality_stage2 <- dat_2stage %>%
      clean_mortality_data() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(transect_mortality = mean(OLD_MORT), .groups = "keep") %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      calculate_mean_mortality(mortality_column = transect_mortality, mortality_type = "Old")
    
    old_mortality_site <- rbind(old_mortality_stage1, old_mortality_stage2)
  } else {
    old_mortality_site <- dat_1stage %>%
      clean_mortality_data() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      calculate_mean_mortality(mortality_column = OLD_MORT, mortality_type = "Old")
  }
  
  #### Apply Weighting and Compute Regional Estimates ####
  weighted_data <- NCRMP_make_weighted_demo_data(project, inputdata = old_mortality_site, region, datatype = "mortality")
  list2env(weighted_data, envir = environment())
  
  old_mortality_strata <- mortality_strata %>% dplyr::mutate(MORTALITY_TYPE = "Old")
  domain_estimate_old_mortality <- Domain_est %>% dplyr::mutate(MORTALITY_TYPE = "Old")
  
  #### Export Results ####
  output <- list(
    "old_mortality_site" = old_mortality_site,
    "old_mortality_strata" = old_mortality_strata,
    "domain_estimate_old_mortality" = domain_estimate_old_mortality
  )
  
  return(output)
}
