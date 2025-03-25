## Function to load NTOT files and calculate wh

# Purpose:
# support function to load NTOT files and calculate wh


## Tag: data analysis


# outputs created in this file --------------
# ntot


# CallS:
# NTOTs

# output gets called by:
# NCRMP_make_weighted_demo_data.R

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Mar 2025


##############################################################################################################################

#' Creates weights for coral demographic data
#'
#' Creates weighting scheme for coral demographic data from a single region.
#' NCRMP utilizes a stratified random sampling design. Generally, weighting is
#' based on the number of grid cells in the sample frame in each stratum, to provide
#' regional estimates. This function is called by broader calculation functions
#' such as [NCRMP_make_weighted_demo_data()],
#' which will provide the inputdata object.
#'
#'
#'
#' @param project A string indicating the project, "NCRMP" or NCRMP and DRM combined ("NCRMP_DRM").
#' @param inputdata A dataframe of coral demographic data. Can be in various forms.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @return A dataframe of weighting scheme for corresponding region.
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#'

load_NTOT <- function(region, inputdata, project){

  #### Helper Functions ####

  process_ntot <- function(data, year, strat_mapping = NULL){
    data <- data %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))
  }


  #### SEFCRI ####

  SEFL_16_18_process <- function(data){
    data <- data %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH2"~"NEAR1",
                                             STRAT == "PTDP0"~"OFFR0",
                                             STRAT == "PTDP1"~"OFFR1", TRUE ~ as.character(STRAT))) %>%
      dplyr::group_by(YEAR, REGION, STRAT, PROT, GRID_SIZE) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(STRAT != "RGDP1" & STRAT != "RGDP0")
  }

  if(region == "SEFCRI") {
    if(project == "NCRMP" || project == "NULL"){

      ntot <- bind_rows(
        SEFL_2014_NTOT %>% dplyr::filter(STRAT %in% c("MIDR1", "MIDR0")) %>% process_ntot(),
        SEFL_2016_NTOT %>% SEFL_16_18_process() %>% process_ntot(),
        SEFL_2018_NTOT %>% SEFL_16_18_process() %>% process_ntot(),
        SEFL_2020_NTOT %>% dplyr::filter(!STRAT %in% c("RGDP1", "RGDP0")) %>% process_ntot(),
        SEFL_2022_NTOT %>% dplyr::filter(!STRAT %in% c("RGDP1", "RGDP0")) %>% process_ntot()
      )
    }


    if(project == "NCRMP_DRM"){

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))

      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()

      # create a data frame of the full NTOTs for FLK
      NTOT_all <- dplyr::bind_rows(SEFL_2014_NTOT, SEFL_2014_NTOT %>% dplyr::mutate(YEAR = 2015),
                                   SEFL_2016_NTOT, SEFL_2018_NTOT %>% dplyr::mutate(YEAR = 2017),
                                   SEFL_2018_NTOT, SEFL_2018_NTOT %>% dplyr::mutate(YEAR = 2019),
                                   SEFL_2020_NTOT, SEFL_2020_NTOT %>% dplyr::mutate(YEAR = 2021),
                                   SEFL_2022_NTOT) %>%
        dplyr::mutate(REGION = "SEFCRI",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT))

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

    }

  }

  #### FLK ####

  process_FLK_NTOT <- function(data){
    data %>%
      mutate(PROT = 0) %>%
      dplyr::group_by(REGION, YEAR, PROT, STRAT, GRID_SIZE) %>%
      dplyr::summarise(NTOT = sum(NTOT), .groups = "drop")
  }

  if(region == "FLK") {

    if(project == "NCRMP" || project == "NULL"){

      # Use a loop to create a unique lists for each year of strata sampled
      # Filter NTOT to only strata sampled that year (previously done manually)
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()
      ### UPDATE IN DEC. 2023!!
      # PROT is re-coded here to 0 for ALL strata as fish and benthics met 12/19/23
      # to determine that it is not appropriate to keep PROT in the analysis strat
      # in FLK because the data aren't allocated that way
      # NTOTs must be re-calculated with PROT=0 here
      NTOT_all <- dplyr::bind_rows(
        process_FLK_NTOT(FLK_2014_NTOT),
        process_FLK_NTOT(FLK_2016_NTOT),
        process_FLK_NTOT(FLK_2018_NTOT),
        process_FLK_NTOT(FLK_2020_NTOT),
        process_FLK_NTOT(FLK_2022_NTOT)
      ) %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))


      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT))

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }
      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))
    }


    if(project == "NCRMP_DRM") {

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()

      ### UPDATE IN DEC. 2023!!
      # PROT is re-coded here to 0 for ALL strata as fish and benthics met 12/19/23
      # to determine that it is not appropriate to keep PROT in the analysis strat
      # in FLK because the data aren't allocated that way
      # NTOTs must be re-calculated with PROT=0 here
      FLK_NTOTs <- dplyr::bind_rows(
        process_FLK_NTOT(FLK_2014_NTOT),
        process_FLK_NTOT(FLK_2016_NTOT),
        process_FLK_NTOT(FLK_2018_NTOT),
        process_FLK_NTOT(FLK_2020_NTOT),
        process_FLK_NTOT(FLK_2022_NTOT)
      ) %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT))

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))
    }

    #### MIR ####
    if(project == "MIR"){

      ntot22 <- FLK_MIR_2022_NTOT %>%
        dplyr::ungroup() %>%
        # Remove strata not sampled - this will be unique to each year and need to be updated. See Tortugas for examples.
        dplyr::filter(STRAT_CORA != "CS05",
                      STRAT_CORA != "CS16",
                      ANALYSIS_STRATUM != "CS02 / PROT = 1",
                      ANALYSIS_STRATUM != "CS07 / PROT = 1",
                      ANALYSIS_STRATUM != "CS12 / PROT = 0",
                      ANALYSIS_STRATUM != "CS14 / PROT = 1",
                      ANALYSIS_STRATUM != "CS15 / PROT = 1") %>%
        dplyr::mutate(YEAR = 2022,
                      ngrtot = sum(NTOT))

      ntot <- ntot22
    }
  }

  #### Tortugas ####
  if(region == "Tortugas") {

      if(project == "NCRMP" || project == "NULL") {

        # Create a helper function for processing the NTOT data
        process_tort_ntot_data <- function(data, year, exclude_strata = NULL, exclude_prot = NULL) {
          data %>%
            dplyr::mutate(
              REGION = "Tortugas",
              ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
              PROT = as.integer(PROT)
            ) %>%
            dplyr::filter(
              !(STRAT %in% exclude_strata),
              !(ANALYSIS_STRATUM %in% exclude_prot)
            ) %>%
            dplyr::mutate(ngrtot = sum(NTOT))
        }

        #define exclustion
        exclusions <- list(
          ntot14 = list(exclude_strata = "SPGR_LR", exclude_prot = NULL),
          ntot16 = list(exclude_strata = NULL, exclude_prot = "ISOL_LR / PROT = 0"),
          ntot18 = list(exclude_strata = "SPGR_LR", exclude_prot = c("ISOL_LR / PROT = 0", "ISOL_LR / PROT = 1")),
          ntot20 = list(exclude_strata = NULL, exclude_prot = c("T09 / PROT = 0", "T09 / PROT = 1", "T03 / PROT = 2", "T06 / PROT = 2")),
          ntot22 = list(exclude_strata = NULL, exclude_prot = c("T07 / PROT = 1", "T09 / PROT = 1", "T04 / PROT = 2"))
        )

        #Process data for each year
        ntot_list <- list(
          ntot14 = process_tort_ntot_data(Tort_2014_NTOT, 2014, exclusions$ntot14$exclude_strata, exclusions$ntot14$exclude_prot),
          ntot16 = process_tort_ntot_data(Tort_2016_NTOT, 2016, exclusions$ntot16$exclude_strata, exclusions$ntot16$exclude_prot),
          ntot18 = process_tort_ntot_data(Tort_2018_NTOT, 2018, exclusions$ntot18$exclude_strata, exclusions$ntot18$exclude_prot),
          ntot20 = process_tort_ntot_data(Tort_2020_NTOT, 2020, exclusions$ntot20$exclude_strata, exclusions$ntot20$exclude_prot),
          ntot22 = process_tort_ntot_data(Tort_2022_NTOT, 2022, exclusions$ntot22$exclude_strata, exclusions$ntot22$exclude_prot)
        )

        #combine dat
        ntot <- dplyr::bind_rows(ntot_list)

      }


    if(project == "NCRMP_DRM") {

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()
      # create a data frame of the full NTOTs for FLK
      NTOT_all <- dplyr::bind_rows(Tort_2014_NTOT, Tort_2016_NTOT %>% dplyr::mutate(YEAR = 2015),
                                   Tort_2016_NTOT, Tort_2018_NTOT %>% dplyr::mutate(YEAR = 2017),
                                   Tort_2018_NTOT, Tort_2018_NTOT %>% dplyr::mutate(YEAR = 2019),
                                   Tort_2020_NTOT, Tort_2020_NTOT %>% dplyr::mutate(YEAR = 2021),
                                   Tort_2022_NTOT) %>%
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT))

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

    }

  }

  #### USVI Function ####

  process_USVI_NTOT <- function(data, year){
    data %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = year,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

  }

  #### STTSTJ ####
  if(region == "STTSTJ"){

    ntot13 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ",
                    STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
      process_USVI_NTOT(year = 2013)

    ntot15 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      process_USVI_NTOT(year = 2015)

    ntot17 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      process_USVI_NTOT(year = 2017)

    ntot19 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      process_USVI_NTOT(year = 2019)

    ntot21 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      process_USVI_NTOT(year = 2021)

    ntot23 <- USVI_2023_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      process_USVI_NTOT(year = 2023)

    ntot <- dplyr::bind_rows(ntot13, ntot15, ntot17, ntot19, ntot21, ntot23)

  }
  #### STX ####
  if(region == "STX"){

    ntot15 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW", # Hard shlw was not sampled in 2015
                    STRAT != "HARD_DEEP") %>% # Hard deep was not sampled in 2015
      process_USVI_NTOT(year = 2015)

    ntot17 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW") %>%
      process_USVI_NTOT(year = 2017)

    ntot19 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      process_USVI_NTOT(year = 2019)

    ntot21 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::filter(!(STRAT == "BDRK_SHLW" | STRAT == "BDRK_DEEP")) %>% # 2021 we didnt sample BDRK shallow OR deep
      process_USVI_NTOT(year = 2021)

    ntot23 <- USVI_2023_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      process_USVI_NTOT(year = 2023)

    ntot <- dplyr::bind_rows(ntot15, ntot17, ntot19, ntot21, ntot23)

  }

  #### PRICO ####
  PRICO_data_clean <- function(data, year){

    data <- data %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = year,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))
  }


  if(region == "PRICO"){

    ntot14 <- PRICO_2023_NTOT %>%
      dplyr::filter(STRAT != "HARD_DEEP", # Hard shlw was not sampled in 2014
                    STRAT != "HARD_SHLW") %>% # Hard deep was not sampled in 2014
      PRICO_data_clean(year = 2014)

    ntot16 <- PRICO_data_clean(data = PRICO_2023_NTOT, year = 2014)

    ntot19 <- PRICO_data_clean(data = PRICO_2023_NTOT, year = 2019)

    ntot21 <- PRICO_data_clean(data = PRICO_2023_NTOT, year = 2021)


    #2023 NTOT had HARD regions removed
    ntot23 <- PRICO_2023_NTOT %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(HABITAT_CD != "HARD") %>% #This might be redundant
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot19, ntot21, ntot23)
  }

  #### FGB ####
  if(region == "FGB"){

    FGB_data_clean <- function(data, year){

      data <- data %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_,
                      YEAR = year) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH_STRAT, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT),
                         ngrtot = sum(NTOT)) %>%
        dplyr::ungroup()
    }
    years <- c(2013, 2015, 2018, 2022)
    ntot <- bind_rows(lapply(years, function(y) FGB_data_clean(FGBNMS_2022_NTOT, y)))
  }

  ntot <- ntot %>%
    dplyr::mutate(wh = NTOT/ngrtot) %>%
    dplyr::mutate(PROT = as.factor(PROT))

  ####Export####
  return(ntot)
}
