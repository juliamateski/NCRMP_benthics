## Function to calculate coral cover by species

# Purpose:
# creates csv files with coral cover


## Tag: data analysis


# outputs created in this file --------------
#
#
#


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz
# Last update: Sept 2024


##############################################################################################################################

#' Creates colony coral cover summary data frames
#'
#'
#' Calculates coral cover strata means and regional weighted means, by coral species. Also
#' produces a figure of percent cover by species from most recent year of data for the
#' selected region.
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param ptitle A string indicating the plot title.
#' @param project A string indicating the project: "NCRMP" or "MIR".
#' @param file_path A string indicating the filepath for the figure output by this function.
#' @return A list of dataframes and a jpeg. Dataframes include strata mean cover by species and regional weighted mean cover by species.
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_colony_percent_cover_DEPRECIATED <- function(region, ptitle, file_path, project = "NULL"){

  #############
  # coral species used in allocation
  #############


  if(region == "SEFCRI"){

    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_SEFCRI_2014_22_percent_cover_species,
                                                        project = project)
  }

  if(region == "FLK"){

    if(project == "NCRMP" || project == "NULL") {

    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_FLK_2014_22_percent_cover_species,
                                                        project = project)
  }

  if(project == "MIR"){

    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = MIR_2022_percent_cover_species_DUMMY,
                                                        project = "MIR")

  }
  }


  if(region == "Tortugas"){

    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_Tort_2014_22_percent_cover_species,
                                                        project = project)
  }



  if(region=="PRICO") {

    # Puerto Rico

    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_PRICO_2014_23_percent_cover_species,
                                                        project = project)

  }

  if(region=="STTSTJ") {

    # STT-STJ
    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_STTSTJ_2013_23_percent_cover_species,
                                                        project = project)

  }

  if(region=="STX") {
    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_STX_2015_23_percent_cover_species,
                                                        project = project)

  }

  if(region=="GOM") {
    tmp <- NCRMP_make_weighted_species_coral_cover_data(region = region,
                                                        sppcvr = NCRMP_FGBNMS_2013_24_percent_cover_species,
                                                        project = project)

  }


  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])




  g1 <-  region_means %>%
    dplyr::ungroup() %>%
    # exclude occurrences of 0
    dplyr::filter(avCvr > 0,
                  YEAR >= 2024) %>%

    ggplot(.,
           aes(x = reorder(SPECIES_NAME, avCvr),
               y = avCvr,
               fill = 'even')) +
    # geom_hline(yintercept = c(0.25, 0.5, 0.75),
    # colour = "light grey") +
    geom_bar(stat = "identity",
             fill = "deepskyblue4") +
    ggtitle(paste(ptitle, "Species Percent Cover", sep = " ")) +
    # scale_fill_manual(values = c( "#0a4595")) +
    theme_light() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(face ="italic"),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(t = 1, r = 1, b = 1, l = 1), "mm"),
          plot.title = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "bold")) +
    coord_flip() +
    # scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75)) +
    guides(fill = "none")



  ggsave(filename = paste(region_means$REGION[1], "species_cover.jpeg", sep = "_"),
         path = file_path,
         plot = g1,
         width = 9.8,
         height = 6.5,
         dpi = 300,
         units = "in",
         device = "jpg")






  ################
  # Export
  ################


  # Create list to export
  output <- list(
    "region_means" = region_means,
    "strata_means" = strata_means)

  return(output)




}
