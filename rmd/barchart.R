
#' Make barchart
#'
#' @param data GHE data downloaded from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
#' @param country_iso ISO code for country to include
#' @param year Year to plot
#' @param metric "death_fraction", "death_rate", or "death_count"
#'
#' @return
#' @export
#'
#' @examples
make_barchart <- function(data, country_iso, year, metric) {
  
  if (! country_iso %in% unique(data$DIM_COUNTRY_CODE)) {
    stop("Data does not contain the ISO requested.")
  }
  if (! year %in% unique(data$DIM_YEAR_CODE)) {
    stop("Data does not contain the year requested.")
  }
  
  # prep data
  data_barchart <- data %>%
    filter(DIM_COUNTRY_CODE == country_iso &
             DIM_YEAR_CODE == year &
             DIM_AGEGROUP_CODE != "TOTAL" &
             DIM_SEX_CODE == "TOTAL" &
             FLAG_LEVEL == 1)
  
  # format age
  data_barchart$DIM_AGEGROUP_CODE <- gsub("Y", "", data_barchart$DIM_AGEGROUP_CODE)
  data_barchart$DIM_AGEGROUP_CODE <- gsub("T", "-", data_barchart$DIM_AGEGROUP_CODE)
  data_barchart$DIM_AGEGROUP_CODE <- gsub("GE_85", "85PLUS", data_barchart$DIM_AGEGROUP_CODE)
  agegroups <- c(paste0(c(0,1,seq(5,80,5)), "-", c(1,4,seq(9,84,5))), "85PLUS")
  data_barchart$DIM_AGEGROUP_CODE <- factor(data_barchart$DIM_AGEGROUP_CODE, levels = agegroups)
  
  # format cause group
  lev1_causes <- c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other COVID-19 pandemic-related outcomes")
  data_barchart$DIM_GHECAUSE_TITLE <- factor(data_barchart$DIM_GHECAUSE_TITLE, lev1_causes)
  
  # pick y-axis
  if (metric == "death_fraction") {
    data_barchart$yvar <- data_barchart$VAL_DEATHS_COUNT_NUMERIC
    position <- "fill"
    ytitle <- "Percent of total deaths"
  } else if (metric == "death_rate") {
    data_barchart$yvar <- data_barchart$VAL_DEATHS_RATE100K_NUMERIC
    position <- "stack"
    ytitle <- "Total deaths per 100,000"
  } else if (metric == "death_count") {
    data_barchart$yvar <- data_barchart$VAL_DEATHS_COUNT_NUMERIC
    position <- "stack"
    ytitle <- "Total number of deaths"
  } else {
    stop("Invalid metric argument.")
  }
  
  # make plot
  gg <- ggplot(data_barchart, aes(x = DIM_AGEGROUP_CODE, y = yvar, fill = DIM_GHECAUSE_TITLE)) +
    geom_bar(position = position, stat = "identity", alpha = 0.7) +
    theme_classic() +
    labs(x = "", y = ytitle, fill = "") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, family="calibri"),
          axis.text.y = element_text(family="calibri", size = 9),
          axis.title.y = element_text(family="calibri", size = 10),
          legend.text = element_text(family = "calibri", size = 10),
          legend.position = "bottom") +
    scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00", "#A6228C"))+
    guides(fill=guide_legend(nrow=4, byrow=TRUE)) 
    # scale_fill_brewer(palette = "Set1")
  print(gg)

}