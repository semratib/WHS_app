
#' Make arrow diagram
#'
#' @param data GHE data downloaded from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
#' @param data_comp_year GHE data downloaded for comparison year
#' @param country_iso ISO code for country to include
#' @param year Year to plot
#' @param comp_year Year to plot as comparison
#' @param metric "deaths" or "dalys"
#'
#' @return
#' @export
#'
#' @examples
make_arrow <- function(data, data_comp_year, country_iso, year, comp_year, metric = "deaths") {
  
  if (! country_iso %in% unique(data$DIM_COUNTRY_CODE)) {
    stop("Data does not contain the ISO requested.")
  }
  if (! year %in% unique(data$DIM_YEAR_CODE)) {
    stop("Data does not contain the year requested.")
  }
  if (! metric %in% c("deaths", "dalys")) {
    stop("Metric must be deaths or dalys.")
  }
  
  # subset data
  data_arrow <- data %>%
    filter(DIM_COUNTRY_CODE == country_iso &
             # DIM_YEAR_CODE == year &
             # DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE == "TOTAL" &
             FLAG_LEVEL == 2)
  data_arrow_comp <- data_comp_year %>%
    filter(DIM_COUNTRY_CODE == country_iso &
             # DIM_YEAR_CODE == comp_year &
             # DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE == "TOTAL" &
             FLAG_LEVEL == 2)
  data_arrow <- plyr::rbind.fill(data_arrow, data_arrow_comp)
  
  # compute cause fractions and cause ranks
  data_arrow <- data_arrow %>%
    group_by(DIM_YEAR_CODE) %>%
    mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
           # prop_dalys = VAL_DALY_COUNT_NUMERIC / sum(VAL_DALY_COUNT_NUMERIC),
           rank_deaths = rank(-prop_deaths, ties.method = "first"),
           # rank_dalys = rank(-prop_dalys, ties.method = "first")
           ) %>%
    select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, prop_deaths, rank_deaths,
           # prop_dalys, rank_dalys
           ) %>%
    pivot_wider(id_cols = c(FLAG_CAUSEGROUP, DIM_GHECAUSE_TITLE), names_from = DIM_YEAR_CODE,
                values_from = c(prop_deaths, rank_deaths
                                # prop_dalys,  rank_dalys
                                )) %>%
    rename(rank_year1 = paste0("rank_", metric, "_", comp_year),
           rank_year2 = paste0("rank_", metric, "_", year)) %>%
    mutate(increasing_rank = rank_year1 < rank_year2)
  
  # merge on categorization for level 1 cause group
  lev1 <- data %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  data_arrow <- data_arrow %>% left_join(lev1, by = "FLAG_CAUSEGROUP")
  lev1_causes <- c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries")
  data_arrow$lev1_name <- factor(data_arrow$lev1_name, levels = lev1_causes)
  
  # create plot
  gg <- ggplot(data_arrow, aes(x=0, xend=1, y=-rank_year1, yend=-rank_year2)) +
    geom_segment(aes(color=lev1_name, lty = increasing_rank), show.legend = F) +
    geom_label(aes(x = -2.1, y = -rank_year1, label = paste0(rep(" ", 70), collapse = ""), fill = lev1_name), hjust=0, alpha=0.4, size=3, family="calibri") +
    geom_label(aes(x = 1.1, y = -rank_year2, label = paste0(rep(" ", 70), collapse = ""), fill = lev1_name), hjust=0, alpha=0.4, size=3, family="calibri") +
    geom_label(aes(x = -2.1, y = -rank_year1, label = paste0(rank_year1, " ", DIM_GHECAUSE_TITLE), fill = lev1_name),
               show.legend = F, hjust=0, alpha=0, size=3, label.size = NA, family="calibri") +
    geom_label(aes(x = 1.1, y = -rank_year2, label = paste0(rank_year2, " ", DIM_GHECAUSE_TITLE), fill = lev1_name),
               show.legend = F, hjust=0, alpha=0, size=3, label.size = NA, family="calibri") +
    geom_label(aes(x=-2.1, y=1, label=comp_year), hjust=0, family="calibri", size = 4.5, label.size = NA) +
    geom_label(aes(x=1.1, y=1, label=year), hjust=0, family="calibri", size = 4.5, label.size = NA) +
    # scale_colour_brewer(palette="Set1") +
    # scale_fill_brewer(palette="Set1") +
    scale_color_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
    scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(family = "calibri", size = 10)) +
    guides(color=guide_legend(nrow=4, byrow=TRUE),
           fill=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")),
           lty="none") +
    labs(color = "", fill = "") +
    scale_x_continuous(limits=c(-2.6,3.5))
  print(gg)
}







