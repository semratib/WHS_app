
#' Make heatmap
#'
#' @param data GHE data downloaded from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
#' @param country_iso ISO code for country to feature
#' @param year Year to plot
#' @param country_table Output data.frame from ghost::gho_dimension_values("COUNTRY")
#' @param who_region_countries ISO codes for countries in WHO region
#' @param metric "deaths" or "dalys"
#'
#' @return
#' @export
#'
#' @examples
make_heatmap <- function(data, country_iso, year, country_table, who_region_countries, metric = "deaths") {
  
  if (! country_iso %in% unique(data$DIM_COUNTRY_CODE)) {
    stop("Data does not contain the ISO requested.")
  }
  if (! year %in% unique(data$DIM_YEAR_CODE)) {
    stop("Data does not contain the year requested.")
  }
  if (! metric %in% c("deaths", "dalys")) {
    stop("Metric must be deaths or dalys.")
  }
  
  # subset data and get rank and cause-fraction
  data_heatmap <- data %>%
    filter(DIM_COUNTRY_CODE %in% who_region_countries &
             # DIM_YEAR_CODE == year &
             # DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE == "TOTAL" &
             FLAG_LEVEL == 2) %>%
    group_by(DIM_COUNTRY_CODE) %>%
    mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
           # prop_dalys = VAL_DALY_COUNT_NUMERIC / sum(VAL_DALY_COUNT_NUMERIC),
           rank_deaths = rank(-prop_deaths, ties.method = "first"),
           # rank_dalys = rank(-prop_dalys, ties.method = "first")
           ) %>%
    ungroup() %>%
    rename(rank = paste0("rank_", metric)) %>%
    select(DIM_COUNTRY_CODE, DIM_GHECAUSE_TITLE, rank)
  
  # add country name and pick order
  country_name_map <- country_table %>% select(DIM_COUNTRY_CODE = Code, country_name = Title) 
  # %>%  mutate(country_name = ifelse(country_name=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom", country_name))
  data_heatmap <- merge(data_heatmap, country_name_map, by = "DIM_COUNTRY_CODE", all.x=T)
  country_order <- c(country, setdiff(unique(data_heatmap$country_name), country))
  data_heatmap$country_name <- factor(data_heatmap$country_name, levels = rev(country_order))
  
  # rank causes by country-specific death ranking
  cause_order <- data_heatmap %>% filter(DIM_COUNTRY_CODE == country_iso) %>%
    arrange(-rank) %>% pull(DIM_GHECAUSE_TITLE)
  data_heatmap$DIM_GHECAUSE_TITLE <- factor(data_heatmap$DIM_GHECAUSE_TITLE, levels = rev(cause_order))
  
  # rank category
  data_heatmap$rank_cat <- cut(data_heatmap$rank, breaks=c(1,2,3,5,8,14,50), right=F)
  
  country_face <- c(rep("plain", length(country_order)-1), "bold")
  
  data_heatmap$country_name = str_wrap(data_heatmap$country_name, width = 40)
  
  # make plot
  gg <- ggplot(data_heatmap, aes(x=DIM_GHECAUSE_TITLE, y=country_name, fill=rank, label=rank, family="calibri")) +
    geom_tile(alpha = 0.7) +
    geom_text() +
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust=0, family="calibri", size = 11),
          axis.text.y = element_text(face = country_face, family="calibri", size = 11)) +
    labs(x="", y="", fill="Rank") +
    # scale_fill_brewer(palette = "RdYlBu") +
    scale_fill_gradient2(
      # low = "#009ADE",
      # high = "#80BC00",
      # aesthetics = "fill"
      low = "#EF3842",
      mid = "#F4A81D",
      high = "#009ADE", #"#2AA6DC",
      midpoint = 12
    ) +
    scale_x_discrete(position = "top")
  print(gg)
  
}

