
#' Make pyramid
#'
#' @param data_by_sex All-age sex-specific data from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
#' @param data Data from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
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
make_pyramid <- function(data_by_sex, data, country_iso, year, who_region_countries, country_table, metric = "deaths") {
  
  # subset data
  data_pyramid <- data_by_sex %>%
    filter(DIM_COUNTRY_CODE %in% who_region_countries &
             # DIM_YEAR_CODE == year &
             # DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE != "TOTAL" &
             FLAG_LEVEL == 1)
  
  # merge on country names
  country_name_map <- country_table %>%
    filter(Code %in% who_region_countries) %>%
    select(DIM_COUNTRY_CODE = Code, country_name = Title) %>% 
    mutate(country_name = ifelse(country_name=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom", country_name))
  data_pyramid <- merge(data_pyramid, country_name_map, by = "DIM_COUNTRY_CODE", all.x=T)
  
  # pick output variable
  if (metric == "deaths") {
    data_pyramid$output_var <- data_pyramid$VAL_DEATHS_RATE100K_NUMERIC
    data$output_var <- data$VAL_DEATHS_RATE100K_NUMERIC
    ytitle <- "Deaths per 100,000"
  } else if (metric == "dalys") {
    data_pyramid$output_var <- data_pyramid$VAL_DALY_RATE100K_NUMERIC
    data$output_var <- data$VAL_DALY_RATE100K_NUMERIC
    ytitle <- "DALYs per 100,000"
  } else {
    stop("Invalid metric specified.")
  }
  
  # pick country order
  country_order_iso <- data %>%
    filter(DIM_COUNTRY_CODE %in% who_region_countries &
             # DIM_YEAR_CODE == year & DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE == "TOTAL" & 
             FLAG_LEVEL == 0) %>%
    arrange(output_var) %>%
    pull(DIM_COUNTRY_CODE)
  country_order_iso <- c(country_iso, rev(setdiff(country_order_iso, country_iso)))
  country_name_map$DIM_COUNTRY_CODE <- factor(country_name_map$DIM_COUNTRY_CODE, levels = country_order_iso)
  country_order <- country_name_map %>% arrange(DIM_COUNTRY_CODE) %>% pull(country_name)
  data_pyramid$country_name <- factor(data_pyramid$country_name, levels = country_order)
  
  # order causes
  lev1_causes <- c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other COVID-19 pandemic-related outcomes")
  data_pyramid$DIM_GHECAUSE_TITLE <- factor(data_pyramid$DIM_GHECAUSE_TITLE, levels = lev1_causes)
  
  country_face <- c("bold", rep("plain", length(country_order)-1))
  
  # flip direction for males
  data_pyramid$output_var <- 
    ifelse(data_pyramid$sex == "Male",
           # data_pyramid$DIM_SEX_CODE == "MALE",
           data_pyramid$output_var * -1,
           data_pyramid$output_var)
  # data_pyramid$DIM_SEX_CODE <- factor(data_pyramid$DIM_SEX_CODE, levels = c("MALE", "FEMALE"), labels = c("Male", "Female"))
  data_pyramid$sex <- factor(data_pyramid$sex, levels = c("Male", "Female"), labels = c("Male", "Female"))
  
  
  # make plot
  p <- ggplot(data_pyramid, aes(x = country_name, y = output_var, fill = DIM_GHECAUSE_TITLE)) + 
    geom_col(alpha = 0.7) +
    facet_share(~sex, dir = "h", 
                scales = 'free_x', 
                reverse_num = TRUE) + 
    coord_flip(clip = 'off') +
    # scale_fill_brewer(palette = 'Set1') +
    # scale_color_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
    scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00", "#A6228C"))+
    theme_classic() +
    theme(
      strip.background = element_blank(),
      plot.margin = unit(c(0,0.5,0,-4.5), "cm"),
      strip.text = element_text(face = 'bold', family="calibri", size = 10),
      legend.position = "bottom",
      axis.title.y = element_blank(),
      legend.text = element_text(family = "calibri", size = 10),
      axis.text.y = element_text(face = country_face, family="calibri", size = 10)
    ) +
    labs(y = ytitle, fill = "") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE))
  # gp <- ggplotGrob(p)
  # gp$widths[4] <- unit(0, 'cm')
  # grid::grid.newpage()
  # grid::grid.draw(gp)
  print(p)
}
