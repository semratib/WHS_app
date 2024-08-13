
#' Make treemap
#'
#' @param data GHE data downloaded from https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
#' @param country_iso ISO code for country to include
#' @param year Year to plot
#'
#' @return
#' @export
#'
#' @examples
make_treemap <- function(data, country_iso, year) {

  # if (! country_iso %in% unique(data$DIM_COUNTRY_CODE)) {
  #   stop("Data does not contain the ISO requested.")
  # }
  if (! year %in% unique(data$DIM_YEAR_CODE)) {
    stop("Data does not contain the year requested.")
  }

  # subset data
  data_treemap <- data %>%
    filter(DIM_COUNTRY_CODE == country_iso &
             # DIM_YEAR_CODE == year &
             # DIM_AGEGROUP_CODE == "TOTAL" &
             # DIM_SEX_CODE == "TOTAL" &
             FLAG_TREEMAP == 1)

  # add level 1 cause group
  lev1 <- data %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  data_treemap <- data_treemap %>% left_join(lev1, by = "FLAG_CAUSEGROUP")
  lev1_causes <- c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other COVID-19 pandemic-related outcomes")
  data_treemap$lev1_name <- factor(data_treemap$lev1_name, levels = lev1_causes)

  # add cause fraction to label if over 5%
  data_treemap <- data_treemap %>%
    mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
    mutate(cause_title = ifelse(cause_fraction > 0.05,
                                paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "),
                                DIM_GHECAUSE_TITLE)) %>%
    mutate(mycolor = case_when(
      lev1_name=="Communicable, maternal, perinatal and nutritional conditions" ~"#F26829",
      lev1_name=="Noncommunicable diseases" ~ "#009ADE",
      lev1_name=="Injuries" ~ "#80BC00",
      lev1_name=="Other COVID-19 pandemic-related outcomes" ~ "#A6228C"
    )) %>%
    mutate(mycolor = ifelse(is.na(cause_title), NA, mycolor))

  # get total death count for title
  total_deaths <- data %>%
    filter(DIM_COUNTRY_CODE == country_iso & 
             # DIM_YEAR_CODE == year &
             # DIM_AGEGROUP_CODE == "TOTAL" & 
             # DIM_SEX_CODE == "TOTAL" &
             FLAG_LEVEL == 0) %>%
    pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()

  # make treemap
  treemap::treemap(data_treemap,
          index=c("lev1_name","cause_title"),
          vSize="VAL_DEATHS_COUNT_NUMERIC",
          type="color",
          vColor = "mycolor",
          # palette = "Set1",
          fontsize.labels=c(0,9),
          fontcolor.labels=c("white","white"),
          fontface.labels=c(2,1),
          fontfamily.labels=c("sans"),
          fontfamily.title = "sans",
          bg.labels=c("transparent"),
          border.col=c("white","white"),
          align.labels=list(
            c("center", "center"),
            c("right", "bottom")
          ),
          overlap.labels=0.5,
          inflate.labels=T,
          position.legend = "none",
          # fontsize.legend = 8,
          fontsize.title = 11
          # title=paste0("Total deaths, ", year, ": ", format(total_deaths, big.mark = ",", scientific = F))
  )

}

# make_treemap <- function(data, country_iso, year) {
#   
# }
  







