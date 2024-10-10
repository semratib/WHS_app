library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)

library(bslib)
library(tidyverse)
library(whoville)
library(highcharter)
library(shiny)
library(shinycssloaders)
library(gifski)
library(cowplot)
library(demCore)
library(data.table)
library(ggrepel)
library(sf)
library(scales)
library(DT)
library(ggfittext)
library(treemap)
library(patchwork)
#library(rgdal)
#library(geojsonio)
#library(kableExtra)
#options(knitr.kable.NA = '')
library(flextable)
library(DemoDecomp)
library(ggbump)
library(openxlsx)
library(patchwork)
library(ggnewscale)
# library(webshot)
#library(gt)

source("rmd/graph_functions.R")

load(file = "data_2021v2.rda")
load(file = "sdgdata_new.rda")
set.seed(1)

# customcolors <- c("#1C99DB", "#85C122", "#F2662A", "#F2A60D", "#A51C87", "#5F3B8B" ,
#                   "#EEDCD5", "#00FCDE", "#CA00FB", "#FB0D94", "#7D2E16", "#1653FB",
#                   "#0D6053", "#FC000D", "#22FE2A", "#86760D", "#FD0DE4", "#FABDFF", 
#                   "#F1E30D", "#A9EBFF", "#FE97B5", "#845D6E", "#B3F0BB", "#B083FE",
#                   "#22E989", "#AB0035", "#167A26", "#FBBB98", "#E673EE", "#EBE695")
# customcolors = createPalette(30,  c("#009ADE", "#80BC00", "#F26829", "#F4A81D", "#A6228C", "#5B2C86"))

customcolors <- c("#1C99DB", "#85C122", "#F2A60D", "#A51C87", "#A200FF", "#EFDDDA", 
                  "#00FCDE" ,"#A21C16", "#226542" ,"#7D4F26" ,"#D199FF" ,"#564970",
                  "#22FE2A", "#F894AC", "#0072FF", "#F1E30D", "#A7EAFF", "#B2EEB9" ,
                  "#CABB73", "#22E989", "#FF781C", "#FB356A" ,"#4740AD" ,"#F9BBF1" ,
                  "#AA00BD", "#F83DCB", "#167F8B")

# cod00_ <- read.csv("input/cod2000_byage.csv")

cod_decomp_time <- read.csv("input/level1_byage_sex_year.csv") %>% 
  mutate(sex = case_when(
    DIM_SEX_CODE=="TOTAL" ~ "Both sexes", 
    DIM_SEX_CODE=="FEMALE" ~ "Female", 
    DIM_SEX_CODE=="MALE" ~ "Male")) %>% 
  mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
         region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(age = str_remove(DIM_AGEGROUP_CODE, "^Y")) %>% 
  mutate(age = str_extract(age, "^[:digit:]+")) %>% 
  mutate(age = ifelse(DIM_AGEGROUP_CODE=="YGE_85", "85", age)) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(DIM_AGEGROUP_CODE != "TOTAL")

start_year <- c("2000", "2005", "2010", "2015", "2019")
end_year <- c("2005", "2010", "2015", "2019", "2021")
years_decomp <- data.frame(start_year, end_year)

data_pyramid <- read.csv("input/data_pyramid.csv") %>% 
  mutate(sex = case_when(
    DIM_SEX_CODE=="TOTAL" ~ "Both sexes", 
    DIM_SEX_CODE=="FEMALE" ~ "Female", 
    DIM_SEX_CODE=="MALE" ~ "Male")) %>% 
  mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
         region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region"))

country_table <- read.csv("rmd/country_table.csv")

# Myhcmap <- rjson::fromJSON(file="poly.json")
# Myhcmap2 <- rjson::fromJSON(file="line.json")
# Myhcmap3 <- rjson::fromJSON(file="poly2.json")

iso3sdg_ <- filtered_indicator_values %>% filter(aggregation_level == "iso3") %>% distinct(country) %>% pull()
indsdg_ <- filtered_indicator_values %>% arrange(indicator_name) %>% distinct(indicator_name) %>% pull()
sdg_sort <- filtered_indicator_values %>% distinct(indicator_name, small_is_best, units) %>% 
  mutate(small_is_best = ifelse(
    is.na(small_is_best),
    case_when(
      indicator_name=="Average Service Coverage" ~ FALSE,
      indicator_name=="UHC Single Measure" ~ FALSE,
      indicator_name=="Prevent" ~ FALSE
    ),
    small_is_best
  ))

sexinclude_ind <- data %>% filter(!is.na(sex)) %>% distinct(name) %>% pull(name)
locinclude_ind <- data %>% filter(!is.na(location)) %>% distinct(name) %>% pull(name)

inds <- data %>% distinct(name) %>% pull()
year_ <- data %>% distinct(year) %>% arrange(desc(year)) %>% filter(year<2025) %>%  pull()

iso3le <- data %>%
  filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>% 
  filter(geotype %in% c("GLOBAL", "REGION", "COUNTRY")) %>% 
  mutate(geotype = factor(geotype, levels = c("GLOBAL", "REGION", "COUNTRY"))) %>% 
  arrange(geotype, country) %>% 
  distinct(country) %>% 
  pull()


iso3cod <- cod19 %>% distinct(country) %>% pull()

top10 <- top10 %>% 
  left_join(
    cod19 %>% distinct(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP)
  ) %>% 
  mutate(group = case_when(
    FLAG_CAUSEGROUP==2 ~ "Communicable, maternal, perinatal and nutritional conditions" ,
    FLAG_CAUSEGROUP==1 ~ "Noncommunicable diseases",
    FLAG_CAUSEGROUP==3 ~ "Injuries",
    FLAG_CAUSEGROUP==4 ~ "Other COVID-19 pandemic-related outcomes"
  ))

lev1_causes <- c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other COVID-19 pandemic-related outcomes")
level2 <- cod19 %>% filter(FLAG_LEVEL==2) %>% distinct(DIM_GHECAUSE_TITLE) %>% pull()
# level3 <- cod19 %>% filter(FLAG_LEVEL==3) %>% distinct(DIM_GHECAUSE_TITLE) %>% pull()

tabletext <- htmltools::HTML("The statistics shown here are official WHO statistics for selected health-related Sustainable Development Goal (SDG) indicators and selected Thirteenth General 
Programme of Work (GPW13) indicators, based on data available in early 2024. In addition, summary measures of health, such as (healthy) life expectancy, are included. These statistics have 
been compiled primarily from publications and databases produced and maintained by WHO, United Nations bodies of which WHO is a member and other international organizations.  

The type of data used for each data series (comparable estimate or primary data) is indicated. Primary data are typically compiled from routine reporting or from publicly available sources 
such as Demographic and Health Surveys. Statistics are presented as they are reported or with minimal adjustment. Comparable estimates are achieved by adjusting or modelling country data to 
allow comparisons across countries and areas, and over time. Comparable estimates for the same reference years are produced for countries and areas with underlying primary data and, in some 
cases, for those without. Comparable estimates are subject to considerable uncertainty, especially for countries where the availability and quality of the underlying primary data are limited. 
Uncertainty intervals and other details on the indicators and statistics presented here can be found at the 
<a href='https://www.who.int/data/gho/data/themes/world-health-statistics'>WHO Global Health Observatory</a>.<br><br> 

Although every effort has been made to maximize the comparability of statistics across countries and areas, and over time, data series based on primary data may differ in terms of the 
definitions, data collection methods, population coverage and estimation methods used. For indicators with a reference period expressed as a range, country or area values refer to the 
latest available year in the range unless otherwise noted. In some cases, in the absence of a recent set of data for a specific SDG or GPW13 indicator, a proxy indicator is presented here. 

WHO regional and global aggregates for rates and ratios are presented as weighted averages when relevant, whereas they are the sums for absolute numbers. Some WHO regional and global 
aggregates may include country and area estimates that are not individually reported. See the metadata tab on the 
<a href='https://www.who.int/data/gho/data/themes/world-health-statistics'>WHO Global Health Observatory</a> for more information.<br><br> 

The notation “-” indicates that data are not applicable or not available.")


# Filter table
annexdata3 <- data %>% 
  mutate(sex = ifelse(sex==" " | is.na(sex), "-", sex)) %>%
  mutate(location = case_when(location=="RESIDENCEAREATYPE_RUR" ~ "Rural",
                              location=="RESIDENCEAREATYPE_URB" ~ "Urban",
                              location=="RESIDENCEAREATYPE_TOTL" ~ "Both Rural and Urban")) %>%
  mutate(location = ifelse(location==" " | is.na(location), "-", location)) %>% 
  select(name, country, region_name, country, year, sex, location, stringvalue, value, sort_country)


regions2 <- annexdata3 %>% arrange(region_name) %>% distinct(region_name) %>% pull()
isoreg2 <- data %>% arrange(desc(geotype), country) %>% distinct(country) %>% pull()

ind <- annexdata3 %>% distinct(name) %>% pull()
yeartab <- annexdata3 %>% distinct(year) %>% arrange(desc(year)) %>% pull()


### HEATMAP

# Global
globalheat <- cod19 %>%
  filter(FLAG_LEVEL == 2) %>%
  filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
  mutate(country = "Global") %>%
  select(country, sex, DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC) %>% 
  group_by(country, sex, DIM_GHECAUSE_TITLE) %>% 
  summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
  ungroup() %>%
  left_join(
    cod19 %>%
      filter(FLAG_LEVEL == 2) %>%
      filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
      mutate(country = "Global") %>% 
      group_by(country, sex) %>% 
      summarise(total_deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      ungroup()
  ) %>%
  group_by(country, sex) %>% 
  mutate(prop_deaths = deaths/total_deaths,
         rank = rank(-prop_deaths, ties.method = "first")) %>% 
  ungroup()%>%
  mutate(prop_deaths = round(prop_deaths*100, 1)) %>% 
  select(country, sex, DIM_GHECAUSE_TITLE, prop_deaths, rank)

# Region
regionheat <- cod19 %>%
  filter(FLAG_LEVEL == 2) %>%
  filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
  select(country = region2, sex, DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC) %>% 
  group_by(country, sex, DIM_GHECAUSE_TITLE) %>% 
  summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
  ungroup() %>%
  left_join(
    cod19 %>%
      filter(FLAG_LEVEL == 2) %>%
      filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
      select(country = region2, sex, DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC) %>% 
      group_by(country, sex) %>% 
      summarise(total_deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      ungroup()
  ) %>%
  group_by(country, sex) %>% 
  mutate(prop_deaths = deaths/total_deaths,
         rank = rank(-prop_deaths, ties.method = "first")) %>% 
  ungroup()%>%
  mutate(prop_deaths = round(prop_deaths*100, 1)) %>% 
  select(country, sex, DIM_GHECAUSE_TITLE, prop_deaths, rank)

# Country
countryheat <- cod19 %>%
  filter(FLAG_LEVEL == 2) %>%
  filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
  group_by(country, sex) %>%
  mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
         rank = rank(-prop_deaths, ties.method = "first")) %>%
  ungroup() %>%
  mutate(prop_deaths = round(prop_deaths*100, 1)) %>% 
  select(country, sex, DIM_GHECAUSE_TITLE, prop_deaths, rank)

heatdata <- rbind(globalheat, regionheat, countryheat)

rm(globalheat, regionheat, countryheat)


# Heatmap version 2

# globalheatv2 <- cod19 %>%
#   filter(FLAG_LEVEL %in% c(1,2,3)) %>%
#   filter(DIM_AGEGROUP_CODE==101) %>% 
#   mutate(country = "Global") %>%
#   select(country, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC, ATTR_POPULATION_NUMERIC) %>% 
#   group_by(country, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE) %>% 
#   summarise(
#     deaths = sum(VAL_DEATHS_COUNT_NUMERIC),
#     pop = sum(ATTR_POPULATION_NUMERIC)
#     ) %>% 
#   ungroup() %>%
#   mutate(mort_rate = round((deaths/pop)*100000, 1)) 
# 
# regionheatv2 <- cod19 %>%
#   filter(FLAG_LEVEL %in% c(1,2,3)) %>%
#   filter(DIM_AGEGROUP_CODE==101) %>% 
#   select(country = region2, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC, ATTR_POPULATION_NUMERIC) %>% 
#   group_by(country, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE) %>% 
#   summarise(
#     deaths = sum(VAL_DEATHS_COUNT_NUMERIC),
#     pop = sum(ATTR_POPULATION_NUMERIC)
#   ) %>% 
#   ungroup() %>%
#   mutate(mort_rate = round((deaths/pop)*100000, 1)) 

countryheatv2_2021 <- cod19 %>%
  filter(FLAG_LEVEL %in% c(1,2,3)) %>%
  filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
  select(country, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE, deaths=VAL_DEATHS_COUNT_NUMERIC, pop=ATTR_POPULATION_NUMERIC) %>% 
  mutate(mort_rate = round((deaths/pop)*100000, 1),
         year = 2021)

countryheatv2_2000 <- cod00 %>%
  filter(FLAG_LEVEL %in% c(1,2,3)) %>%
  filter(DIM_AGEGROUP_CODE=="TOTAL") %>% 
  select(country, sex, FLAG_LEVEL, DIM_GHECAUSE_TITLE, deaths=VAL_DEATHS_COUNT_NUMERIC, pop=ATTR_POPULATION_NUMERIC) %>% 
  mutate(mort_rate = round((deaths/pop)*100000, 1),
         year = 2000)

heatdatav2 <- rbind(countryheatv2_2021, countryheatv2_2000)
# heatdatav2 <- rbind(globalheatv2, regionheatv2, countryheatv2)

rm(countryheatv2_2021, countryheatv2_2000)

# ## Testing bump chart
# library(ggbump)
# 
# df_compare <- full_join(
#   heatdatav2_year1 %>% select(rank1 = rank, country), 
#   heatdatav2_year2 %>% select(rank2 = rank, country)) %>% 
#   mutate(type = case_when(
#     rank2<rank1 ~ "increase",
#     rank2>rank1 ~ "decrease",
#     rank2==rank1 ~ "same"
#   ))
# 
# test <- df %>% 
#   left_join(df_compare %>% select(country, type)) %>% 
#   mutate(year = ifelse(year==2021, 2001, year))
# test %>% 
#   ggplot(aes(x = year, y = rev(rank), group = country, color = type)) +
#   ggbump::geom_bump(size = 1.5, show.legend = F) +
#   geom_point(size = 6, show.legend = F) +
#   # geom_text(data = test %>% filter(year==2000),
#   #           aes(x = year-0.1, y = rev(rank), 
#   #               label = str_wrap(country, 30)),
#   #           size = 4, color = "black") +
#   # geom_text(data = test %>% filter(year==2001),
#   #           aes(x = year+0.1, y = rev(rank), 
#   #               label = str_wrap(country, 30)),
#   #           size = 4, color = "black") +
#   scale_x_continuous(labels = c("2000", "2021"), breaks = c(2000, 2001))+
#   scale_color_manual(
#     values = c("decrease" = "#80BC00",
#                "increase" = "#EF3842",
#                "same" = "#009ADE")
#   )+ theme_void()
  
#### 3B Contributions
config_all <- config::get(
  file = "input/all.yml",
  config = Sys.getenv("USER")
)

ind_labels <- openxlsx::readWorkbook("input/indicator_labels WHS 2.xlsx")

inds <- get_inds(get_forecast_inds = FALSE, get_billion_inds = TRUE)
inds$uhc <- c(inds$uhc, "population")
inds$hep <- config_all$hep_intermediate_inds
inds_summary <- c("hpop" = "hpop_healthier", "uhc" = "uhc_billion", "hep" = "hep_idx")

inds_all <- inds %>%
  unlist(use.names = FALSE) %>%
  unique() %>%
  c(inds_summary) %>%
  unname()

all_plt_dat <- arrow::read_parquet("input/2024-03-26-10-06_summary.parquet") %>% 
  mutate(country = whoville::iso3_to_names(aggregate_id),
         country = ifelse(is.na(country),
                          case_when(
                            aggregate_id=="global" ~"Global",
                            aggregate_id=="AFR" ~"African Region",
                            aggregate_id=="AMR" ~"Region of the Americas",
                            aggregate_id=="EMR" ~"Eastern Mediterranean Region",
                            aggregate_id=="EUR" ~"European Region",
                            aggregate_id=="SEAR" ~"South-East Asia Region",
                            aggregate_id=="WPR" ~"Western Pacific Region"
                          ),
                          country)
  ) %>% 
  mutate(
    contribution = mean_contribution,
    contribution_mln = contribution / 1e6
  ) 


## Maternal Mortality

rmnch_est <- read_sf("input/rmnch/map_estimates_rmnch_all_scaled_update.shp") %>% 
  st_drop_geometry() %>% 
  # filter(iso3==country_iso) %>% 
  mutate(region_ = str_to_title(region)) %>% 
  mutate(width = round(upper - lower, digits = 1)) %>% 
  mutate(country =iso3_to_names(iso3)) %>% 
  filter(country %in% c("Benin", "Burkina Faso", "Central African Republic", "Democratic Republic of the Congo", 
                        "Guinea-Bissau", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "South Africa", "United Republic of Tanzania", "Zambia"))

# test <- rmnch_est %>% filter(iso3=="NGA")
# exportJson <- rjson::toJSON(test)
# county_json <- geojsonio::geojson_json(test)
# write(county_json, "input/rmnch/map_NGA.json")

for(i in c("BEN", "BFA", "CAF", "COD", "GNB", "RWA", "SEN", "SLE", "TZA", "ZMB", "NGA", "ZAF")){ ## SA & Nigeria doesn't have a map yet
  
  assign(paste0("map", i), rjson::fromJSON(file=paste0("input/rmnch/map_", i, ".json")))
  
}

data_mmr <- read.csv("input/mmr.csv") %>% filter(ParentLocationCode=="AFR")

mmr_region <- read.csv("input/mmr_region.csv") %>% filter(Region.code %in% c("Global", "AFR"))

final_ <- read.csv("input/mmr_forecast.csv") %>% mutate(country =iso3_to_names(iso3))

iso3sdg_ <- filtered_indicator_values %>% filter(aggregation_level == "iso3") %>% distinct(country) %>% pull()
indsdg_ <- filtered_indicator_values %>% arrange(indicator_name) %>% distinct(indicator_name) %>% pull()
sdg_sort <- filtered_indicator_values %>% distinct(indicator_name, small_is_best, units) %>% 
  mutate(small_is_best = ifelse(
    is.na(small_is_best),
    case_when(
      indicator_name=="Average Service Coverage" ~ FALSE,
      indicator_name=="UHC Single Measure" ~ FALSE,
      indicator_name=="Prevent" ~ FALSE
    ),
    small_is_best
  ))

iso3le <- c("Global", "African Region", 
            "Benin", "Burkina Faso", "Central African Republic", "Democratic Republic of the Congo", 
            "Guinea-Bissau", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "South Africa", "United Republic of Tanzania", "Zambia")

iso3afr <- data %>% filter(region_name=="African Region") %>% arrange(country) %>% distinct(country) %>% pull()
iso3afr_mmr <- data_mmr %>% filter(IndicatorCode == "MDG_0000000026" & Period>=2000) %>% distinct(Location) %>% arrange(Location) %>% pull()

poly_filepath = "input/shapefiles/MapTemplate_generalized_2013/Shapefiles/"
poly <- st_read(dsn = poly_filepath, layer = "general_2013")
poly_mask <- st_read(dsn = poly_filepath, layer = "maskpoly_general_2013")
poly_line <- st_read(dsn = poly_filepath, layer = "maskline_general_2013")


mmr_region_ <- mmr_region %>% 
  mutate(mmr = as.numeric(str_extract(Maternal.mortality.ratio..per.100.000.live.births., "^[:digit:]+"))) %>% 
  select(year = Year, WHO.region, mmr) %>% 
  filter(year>=2000)

roc_region <- mmr_region_ %>% 
  filter(year==2000 | year==2020) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = mmr) %>% 
  mutate(roc = (log(`2020`/`2000`)) / 20) %>% 
  mutate(n_years = (log(70/`2020`)) / roc) %>%
  mutate(`2021` = (exp(roc*1))*`2020`,
         `2022` = (exp(roc*2))*`2020`,
         `2023` = (exp(roc*3))*`2020`,
         `2024` = (exp(roc*4))*`2020`,
         `2025` = (exp(roc*5))*`2020`,
         `2026` = (exp(roc*6))*`2020`,
         `2027` = (exp(roc*7))*`2020`,
         `2028` = (exp(roc*8))*`2020`,
         `2029` = (exp(roc*9))*`2020`,
         `2030` = (exp(roc*10))*`2020`) %>% 
  pivot_longer(cols = c(`2021`, `2022`, `2023`, `2024`, `2025`, `2026`,`2027`, `2028`, `2029`, `2030`), names_to = "year", values_to = "mmr") %>% 
  mutate(mmr = round(mmr, digits = 0)) %>% 
  select(WHO.region, year, mmr)

mmr_region__ <- rbind(mmr_region_, roc_region) %>% 
  mutate(year = as.numeric(year)) %>% 
  rbind()

