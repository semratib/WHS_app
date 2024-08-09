#Create datasets
# COD data prep
# cod00 <- read.csv("C:/Users/samra/Documents/DNA/WHS_app_rmd/input/data_cod_2000.csv") %>% 
# cod00 <- read.csv("C:/Users/samra/Downloads/cod_2000_allsexes.csv") %>% 
#   filter(DIM_AGEGROUP_CODE=="ALLAges") %>% 
#            # DIM_SEX_CODE == "BTSX") %>% 
#   # rename(DIM_COUNTRY_CODE = X...DIM_COUNRY_CODE) %>%
#   mutate(country =iso3_to_names(DIM_COUNTRY_CODE),
#          region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
#   mutate(sex = case_when(
#     DIM_SEX_CODE=="BTSX" ~ "Both sexes", 
#     DIM_SEX_CODE=="FMLE" ~ "Female", 
#     DIM_SEX_CODE=="MLE" ~ "Male")) %>% 
#   mutate(region2 = case_when(
#     region=="WPR" ~"Western Pacific Region",
#     region=="SEAR" ~ "South-East Asia Region",
#     region=="EMR" ~ "Eastern Mediterranean Region",
#     region=="EUR" ~ "European Region",
#     region=="AMR" ~ "Region of the Americas",
#     region=="AFR" ~ "African Region")) 
  # select(-c(VAL_DALY_RATE100K_NUMERIC, VAL_DALY_COUNT_NUMERIC, VAL_DALY_COUNT_LOW, VAL_DALY_COUNT_HIGH))


# cod19 <- read.csv("C:/Users/samra/Documents/DNA/WHS_app_rmd/input/data_cod_2019.csv") %>% 
# cod19 <- read.csv("C:/Users/samra/Downloads/cod_2019_allsexes.csv") %>% 
#   filter(DIM_AGEGROUP_CODE=="ALLAges") %>% 
#            # DIM_SEX_CODE == "BTSX") %>% 
#   # rename(DIM_COUNTRY_CODE = X...DIM_COUNRY_CODE) %>%
#   mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
#          region = iso3_to_regions(DIM_COUNTRY_CODE))%>% 
#   mutate(sex = case_when(
#     DIM_SEX_CODE=="BTSX" ~ "Both sexes", 
#     DIM_SEX_CODE=="FMLE" ~ "Female", 
#     DIM_SEX_CODE=="MLE" ~ "Male")) %>% 
#   mutate(region2 = case_when(
#     region=="WPR" ~"Western Pacific Region",
#     region=="SEAR" ~ "South-East Asia Region",
#     region=="EMR" ~ "Eastern Mediterranean Region",
#     region=="EUR" ~ "European Region",
#     region=="AMR" ~ "Region of the Americas",
#     region=="AFR" ~ "African Region")) %>% 
#   # ## FAKE 2021 DATA
#   rename(VAL_DEATHS_COUNT_NUMERIC_V1 = VAL_DEATHS_COUNT_NUMERIC,
#          ATTR_POPULATION_NUMERIC_V1 = ATTR_POPULATION_NUMERIC) %>% 
#   mutate(VAL_DEATHS_COUNT_NUMERIC = VAL_DEATHS_COUNT_NUMERIC_V1*1.1,
#          ATTR_POPULATION_NUMERIC = ATTR_POPULATION_NUMERIC_V1*1.1) %>% 
#   select(-c(VAL_DEATHS_COUNT_NUMERIC_V1, ATTR_POPULATION_NUMERIC_V1))
  # select(-c(VAL_DALY_RATE100K_NUMERIC, VAL_DALY_COUNT_NUMERIC, VAL_DALY_COUNT_LOW, VAL_DALY_COUNT_HIGH))


# cod19_top10 <- read.csv("C:/Users/samra/Downloads/cod_2019_allsexes.csv") %>% 
#   filter(DIM_AGEGROUP_CODE=="ALLAges") %>% 
#   # rename(DIM_COUNTRY_CODE = X...DIM_COUNRY_CODE) %>%
#   mutate(country =iso3_to_names(DIM_COUNTRY_CODE),
#          region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
#   mutate(region2 = case_when(
#     region=="WPR" ~"Western Pacific Region",
#     region=="SEAR" ~ "South-East Asia Region",
#     region=="EMR" ~ "Eastern Mediterranean Region",
#     region=="EUR" ~ "European Region",
#     region=="AMR" ~ "Region of the Americas",
#     region=="AFR" ~ "African Region")) %>% 
#   mutate(sex = case_when(
#     DIM_SEX_CODE=="BTSX" ~ "Both sexes", 
#     DIM_SEX_CODE=="FMLE" ~ "Female", 
#     DIM_SEX_CODE=="MLE" ~ "Male")) %>% 
#   # ## FAKE 2021 DATA
#   rename(VAL_DEATHS_COUNT_NUMERIC_V1 = VAL_DEATHS_COUNT_NUMERIC) %>% 
#   mutate(VAL_DEATHS_COUNT_NUMERIC = VAL_DEATHS_COUNT_NUMERIC_V1*1.1)
# 
# 
# top5region <- cod19_top10 %>% 
#   filter(FLAG_TREEMAP == 1) %>%
#   group_by(region2, DIM_GHECAUSE_TITLE, sex) %>% 
#   summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
#   ungroup() %>% 
#   arrange(region2, sex, desc(deaths)) %>% 
#   rename(region = region2)
# 
# top5region_ <- top5region %>%
#   group_by(region, sex) %>%
#   arrange(region, sex, desc(deaths)) %>%
#   slice(1:10) 
# 
# top5global <- cod19_top10 %>% 
#   filter(FLAG_TREEMAP == 1) %>%
#   group_by(DIM_GHECAUSE_TITLE, sex) %>% 
#   summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
#   ungroup() %>% 
#   mutate(region = "Global") %>%
#   group_by(region, sex) %>%
#   arrange(sex, desc(deaths)) %>%
#   slice(1:10)
# 
# top5country <-  cod19_top10 %>% 
#   filter(FLAG_TREEMAP == 1) %>%
#   group_by(country, DIM_GHECAUSE_TITLE, sex) %>% 
#   summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
#   ungroup() %>% 
#   rename(region = country)
# 
# top5country_ <- top5country %>%
#   group_by(region, sex) %>%
#   arrange(region, sex, desc(deaths)) %>%
#   slice(1:10)
# 
# 
# top10<- rbind(top5global, top5region_, top5country_) 


# cod_age <- read.csv("C:/Users/samra/Downloads/data_export_DEX_CMS_GHE_FULL.csv (7)/data_export_DEX_CMS_GHE_FULL.csv") %>% 
# # cod_age_new2 <- cod_age_new %>% 
#   mutate(sex = case_when(
#     DIM_SEX_CODE=="BTSX" ~ "Both sexes", 
#     DIM_SEX_CODE=="FMLE" ~ "Female", 
#     DIM_SEX_CODE=="MLE" ~ "Male")) %>% 
# # cod_age <- cod_age_new2 %>% 
#   mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
#          region = iso3_to_regions(DIM_COUNTRY_CODE))%>% 
#   mutate(region2 = case_when(
#     region=="WPR" ~"Western Pacific Region",
#     region=="SEAR" ~ "South-East Asia Region",
#     region=="EMR" ~ "Eastern Mediterranean Region",
#     region=="EUR" ~ "European Region",
#     region=="AMR" ~ "Region of the Americas",
#     region=="AFR" ~ "African Region")) %>% 
#   # ## FAKE 2021 DATA
#   rename(VAL_DEATHS_COUNT_NUMERIC_V1 = VAL_DEATHS_COUNT_NUMERIC,
#          ATTR_POPULATION_NUMERIC_V1 = ATTR_POPULATION_NUMERIC) %>% 
#   mutate(VAL_DEATHS_COUNT_NUMERIC = VAL_DEATHS_COUNT_NUMERIC_V1*1.1,
#          ATTR_POPULATION_NUMERIC = ATTR_POPULATION_NUMERIC_V1*1.1) %>% 
#   select(-c(VAL_DEATHS_COUNT_NUMERIC_V1, ATTR_POPULATION_NUMERIC_V1))

#######################################################
# load("GHE2021_CoD.RData")
# cod2021 <- ghe.out.fin %>% filter(year==2021)
# cod2000 <- ghe.out.fin %>% filter(year==2000)
# write.csv(cod2021, "cod2021.csv")
# write.csv(cod2000, "cod2000.csv")

cod2021 <- read.csv("cod2021.csv")
cod2000 <- read.csv("cod2000.csv")

# Create cod 2000 data
cod2000_allage <- cod2000 %>% 
  filter(age != 0.1) %>% 
  filter(age != 0.11) %>% 
  group_by(iso3, sex, causename, ghecause) %>% 
  summarise(deaths = sum(dths),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  left_join(cod2000 %>% distinct(ghecause, causegroup, level, treemap)) %>% 
  arrange(iso3, sex, ghecause) %>% 
  mutate(country =iso3_to_names(iso3),
         region = iso3_to_regions(iso3)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(sex = case_when(
    sex==3 ~ "Both sexes", 
    sex==2 ~ "Female", 
    sex==1 ~ "Male")) %>% 
  mutate(DIM_YEAR_CODE = 2000)

# Create cod 2021 data
cod2021_allage <- cod2021 %>% 
  filter(age != 0.1) %>% 
  filter(age != 0.11) %>% 
  group_by(iso3, sex, causename, ghecause) %>% 
  summarise(deaths = sum(dths),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  left_join(cod2000 %>% distinct(ghecause, causegroup, level, treemap)) %>% 
  arrange(iso3, sex, ghecause) %>% 
  mutate(age=101)

cod2021v2 <- cod2021 %>% 
  filter(age != 0.1) %>% 
  filter(age != 0.11) %>% 
  select(iso3, sex, age, causename, ghecause, deaths = dths, pop, ghecause, causegroup, level, treemap) %>% 
  arrange(iso3, sex, ghecause)

cod2021_final <- rbind(cod2021v2, cod2021_allage) %>% 
  mutate(country =iso3_to_names(iso3),
         region = iso3_to_regions(iso3)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(sex = case_when(
    sex==3 ~ "Both sexes", 
    sex==2 ~ "Female", 
    sex==1 ~ "Male"))%>% 
  mutate(DIM_YEAR_CODE = 2021)

top5region <- cod2021_final %>% 
  filter(age==101) %>% 
  filter(causename!="COVID-19") %>% 
  filter(causename!="Other pandemic related causes") %>% 
  filter(treemap == 1) %>%
  group_by(region2, causename, sex) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup() %>% 
  arrange(region2, sex, desc(deaths)) %>% 
  rename(region = region2)

top5region_ <- top5region %>%
  group_by(region, sex) %>%
  arrange(region, sex, desc(deaths)) %>%
  slice(1:10) 

top5global <- cod2021_final %>%
  filter(age==101) %>% 
  filter(causename!="COVID-19") %>% 
  filter(causename!="Other pandemic related causes") %>% 
  filter(treemap == 1) %>%
  group_by(causename, sex) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup() %>% 
  mutate(region = "Global") %>%
  group_by(region, sex) %>%
  arrange(sex, desc(deaths)) %>%
  slice(1:10)

top5country <- cod2021_final %>%
  filter(age==101) %>% 
  filter(causename!="COVID-19") %>% 
  filter(causename!="Other pandemic related causes") %>% 
  filter(treemap == 1) %>%
  group_by(country, causename, sex) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup() %>% 
  rename(region = country)

top5country_ <- top5country %>%
  group_by(region, sex) %>%
  arrange(region, sex, desc(deaths)) %>%
  slice(1:10)

top10<- rbind(top5global, top5region_, top5country_) 


######################################################

# Indicator table
# annexdata <- read.csv("input/LAdata (2).csv", encoding = "UTF-8") %>%
annexdata <- read.csv("C:/Users/samra/Documents/DNA/WHS_app/input/LAdata_2024-05-25.csv", encoding = "UTF-8") %>%
  rename(name = namefromannex) %>% 
  mutate(region = whoville::iso3_to_regions(iso3, name = TRUE)) %>% 
  mutate(region_name = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(region_name = ifelse(iso3=="GLOBAL", "Global", region_name)) %>% 
  mutate(region_name = ifelse(
    iso3 %in% c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR") & is.na(region_name), 
    case_when(
      iso3=="WPR" ~"Western Pacific Region",
      iso3=="SEAR" ~ "South-East Asia Region",
      iso3=="EMR" ~ "Eastern Mediterranean Region",
      iso3=="EUR" ~ "European Region",
      iso3=="AMR" ~ "Region of the Americas",
      iso3=="AFR" ~ "African Region"),
    region_name
  )) %>% 
  filter(!is.na(region_name))


# annexdata_v2 <- read.csv("C:/Users/samra/Downloads/2000data (1).csv", encoding = "UTF-8") %>% 
# annexdata_v2 <- openxlsx::read.xlsx("C:/Users/samra/Downloads/2000data.xlsx") %>%
data_ <- openxlsx::read.xlsx("C:/Users/samra/Downloads/2000data.xlsx") %>%
  rename(name = namefromannex,
         GHOcode = ghocode) %>% 
  select(geotype, iso3, year, age, location, sex, value, footnotes, sort, name, GHOcode, stringvalue) %>% 
  mutate(name= ifelse(age %in% c("AGEGROUP_YEARS10-14","AGEGROUP_YEARS15-19"), 
                      case_when(
                        age=="AGEGROUP_YEARS10-14" ~ "Adolescent birth rate (per 1000 women) for girls aged 10-14",
                        age=="AGEGROUP_YEARS15-19" ~ "Adolescent birth rate (per 1000 women) for girls aged 15-19"
                      ),
                      name)) %>% 
  mutate(region = whoville::iso3_to_regions(iso3, name = TRUE)) %>% 
  mutate(region_name = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(region_name = ifelse(iso3=="GLOBAL", "Global", region_name)) %>% 
  mutate(region_name = ifelse(
    iso3 %in% c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR") & is.na(region_name), 
    case_when(
      iso3=="WPR" ~"Western Pacific Region",
      iso3=="SEAR" ~ "South-East Asia Region",
      iso3=="EMR" ~ "Eastern Mediterranean Region",
      iso3=="EUR" ~ "European Region",
      iso3=="AMR" ~ "Region of the Americas",
      iso3=="AFR" ~ "African Region"),
    region_name
  )) %>% 
  mutate(country = whoville::iso3_to_names(iso3),
         country = ifelse(is.na(country), 
                          case_when(
                            iso3=="GLOBAL" ~"Global",
                            iso3=="AFR" ~"African Region",
                            iso3=="AMR" ~"Region of the Americas",
                            iso3=="EMR" ~"Eastern Mediterranean Region",
                            iso3=="EUR" ~"European Region",
                            iso3=="SEAR" ~"South-East Asia Region",
                            iso3=="WPR" ~"Western Pacific Region"
                          ), 
                          country)) %>% 
  rename(sex2 = sex) %>% 
  mutate(sex = case_when(
    sex2=="SEX_BTSX" ~ "Both sexes",
    sex2=="SEX_FMLE" ~ "Female",
    sex2=="SEX_MLE" ~ "Male"
  )) %>% 
  select(-c(sex2))


lehale <- read.csv("LE_HALE_GHE2021.csv") %>% 
  filter(age==0) %>% 
  mutate(sex = case_when(
    sex==3 ~ "Both sexes", 
    sex==2 ~ "Female", 
    sex==1 ~ "Male")) %>% 
  mutate(country = case_when(
    whoname=="World" ~"Global",
    whoname=="1_Afr" ~"African Region",
    whoname=="2_Amr" ~"Region of the Americas",
    whoname=="5_Emr" ~"Eastern Mediterranean Region",
    whoname=="4_Eur" ~"European Region",
    whoname=="3_Sear" ~ "South-East Asia Region",
    whoname=="6_Wpr" ~ "Western Pacific Region",
    TRUE ~ whoname
  )) %>% 
  mutate(age = NA, 
         location = NA,
         footnotes = NA) %>% 
  mutate(region = whoville::iso3_to_regions(iso3, name = TRUE)) %>% 
  mutate(region_name = case_when(
    region=="WPR" ~"Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(region_name = ifelse(iso3=="World", "Global", region_name)) %>% 
  mutate(region_name = ifelse(
    whoname %in% c("1_Afr", "2_Amr", "5_Emr", "4_Eur", "3_Sear", "6_Wpr") & is.na(region_name), 
    case_when(
      whoname=="1_Afr" ~"African Region",
      whoname=="2_Amr" ~"Region of the Americas",
      whoname=="5_Emr" ~"Eastern Mediterranean Region",
      whoname=="4_Eur" ~"European Region",
      whoname=="3_Sear" ~ "South-East Asia Region",
      whoname=="6_Wpr" ~ "Western Pacific Region"),
    region_name
  )) %>% 
  mutate(geotype= case_when(
    whoname %in% c("1_Afr", "2_Amr", "5_Emr", "4_Eur", "3_Sear", "6_Wpr") ~ "REGION",
    region_name=="Global" ~"GLOBAL",
    TRUE ~ "COUNTRY"
  )) %>% 
  select(-c(whoname))

le <- lehale %>% 
  rename(value = LE) %>%
  select(-c(HALE)) %>% 
  mutate(name = "Life expectancy at birth (years)",
         GHOcode = "WHOSIS_000001",
         sort = 3,
         stringvalue = round(value, 1)) 

hale <- lehale %>% 
  rename(value = HALE) %>%
  select(-c(LE)) %>% 
  mutate(name = "Healthy life expectancy at birth (years)",
         GHOcode = "WHOSIS_000002",
         sort = 4,
         stringvalue = round(value, 1)) 

data <- data_ %>%
  filter(GHOcode != "WHOSIS_000001") %>% 
  filter(GHOcode != "WHOSIS_000002") %>% 
  rbind(le) %>% 
  rbind(hale) %>% 
  filter(country != "2_LMI") %>% 
  filter(country != "3_UMI") %>%
  mutate(sort_country = ifelse(country %in% c("Global", "African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region"), 
                       case_when(country=="Global" ~ 1,
                                 country %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                                "Region of the Americas", "South-East Asia Region", "Western Pacific Region") ~ 2),
                       3))
  
  


# data_2021lehale <- data %>% 
#   filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>%
#   filter(year==2000) %>% 
#   select(country, GHOcode, name, sex, value_2000 = value) %>% 
#   left_join(data %>% 
#               filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>%
#               filter(year==2019) %>% 
#               select(country, GHOcode, name, sex, value_2019 = value)) %>% 
#   mutate(arc = (value_2019-value_2000) / 19) %>% 
#   mutate(value_2021 = value_2019+(arc*2)) %>%
#   select(country, GHOcode, name, sex, value_2021)
# 
# data_2021lehale2 <- data %>% 
#   filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>%
#   filter(year==2019) %>% 
#   left_join(data_2021lehale) %>% 
#   select(-c(value)) %>% 
#   rename(value = value_2021) %>% 
#   mutate(year=2021)
# 
# datav2 <- rbind(data, data_2021lehale2)
# data <- datav2
  

# data <- read.csv("input/data_gho2023.csv") %>% 
#   filter(year>1999) %>% 
#   mutate(country = whoville::iso3_to_names(geo),
#          # country = ifelse(is.na(country), geo, country),
#          country = ifelse(is.na(country), 
#                           case_when(
#                             geo=="GLOBAL" ~"Global",
#                             geo=="AFR" ~"African Region",
#                             geo=="AMR" ~"Region of the Americas",
#                             geo=="EMR" ~"Eastern Mediterranean Region",
#                             geo=="EUR" ~"European Region",
#                             geo=="SEAR" ~"South-East Asia Region",
#                             geo=="WPR" ~"Western Pacific Region"
#                           ), 
#                           country),
#          value = as.numeric(value)) %>% 
#   rename(sex2 = sex) %>% 
#   mutate(sex = case_when(
#     sex2=="SEX_BTSX" ~ "Both sexes",
#     sex2=="SEX_FMLE" ~ "Female",
#     sex2=="SEX_MLE" ~ "Male"
#   )) %>% 
#   arrange(GHOcode, geotype, geo, year) %>% 
#   select(-c(sex2))


##################################

cod19 <- cod2021_final %>% rename(DIM_GHECAUSE_TITLE = causename,
                                  ATTR_POPULATION_NUMERIC = pop,
                                  VAL_DEATHS_COUNT_NUMERIC = deaths,
                                  DIM_COUNTRY_CODE = iso3,
                                  DIM_GHECAUSE_CODE = ghecause,
                                  DIM_AGEGROUP_CODE = age,
                                  FLAG_TREEMAP = treemap,
                                  FLAG_CAUSEGROUP = causegroup,
                                  FLAG_LEVEL = level)

cod00 <- cod2000_allage %>% rename(DIM_GHECAUSE_TITLE = causename,
                                  ATTR_POPULATION_NUMERIC = pop,
                                  VAL_DEATHS_COUNT_NUMERIC = deaths,
                                  DIM_COUNTRY_CODE = iso3,
                                  DIM_GHECAUSE_CODE = ghecause,
                                  # DIM_AGEGROUP_CODE = age,
                                  FLAG_TREEMAP = treemap,
                                  FLAG_CAUSEGROUP = causegroup,
                                  FLAG_LEVEL = level)

top10 <- top10 %>%  rename(DIM_GHECAUSE_TITLE = causename)

save(data, cod00, cod19, top10, annexdata, file = "data_2021v2.rda")

# save(data, cod00, cod19, cod_age, top10, annexdata, file = "data.rda")

# save(data, cod00, cod19, cod_age, top10, annexdata, file = "data_fake2021.rda")

## For SDG Table
table_dat <- read.csv("input/2023_05_18_contributions_tracers_dump.v4.csv")
str_date <- format(Sys.time(), "%Y.%m.%d")

sdg_goals <- readxl::read_xlsx(path = "input/GPW13_3B_data_with_SDG_Acceleration_scenarios_indicator_update_18.11.xlsx") %>%
  rename(ind = `GPW13 indicator code`, sdg_2030_goal = `2030 (SDG targets)`) %>%
  left_join(billionaiRe::indicator_df, by = "ind") %>%
  select(ind, sdg_2030_goal)

export_dat <- table_dat %>%
  mutate(sdg = get_ind_metadata(ind, "sdg"),
         name = get_ind_metadata(ind, "short_name"),
         units = get_ind_metadata(ind, "unit_raw")) %>%
  select(aggregation_level, aggregate_id, year, ind, billion, name, units, sdg, median_raw_value, raw_value_lower, raw_value_upper) %>%
  rename(value = median_raw_value, lower = raw_value_lower, upper = raw_value_upper)

joined <- full_join(sdg_goals, export_dat, by = c("ind"))

transformed_inds <- c("prevent", "hep_idx")

transform_export_dat <- table_dat %>%
  mutate(sdg = get_ind_metadata(ind, "sdg"),
         name = get_ind_metadata(ind, "short_name"),
         units = get_ind_metadata(ind, "unit_raw")) %>%
  mutate(value = case_when(ind %in% transformed_inds ~ median_transform_value, .default = median_raw_value),
         lower = case_when(ind %in% transformed_inds ~ transform_value_lower, .default = raw_value_lower),
         upper = case_when(ind %in% transformed_inds ~ transform_value_upper, .default = raw_value_upper)) %>%
  select(
    aggregation_level,
    aggregate_id,
    year,
    ind,
    billion,
    name,
    units,
    sdg,
    value,
    lower,
    upper
  ) %>%
  full_join(sdg_goals, by = c("ind")) %>%
  filter(!(ind == "espar" & billion == "uhc")) %>%
  mutate(billion = case_when(ind == "espar" ~ "uhc/hep", .default = billion))

exclude_indicators <-
  c("water_urban",
    "water_rural",
    "hpop_sanitation_rural",
    "hpop_sanitation_urban",
    "doctors",
    "nurses",
    "fh",
    "measles",
    "meningitis",
    "polio",
    "surviving_infants",
    "yellow_fever",
    "hpop_healthier",
    "hep_idx"
  )

filtered_indicator_values <- transform_export_dat %>%
  mutate(billion = billionaiRe::get_ind_billion(ind),
         small_is_best = billionaiRe::get_ind_metadata(ind, "small_is_best"),
         sdg = case_when(str_starts(sdg, "SDG") ~ sdg, .default = NA_character_)) %>%
  rename(indicator_name = name,
         gpw13_indicator_code = ind,
         sdg_code = sdg) %>%
  filter(
    !str_starts(gpw13_indicator_code, "hpop_healthier_"),
    !str_detect(gpw13_indicator_code, "routine"),
    !str_detect(gpw13_indicator_code, "campaign"),
    !gpw13_indicator_code %in% exclude_indicators
  ) %>% 
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
  )

ind_df <- billionaiRe::indicator_df %>% 
  select(gpw13_indicator_code = ind, order) %>% 
  mutate(order = ifelse(gpw13_indicator_code %in% c("uhc_sm", "asc"), 35, order))

save(filtered_indicator_values, ind_df, file = "sdgdata.rda")

####################
str_date <- format(Sys.time(), "%Y.%m.%d")
table_dat <- arrow::read_parquet("C:/Users/samra/Documents/DNA/CountryReportWHO/input/2024-03-26-10-06_summary.parquet", as_data_frame = FALSE) %>%
# table_dat <- read.csv("C:/Users/samra/Documents/DNA/CountryReportWHO/input/2024-03-26-10-06_for2018_2030.csv") %>%
  filter(scenario == "baseline") %>%
  collect()

sdg_goals <- readxl::read_xlsx(path = "C:/Users/samra/Documents/DNA/CountryReportWHO/input/GPW13_3B_data_with_SDG_Acceleration_scenarios_indicator_update_18.11.xlsx") %>%
  rename(ind = `GPW13 indicator code`, sdg_2030_goal = `2030 (SDG targets)`) %>%
  left_join(billionaiRe::indicator_df, by = "ind") %>%
  select(ind, sdg_2030_goal)

# Indicator metadata. Function `get_ind_metadata()` was failing for me on large table sizes.
ind_metadata <- billionaiRe::indicator_df %>%
  select(ind, sdg, short_name, small_is_best, unit_raw, unit_transformed)

export_dat <- table_dat %>%
  left_join(ind_metadata, by = "ind") %>%
  select(
    aggregation_level,
    aggregate_id,
    year,
    ind,
    billion,
    name = short_name,
    units = unit_raw,
    sdg,
    value = raw_value_mean,
    lower = raw_value_0.05,
    upper = raw_value_0.95
  )

joined <- full_join(sdg_goals, export_dat, by = c("ind"))

transformed_inds <- c("prevent", "hep_idx")

transform_export_dat <- table_dat %>%
  left_join(ind_metadata, by = "ind") %>%
  mutate(value = case_when(ind %in% transformed_inds ~ transform_value_mean, .default = raw_value_mean),
         lower = case_when(ind %in% transformed_inds ~ transform_value_0.05, .default = raw_value_0.05),
         upper = case_when(ind %in% transformed_inds ~ transform_value_0.95, .default = raw_value_0.95),
         units = case_when(ind %in% transformed_inds ~ unit_transformed, .default = unit_raw)) %>%
  select(
    aggregation_level,
    aggregate_id,
    year,
    ind,
    billion,
    name = short_name,
    units,
    small_is_best,
    sdg,
    value,
    lower,
    upper
  ) %>%
  full_join(sdg_goals, by = c("ind")) %>%
  filter(!(ind == "espar" & billion == "uhc")) %>%
  mutate(billion = case_when(ind == "espar" ~ "uhc/hep", .default = billion))

exclude_indicators <-
  c("water_urban",
    "water_rural",
    "hpop_sanitation_rural",
    "hpop_sanitation_urban",
    "doctors",
    "nurses",
    "fh",
    "measles",
    "meningitis",
    "polio",
    "surviving_infants",
    "yellow_fever",
    "hpop_healthier",
    "hep_idx",
    "uhc_sm_unadjusted",
    "population",
    "prop_acc_ess_serv"
  )

filtered_indicator_values <- transform_export_dat %>%
  mutate(sdg = case_when(str_starts(sdg, "SDG") ~ sdg, .default = NA_character_)) %>%
  rename(indicator_name = name,
         gpw13_indicator_code = ind,
         sdg_code = sdg) %>%
  filter(
    !str_starts(gpw13_indicator_code, "hpop_healthier_"),
    !str_detect(gpw13_indicator_code, "uhc_billion"),
    !str_detect(gpw13_indicator_code, "routine"),
    !str_detect(gpw13_indicator_code, "campaign"),
    !gpw13_indicator_code %in% exclude_indicators
  ) %>% 
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
  )

ind_df <- billionaiRe::indicator_df %>% 
  select(gpw13_indicator_code = ind, order) %>% 
  mutate(order = ifelse(gpw13_indicator_code %in% c("uhc_sm", "asc"), 35, order))

save(filtered_indicator_values, ind_df, file = "sdgdata_new.rda")


#################################################################################################

glob <- data.frame(
  level1 = c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other pandemic related  deaths"),
  deaths   = c(18722976,43443455, 4466053, 1847797)
)

total_deaths <- glob %>% 
  summarise(deaths = sum(deaths)) %>% pull()

glob$level1 <- factor(glob$level1, levels = c("Communicable, maternal, perinatal and nutritional conditions", "Noncommunicable diseases", "Injuries", "Other pandemic related  deaths"))

glob <- glob %>%
  mutate(cause_fraction = deaths / sum(deaths)) %>%
  mutate(cause_title = paste(level1, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "))

# make treemap
tm <- treemap::treemap(glob,
                            index=c("cause_title"),
                            vSize="deaths",
                            type="index",
                            palette = "Set1",
                            draw = FALSE
)

tm_plot_data <- tm$tm %>% 
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>% 
  mutate(primary_group = ifelse(is.na(cause_title), 1.2, .5)) %>% 
  mutate(color = case_when(
    cause_title=="Communicable, maternal, perinatal and nutritional conditions (27.3%)" ~"#F26829",
    cause_title=="Noncommunicable diseases (63.4%)" ~ "#009ADE",
    cause_title=="Injuries (6.5%)" ~ "#80BC00",
    cause_title=="Other pandemic related  deaths (2.7%)" ~ "#A6228C"
  )) %>% 
  mutate(color = ifelse(is.na(cause_title), NA, color))

pdf("C:/Users/samra/Documents/DNA/treemap_global.pdf", width = 10, height = 7.5)

ggplot(tm_plot_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "white", alpha=0.5) +
  scale_fill_identity() +
  scale_size(range = range(tm_plot_data$primary_group)) +
  ggfittext::geom_fit_text(aes(label = cause_title), min.size = 4, color = "black", grow = TRUE, reflow = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void()+
  labs(title = "Total deaths: 68,480,281",
       caption = "Note: Deaths considered 'Other pandemic related deaths' could also be attributed to the other categories.")+
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(hjust = 0.5, size = 24, vjust = 3),
    plot.caption = element_text(vjust = -10)
  )
dev.off()


