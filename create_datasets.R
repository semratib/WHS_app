
library(tidyverse)

# Cause of Death  --------------------------------------------------------------
# Data downloaded from: https://extranet.who.int/xmart4/DEX_CMS/data/GHE_FULL
# - with all age groups
# - with year 2021 and 2000
# - with all sexes

cod <- read.csv("input\\data_export_DEX_cod_20240930.csv")

table(cod$DIM_YEAR_CODE, useNA = "always")
table(cod$FLAG_LEVEL)
table(cod$DIM_SEX_CODE)
table(cod$DIM_AGEGROUP_CODE)


cod_ <- cod %>% 
  mutate(sex = case_when(
    DIM_SEX_CODE=="TOTAL" ~ "Both sexes", 
    DIM_SEX_CODE=="FEMALE" ~ "Female", 
    DIM_SEX_CODE=="MALE" ~ "Male")) %>%
  mutate(country = iso3_to_names(DIM_COUNTRY_CODE),
         region = iso3_to_regions(DIM_COUNTRY_CODE)) %>% 
  mutate(region2 = case_when(
    region=="WPR" ~ "Western Pacific Region",
    region=="SEAR" ~ "South-East Asia Region",
    region=="EMR" ~ "Eastern Mediterranean Region",
    region=="EUR" ~ "European Region",
    region=="AMR" ~ "Region of the Americas",
    region=="AFR" ~ "African Region")) %>% 
  mutate(age = str_remove(DIM_AGEGROUP_CODE, "^Y")) %>% 
  mutate(age = str_extract(age, "^[:digit:]+")) %>% 
  mutate(age = ifelse(DIM_AGEGROUP_CODE=="YGE_85", "85", age)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age = ifelse(is.na(age) & DIM_AGEGROUP_CODE=="TOTAL", 101, age)) %>% 
  rename(FLAG_CAUSEGROUP = DIM_CAUSE_GROUP,
         VAL_DEATHS_COUNT_NUMERIC = VAL_DTHS_COUNT_NUMERIC,
         VAL_DEATHS_RATE100K_NUMERIC = VAL_DTHS_RATE100K_NUMERIC)

table(cod_$age, cod_$DIM_AGEGROUP_CODE, useNA = "always")
table(cod_$sex, cod_$DIM_SEX_CODE, useNA = "always")

cod19 <- cod_ %>% filter(DIM_YEAR_CODE==2021)
cod00 <- cod_ %>% filter(DIM_YEAR_CODE==2000)


## Create top 10 data  ---------------------------------------------------------

top5region <- cod19 %>% 
  filter(age==101) %>% 
  filter(FLAG_TREEMAP == 1) %>%
  group_by(region2, DIM_GHECAUSE_TITLE, sex) %>% 
  summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
  ungroup() %>% 
  arrange(region2, sex, desc(deaths)) %>% 
  rename(region = region2)

top5region_ <- top5region %>%
  group_by(region, sex) %>%
  arrange(region, sex, desc(deaths)) %>%
  slice(1:10) 

top5global <- cod19 %>%
  filter(age==101) %>% 
  filter(FLAG_TREEMAP == 1) %>%
  group_by(DIM_GHECAUSE_TITLE, sex) %>% 
  summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
  ungroup() %>% 
  mutate(region = "Global") %>%
  group_by(region, sex) %>%
  arrange(sex, desc(deaths)) %>%
  slice(1:10)

top5country <- cod19 %>%
  filter(age==101) %>% 
  filter(FLAG_TREEMAP == 1) %>%
  group_by(country, DIM_GHECAUSE_TITLE, sex) %>% 
  summarise(deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
  ungroup() %>% 
  rename(region = country)

top5country_ <- top5country %>%
  group_by(region, sex) %>%
  arrange(region, sex, desc(deaths)) %>%
  slice(1:10)

top10 <- rbind(top5global, top5region_, top5country_) 


# Data for SDG Benchmarking  ---------------------------------------------------

# With all indicators

data_ <- openxlsx::read.xlsx("input/2000data.xlsx") %>%
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


## Update LE and HALE    -------------------------------------------------------
lehale <- read.csv("input/LE_HALE_GHE2021.csv") %>% 
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
  mutate(sort_country = ifelse(
    country %in% c("Global", "African Region", "Eastern Mediterranean Region", 
                   "European Region", "Region of the Americas", 
                   "South-East Asia Region", "Western Pacific Region"), 
    case_when(country=="Global" ~ 1,
              country %in% c("African Region", "Eastern Mediterranean Region", 
                             "European Region", "Region of the Americas", 
                             "South-East Asia Region", "Western Pacific Region") ~ 2),
    3)
    )


# Triple Billion data --------------------------------------------------------- 
str_date <- format(Sys.time(), "%Y.%m.%d")
table_dat <- arrow::read_parquet("input/2024-03-26-10-06_summary.parquet", as_data_frame = FALSE) %>%
  filter(scenario == "baseline") %>%
  collect()

sdg_goals <- readxl::read_xlsx(path = "input/GPW13_3B_data_with_SDG_Acceleration_scenarios_indicator_update_18.11.xlsx") %>%
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

save(data, cod00, cod19, top10, filtered_indicator_values, ind_df, file = "datasets.rda")

