

gete0 <- function(data){
  dt <- as.data.table(data)
  id_cols <- c("age_start", "age_end", "sex", "year")
  
  # add ax, qx initial values
  dt <- hierarchyUtils::gen_length(dt, col_stem = "age")
  dt[, ax := mx_to_ax(mx = mx, age_length = age_length)]
  gen_u5_ax_from_mx(dt, id_cols = id_cols)
  dt[, qx := mx_ax_to_qx(mx = mx, ax = ax, age_length = age_length)]
  gen_lx_from_qx(dt, id_cols = id_cols)
  gen_dx_from_lx(dt, id_cols = id_cols) # dx required for iteration
  dt[, `:=` (age_length = NULL, lx = NULL)]
  
  # do iteration for graduation method on ax, then get whole lifetable
  iterate_ax(dt, id_cols = id_cols)
  gen_u5_ax_from_mx(dt, id_cols = id_cols)
  lifetable(dt, id_cols = id_cols, preserve_u5 = TRUE)
  
  # get e0
  e0 <- dt[age_start == 0]
  e0 <- e0[, list(sex, year, value = ex)]
  e0 <- e0 %>% as.data.frame() %>% mutate(name="Life expectancy at birth (years)") 
  return(e0)
}


maketreemapdata_levl1 <- function(regionle, sexcod){
  
  if(regionle=="Global"){
    
    data_treemap <- cod19 %>%
      filter(FLAG_LEVEL == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>% 
      ungroup() 
    
    data_treemap_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
      ungroup()
  
    
  }else if(
    regionle %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
  ){
    
    data_treemap_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 1 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
      ungroup()
    
    data_treemap <- cod19 %>%
      filter(FLAG_LEVEL == 1 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
      ungroup()

    
  }else{
    
    data_treemap <- cod19 %>%
      filter(country == regionle & FLAG_LEVEL == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL")
    
    data_treemap_2000 <- cod00 %>%
      filter(country == regionle & FLAG_LEVEL == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL")
    
  }
  
  list_treemap <- list(data_treemap, data_treemap_2000)
  
  return(list_treemap)
}

maketreemapdata_levl2 <- function(regionle, sexcod){
  
  if(regionle=="Global"){
    
    data_treemap_2000 <- cod00 %>%
      filter(FLAG_TREEMAP == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
      ungroup() %>% 
      mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
    
    data_treemap <- cod19 %>%
      filter(FLAG_TREEMAP == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>% 
      ungroup() %>% 
      mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
    
  } else if(regionle %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
    
    data_treemap_2000 <- cod00 %>%
      filter(FLAG_TREEMAP == 1 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
      ungroup() %>% 
      mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
    
    data_treemap <- cod19 %>%
      filter(FLAG_TREEMAP == 1 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>% 
      group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
      ungroup() %>% 
      mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
    
  }else{
    
    data_treemap <- cod19 %>%
      filter(country == regionle & FLAG_TREEMAP == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") 
    
    data_treemap_2000 <- cod00 %>%
      filter(country == regionle & FLAG_TREEMAP == 1 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL")
    
  }
  
  list_treemap <- list(data_treemap, data_treemap_2000)
  
  return(list_treemap)
}

maketreemapgraph_levl1 <- function(data_list, regionle, sexcod){
  
  data_treemap <- data_list[[1]]
  data_treemap_2000 <- data_list[[2]]

  if(regionle=="Global"){

    total_deaths <- cod19 %>%
      filter(FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  }else if(regionle %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") ){

    total_deaths <- cod19 %>%
      filter(FLAG_LEVEL == 0 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 0 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  }else{

    total_deaths <- cod19 %>%
      filter(country == regionle & FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(country == regionle & FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  }
  
  # 2019
  lev1 <- cod19 %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  
  data_treemap <- data_treemap %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
  
  data_treemap$lev1_name <- factor(data_treemap$lev1_name, levels = lev1_causes)
  
  # add cause fraction to label if over 5%
  data_treemap <- data_treemap %>%
    mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
    mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "))
  
  # make treemap
  tm_2019 <- treemap::treemap(data_treemap,
                              index=c("lev1_name","cause_title"),
                              vSize="VAL_DEATHS_COUNT_NUMERIC",
                              type="index",
                              palette = "Set1",
                              draw = FALSE)
  
  tm_plot_data <- tm_2019$tm %>% 
    mutate(x1 = x0 + w,
           y1 = y0 + h) %>% 
    mutate(x = (x0+x1)/2,
           y = (y0+y1)/2) %>% 
    mutate(primary_group = ifelse(is.na(cause_title), 1.2, .5)) %>% 
    mutate(color = case_when(
      lev1_name=="Communicable, maternal, perinatal and nutritional conditions" ~"#F26829",
      lev1_name=="Noncommunicable diseases" ~ "#009ADE",
      lev1_name=="Injuries" ~ "#80BC00",
      lev1_name=="Other COVID-19 pandemic-related outcomes" ~ "#A6228C"
    )) %>% 
    mutate(color = ifelse(is.na(cause_title), NA, color))
  
  tm1 <- ggplot(tm_plot_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
    geom_rect(aes(fill = color, size = primary_group),
              show.legend = FALSE, color = "white", alpha=0.5) +
    scale_fill_identity() +
    scale_size(range = range(tm_plot_data$primary_group)) +
    ggfittext::geom_fit_text(aes(label = cause_title), min.size = 4, color = "black", grow = TRUE, reflow = TRUE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void()+
    labs(title = paste0("Total deaths in 2021: ", format(total_deaths, big.mark = ",", scientific = F)))+
    theme(
      plot.margin = unit(c(0, 0, 0, 1.5), "cm"),
      plot.title = element_text(hjust = 0.5, size = 18)
    )
  
  
  # 2000 
  lev1 <- cod00  %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  data_treemap_2000 <- data_treemap_2000 %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
  
  data_treemap_2000$lev1_name <- factor(data_treemap_2000$lev1_name, levels = lev1_causes)
  
  # add cause fraction to label if over 5%
  data_treemap_2000 <- data_treemap_2000 %>%
    mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
    mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "))
  
  # make treemap
  tm_2000 <- treemap::treemap(data_treemap_2000,
                              index=c("lev1_name","cause_title"),
                              vSize="VAL_DEATHS_COUNT_NUMERIC",
                              type="index",
                              palette = "Set1",
                              draw = FALSE
  )
  
  tm_plot_data_2000 <- tm_2000$tm %>% 
    mutate(x1 = x0 + w,
           y1 = y0 + h) %>% 
    mutate(x = (x0+x1)/2,
           y = (y0+y1)/2) %>% 
    mutate(primary_group = ifelse(is.na(cause_title), 1.2, .5)) %>% 
    mutate(color = case_when(
      lev1_name=="Communicable, maternal, perinatal and nutritional conditions" ~"#F26829",
      lev1_name=="Noncommunicable diseases" ~ "#009ADE",
      lev1_name=="Injuries" ~ "#80BC00",
      lev1_name=="Other COVID-19 pandemic-related outcomes" ~ "#A6228C"
    )) %>% 
    mutate(color = ifelse(is.na(cause_title), NA, color))
  
  
  tm2 <- ggplot(tm_plot_data_2000, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
    geom_rect(aes(fill = color, size = primary_group),
              show.legend = FALSE, color = "white", alpha=0.5) +
    scale_fill_identity() +
    scale_size(range = range(tm_plot_data$primary_group)) +
    ggfittext::geom_fit_text(aes(label = cause_title), min.size = 4, color = "black", grow = TRUE, reflow = TRUE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void()+
    labs(title = paste0("Total deaths in 2000: ", format(total_deaths_2000, big.mark = ",", scientific = F)))+
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.position = "bottom"
    )
  
  final <- patchwork::wrap_plots(tm2, tm1, ncol = 2) 
  
  return(final)
}

maketreemapgraph_levl2 <- function(data_list, regionle, sexcod){
  
  if(regionle=="Global"){

    total_deaths <- cod19 %>%
      filter(FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  } else if(regionle %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") ){

    total_deaths <- cod19 %>%
      filter(FLAG_LEVEL == 0 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(FLAG_LEVEL == 0 & region2==regionle & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      group_by(DIM_YEAR_CODE) %>% 
      summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  }else{

    total_deaths <- cod19 %>%
      filter(country == regionle & FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
    total_deaths_2000 <- cod00 %>%
      filter(country == regionle & FLAG_LEVEL == 0 & sex==sexcod & DIM_AGEGROUP_CODE=="TOTAL") %>%
      pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
    
  }
  
  
  data_treemap <- data_list[[1]]
  data_treemap_2000 <- data_list[[2]]
  
  # 2019
  lev1 <- cod19 %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  data_treemap <- data_treemap %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
  
  data_treemap$lev1_name <- factor(data_treemap$lev1_name, levels = lev1_causes)
  
  # add cause fraction to label if over 5%
  data_treemap <- data_treemap %>%
    mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
    mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "))
  
  # make treemap
  tm_2019 <- treemap::treemap(data_treemap,
                              index=c("lev1_name","cause_title"),
                              vSize="VAL_DEATHS_COUNT_NUMERIC",
                              type="index",
                              palette = "Set1",
                              draw = FALSE)
  
  tm_plot_data <- tm_2019$tm %>% 
    mutate(x1 = x0 + w,
           y1 = y0 + h) %>% 
    mutate(x = (x0+x1)/2,
           y = (y0+y1)/2) %>% 
    mutate(primary_group = ifelse(is.na(cause_title), 1.2, .5)) %>% 
    mutate(color = case_when(
      lev1_name=="Communicable, maternal, perinatal and nutritional conditions" ~"#F26829",
      lev1_name=="Noncommunicable diseases" ~ "#009ADE",
      lev1_name=="Injuries" ~ "#80BC00",
      lev1_name=="Other COVID-19 pandemic-related outcomes" ~ "#A6228C"
    )) %>% 
    mutate(color = ifelse(is.na(cause_title), NA, color))
  
  tm1 <- ggplot(tm_plot_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
    geom_rect(aes(fill = color, size = primary_group),
              show.legend = FALSE, color = "white", alpha=0.5) +
    scale_fill_identity() +
    scale_size(range = range(tm_plot_data$primary_group)) +
    ggfittext::geom_fit_text(aes(label = cause_title), min.size = 4, color = "black", grow = TRUE, reflow = TRUE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void()+
    labs(title = paste0("Total deaths in 2021: ", format(total_deaths, big.mark = ",", scientific = F)))+   ######### CHANGE BACK TO 2019
    theme(
      plot.margin = unit(c(0, 0, 0, 1.5), "cm"),
      plot.title = element_text(hjust = 0.5, size = 18)
    )
  
  
  # 2000   
  lev1 <- cod00  %>%
    filter(FLAG_LEVEL == 1) %>%
    select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
    unique()
  data_treemap_2000 <- data_treemap_2000 %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
  
  data_treemap_2000$lev1_name <- factor(data_treemap_2000$lev1_name, levels = lev1_causes)
  
  # add cause fraction to label if over 5%
  data_treemap_2000 <- data_treemap_2000 %>%
    mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
    mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " "))
  
  # make treemap
  tm_2000 <- treemap::treemap(data_treemap_2000,
                              index=c("lev1_name","cause_title"),
                              vSize="VAL_DEATHS_COUNT_NUMERIC",
                              type="index",
                              palette = "Set1",
                              draw = FALSE
  )
  
  tm_plot_data_2000 <- tm_2000$tm %>% 
    mutate(x1 = x0 + w,
           y1 = y0 + h) %>% 
    mutate(x = (x0+x1)/2,
           y = (y0+y1)/2) %>% 
    mutate(primary_group = ifelse(is.na(cause_title), 1.2, .5)) %>% 
    mutate(color = case_when(
      lev1_name=="Communicable, maternal, perinatal and nutritional conditions" ~"#F26829",
      lev1_name=="Noncommunicable diseases" ~ "#009ADE",
      lev1_name=="Injuries" ~ "#80BC00",
      lev1_name=="Other COVID-19 pandemic-related outcomes" ~ "#A6228C"
    )) %>% 
    mutate(color = ifelse(is.na(cause_title), NA, color))
  
  
  tm2 <- ggplot(tm_plot_data_2000, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
    geom_rect(aes(fill = color, size = primary_group),
              show.legend = FALSE, color = "white", alpha=0.5) +
    scale_fill_identity() +
    scale_size(range = range(tm_plot_data$primary_group)) +
    ggfittext::geom_fit_text(aes(label = cause_title), min.size = 4, color = "black", grow = TRUE, reflow = TRUE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void()+
    labs(title = paste0("Total deaths in 2000: ", format(total_deaths_2000, big.mark = ",", scientific = F)))+
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.position = "bottom"
    )
  
  legt <- ggplot() +
    geom_tile(aes(x = 1, y = 1), fill = "#F26829", alpha=0.5)+
    geom_tile(aes(x = 2, y = 1), fill = "#009ADE", size = 1, alpha=0.5) +
    geom_tile(aes(x = 3, y = 1), fill = "#80BC00", size = 1, alpha=0.5) +
    geom_tile(aes(x = 4, y = 1), fill = "#A6228C", size = 1, alpha=0.5) +
    geom_text(aes(x = 1, y = 0.5, label = "Communicable, maternal, perinatal\nand nutritional conditions"), size=4.5, vjust = 1.1)+
    geom_text(aes(x = 2, y = 0.5, label = "Noncommunicable diseases"), size=4.5, vjust = 1.1)+
    geom_text(aes(x = 3, y = 0.5, label = "Injuries"), size=4.5, vjust = 1.1)+
    geom_text(aes(x = 4, y = 0.5, label = "Other COVID-19 pandemic-related outcomes"), size=4.5, vjust = 1.1)+
    theme_void()+
    scale_y_continuous(limits = c(-1,2.5))
  
  final <- ggdraw() +
    draw_plot(patchwork::wrap_plots(tm2, tm1, ncol = 2), 0, 0.1, 1, 0.85) +
    draw_plot(legt, 0.02, 0.045, 0.95, 0.055)
  
  return(final)
  
}


make_sdg_table <- function(filtered_data, level){
  
  indicator_values <- filtered_data %>% 
    filter(year %in% c(2018, 2030)) %>% 
    # filter(aggregate_id == level) %>%
    filter(country == level) %>%
    select(-all_of(c("lower", "upper"))) %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "mean_value_",
    ) %>%
    mutate(`percentage_change_2018-2030` = 100 * (mean_value_2030 - mean_value_2018) / mean_value_2018,
           within_10p_target = case_when(
             small_is_best ~ mean_value_2030 <= sdg_2030_goal + 0.1 * sdg_2030_goal,
             TRUE ~ mean_value_2030 >= sdg_2030_goal - 0.1 * sdg_2030_goal
           ),
           forecast_likely_exceed_target = case_when(
             small_is_best ~ mean_value_2030 <= sdg_2030_goal,
             TRUE ~  mean_value_2030 >= sdg_2030_goal
           ),
           ach = case_when(forecast_likely_exceed_target ~ "Likely Achieve by 2030",
                           within_10p_target ~ "Won't Achieve by 2030, but within 10% of target",
                           !forecast_likely_exceed_target ~ "Won't Achieve by 2030",
                           .default = ""),
           mean_value_2018 = round(mean_value_2018, 0),
           mean_value_2030 = round(mean_value_2030, 0),
           `percentage_change_2018-2030` = round(`percentage_change_2018-2030`, 0)) %>% 
    select(
      country,
      aggregation_level,
      aggregate_id,
      billion,
      indicator_name,
      gpw13_indicator_code,
      sdg_code,
      sdg_2030_goal,
      mean_value_2018,
      mean_value_2030,
      `percentage_change_2018-2030`,
      ach
    )
  
  global_dat <- indicator_values %>%
    # filter(aggregate_id == level) %>%
    filter(country == level) %>%
    select(-all_of(c("aggregation_level"))) %>%
    arrange(sdg_code)
  
  table_dat <- global_dat %>%
    arrange(desc(billion), sdg_code, indicator_name) %>% 
    mutate(
      billion = toupper(billion),
      sdg_2030_goal = as.character(sdg_2030_goal)
    ) %>%
    replace_na(list("sdg_code" = "-", "sdg_2030_goal" = "")) %>% 
    left_join(ind_df) %>% 
    arrange(order) %>% 
    select(
      billion,
      indicator_name,
      sdg_code,
      sdg_2030_goal,
      mean_value_2018,
      mean_value_2030,
      `percentage_change_2018-2030`,
      ach
    ) %>%
    rename(
      Billion = billion,
      Indicator = indicator_name,
      SDG = sdg_code,
      `2030 Target` = sdg_2030_goal,
      `2018` = mean_value_2018,
      `2030` = mean_value_2030,
      `% Change 2018-2030` = `percentage_change_2018-2030`
    ) %>% 
    mutate(Indicator = case_when(Indicator == "Child treatment" ~ "Child Health Care Seeking",
                                 Indicator == "Health security" ~ "Preparedness",
                                 .default = Indicator))
  
  return(table_dat)
}


make_donut_uhc <- function(filtered_data, level, year_pie){
  
  donut_data <- filtered_data %>% 
    filter(billion == "uhc") %>%
    filter(country == level) %>%
    filter(year %in% c(year_pie, 2030)) %>% 
    mutate(year_ = case_when(year==year_pie ~ "start_year", year==2030 ~ "goal_year")) %>% 
    select(aggregate_id, year_, indicator_name, gpw13_indicator_code, billion, value, sdg_2030_goal) %>% 
    pivot_wider(names_from = year_, values_from = value) %>% 
    mutate(start_year = round(start_year, 0),
           goal_year = round(goal_year, 0)) %>% 
    filter(gpw13_indicator_code != "uhc_sm" &
             gpw13_indicator_code != "asc") %>%
    mutate(indicator_name = case_when(indicator_name == "Child treatment" ~ "Child Health Care Seeking",
                                      indicator_name == "Health security" ~ "Preparedness",
                                      .default = indicator_name)) %>% 
    arrange(goal_year)
  
  ind <- donut_data$indicator_name
  
  donut_data$indicator_name <- factor(donut_data$indicator_name, levels = c(ind))
  
  donut_data$xmin <- 0:(nrow(donut_data)-1)
  donut_data$xmax <- donut_data$xmin + 1
  
  get_color_pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
  color_pal <- get_color_pal(nrow(donut_data))
  # Draw plot
  
  donut <- ggplot(data = donut_data) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = 0, ymax = goal_year,
                  fill=indicator_name), alpha=0.4) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = 0, ymax = start_year,
                  fill=indicator_name)) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = sdg_2030_goal, ymax = sdg_2030_goal, 
                  color = indicator_name), fill = "white", alpha=0, show.legend = FALSE) +
    
    geom_text(data = donut_data %>% filter(row_number()<=4),
              aes(label = round(start_year, digits = 0), x = xmin+0.5, y = start_year/2), size = 6) +
    geom_text(data = donut_data %>% filter(row_number()>4),
              aes(label = round(start_year, digits = 0), x = xmin+0.5, y = 30), size = 6) +
    # geom_text(aes(label = round(start_year, digits = 0), x = xmin+0.5, y = start_year/2), size = 2) +
    # geom_text(aes(label = round(goal_year, digits = 0), x = xmin+0.5, y = goal_year), size = 2.5, color = "blue") +
    
    geom_text(data = donut_data %>% filter(row_number()==nrow(donut_data)),
              aes(label = paste("2030 Target: ", sdg_2030_goal), 
                  x = xmin+0.5, y = sdg_2030_goal+10), 
              size = 6, fontface = "bold.italic", 
              hjust = 0.8
    ) +
    
    geom_text(data = donut_data %>% filter(row_number()!=nrow(donut_data)),
              aes(label =sdg_2030_goal,
                  x = xmin+0.5, y = sdg_2030_goal+6),
              size = 6, fontface = "bold.italic") +
    
    coord_polar() +
    theme_void()+
    scale_y_continuous(limits = c(-20, 110))+
    labs(caption = str_wrap("The lines along the perimeter are the 2030 targets for each indicator, with the values presented in bold italic font. 
                           The solid areas are the 2018 baseline values, these values are presented within the pie chart. 
                          The lightly shaded areas are the 2030 forecasts.", 100)
    )+
    theme(legend.position = "right",
          legend.text = element_text(size = 14),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.caption = element_text(size=16)
    ) +
    scale_fill_manual(values = color_pal) +
    scale_color_manual(values = color_pal) +
    labs(fill = "") +
    guides(fill = guide_legend(
      ncol = 1,
      byrow = FALSE,
      override.aes = list(size = 0.75)))
  
  print(donut)
}

plot_decomposition <- function(plt_dat,
                               plt_billion,
                               plt_net_ind,
                               # plt_aggregate_id,
                               # plt_aggregation_level,
                               plt_inds_color_pal,
                               plt_title,
                               label_years,
                               size_text) {
  # 
  # if (plt_aggregation_level == "iso3") {
  #   country_name <- whoville::iso3_to_names(plt_aggregate_id)
  #   country_region <- whoville::iso3_to_regions(plt_aggregate_id)
  #   plt_subtitle <- "{country_region} Region: {country_name}" %>%
  #     str_glue()
  # } else if (plt_aggregation_level == "region") {
  #   country_region <- plt_aggregate_id
  #   plt_subtitle <- "{country_region} Region" %>%
  #     str_glue()
  # } else if (plt_aggregation_level == "global") {
  #   plt_subtitle <- "Global" %>%
  #     str_glue()
  # }
  
  net_dat <- plt_dat %>%
    filter(ind == plt_net_ind) %>%
    mutate(
      point_label = case_when(
        year %in% label_years ~ contribution_mln %>% round(digits = 0),
        .default = NA_real_
      )
    ) %>%
    select(ind, year, contribution_mln, point_label)
  
  calc_label_points <- function(dat) {
    dat %>%
      mutate(
        ind_idx = as.integer(ind),
        ind_label = ind
      ) %>%
      arrange(-ind_idx) %>%
      mutate(
        y_upper_bar = cumsum(contribution_mln),
        y_lower_bar = c(0, y_upper_bar[-n()]),
        y_midpoint = (y_upper_bar + y_lower_bar) / 2,
      ) %>%
      select(ind, ind_idx, ind_label, year, contribution_mln, y_upper_bar, y_lower_bar, y_midpoint)
  }
  
  label_positive_points <- plt_dat %>%
    filter(year == 2030 & contribution_mln >= 0 & ind != plt_net_ind) %>%
    calc_label_points()
  label_negative_points <- plt_dat %>%
    filter(year == 2030 & contribution_mln < 0 & ind != plt_net_ind) %>%
    calc_label_points()
  label_points <- bind_rows(label_positive_points, label_negative_points) %>%
    mutate(
      pct_contribution = abs(contribution_mln) / sum(abs(contribution_mln)) * 100,
      ind_label = case_when(
        plt_billion == "hep" ~ ind,
        pct_contribution > 1 ~ ind,
        .default = NA
      )
    )
  label_ind_in_legend <- label_points %>%
    filter(is.na(ind_label)) %>%
    arrange(ind) %>%
    pull(ind)
  
  plt_dat <- plt_dat %>%
    filter(ind != plt_net_ind) %>%
    mutate(ind_label = if_else(year == 2030, ind, NA)) %>%
    select(ind, year, ind_label, contribution_mln)
  
  plt <- plt_dat %>%
    ggplot(aes(x = year, y = contribution_mln)) +
    geom_col(aes(fill = ind), position = "stack", na.rm = F) +
    geom_line(data = net_dat, aes(y = contribution_mln, color = "Net Contribution")) +
    geom_point(data = net_dat, aes(y = contribution_mln, color = "Net Contribution")) +
    ggrepel::geom_text_repel(
      data = label_points,
      mapping = aes(x = 2030.45, y = y_midpoint, label = ind_label, color = ind),
      # vjust aligns the segments vertically in the middle of each bar section
      # position = position_stack(vjust = 0.5),
      na.rm = T,
      # align all indicator labels at 20301 onwards and only shift vertically
      direction = "y",
      nudge_y = 1,
      xlim = c(2031, Inf),
      # still label indicators that are close to equal
      max.overlaps = 100,
      # effectively skip drawing line to label
      # min.segment.length = 100,
      show.legend = FALSE,
      size = size_text
    ) +
    ggrepel::geom_label_repel(
      data = net_dat,
      aes(y = contribution_mln, label = point_label, color = "Net Contribution"),
      show.legend = F, na.rm = T, size = size_text
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_fill_manual(
      values = plt_inds_color_pal,
      # only show unlabeled indicators in legend
      breaks = label_ind_in_legend
    ) +
    scale_color_manual(
      values = c("Net Contribution" = "grey20", plt_inds_color_pal),
      # Only show 'Net Contribution' in legend
      breaks = c("Net Contribution")
    ) +
    xlab("") +
    ylab("Contribution (millions)") +
    scale_x_continuous(breaks = 2018:2030) +
    scale_y_continuous(n.breaks = 10) +
    guides(
      color = guide_legend(order = 1),
      fill  = guide_legend(order = 2)
    ) +
    labs(
      # title = plt_title,
      # subtitle = plt_subtitle,
      # caption = "*Preliminary estimates subject to changes based on Member State consultation. Do not distribute.",
      fill = "Other indicators\nbelow 1% of the total\nabsolute contribution in 2030",
      color = ""
    ) +
    theme_classic() +
    # Allow labels to bleed past the canvas boundaries
    coord_cartesian(clip = 'off') +
    theme(
      axis.text.x=element_text(size=16),
      axis.title.x =element_text(size=16),
      axis.text.y=element_text(size=16),
      axis.title.y=element_text(size=16),
      plot.title = element_text(size = 16),
      # adjust margins to give more space for labels
      # Remember, the margins are t-r-b-l
      # see defaults with `theme_classic()$plot.margin`
      plot.margin = unit(c(5.5, 200, 5.5, 5.5), "pt"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box = "vertical",
      legend.box.background = element_rect(colour = "white")
    )
  
  return(list(plt = plt, plt_dat = plt_dat, net_dat = net_dat))
}

get_inds <- function(get_forecast_inds = TRUE, get_billion_inds = FALSE) {
  stopifnot(
    "Exactly one of 'get_forecast_inds' and 'get_billion_inds' can be TRUE" =
      xor(get_forecast_inds, get_billion_inds)
  )
  
  if (get_forecast_inds) {
    inds_possible <- list(
      uhc = billionaiRe::billion_ind_codes("uhc") %>%
        purrr::discard(~.x %in% c("hwf", "fh")),
      hpop = billionaiRe::billion_ind_codes("hpop"),
      hep = billionaiRe::billion_ind_codes("hep", include_subindicators = FALSE) %>%
        purrr::discard(~stringr::str_ends(.x, "_campaign")) %>%
        # don't need since we estimate 'detect_respond' directly
        purrr::discard(~.x %in% c("detect", "respond", "notify")) %>%
        purrr::discard(~.x %in% c("surviving_infants"))
    )
  }
  else if (get_billion_inds) {
    inds_possible <- list(
      uhc = billionaiRe::billion_ind_codes("uhc") %>%
        purrr::discard(~.x %in% c("doctors", "nurses")),
      hpop = billionaiRe::billion_ind_codes("hpop"),
      hep = billionaiRe::billion_ind_codes("hep", include_subindicators = FALSE) %>%
        # don't need since we estimate 'detect_respond' directly
        purrr::discard(~.x %in% c("detect", "respond", "notify")) %>%
        purrr::discard(~.x %in% c("surviving_infants"))
    )
  }
  
  return(inds_possible)
}