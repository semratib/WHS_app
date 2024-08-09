# lehegraph <- function(dataset, iso3, sexgph){
#   
#   exp <- dataset %>% 
#     filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>% 
#     filter(country==iso3) %>% 
#     filter(sex %in% sexgph) %>% 
#     select(country, year, sex, name, value) 
#   
#   exp2 <- exp %>% 
#     pivot_wider(names_from = name, values_from = value)
#   
#   max <- exp %>% 
#     select(value) %>% 
#     arrange(desc(value)) %>% 
#     filter(row_number()==1) %>% pull()
#   
#   min <- exp %>%
#     select(value) %>%
#     arrange(value) %>% 
#     filter(row_number()==1) %>% pull()
#   
#   ggplot()+
#     geom_ribbon(data = exp2, aes(x=year, ymax=`Life expectancy at birth (years)`, ymin=`Healthy life expectancy at birth (years)`), fill="grey", alpha=0.3) +
#     geom_line(data = exp, aes(x=year, y=value, color=name), size = 1.2) +
#     geom_point(data = exp %>% filter(year %in% c(2000, 2010, 2019)), 
#                aes(x=year, y=value, color=name), size = 3, show.legend=F) +
#     geom_text(data = exp %>% filter(year %in% c(2000, 2010, 2019) & name=="Life expectancy at birth (years)"), 
#               aes(x=year, y=value, label = round(value,0)), vjust=-1.5, size=4.5, color = "#009ADE", ) +
#     geom_text(data = exp %>% filter(year %in% c(2000, 2010, 2019) & name=="Healthy life expectancy at birth (years)"), 
#               aes(x=year, y=value, label = round(value,0)), vjust=2, size=4.5, color ="#80BC00") +
#     theme_classic()+
#     ylab("")+ xlab("")+
#     scale_y_continuous(limits = c(round(min, 0)-2,round(max, 0)+2)) +
#     scale_color_manual(values = c("Life expectancy at birth (years)" ="#009ADE",
#                                   "Healthy life expectancy at birth (years)" = "#80BC00")) +
#     facet_wrap(~sex) +
#     theme(axis.text=element_text(size=14),
#           legend.position = "bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size = 14),
#           strip.background = element_blank(),
#           strip.text = element_text(size=14))
# }

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
# data_to_hierarchical <- function(data, group_vars, size_var, colors = c("#F6A27C", "#79B5E3", "#BE73AD", "#AACF7F")) {
#   
#   dat <- data %>%
#     select({{ group_vars }}, {{ size_var }})
#   
#   ngvars <- ncol(dat) - 1
#   
#   names(dat) <- c(str_c("group_var_", seq(1, ngvars)), "value")
#   
#   gvars <- names(dat)[seq(1, ngvars)]
#   
#   # group to calculate the sum if there are duplicated combinations
#   dat <- dat %>%
#     group_by_at(all_of(gvars)) %>%
#     summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
#     ungroup() %>%
#     mutate_if(is.factor, as.character) %>%
#     arrange(desc(value))
#   
#   dat$group_var_1 <- factor(dat$group_var_1, levels = lev1_causes)
#   
#   dout <- map(seq_along(gvars), function(depth = 1) {
#     datg <- dat %>%
#       arrange(group_var_1) %>%
#       select(1:depth) %>%
#       distinct()
#     
#     datg_name <- datg %>%
#       select(depth) %>%
#       rename_all(~"name")
#     
#     datg_id <- datg %>%
#       unite("id", everything(), sep = "_") %>%
#       mutate(id = str_to_id_vec(id))
#     
#     datg_parent <- datg %>%
#       select(1:(depth - 1)) %>%
#       unite("parent", everything(), sep = "_") %>%
#       mutate(parent = str_to_id(parent))
#     
#     dd <- list(datg_name, datg_id)
#     
#     # depth != 1 add parents
#     if (depth != 1) dd <- dd %>% append(list(datg_parent))
#     
#     # depth == 1 add colors
#     if (depth == 1 & !is.null(colors)) {
#       dd <- dd %>% append(list(tibble(color = rep(colors, length.out = nrow(datg)))))
#     }
#     
#     # depth == lastdepth add value
#     if (depth == ngvars) dd <- dd %>% append(list(dat %>% select(value)))
#     
#     dd <- dd %>%
#       bind_cols() %>%
#       mutate(level = depth)
#     
#     dd
#     
#     list_parse(dd)
#   })
#   
#   dout <- reduce(dout, c)
#   
#   dout
# }
# 
# 
# str_to_id_vec <- function(x) {
#   
#   # x <- c("A_ aa", "A_  Aa", "a_   aa"
#   
#   tibble(
#     var = x,
#     id = str_to_id(x),
#     un = cumsum(duplicated(id))
#   ) %>%
#     mutate(
#       un = ifelse(un == 0, "", str_c("_", un)),
#       id2 = str_c(id, un)
#     ) %>%
#     pull("id2")
# }