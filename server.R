#################################################################################################################
## server
#################################################################################################################

server = function(input, output, session) { 

  
  #################################################################################################################
  #### Figure sentences ####
  #################################################################################################################
  
  # Country profile
  output$lehetext <- renderText({year <- 2021; paste0("Life expectancy at birth and healthy life expectancy, ", input$iso3le, ", 2000-", year, ".")})
  output$lecodtext <- renderText({year <- 2021; paste0("Life expectancy in ", year, " as a result of changes to the top 10 causes of death in  ", year, ", ", input$iso3le, ".")})
  output$ranktext <- renderText({year <- 2021; paste0("Change in rank for level 2 causes of death between 2000 and ", year, ", ", input$iso3le, ".")})
  output$treemaptext <- renderText({year <- 2021; paste0("All-age cause of deaths in 2000 and ", year, ", ", input$iso3le, ".")})
  output$sdgtext <- renderText({paste0("Triple Billions indicator progress in ", input$iso3le, ", 2018-2030.")})
  output$ledecomptext <- renderText({paste0("Attribution of changes in life expectancy at birth (2000 to 2021) to changes in the top causes of death in 2021, ", input$iso3le, ".")})
  output$ledecomptext_time <- renderText({paste0("Decomposition of the change in life expectancy at birth by level 1 causes of death, ", input$iso3le, ".")})
  output$TB_contrib_text <- renderText({paste0(input$TB, " indicator contributions to the Triple Billions, ", input$iso3le, ".")})
  output$uhctext <- renderText({paste0("UHC tracer indicator progress in ",  input$iso3le, ", 2018-2030.")})
  
  # Benchmarking
  output$heatmapv2text <- renderText({paste0("Compare the countries with the highest burden of ", input$codheatv2, " in 2000 and 2021.")})
  output$sdgv2text <- renderText({paste0("Countries with lowest performance in ", input$sdgind, " in 2018 and 2021.")})
  
  # MMR
  output$trajtext <- renderText({paste0("Potential trajectories of MMR in ",  input$iso3cp, ", 2000-2030.")})
  output$rmnchtext <- renderText({paste0("Sub-national estimates of RMNCH service coverage from most recent household survey in ", input$iso3cp, ".")})
  
  #################################################################################################################
  ####  Country Profile ####
  #################################################################################################################
  
  # Download links
  selected_data <- reactive({
    if(input$pages == "Life expectancy and Healthy life expectancy time-series"){
      select_tbl <- ledata()
    }else if(input$pages == "Life expectancy calculator"){
      select_tbl <- lecalc_data()
    }else if(input$pages == "Life expectancy decomposition by cause of death"){
      select_tbl <- decomp_data() %>% select(sex, cause = name, contribution_to_change_in_life_expectancy_in_years = value)
    }else if(input$pages == "Life expectancy decomposition over time"){
      select_tbl <- decomp_data_time() %>% select(sex, year, cause = name, contribution_to_change_in_life_expectancy_in_years = value)
    }else if(input$pages == "Change in rank of cause of death"){
      select_tbl <- codrank_data()
    }else if(input$pages == "Change in distribution of cause of death"){
      select_tbl <- data_tree_csv()
    }else if(input$pages == "Triple Billions Indicator Progress"){
      select_tbl <- datatablesdg()
    }else if(input$pages == "Triple Billions Indicator Contributions"){
      select_tbl <- TBcontrib_data()$plt_dat
    }
    return(select_tbl)
  })
  
  selected_image <- reactive({
    if(input$pages == "Life expectancy and Healthy life expectancy time-series"){
      select_tbl <- legraph_pdf()
    }else if(input$pages == "Life expectancy calculator"){
      select_tbl <- lecalc_barline_pdf()
    }else if(input$pages == "Life expectancy decomposition by cause of death"){
      select_tbl <- decomp_graph_pdf()
    }else if(input$pages == "Life expectancy decomposition over time"){
      select_tbl <- decomp_time_graph()
    }else if(input$pages == "Change in rank of cause of death"){
      select_tbl <- codrank_graph_pdf()
    }else if(input$pages == "Change in distribution of cause of death"){
      select_tbl <- tree_graph_pdf()
    }else if(input$pages == "Triple Billions Indicator Progress"){
      select_tbl <- tablesdg()
    }else if(input$pages == "Triple Billions Indicator Contributions"){
      select_tbl <- TBcontrib_data()$plt + labs(title = paste0(input$TB, " indicator contributions to the Triple Billions, ", input$iso3le, ", 2018-2030."))
    }
    return(select_tbl)
  })

  output$data_all <- downloadHandler(
    filename = function() {
      paste0("data - ", input$pages, " - ", input$iso3le, ".csv")
      # paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_data(), file, row.names = FALSE)
    }
  )
  
  output$jpeg_all <- downloadHandler(
    filename = function() {
      if(input$pages == "Triple Billions Indicator Progress"){
        paste("triplebillion", ".png", sep = "")
      }else{
        # paste("graph", ".jpeg", sep = "")
        paste0("graph - ", input$pages, " - ", input$iso3le, ".jpeg")
      }
    },
    content = function(file) {
      if(input$pages == "Triple Billions Indicator Progress"){
        save_as_image(selected_image(), path = file, webshot = "webshot")
      }else{
        ggsave(selected_image(), filename = file, width = 16, height = 8, units = "in", dpi = 100)
      }
    }
  )
  
  output$pdf_all <- downloadHandler(
    filename = function() {
      # paste("graph", ".pdf", sep = "")
      paste0("graph - ", input$pages, " - ", input$iso3le, ".pdf")
      
    },
    content = function(file) {
      ggsave(selected_image(), filename = file, width = 16, height = 8, units = "in")
    }
  )
  
  # observe({
  #   if(input$pages == "Generate country report") {
  #     shinyjs::disable("data_all")
  #   }
  # })
  
  observe({
    shinyjs::toggleState("data_all", condition = (input$pages!= "Generate country report"))
  })
  
  observe({
    shinyjs::toggleState("jpeg_all", condition = (input$pages!= "Generate country report"))
  })
  
  observe({
    shinyjs::toggleState("pdf_all", condition = (input$pages!= "Generate country report"))
  })
  
  # Life and health life expectancy
  ledata <- reactive({
   data %>% 
      filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>% 
      filter(year %in% c(2000, 2010, 2019, 2020, 2021)) %>%
      filter(country==input$iso3le) %>% 
      filter(sex %in% input$sex) %>% 
      select(country, year, sex, name, value) %>% 
      mutate(value = round(value, 1))
    
  })
  
  legraph <- reactive({
    
    exp <- ledata()
    exp2 <- exp %>%
      pivot_wider(names_from = name, values_from = value)
    
    max <- exp %>% 
      select(value) %>% 
      arrange(desc(value)) %>% 
      filter(row_number()==1) %>% pull()
    
    min <- exp %>%
      select(value) %>%
      arrange(value) %>% 
      filter(row_number()==1) %>% pull()
    
    # map(unique(exp$sex), function(x){
    #     highchart() %>% 
        # hchart(., "line", hcaes(x = year, y = value, color = name, group = name),
        #        dataLabels = list(
        #                        enabled = TRUE,
        #                        format = "{point.value}",
        #                        style = list(
        #                          textShadow=F,
        #                          fontSize = "15px"
        #                        )
        #                      )) %>%
      #   hc_add_series(data = exp2 %>% filter(sex == x), 'arearange', 
      #                 hcaes(x = year, 
      #                       low = `Healthy life expectancy at birth (years)`, 
      #                       high = `Life expectancy at birth (years)`), color = "#e6e7e8", name = "Difference in life and healthy life expectancy") %>%
      #   hc_add_series(data = exp %>% filter(sex == x), 
      #                 'line', hcaes(x = year, y = value, color = name, group = name),
      #               dataLabels = list(
      #                 enabled = TRUE,
      #                 format = "{point.value}",
      #                 style = list(
      #                   textShadow=F,
      #                   fontSize = "16px"
      #                 )
      #               )) %>%
      # hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2019, 2020, 2021), labels = list(rotation=-45)) %>%
      # hc_yAxis(title = list(text= ""), min = min-5, max = max+2)  %>%
      # hc_plotOptions(enableMouseTracking = T)
      # }) %>% 
      # hw_grid() 
    # %>% 
    #   htmltools::browsable()
    
    
   graph <- ggplot()+
      geom_ribbon(data = exp2, aes(x=year, ymax=`Life expectancy at birth (years)`, ymin=`Healthy life expectancy at birth (years)`), fill="grey", alpha=0.3) +
      geom_line(data = exp, aes(x=year, y=value, color=name), size = 1.2) +
      geom_point(data = exp, aes(x=year, y=value, color=name), size = 3, show.legend=F) +
      geom_text(data = exp %>% filter(name=="Life expectancy at birth (years)" & year!= 2020),
                aes(x=year, y=value, label = round(value,1)), vjust=-1.5, size=4.5, color = "#009ADE", ) +
      geom_text(data = exp %>% filter(name=="Healthy life expectancy at birth (years)" & year!= 2020),
                aes(x=year, y=value, label = round(value,1)), vjust=2, size=4.5, color ="#80BC00") +
      theme_classic()+
      ylab("")+ xlab("")+ labs(caption = "Grey shaded area represents the difference between life expectancy and healthy life expectancy.")+
      scale_y_continuous(limits = c(round(min, 0)-2,round(max, 0)+2)) +
      scale_x_continuous(breaks = c(2000, 2010, 2019, 2020, 2021)) +
      # scale_x_continuous(breaks = c(2000, 2010, 2019)) +
      scale_color_manual(values = c("Life expectancy at birth (years)" ="#009ADE",
                                    "Healthy life expectancy at birth (years)" = "#80BC00")) +
      facet_wrap(~sex) +
      theme(axis.text=element_text(size=14),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
            plot.caption = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            strip.background = element_blank(),
            strip.text = element_text(size=14))
   
   graph

  }) 
  
  legraph_pdf <- reactive({
    legraph()+
      labs(subtitle = paste0("Life expectancy and healthy life expectancy, ", input$iso3le, ", 2000-2021."))+
      theme(plot.subtitle = element_text(size = 15))
  })
  
  output$lehe <- renderPlot({ legraph() })
  

  

  # Top 5 causes life expectancy
  codiso3data <- reactive({
    top10 %>% 
      filter(region == input$iso3le & sex == input$sexle) %>%
      arrange(desc(deaths)) %>%
      filter(row_number()<11)
  })
  
  output$cod1 <- renderText({
    codiso3data() %>% filter(row_number()==1) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
    }) 
  output$cod2 <- renderText({
    codiso3data() %>% filter(row_number()==2) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
    }) 
  output$cod3 <- renderText({
    codiso3data() %>% filter(row_number()==3) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
    })
  output$cod4 <- renderText({
    codiso3data() %>% filter(row_number()==4) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
    })
  output$cod5 <- renderText({
    codiso3data() %>% filter(row_number()==5) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
    })
  output$cod6 <- renderText({
    codiso3data() %>% filter(row_number()==6) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
  }) 
  output$cod7 <- renderText({
    codiso3data() %>% filter(row_number()==7) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
  }) 
  output$cod8 <- renderText({
    codiso3data() %>% filter(row_number()==8) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
  })
  output$cod9 <- renderText({
    codiso3data() %>% filter(row_number()==9) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
  })
  output$cod10 <- renderText({
    codiso3data() %>% filter(row_number()==10) %>% select(DIM_GHECAUSE_TITLE) %>% pull()
  })
  
  lecalc_bar_data <- reactive({
    test_ <- codiso3data()
    
    cod1_ <- test_$DIM_GHECAUSE_TITLE[1]
    cod2_ <- test_$DIM_GHECAUSE_TITLE[2]
    cod3_ <- test_$DIM_GHECAUSE_TITLE[3]
    cod4_ <- test_$DIM_GHECAUSE_TITLE[4]
    cod5_ <- test_$DIM_GHECAUSE_TITLE[5]
    cod6_ <- test_$DIM_GHECAUSE_TITLE[6]
    cod7_ <- test_$DIM_GHECAUSE_TITLE[7]
    cod8_ <- test_$DIM_GHECAUSE_TITLE[8]
    cod9_ <- test_$DIM_GHECAUSE_TITLE[9]
    cod10_ <- test_$DIM_GHECAUSE_TITLE[10]
    
    test_ %>% mutate(new_deaths = case_when(
      DIM_GHECAUSE_TITLE==cod1_ ~ deaths*(input$scalar1/100),
      DIM_GHECAUSE_TITLE==cod2_ ~ deaths*(input$scalar2/100),
      DIM_GHECAUSE_TITLE==cod3_ ~ deaths*(input$scalar3/100),
      DIM_GHECAUSE_TITLE==cod4_ ~ deaths*(input$scalar4/100),
      DIM_GHECAUSE_TITLE==cod5_ ~ deaths*(input$scalar5/100),
      DIM_GHECAUSE_TITLE==cod6_ ~ deaths*(input$scalar6/100),
      DIM_GHECAUSE_TITLE==cod7_ ~ deaths*(input$scalar7/100),
      DIM_GHECAUSE_TITLE==cod8_ ~ deaths*(input$scalar8/100),
      DIM_GHECAUSE_TITLE==cod9_ ~ deaths*(input$scalar9/100),
      DIM_GHECAUSE_TITLE==cod10_ ~ deaths*(input$scalar10/100),
      .default = deaths))
      
  })
  
  output$barcod <- renderPlot({
    
    lecalc_bar_data() %>% ggplot() +
      geom_col(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths, fill=group), width = 0.5, size = 1, alpha=0.5)+ 
      geom_text(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths/2, label = format(round(new_deaths, digits = 0), big.mark=",")), color = "black", size = 5)+
      theme_void()+
      scale_fill_manual(values = c("Communicable, maternal, perinatal and nutritional conditions" = "#F26829", 
                                   "Noncommunicable diseases" = "#009ADE", 
                                   "Injuries" = "#80BC00",
                                   "Other COVID-19 pandemic-related outcomes" = "#A6228C"))+
      theme(
        plot.margin = unit(c(0,1,0,0), "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.margin = margin(t=5),
        axis.title.y = element_blank(), axis.line.y = element_blank(),
        axis.title.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
      )+
      guides(color=guide_legend(nrow=4, byrow=TRUE),
             fill=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")),
             lty="none") +
      coord_flip()
    
    })
  
  lecalc_data <- reactive({
    
    if(input$scalar1==100 &
       input$scalar2==100 &
       input$scalar3==100 &
       input$scalar4==100 &
       input$scalar5==100 &
       input$scalar6==100 &
       input$scalar7==100 &
       input$scalar8==100 &
       input$scalar9==100 &
       input$scalar10==100){
      
      exp <- data %>%
        filter(GHOcode=="WHOSIS_000001" & year %in% c(2000, 2005, 2010, 2019, 2020, 2021)) %>%
        filter(country==input$iso3le & sex==input$sexle) %>%
        select(year, name, sex, value) %>% 
        mutate(value = round(value, digits = 1))
      
    }else{
      
      yearle <- 2021
      
      codtest <- codiso3data()
      
      cod1_ <- codtest$DIM_GHECAUSE_TITLE[1]
      cod2_ <- codtest$DIM_GHECAUSE_TITLE[2]
      cod3_ <- codtest$DIM_GHECAUSE_TITLE[3]
      cod4_ <- codtest$DIM_GHECAUSE_TITLE[4]
      cod5_ <- codtest$DIM_GHECAUSE_TITLE[5]
      cod6_ <- codtest$DIM_GHECAUSE_TITLE[6]
      cod7_ <- codtest$DIM_GHECAUSE_TITLE[7]
      cod8_ <- codtest$DIM_GHECAUSE_TITLE[8]
      cod9_ <- codtest$DIM_GHECAUSE_TITLE[9]
      cod10_ <- codtest$DIM_GHECAUSE_TITLE[10]
      
      if(input$iso3le=="Global"){
        
        code0 <- cod19 %>% 
          filter(FLAG_TREEMAP==1 & sex==input$sexle & DIM_AGEGROUP_CODE!="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          ungroup() %>% 
          mutate(new_deaths = case_when(
            DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar1/100),
            DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar2/100),
            DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar3/100),
            DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar4/100),
            DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar5/100),
            DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar6/100),
            DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar7/100),
            DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar8/100),
            DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar9/100),
            DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar10/100),
            .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
          group_by(age, sex) %>% 
          summarise(new_deaths = sum(new_deaths)) %>% 
          ungroup() %>% 
          left_join(
            cod19 %>% 
              filter(DIM_GHECAUSE_CODE==0 & sex==input$sexle & DIM_AGEGROUP_CODE!="TOTAL") %>% 
              group_by(age, sex) %>% 
              summarise(ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
              ungroup()
          ) %>% 
          mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
          mutate(age_start = age,
                 age_end = ifelse(age<5,
                                  case_when(age==0 ~ 1,
                                            age==1 ~ 5),
                                  age+5),
                 age_end = ifelse(age_start==85, Inf, age_end)) %>%
          mutate(year=yearle, 
                 sex=case_when(sex=="Both sexes" ~ "both",
                               sex=="Female" ~ "female",
                               sex=="Male" ~ "male")) %>% 
          arrange(age_start) %>% 
          select(age_start, age_end, sex, year, mx) 
        
      } else if (
        input$iso3le=="African Region" |
        input$iso3le=="Eastern Mediterranean Region" | 
        input$iso3le=="European Region" | 
        input$iso3le=="Region of the Americas" |
        input$iso3le=="South-East Asia Region" |
        input$iso3le=="Western Pacific Region"
      ) {
        
        code0 <- cod19 %>% 
          filter(FLAG_TREEMAP==1 & sex==input$sexle & region2==input$iso3le & DIM_AGEGROUP_CODE!="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          ungroup() %>% 
          mutate(new_deaths = case_when(
            DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar1/100),
            DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar2/100),
            DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar3/100),
            DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar4/100),
            DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar5/100),
            DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar6/100),
            DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar7/100),
            DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar8/100),
            DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar9/100),
            DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar10/100),
            .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
          group_by(age, sex) %>% 
          summarise(new_deaths = sum(new_deaths)) %>% 
          ungroup() %>% 
          left_join(
            cod19 %>% 
              filter(DIM_GHECAUSE_CODE==0 & sex==input$sexle & region2==input$iso3le & DIM_AGEGROUP_CODE!="TOTAL") %>% 
              group_by(age, sex) %>% 
              summarise(ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
              ungroup()
          ) %>% 
          mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
          mutate(age_start = age,
                 age_end = ifelse(age<5,
                                  case_when(age==0 ~ 1,
                                            age==1 ~ 5),
                                  age+5),
                 age_end = ifelse(age_start==85, Inf, age_end)) %>%
          mutate(year=yearle, 
                 sex=case_when(sex=="Both sexes" ~ "both",
                               sex=="Female" ~ "female",
                               sex=="Male" ~ "male")) %>%
          arrange(age_start) %>% 
          select(age_start, age_end, sex, year, mx) 
        
        
      } else {
        
        test <- cod19 %>% filter(country==input$iso3le & sex==input$sexle & DIM_AGEGROUP_CODE!="TOTAL")  
        
        code0 <- test %>%
          filter(FLAG_TREEMAP==1) %>% 
          mutate(new_deaths = case_when(
            DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar1/100),
            DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar2/100),
            DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar3/100),
            DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar4/100),
            DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar5/100),
            DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar6/100),
            DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar7/100),
            DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar8/100),
            DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar9/100),
            DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*(input$scalar10/100),
            .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
          group_by(DIM_COUNTRY_CODE, age, sex) %>% 
          summarise(new_deaths = sum(new_deaths)) %>% 
          ungroup() %>% 
          left_join(
            test %>% 
              filter(DIM_GHECAUSE_CODE==0) %>% 
              select(DIM_COUNTRY_CODE, age, ATTR_POPULATION_NUMERIC)
          ) %>%
          mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
          mutate(age_start = age,
                 age_end = ifelse(age<5,
                                  case_when(age==0 ~ 1,
                                            age==1 ~ 5),
                                  age+5),
                 age_end = ifelse(age_start==85, Inf, age_end)) %>% 
          mutate(year=yearle,
                 sex=case_when(sex=="Both sexes" ~ "both",
                               sex=="Female" ~ "female",
                               sex=="Male" ~ "male")) %>% 
          arrange(age_start) %>% 
          select(age_start, age_end, sex, year, mx)
        
      }
      
      e0 <- gete0(code0) %>% mutate(type = "Potential")

      exp <- data %>% 
        filter(GHOcode=="WHOSIS_000001" & year %in% c(2000, 2005, 2010, 2019, 2020, 2021)) %>%
        filter(country==input$iso3le & sex==input$sexle) %>% 
        mutate(type = "Current",
               sex=case_when(sex=="Both sexes" ~ "both",
                             sex=="Female" ~ "female",
                             sex=="Male" ~ "male")) %>% 
        select(year, sex, name, value, type)
      
      exp <- exp %>% 
        rbind(e0) %>%
        rbind(exp %>% filter(year==2020) %>% mutate(type = "Potential")) %>%
        mutate(value = round(value, digits = 1)) %>% 
        mutate(noTooltip = ifelse(year==2020 & type=="Potential", T, F))
      
    }

    return(exp)
  })
  
  output$codle <- renderHighchart({ 
    
     if(input$scalar1==100 &
        input$scalar2==100 &
        input$scalar3==100 &
        input$scalar4==100 &
        input$scalar5==100 &
        input$scalar6==100 &
        input$scalar7==100 &
        input$scalar8==100 &
        input$scalar9==100 &
        input$scalar10==100){
    
       exp <- lecalc_data()
       
       max <- exp %>%
         select(value) %>%
        arrange(desc(value)) %>%
         filter(row_number()==1) %>% pull()
    
       min <- exp %>%
         select(value) %>%
         arrange(value) %>%
         filter(row_number()==1) %>% pull()
       
       exp %>%
         hchart('line', hcaes(x = year, y = value), color = "#009ADE", name = "Current", 
                dataLabels = list(
                  enabled = TRUE,
                  format = "{point.value}",
                  style = list(
                    textShadow=F,
                    fontSize = "16px"
                  )
                  )
                ) %>% 
         hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2019, 2020, 2021), labels = list(rotation=-45)) %>%
         hc_yAxis(title = list(text= ""), min = min-5, max = max+2)  %>%
         hc_tooltip(crosshairs = F, shared = F,
                    formatter= JS(
                      paste0('function() {
                      return this.series.name + "</b><br/>LE: <b>" + this.point.y + "<br/>Year: <b>"+this.point.x
                             }'
                             )
                      )
                    )

    }else{
      
      exp_ <- lecalc_data()
      
      max <- exp_ %>% 
        select(value) %>% 
        arrange(desc(value)) %>% 
        filter(row_number()==1) %>% pull()
      min <- exp_ %>%
        select(value) %>%
        arrange(value) %>% 
        filter(row_number()==1) %>% pull()
      
      highchart() %>%
        hc_add_series(data = exp_ %>% filter(type == "Potential"), 'line', name = "Potential", hcaes(x = year, y = value), color = "#00205C", 
                      dataLabels = list(
                        enabled = TRUE,
                        format = "{point.value}",
                        style = list(
                          textShadow=F,
                          fontSize = "16px"
                        )
                      )) %>%
        hc_add_series(data = exp_ %>% filter(type == "Current"), 'line', name = "Current", hcaes(x = year, y = value), color = "#009ADE",
                      dataLabels = list(
                        enabled = TRUE,
                        format = "{point.value}",
                        style = list(
                          textShadow=F,
                          fontSize = "16px"
                        )
                      )) %>% #, animation = FALSE
        hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2019, 2020, 2021), labels = list(rotation=-45)) %>%
        # hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2019), labels = list(rotation=-45)) %>% 
        hc_yAxis(title = list(text= ""), min = min-5, max = max+2)  %>%
        hc_tooltip(crosshairs = F, shared = F,
                   formatter= JS(
                     paste0('function() {
                       if(!this.point.noTooltip) {
                         return this.series.name + "</b><br/>LE: <b>" + this.point.y + "<br/>Year: <b>"+this.point.x}
                       return false;
                       }'
                     )
                   )
        )%>%
        hc_plotOptions(enableMouseTracking = T)
      
      }
    
  })
  
  lecalc_barline_pdf <- reactive({
    
    barcod <- lecalc_bar_data() %>% 
      ggplot() +
      geom_col(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=deaths), width = 0.5, size = 1, alpha=0.5, fill = "grey")+ 
      geom_col(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths, fill=group), width = 0.5, size = 1, alpha=0.5)+ 
      geom_text(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths/2, label = format(round(new_deaths, digits = 0), big.mark=",")), color = "black", size = 3)+
      theme_void()+
      scale_fill_manual(values = c("Communicable, maternal, perinatal and nutritional conditions" = "#F26829", 
                                   "Noncommunicable diseases" = "#009ADE", 
                                   "Injuries" = "#80BC00",
                                   "Other COVID-19 pandemic-related outcomes" = "#A6228C"))+
      labs(subtitle = "Number of deaths, 2021") +
      theme(
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.margin = margin(t=5),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
      )+
      guides(color=guide_legend(nrow=4, byrow=TRUE),
             fill=guide_legend(nrow=4, byrow=TRUE, override.aes = aes(label = "")),
             lty="none") +
      coord_flip()
      
    max <- lecalc_data() %>%
      select(value) %>%
      arrange(desc(value)) %>%
      filter(row_number()==1) %>% pull()
    
    min <- lecalc_data() %>%
      select(value) %>%
      arrange(value) %>%
      filter(row_number()==1) %>% pull()
    
    if(input$scalar1==100 &
       input$scalar2==100 &
       input$scalar3==100 &
       input$scalar4==100 &
       input$scalar5==100 &
       input$scalar6==100 &
       input$scalar7==100 &
       input$scalar8==100 &
       input$scalar9==100 &
       input$scalar10==100){
      
      legraph <- lecalc_data() %>% 
        ggplot() +
        geom_line(aes(x=year, y=value), size = 1.2, color = "#009ADE") +
        geom_point(aes(x=year, y=value), size = 3, color = "#009ADE", show.legend=F) +
        geom_text(aes(x=year, y=value, label = round(value,1)), vjust=-2, size=4) +
        labs(subtitle = "Life expectancy at birth (years)") +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 9)
        )+
        scale_y_continuous(limits = c(min-5, max+3))
    
    }else{
      
      legraph <- lecalc_data() %>% 
        ggplot() +
        geom_line(aes(x=year, y=value, color = type), size = 1.2) +
        geom_point(aes(x=year, y=value, color = type), size = 3) +
        geom_text(aes(x=year, y=value, label = round(value,1)), vjust=-1, size=4) +
        labs(subtitle = "Life expectancy at birth (years)") +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 9),
          legend.position = "bottom",
          legend.title = element_blank()
        )+
        scale_y_continuous(limits = c(min-5, max+3))+
        scale_color_manual(values = c("Current" = "#009ADE", "Potential" = "#00205C"))
      
    }
   
    patchwork::wrap_plots(barcod + 
                            labs(caption = "Note: Grey bars represent the original number of deaths.") +
                            theme(plot.caption = element_text(size = 9)), 
                          legraph, 
                          ncol = 2) + 
      plot_annotation(subtitle = paste0("Life expectancy in 2021 as a result of changes to the top 10 causes of death in 2021: ", input$iso3le, ", ", input$sexle, "."))

  })
  
  
  
  # Life expectancy Decomposition
  
  decomp_data <- reactive({
    
    if(input$iso3le=="Global"){
      
      test21 <- cod19 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup() 
      
      test00 <- cod00 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup()
      
    } else if (
      input$iso3le=="African Region" |
      input$iso3le=="Eastern Mediterranean Region" | 
      input$iso3le=="European Region" | 
      input$iso3le=="Region of the Americas" |
      input$iso3le=="South-East Asia Region" |
      input$iso3le=="Western Pacific Region"
    ) {
      
      test21 <- cod19 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        filter(region2==input$iso3le) %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup()
      
      test00 <- cod00 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        filter(region2==input$iso3le) %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup() 
      
    } else {
      
      test21 <- cod19 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        filter(country==input$iso3le)
      
      test00 <- cod00 %>% 
        filter(FLAG_TREEMAP==1) %>%
        filter(DIM_AGEGROUP_CODE!="TOTAL") %>% 
        filter(country==input$iso3le)
      
    }
    
    top <- top10 %>% 
      filter(region==input$iso3le) %>% 
      ungroup() %>% 
      select(DIM_GHECAUSE_TITLE, country = region, sex) %>% 
      mutate(flag = 1) 
    
    top_levels <- c(unique(top$DIM_GHECAUSE_TITLE), "Other")
    
    test21__ <- test21 %>% 
      left_join(top) %>% 
      mutate(cause = ifelse(flag==1, DIM_GHECAUSE_TITLE, "Other")) %>% 
      mutate(cause = ifelse(is.na(flag), "Other", cause)) %>%
      group_by(age, cause, sex) %>% 
      summarise(new_deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      ungroup() %>% 
      left_join(test21 %>% distinct(age, sex, ATTR_POPULATION_NUMERIC)) %>% 
      mutate(rate = new_deaths/ATTR_POPULATION_NUMERIC) %>% 
      select(age, sex, cause, rate) 
    
    test00__ <- test00 %>% 
      left_join(top) %>% 
      mutate(cause = ifelse(flag==1, DIM_GHECAUSE_TITLE, "Other")) %>% 
      mutate(cause = ifelse(is.na(flag), "Other", cause)) %>%
      group_by(age, cause, sex) %>% 
      summarise(new_deaths = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
      ungroup() %>% 
      left_join(test00 %>% distinct(age, sex, ATTR_POPULATION_NUMERIC)) %>% 
      mutate(rate = new_deaths/ATTR_POPULATION_NUMERIC) %>% 
      select(age, sex, cause, rate) 
    
    out <- data.frame(sex = character(),
                      name = character(), 
                      value = numeric(),
                      age = numeric(),
                      age_end = numeric())
    
    for(i in c("Both sexes", "Female", "Male")){
      
      test21_ <- test21__ %>% 
        filter(sex==i) %>% 
        pivot_wider(names_from = cause, values_from = rate) %>% 
        select(-c(sex)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="age")
      
      test00_ <- test00__ %>% 
        filter(sex==i) %>% 
        pivot_wider(names_from = cause, values_from = rate) %>% 
        select(-c(sex)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="age")
      
      dims <- dim(test21_)
      
      v21_ <- data.matrix(test21_)
      v00_ <- data.matrix(test00_)
      
      v21 <- c(v21_)
      v00 <- c(v00_)
      names <- colnames(v21_)
      
      B <- stepwise_replacement(func = Mxc2e0abrvec,
                                pars1 = v00,
                                pars2 = v21,
                                dims = dims,
                                symmetrical = TRUE,
                                direction = "up") 
      
      B_ <- as.data.frame(matrix(B, nrow = dims[1], ncol = dims[2])) %>% 
        summarise_all(list(sum))
      
      colnames(B_) <- names
      
      B__ <- B_ %>% 
        mutate(sex = i) %>% 
        pivot_longer(cols = -c(sex)) %>% 
        mutate(value = round(value, 2)) 
      
      leold <- data %>% 
        filter(GHOcode=="WHOSIS_000001" & year==2000 & 
                 country==input$iso3le  & sex==i) %>% 
        select(value) %>% 
        pull()
      
      le_start <- leold + sum(B__ %>% filter(value<0) %>% pull(value))
      
      le_end <- leold + sum(B__ %>% filter(value>=0) %>% pull(value))
      
      lecomp2 <- rbind(
        data.frame(sex = i, name = "Test", value = -100),
        data.frame(sex = i, name = "Test", value = 100),
        B__
      ) %>% 
        arrange(value) %>% 
        mutate(age = le_start) %>%
        mutate(age = accumulate(abs(value[-1]), sum, .init = age[1])) %>% 
        arrange(desc(value)) %>% 
        mutate(age_end = le_end) %>% 
        mutate(age_end = accumulate(-abs(value[-1]), sum, .init = age_end[1])) %>% 
        filter(name!="Test")
      
      out <- rbind(out, lecomp2)
      
    }
    
    out$sex <- as.factor(out$sex)
    out$sex.id <- as.integer(out$sex)
    
    return(out)
  })
  
  decomp_graph <- reactive({
    le <- data %>% 
      filter(GHOcode=="WHOSIS_000001" & 
               year %in% c(2000, 2021) &
               country==input$iso3le) %>% 
      select(sex, country, year, value)
    
    le$sex <- as.factor(le$sex)
    le$sex.id <- as.integer(le$sex)
    
    decomp_data() %>% 
      ggplot(aes(x = sex))+
      geom_rect(aes(
        x = sex,
        xmin = sex.id-0.25,
        xmax = sex.id+0.25,
        ymin = age_end,
        ymax = age, 
        fill = name
      )) +
      geom_linerange(
        data = le,
        aes(xmin = sex.id-0.35, xmax = sex.id+0.35, y=value, color = year), size = 1.5, show.legend = F
      ) +
      coord_flip() +
      ylab("Life expectancy at birth (years)")+ xlab("")+
      theme_classic()+
      theme(legend.position = "bottom",
            axis.text.x=element_text(size=20),
            axis.title.x =element_text(size=20),
            axis.text.y=element_text(size=20),
            legend.title = element_blank(),
            legend.text = element_text(size=20)) +
      guides(fill=guide_legend(ncol=3)) +
      scale_fill_manual(values = customcolors)
  })
  
  decomp_graph_pdf <- reactive({
    decomp_graph()+
      labs(subtitle = paste0("Attribution of changes in life expectancy at birth (2000 to 2021) to changes in the top causes of death in 2021, ", input$iso3le, "."),
           caption = str_wrap("Life expectancy is presented as vertical lines: black is life expectancy in 2000, blue is life expectancy in 2021. Causes of death to the left of 2000 contributed to a decrease in life expectancy and causes of death to the right contributed to an increase in life expectany.", 120)) +
      theme(plot.subtitle = element_text(size = 14),
            plot.caption = element_text(size = 10),
            axis.text.x=element_text(size=14),
            axis.title.x =element_text(size=14),
            axis.text.y=element_text(size=14),
            legend.text = element_text(size=14))
  })
  
  output$decomp <- renderPlot({ decomp_graph() }) %>% bindCache(input$iso3le)
  
  
  
  # Life expectancy Decomposition over time
  
  decomp_data_time <- reactive({
    
    if(input$iso3le=="Global"){
      
      cod_ <- cod_decomp_time %>% 
        filter(sex == input$sex_decomptime) %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex, DIM_YEAR_CODE) %>% 
        summarise(VAL_DTHS_COUNT_NUMERIC = sum(VAL_DTHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup() %>% 
        arrange(age, DIM_GHECAUSE_TITLE)
      
    } else if (
      input$iso3le=="African Region" |
      input$iso3le=="Eastern Mediterranean Region" | 
      input$iso3le=="European Region" | 
      input$iso3le=="Region of the Americas" |
      input$iso3le=="South-East Asia Region" |
      input$iso3le=="Western Pacific Region"
    ) {
      
      cod_ <- cod_decomp_time %>% 
        filter(region2==input$iso3le & sex == input$sex_decomptime) %>% 
        group_by(DIM_GHECAUSE_TITLE, age, sex, DIM_YEAR_CODE) %>% 
        summarise(VAL_DTHS_COUNT_NUMERIC = sum(VAL_DTHS_COUNT_NUMERIC),
                  ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup() %>% 
        arrange(age, DIM_GHECAUSE_TITLE)
      
    } else {
      
      cod_ <- cod_decomp_time %>% 
        filter(country == input$iso3le & sex == input$sex_decomptime) %>% 
        arrange(age, DIM_GHECAUSE_TITLE)
      
    }
    
    out <- data.frame(sex = character(),
                      name = character(), 
                      value = numeric(),
                      year = character())
    for(i in 1:5){
      y1 <- years_decomp %>% filter(row_number()==i) %>% pull(start_year)
      y2 <- years_decomp %>% filter(row_number()==i) %>% pull(end_year)
      
      year1 <- cod_ %>% 
        filter(DIM_YEAR_CODE==y1) %>% 
        mutate(rate = VAL_DTHS_COUNT_NUMERIC / ATTR_POPULATION_NUMERIC) %>% 
        select(DIM_YEAR_CODE, age, DIM_GHECAUSE_TITLE, rate) %>% 
        pivot_wider(names_from = DIM_GHECAUSE_TITLE, values_from = rate) %>% 
        select(-c(DIM_YEAR_CODE)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="age")
      
      year2 <- cod_ %>% 
        filter(DIM_YEAR_CODE==y2) %>% 
        mutate(rate = VAL_DTHS_COUNT_NUMERIC / ATTR_POPULATION_NUMERIC) %>% 
        select(DIM_YEAR_CODE, age, DIM_GHECAUSE_TITLE, rate) %>% 
        pivot_wider(names_from = DIM_GHECAUSE_TITLE, values_from = rate) %>% 
        select(-c(DIM_YEAR_CODE)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="age")
      
      dims <- dim(year1)
      
      v1 <- data.matrix(year1)
      v2 <- data.matrix(year2)
      
      v1_ <- c(v1)
      v2_ <- c(v2)
      names <- colnames(v1)
      
      B <- stepwise_replacement(func = Mxc2e0abrvec,
                                pars1 = v1_,
                                pars2 = v2_,
                                dims = dims,
                                symmetrical = TRUE,
                                direction = "up") 
      
      B_ <- as.data.frame(matrix(B, nrow = 19, ncol = 4)) %>% 
        summarise_all(list(sum))
      
      colnames(B_) <- names
      
      B__ <- B_ %>% 
        mutate(sex = input$sex_decomptime) %>%
        pivot_longer(cols = -c(sex)) %>% 
        # mutate(value = round(value, 2)) %>%
        mutate(year = paste0(y1, "-", y2))
      
      leold <- data %>% 
        filter(GHOcode=="WHOSIS_000001" & country==input$iso3le & sex==input$sex_decomptime & year==y1) %>% 
        select(value) %>% 
        pull()
      
      le_start <- leold + sum(B__ %>% filter(value<0) %>% pull(value))
      
      le_end <- leold + sum(B__ %>% filter(value>=0) %>% pull(value))
      
      lecomp2 <- rbind(
        data.frame(sex = input$sex_decomptime, name = "Test", value = -100, year = paste0(y1, "-", y2)),
        data.frame(sex = input$sex_decomptime, name = "Test", value = 100, year = paste0(y1, "-", y2)),
        B__
      ) %>% 
        arrange(value) %>% 
        mutate(age = le_start) %>%
        mutate(age = accumulate(abs(value[-1]), sum, .init = age[1])) %>% 
        arrange(desc(value)) %>% 
        mutate(age_end = le_end) %>% 
        mutate(age_end = accumulate(-abs(value[-1]), sum, .init = age_end[1])) %>% 
        filter(name!="Test")
      
      out <- rbind(out, lecomp2)
      
    }
    
    out$year <- as.factor(out$year)
    out$year.id <- as.integer(out$year)
    
    return(out)
  })
  
  decomp_time_graph <- reactive({
    le_start_ <- data %>% 
      filter(GHOcode=="WHOSIS_000001" & 
               year %in% c(2000, 2005, 2010, 2015, 2019) &
               country==input$iso3le &
               sex==input$sex_decomptime) %>% 
      select(sex, country, year_ = year, value) %>% 
      mutate(year = case_when(
        year_==2000 ~ "2000-2005",
        year_==2005 ~ "2005-2010",
        year_==2010 ~ "2010-2015",
        year_==2015 ~ "2015-2019",
        year_==2019 ~ "2019-2021",
      ))
    
    le_end_ <- data %>% 
      filter(GHOcode=="WHOSIS_000001" & 
               year %in% c(2005, 2010, 2015, 2019, 2021) &
               country==input$iso3le &
               sex==input$sex_decomptime) %>% 
      select(sex, country, year_ = year, value) %>% 
      mutate(year = case_when(
        year_==2005 ~ "2000-2005",
        year_==2010 ~ "2005-2010",
        year_==2015 ~ "2010-2015",
        year_==2019 ~ "2015-2019",
        year_==2021 ~ "2019-2021",
      ))
    
    le <- rbind(le_start_, le_end_) %>% mutate(leg = "Change in life expectancy")
    
    le$year <- as.factor(le$year)
    le$year.id <- as.integer(le$year)
    
    decomp_data_time() %>%   
      ggplot(aes(x = year))+
      geom_rect(aes(
        x = year,
        xmin = year.id-0.25,
        xmax = year.id+0.25,
        ymin = age_end,
        ymax = age, 
        fill = name
      )) +
      geom_line(data = le, aes(x = year, y = value, color = leg) , arrow = arrow(length = unit(0.30,"cm"), type = "closed")) +
      ylab("Life expectancy at birth (years)")+ xlab("Year range")+
      theme_classic()+
      theme(axis.text.x=element_text(size=20),
            axis.title.x =element_text(size=20),
            axis.text.y=element_text(size=20),
            axis.title.y=element_text(size=20),
            legend.title = element_blank(),
            legend.text = element_text(size=20),
            legend.position = "bottom", 
            panel.grid.major = element_line(color = "grey")) +
      scale_fill_manual(values = c("Communicable, maternal, perinatal and nutritional conditions" = "#F26829", 
                                   "Noncommunicable diseases" = "#009ADE", 
                                   "Injuries" = "#80BC00",
                                   "Other COVID-19 pandemic-related outcomes" = "#A6228C"))+
      scale_color_manual(values = c("Change in life expectancy"="black"))+
      guides(fill=guide_legend(ncol=1))
      
  })
  
  output$decomp_time <- renderPlot({ decomp_time_graph() }) %>% bindCache(input$iso3le, input$sex_decomptime)
  
  
  
  # Arrow
  codrank_data <- reactive({
    
    req(input$iso3le)
    
    if(input$iso3le=="Global"){
      
      cod19 %>%
        filter(FLAG_LEVEL == 2 & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        rbind(
          cod00 %>%
            filter(FLAG_LEVEL == 2 & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC))
        ) %>% 
        group_by(DIM_YEAR_CODE) %>%
        mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
               rank_deaths = rank(-prop_deaths, ties.method = "first"),
        ) %>%
        select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, rank_deaths)
      
      }else if(input$iso3le %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
      
      cod19 %>%
        filter(FLAG_LEVEL == 2 & region2==input$iso3le & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        rbind(
          cod00 %>%
            filter(FLAG_LEVEL == 2 & region2==input$iso3le & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC))
        ) %>% 
          group_by(DIM_YEAR_CODE) %>%
          mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
                 rank_deaths = rank(-prop_deaths, ties.method = "first"),
          ) %>%
          select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, rank_deaths)
      
    }else{
      
      cod19 %>%
        filter(country == input$iso3le & FLAG_LEVEL == 2 & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
        select(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC) %>% 
        rbind(
          cod00 %>%
            filter(country == input$iso3le & FLAG_LEVEL == 2 & sex==input$sexcod2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
            select(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC)
        ) %>% 
        group_by(DIM_YEAR_CODE) %>%
        mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
               rank_deaths = rank(-prop_deaths, ties.method = "first"),
        ) %>%
        select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, rank_deaths)
    }
    # return(test)
  })
  
  codrank_graph <- reactive({
      
    year <- 2021
    comp_year <- 2000
    
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    # compute cause fractions and cause ranks
    data_arrow <- codrank_data() %>%
      # group_by(DIM_YEAR_CODE) %>%
      # mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
      #        rank_deaths = rank(-prop_deaths, ties.method = "first"),
      # ) %>%
      # select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, rank_deaths) %>%
      pivot_wider(id_cols = c(FLAG_CAUSEGROUP, DIM_GHECAUSE_TITLE), names_from = DIM_YEAR_CODE,
                  values_from = c(rank_deaths)) %>%
      # rename(rank_year1 = paste0("rank_deaths_", comp_year),
      #        rank_year2 = paste0("rank_deaths_", year)) %>%
      # mutate(increasing_rank = rank_year1 < rank_year2) %>%
      filter(`2000`<11 | `2021`<11) %>%
      left_join(lev1, by = "FLAG_CAUSEGROUP") %>% 
      pivot_longer(cols = c(`2000`, `2021`)) %>% 
      mutate(year = as.numeric(name),
             label = paste0(value, ". ", DIM_GHECAUSE_TITLE))
    
    data_arrow$lev1_name <- factor(data_arrow$lev1_name, levels = lev1_causes)
    
    graph <- data_arrow %>% 
      ggplot(aes(x = year, y = -value, group = DIM_GHECAUSE_TITLE, color = lev1_name)) +
      geom_bump(size = 1.5, show.legend = T, alpha = 0.7) +
      geom_point(size = 3, show.legend = F, alpha = 0.8) +
      geom_text(data = data_arrow %>% filter(year == 2000),
                aes(x = year - 1, y = -value, label = label),
                size = 6, hjust = 1, show.legend = F) +
      geom_text(data = data_arrow %>% filter(year == 2021),
                aes(x = year + 1, y = -value, label = label),
                size = 6, hjust = 0, show.legend = F) +
      scale_x_continuous(limits = c(1990, 2031))+
      scale_color_manual(values = c("#F26829", "#009ADE", "#80BC00")) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 16),
            legend.title = element_blank())+
      guides(color=guide_legend(ncol=1))
    
    graph 
  }) #, height = 500, width = 1100
  
  codrank_graph_pdf <- reactive({
    codrank_graph()+
      labs(subtitle = paste0("Change in rank for level 2 causes of death between 2000 and 2021: ", input$iso3le, ", ", input$sexcod2, "."))+
      theme(plot.subtitle = element_text(size = 16),
            legend.text = element_text(size = 12),
            plot.margin = unit(c(1,1,1,1), "cm"))
  })
  
  output$codrank <- renderPlot({ codrank_graph() })
  
  
  
  
  # Treemap
  data_tree <- reactive({
    
    sexcodle <- input$sexcod
    regionle <- input$iso3le

    if(input$codswitch=="Level 1 causes"){
      maketreemapdata_levl1(regionle = regionle, sexcod = sexcodle)
    } else {
      maketreemapdata_levl2(regionle = regionle, sexcod = sexcodle)
    }
    
  })
  
  tree_graph <- reactive({
    
    sexcodle <- input$sexcod
    regionle <- input$iso3le
    data_list <- data_tree()
    
    if(input$codswitch=="Level 1 causes"){
      final <- maketreemapgraph_levl1(data_list, regionle = regionle, sexcod = sexcodle)
      final 
    } else {
      final <- maketreemapgraph_levl2(data_list, regionle = regionle, sexcod = sexcodle)
      final
    }
    
  })
  
  tree_graph_pdf <- reactive({
    title <- ggdraw() +
      draw_label(
      paste0("All-age cause of deaths in 2000 and 2021: ", input$iso3le, ", ", input$sexcod, "."),
      x = 0,
      hjust = 0
    ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 0)
      )
    
    plot_grid(
      title, 
      tree_graph(),
      ncol = 1,
      rel_heights = c(0.1, 1))
  })
  
  data_tree_csv <- reactive({
    
    data_list <- data_tree()

    rbind(data_list[[1]],data_list[[2]]) %>% 
      left_join(cod19 %>%
                  filter(FLAG_LEVEL == 1) %>%
                  select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
                  unique(), by = "FLAG_CAUSEGROUP")
    
  })
  
  output$treemap <- renderPlot({ tree_graph() })
  
  
  
  
  ## UHC Circle graph
  output$uhcgraph <- renderHighchart({
    
    level=input$iso3le; year_pie=2018
    
    donut_data <- filtered_indicator_values %>% 
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
    
    donut_data2 <- donut_data %>% pivot_longer(cols = c(sdg_2030_goal, start_year, goal_year)) %>% 
      mutate(var = 
               case_when(name=="goal_year" ~ "2. 2030 forcasted value",
                         name=="sdg_2030_goal" ~ "1. 2030 goal",
                         name=="start_year" ~ "3. 2018 value"))
    # highchart() %>% 
    #   hc_add_series(
    #     donut_data2, type = "column",
    #     hcaes(x = indicator_name, y = value, group = var)
    #   ) %>% 
    #   hc_plotOptions(column = list(grouping = F)) %>%
    #   hc_chart(polar = TRUE) 
    
    # make_donut_uhc(filtered_data = filtered_indicator_values, level=input$iso3le, year_pie=2018)
    highchart() %>% 
      # donut_data %>% 
      # hchart(type = "column", hcaes(x =indicator_name, y = start_year, fill = indicator_name))
      hc_add_series(donut_data, type = "column", hcaes(x = indicator_name, y = sdg_2030_goal), opacity = 0.4, name = "2030 Goal", color = "red") %>%
      hc_add_series(donut_data, type = "column", hcaes(x = indicator_name, y = goal_year, color = indicator_name), opacity = 0.7, name = "2030 forecast", show) %>%
      hc_add_series(donut_data, type = "column", hcaes(x = indicator_name, y = start_year, color = indicator_name), name = "2018 value") %>%
      
      # hc_add_series(donut_data, type = "scatter", hcaes(x = indicator_name, y = sdg_2030_goal, color = indicator_name), name = "2030 SDG Goal") %>%
      # hc_add_series(donut_data, "errorbar",
      #               hcaes(x = indicator_name, y = sdg_2030_goal, low = sdg_2030_goal, high = sdg_2030_goal, 
      #                     color = indicator_name),
      #               pointWidth = 20, whiskerLength = 20, # widths set match column
      #               centerInCategory = T, showInLegend = F) %>% 
      hc_chart(polar = TRUE) %>% 
      hc_tooltip(crosshairs = T, shared = T, useHTML=T) %>% 
      hc_plotOptions(column = list(pointWidth = 0.5, grouping = F), enableMouseTracking = T) %>% 
      hc_xAxis(categories = as.list(donut_data$indicator_name)) %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1000, sourceHeight=700,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) 
    
    # highchart() %>% 
    #   # donut_data %>% 
    #   # hchart(type = "column", hcaes(x =indicator_name, y = start_year, fill = indicator_name))
    #   hc_add_series(donut_data, type = "column", hcaes(x = indicator_name, y = start_year, color = indicator_name)) %>%
    #   # hc_add_series(donut_data, type = "column", hcaes(x = indicator_name, y = goal_year, color = indicator_name), opacity = 0.7) %>%
    #   hc_plotOptions(column = list(pointWidth = 0.5)) %>% 
    #   hc_chart(polar = TRUE) 
    
    # donut_data2 %>%  
    #   hchart(type = "column", hcaes(x =indicator_name, y = value, group = var)) %>% 
    #   hc_plotOptions(column = list(grouping = F)) %>%
    #   hc_chart(polar = TRUE) 
    # 
    # hc_add_series_list(
    #   donut_data %>% 
    #     group_by(
    #       name = "actual",
    #       type = "column",
    #       yAxis = 0
    #     ) %>% 
    #     do( data = list_parse(data.frame(x = .$xmax, y = .$start_year)))
    # ) %>% 
    # hc_add_series_list(
    #   donut_data %>% 
    #     group_by(
    #       name = "target",
    #       type = "scatter",
    #       yAxis = 0
    #     ) %>% 
    #     do( data = list_parse(data.frame(x = .$xmax, y = .$sdg_2030_goal)))
    # ) %>% 
    # hc_chart(
    #   events = list(
    #     load = JS("function(){
    #           Highcharts.Renderer.prototype.symbols.line = function(x, y, width, height) {
    #             return ['M', x , y + width / 2, 'L', x+height, y + width / 2];
    #           };
    #           this.series[1].update({marker: {symbol: 'line'}})
    # }")
    #   )
    # ) %>% 
    # hc_plotOptions(
    #   scatter = list(
    #     marker = list(
    #       # This is where I am inserting the Java Script code from the example
    #       symbol = 'line',
    #       # 
    #       lineWidth = 3,
    #       radius = 8,
    #       lineColor = "#000"
    #     )
    #   )
    # ) %>% hc_chart(polar = TRUE) 
    # # hc_add_series(donut_data, type = "line", hcaes(x = xmax, y = sdg_2030_goal, fill = indicator_name, group = indicator_name),
    # #               marker) %>% 
    #   # hc_add_series(donut_data, type = 'arearange', 
    #   #               hcaes(x = xmax, low = sdg_2030_goal, high = sdg_2030_goal))
    # # hc_add_series(donut_data, type = "column", hcaes(x = xmax, y = goal_year, fill = indicator_name, group = indicator_name)) %>% 
    # # hc_plotOptions(column = list(opacity = 0.5)) %>% 
    # hc_chart(polar = TRUE) 
    # %>% 
    #   
    #   highchart() %>%
    #   hc_add_series(donut_data, type = 'bullet',
    #                 hcaes(x = indicator_name, y = goal_year, target = sdg_2030_goal, color = indicator_name)) %>%
    #   hc_chart(polar = TRUE) 
    #   hc_plotOptions(series = list(
    #     pointPadding = 0.25,
    #     pointWidth = 15,
    #     borderWidth = 0,
    #     targetOptions = list(width = '200%')
    #   )) %>% 
    #   hc_chart(polar = TRUE) 
    #   
    
    
  })
  
  
  ## SDG Forecast Table
  datatablesdg <- reactive({
    make_sdg_table(filtered_data = filtered_indicator_values, level = input$iso3le)
  })
  
  tablesdg <- reactive({
    
    table <- datatablesdg()
    
    flextable(table) %>% 
      bg(., bg = "white", part = "all") %>% 
      bg(., ~ ach=="Likely Achieve by 2030", ~ `2030`, bg = "#AACF7F") %>% 
      bg(., ~ ach=="Won't Achieve by 2030, but within 10% of target", ~ `2030`, bg = "#F5C46A") %>% 
      bg(., ~ ach=="Won't Achieve by 2030", ~ `2030`, bg = "#F6A27C") %>% 
      delete_columns(., j = "ach") %>% 
      width(., j =1, width=2, unit = "cm") %>% 
      width(., j =2, width=7, unit = "cm") %>% 
      width(., j =3, width=3, unit = "cm") %>% 
      width(., j =4:6, width=3, unit = "cm") %>% 
      width(., j =7, width=3.5, unit = "cm") %>% 
      align(., j=4:7, align = "center") %>% 
      align(., j=4:7, align = "center", part = "header")
      
    
    # ft <- flextable(table)
    # ft <- bg(ft, bg = "white", part = "all")
    # ft <- bg(ft, ~ ach=="Likely Achieve by 2030", ~ `2030`, bg = "#AACF7F")
    # ft <- bg(ft, ~ ach=="Won't Achieve by 2030, but within 10% of target", ~ `2030`, bg = "#F5C46A")
    # ft <- bg(ft, ~ ach=="Won't Achieve by 2030", ~ `2030`, bg = "#F6A27C")
    # ft <- delete_columns(ft, j = "ach")
    # ft <- width(ft, j =1, width=2, unit = "cm")
    # ft <- width(ft, j =2, width=7, unit = "cm")
    # ft <- width(ft, j =3, width=3, unit = "cm")
    # ft <- width(ft, j =4:6, width=3, unit = "cm")
    # ft <- width(ft, j =7, width=3.5, unit = "cm")
    # ft <- align(ft, j=4:7, align = "center")
    # ft <- align(ft, j=4:7, align = "center", part = "header")
    # ft <- footnote(ft, i= 1, j=6, value = as_paragraph("2030 values are colored based on whether the indicator is likely to achieve the target based on 2030 forecasts: green will reach target, yellow are within 10% of target, orange will not reach target."), ref_symbols = "*", part = "header")
    
  })
  
  tablesdg_pdf <- reactive({ autofit(tablesdg()) })
  
  output$sdgtable <- renderUI({
    tablesdg() %>% htmltools_value()
  })
  
  output$sdgtable_legend <- renderPlot({
    ggplot() +
      geom_tile(aes(x = 1, y = 1), fill = "#AACF7F", alpha=0.8)+
      geom_tile(aes(x = 2, y = 1), fill = "#F5C46A", size = 1, alpha=0.8) +
      geom_tile(aes(x = 3, y = 1), fill = "#F6A27C", size = 1, alpha=0.8) +
      geom_text(aes(x = 1, y = 0.5, label = "Likely Achieve by 2030"), size=6, vjust = 1.1)+
      geom_text(aes(x = 2, y = 0.5, label = str_wrap("Within 10% of target by 2030"), 15), size=6, vjust = 1.1)+
      geom_text(aes(x = 3, y = 0.5, label = "Won't Achieve by 2030"), size=6, vjust = 1.1)+
      theme_void()+
      scale_y_continuous(limits = c(-1,2.5))
  })
  
  
  
  
  ## Triple Billions Contibutions
  
  TBcontrib_data <- reactive({
    
    all_plt_dat <- all_plt_dat %>% 
      filter(
        country == input$iso3le,
        scenario == "baseline",
        ind %in% inds_all,
        year >= config_all$required_start_year,
        year <= 2030,
        year > 2017
      ) %>%
      left_join(ind_labels, by = c("ind", "billion")) %>%
      mutate(name = case_when(name == "Child healthcare" ~ "Child health care", .default = name))
    
    full_color_pal <- c(
      RColorBrewer::brewer.pal(8, "Set2"),
      RColorBrewer::brewer.pal(9, "Set3"),
      RColorBrewer::brewer.pal(8, "Dark2")
    )
    # drop colors that do not show up well with white background or close to duplicate values
    full_color_pal <- full_color_pal[!full_color_pal %in% c("#FFFFB3", "#D9D9D9", "#B3DE69")]
    
    if(input$TB == "UHC"){
      
      uhc_ind_order <- all_plt_dat %>%
        filter(year == 2030 & billion == "uhc" & ind != "uhc_billion") %>%
        arrange(abs(contribution)) %>%
        pull(name)
      uhc_ind_colors <- full_color_pal[1:length(uhc_ind_order)]
      names(uhc_ind_colors) <- uhc_ind_order
      
      uhc_decomp <-
        all_plt_dat %>%
        mutate(ind = factor(name, levels = c(uhc_ind_order, "UHC Billion"))) %>%
        filter(billion == "uhc") %>%
        plot_decomposition(
          plt_billion = "uhc",
          plt_net_ind = "UHC Billion",
          # plt_aggregate_id = "global",
          # plt_aggregation_level = "global",
          plt_inds_color_pal = uhc_ind_colors,
          # plt_title = str_glue("Universal health coverage - Individual indicator contributions"),
          label_years = config_all$years_label_plot %>% c(2019),
          size_text = 6
        )
      
      return(uhc_decomp)
      
    } else if(input$TB == "HPOP"){
      
      hpop_ind_order <- all_plt_dat %>%
        filter(year == 2030 & billion == "hpop" & ind != "hpop_healthier") %>%
        arrange(abs(contribution)) %>%
        pull(name)
      hpop_ind_colors <- full_color_pal[1:length(hpop_ind_order)]
      names(hpop_ind_colors) <- hpop_ind_order
      
      hpop_decomp <-
        all_plt_dat %>%
        mutate(ind = factor(name, levels = c(hpop_ind_order, "Population healthier - HPOP Billion"))) %>%
        filter(billion == "hpop") %>%
        plot_decomposition(
          plt_billion = "hpop",
          plt_net_ind = "Population healthier - HPOP Billion",
          # plt_aggregate_id = "global",
          # plt_aggregation_level = "global",
          plt_inds_color_pal = hpop_ind_colors,
          # plt_title = str_glue("Healthier populations - Individual indicator contributions"),
          label_years = config_all$years_label_plot %>% c(2019),
          size_text = 6
        )
      return(hpop_decomp)
      
    } else if(input$TB == "HEP"){
      
      hep_ind_order <- all_plt_dat %>%
        filter(year == 2030 & billion == "hep" & ind != "hep_idx") %>%
        arrange(abs(contribution)) %>%
        pull(name)
      hep_ind_colors <- full_color_pal[1:length(hep_ind_order)]
      names(hep_ind_colors) <- hep_ind_order
      
      hep_decomp <-
        all_plt_dat %>%
        mutate(ind = factor(name, levels = c(hep_ind_order, "HEPI"))) %>%
        filter(billion == "hep") %>%
        plot_decomposition(
          plt_billion = "hep",
          plt_net_ind = "HEPI",
          plt_inds_color_pal = hep_ind_colors,
          # plt_title = str_glue("Health emergencies protection - Individual indicator contributions"),
          label_years = config_all$years_label_plot %>% c(2019),
          size_text = 6
        )
      
      return(hep_decomp)
      
    }
    
  })
  
  output$TBcontrib <- renderPlot({
    
    graph <- TBcontrib_data()
    
    graph$plt
    
  })
  

  
  
  ## Generate report
  reactive_filename <- reactive({
    paste0("who-country-report-", input$iso3le, ".pdf")
  })
  
  reactive_country_name <- reactive({
    paste0(input$iso3le)
  })
  
  rmd_file <- reactive({
    if(input$iso3le=="Global"){
      "rmd/country-report-global.Rmd"
      
      } else if(input$iso3le %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                   "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
      "rmd/country-report-regional.Rmd"
        } else {
      "rmd/country-report.Rmd"
    }
  })
  
  output$download_button <- downloadHandler(
    
    filename = function() {
      reactive_filename()
    },
    content = function(file) {
      params <- list(
        country = reactive_country_name(),
        country_iso = country_table %>% filter(Title == input$iso3le) %>% pull(Code),
        cod19 = cod19,
        cod00 = cod00,
        top10 = top10,
        data_pyramid = data_pyramid,
        data = data,
        filtered_indicator_values = filtered_indicator_values,
        all_plt_dat = all_plt_dat,
        inds_all = inds_all,
        ind_labels = ind_labels,
        config_all = config_all,
        country_table = country_table
      )
      
      id <- showNotification(
        "Rendering pdf...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render(rmd_file(), 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  rmd_file_new <- reactive({"rmd/country-report_eb_version.Rmd"})
  
  reactive_filename_new <- reactive({
    paste0("who-country-report-v2-", input$iso3le, ".pdf")
  })
  
  output$download_button_new <- downloadHandler(
    
    filename = function() {
      reactive_filename_new()
    },
    content = function(file) {
      params <- list(
        country = reactive_country_name(),
        country_iso = country_table %>% filter(Title == input$iso3le) %>% pull(Code),
        filtered_indicator_values = filtered_indicator_values,
        all_plt_dat = all_plt_dat,
        inds_all = inds_all,
        ind_labels = ind_labels,
        config_all = config_all,
        country_table = country_table,
        country_ref = country_ref
      )
      
      id <- showNotification(
        "Rendering pdf...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render(rmd_file_new(), 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
      
  #################################################################################################################
  ####  Map  ####
  #################################################################################################################
  #
  # observe({
  #   x <- data %>%
  #     filter(name==input$ind) %>%
  #     distinct(year) %>% 
  #     arrange(desc(year)) %>% 
  #     mutate(year = as.character(year)) %>%
  #     pull()
  #   
  #   # Can use character(0) to remove all choices
  #   if (is.null(x))
  #     x <- character(0)
  #   
  #   updateVirtualSelect("yearmap", label = "Select year:", choices = x, selected = x[1])
  # })
  # 
  # mapdata <- reactive({
  #   # yearvalue <- input$yearmap
  #   data %>%
  #     filter(name==input$ind & year==input$yearmap & geotype=="COUNTRY") %>%
  #     mutate(value = round(value, digits = 1)) %>%
  #     rename(ISO_3_CODE = iso3)
  # })
  # 
  # output$map <- renderHighchart ({
  #   year <- input$yearmap
  #   indname <- input$ind
  #   
  #   highchart() %>%
  #     hc_chart(backgroundColor = "white") %>% 
  #     hc_add_series_map(Myhcmap, mapdata(), value = "value", joinBy = "ISO_3_CODE", name = indname) %>%
  #     hc_add_series(mapData = Myhcmap3, showInLegend = F) %>%
  #     hc_add_series(mapData = Myhcmap2, type = "mapline", showInLegend = F) %>%
  #     hc_colorAxis(
  #       minColor = "#3D61A4",
  #       maxColor = "#F26829"
  #     ) %>%
  #     hc_exporting(enabled = TRUE, sourceWidth=1200, sourceHeight=600,
  #                  buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) %>%
  #     hc_subtitle(text = paste0(indname, ", ", year)) %>%
  #     hc_mapNavigation(enabled = TRUE) %>%
  #     hc_loading()
  # })
  
  
  #################################################################################################################
  ####  Indicator Table  ####
  #################################################################################################################
  
  observe({
    x <- annexdata %>%
      filter(name %in% input$indtab2) %>%
      filter(region_name %in% input$regtab2) %>%
      distinct(country) %>% pull()

    updateVirtualSelect("isotab2", label = "Select Country:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata %>%
      filter(name %in% input$indtab2) %>% 
      filter(region_name %in% input$regtab2) %>% 
      distinct(year) %>% arrange(desc(year)) %>% mutate(year = as.character(year)) %>%  pull()
    
    # Can also set the label and select items
    updateVirtualSelect("yeartab2", label = "Year:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata %>%
      filter(name %in% input$indtab2) %>% 
      filter(region_name %in% input$regtab2) %>% 
      distinct(sex) %>% pull()
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "sextab2", label = "Sex:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata %>%
      filter(name %in% input$indtab2) %>% 
      filter(region_name %in% input$regtab2) %>% 
      distinct(location) %>% pull()
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "loctab2", label = "Urban/Rural:", choices = x, selected = x)
  })
  
  tabledata <- reactive({
    annexdata %>%
      filter(name %in% input$indtab2) %>% 
      filter(region_name %in% input$regtab2) %>% 
      filter(country %in% input$isotab2) %>%
      filter(year %in% as.numeric(input$yeartab2)) %>% 
      filter(sex %in% input$sextab2) %>% 
      filter(location %in% input$loctab2) %>% 
      select(`Indicator` = name, `Region/Country` = country, `Year` = year, `Sex` = sex, `Urban/Rural` = location, `Value` = stringvalue)
  })
  
  output$dt <- renderDT({
    
     tabledata() %>%
      datatable(rownames=FALSE, escape = FALSE,
                extensions = c('Buttons',"FixedColumns"),
                options = list(pageLength = 30,
                               paging = F,
                               scrollY = '1300px',
                               fixedColumns = list(leftColumns = 1),
                               scrollX = TRUE,
                               ordering=F,
                               columnDefs = list(list(className = 'dt-center',  targets = "_all")),
                               scrollY = TRUE,
                               dom = 'Bfrtp',
                               buttons = list(
                                 list(extend = 'csv'))))
  })

  
  #################################################################################################################
  ####  Benchmarking  ####
  #################################################################################################################
  
  ## Life expectancy decomp by region
  iso3_selected <- reactive({
    all_countries_table %>%
      filter(country %in% c(input$iso3decomp_bench)) %>% 
      arrange(country) %>% 
      select(iso3) %>% 
      pull()
  })
  
  le_data_decomp_bench <- reactive({
    data %>% 
      filter(GHOcode=="WHOSIS_000001" & 
               year %in% c(2000, 2021) &
               iso3 %in% iso3_selected() &
               sex==input$sexdecomp_bench) %>% 
      select(country, iso3, year, value)
  })
  
  decomp_data_bench <- reactive({
    
    test21 <- cod19 %>% 
      filter(FLAG_LEVEL==2 & DIM_AGEGROUP_CODE!="TOTAL") %>% 
      filter(DIM_COUNTRY_CODE %in% iso3_selected() & sex==input$sexdecomp_bench) %>% 
      mutate(rate = VAL_DEATHS_COUNT_NUMERIC/ATTR_POPULATION_NUMERIC) %>% 
      select(country, iso3 = DIM_COUNTRY_CODE, age, DIM_GHECAUSE_TITLE, rate) %>% 
      pivot_wider(names_from = DIM_GHECAUSE_TITLE, values_from = rate) %>% 
      arrange(country, age)
    
    test00 <- cod00 %>% 
      filter(FLAG_LEVEL==2 & DIM_AGEGROUP_CODE!="TOTAL") %>% 
      filter(DIM_COUNTRY_CODE %in% iso3_selected() & sex==input$sexdecomp_bench) %>% 
      mutate(rate = VAL_DEATHS_COUNT_NUMERIC/ATTR_POPULATION_NUMERIC) %>% 
      select(country, iso3 = DIM_COUNTRY_CODE, age, DIM_GHECAUSE_TITLE, rate) %>% 
      pivot_wider(names_from = DIM_GHECAUSE_TITLE, values_from = rate) %>% 
      arrange(country, age)
    
    le <- le_data_decomp_bench()
    
    out <- data.frame(country = character(), 
                      name = character(), 
                      value = numeric(),
                      age = numeric(),
                      age_end = numeric())
    
    for(i in input$iso3decomp_bench){
      
      j = all_countries_table %>% filter(country==i) %>% select(iso3) %>% pull()
      
      test21_ <- test21 %>% 
        filter(iso3==j) %>% 
        select(-c(country, iso3)) %>% 
        remove_rownames %>%
        column_to_rownames(var="age")
      
      test00_ <- test00 %>% 
        filter(iso3==j) %>% 
        select(-c(country, iso3)) %>% 
        remove_rownames %>%
        column_to_rownames(var="age")
      
      dims <- dim(test21_)
      
      v21_ <- data.matrix(test21_)
      v00_ <- data.matrix(test00_)
      
      v21 <- c(v21_)
      v00 <- c(v00_)
      names <- colnames(v21_)
      
      B <- stepwise_replacement(func = Mxc2e0abrvec,
                                pars1 = v00,
                                pars2 = v21,
                                dims = dims,
                                symmetrical = TRUE,
                                direction = "up") 
      
      B_ <- as.data.frame(matrix(B, nrow = dims[1], ncol = dims[2])) %>% 
        summarise_all(list(sum))
      
      colnames(B_) <- names
      
      B__ <- B_ %>% 
        mutate(country = i) %>% 
        pivot_longer(cols = -c(country)) %>% 
        mutate(value = round(value, 2)) 
      
      leold <- le %>% filter(year==2000 & iso3 == j) %>% select(value) %>% pull()
      
      le_start <- leold + sum(B__ %>% filter(value<0) %>% pull(value))
      
      le_end <- leold + sum(B__ %>% filter(value>=0) %>% pull(value))
      
      lecomp2 <- rbind(
        data.frame(country = i, name = "Test", value = -1000),
        data.frame(country = i, name = "Test", value = 1000),
        B__
      ) %>% 
        arrange(value) %>% 
        mutate(age = le_start) %>%
        mutate(age = accumulate(abs(value[-1]), sum, .init = age[1])) %>% 
        arrange(desc(value)) %>% 
        mutate(age_end = le_end) %>% 
        mutate(age_end = accumulate(-abs(value[-1]), sum, .init = age_end[1])) %>% 
        filter(name!="Test")
      
      out <- rbind(out, lecomp2)
      
    }
    
    out$country <- as.factor(out$country)
    out$country.id <- as.integer(out$country)
    
    return(out)
    
  })
  
  decomp_graph_bench <- reactive({
    
    le <- le_data_decomp_bench()
    le$country <- as.factor(le$country)
    le$country.id <- as.integer(le$country)
    
    decomp_data_bench() %>% 
      ggplot(aes(x = country))+
      geom_rect(aes(
        x = country,
        xmin = country.id-0.35,
        xmax = country.id+0.35,
        ymin = age_end,
        ymax = age, 
        fill = name
      )) +
      geom_linerange(
        data = le,
        aes(xmin = country.id-0.5, xmax = country.id+0.5, y=value, color = year), size = 1, show.legend = F
      ) +
      coord_flip() +
      ylab("Life expectancy at birth (years)")+xlab("")+
      theme_classic()+
      theme(legend.position = "bottom",
            axis.text.x=element_text(size=16),
            axis.title.y =element_text(size=16),
            axis.text.y=element_text(size=16),
            legend.title = element_blank(),
            legend.text = element_text(size = 14))+
      scale_fill_manual(values = customcolors) +
      guides(fill=guide_legend(ncol=4))
    
  })
  
  output$decomp_bench <- renderPlot({ decomp_graph_bench() }) %>% bindEvent(input$ready)
  
  output$data_decomp_bench <- downloadHandler(
    filename = function() {
      paste("life_expectancy_decomp_compare", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(decomp_data_bench() %>% select(country, cause = name, contribution_to_change_in_life_expectancy_in_years = value), file, row.names = FALSE)
    }
  )
  
  output$jpeg_decomp_bench <- downloadHandler(
    filename = function() {
      paste("life_expectancy_decomp_compare", ".jpeg", sep = "")
    },
    content = function(file) {
      ggsave(decomp_graph_bench(), filename = file, width = 15, height = 15, units = "in", dpi = 100)
    }
  )
  
  output$pdf_decomp_bench <- downloadHandler(
    filename = function() {
      paste("life_expectancy_decomp_compare", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(decomp_graph_bench(), filename = file, width = 15, height = 15, units = "in")
    }
  )
  
  
  ## LE graph comparative
  
  observe({
    x <- annexdata %>%
      filter(name==input$indgraph) %>%
      arrange(sort_country, country) %>% 
      distinct(country) %>%
      pull(country)
    
    updateVirtualSelect("iso3graph", label = "Select countries, areas or WHO regions:", choices = x, selected = x[1])
  })
  
  observe({
    x <- annexdata %>%
      filter(name==input$indgraph & country %in% input$iso3graph) %>% 
      distinct(year) %>% 
      arrange(desc(year)) %>% 
      mutate(year = as.character(year)) %>% 
      pull()
    
    # Can also set the label and select items
    updateVirtualSelect("yeargraph", label = "Select years:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata %>%
      filter(name==input$indgraph & country %in% input$iso3graph) %>% 
      distinct(sex) %>% 
      arrange(sex) %>% 
      pull()
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateRadioButtons(session, "sexgraph", label = "Sex:", choices = x, selected = x[1])
  })
  
  legraphdata <- reactive({
    if(input$indgraph %in% locinclude_ind){
      annexdata %>%
        filter(name==input$indgraph &
                 country %in% input$iso3graph &
                 year %in% as.numeric(input$yeargraph) &
                 sex == input$sexgraph &
                 location=="Both Rural and Urban") %>%
        arrange(year) %>%
        mutate(value = round(value, digits = 1)) %>%
        select(country, year, sex, name, value)
    }else{
    annexdata %>%
        filter(name==input$indgraph &
                 country %in% input$iso3graph &
                 year %in% as.numeric(input$yeargraph) &
                 sex == input$sexgraph) %>% 
        arrange(year) %>%
        mutate(value = round(value, digits = 1)) %>%
        select(country, year, sex, name, value)
    }
  })
  
  output$legraph <- renderHighchart({
    
    ind <- input$indgraph
    
    legraphdata() %>% 
      hchart('line', hcaes(x = year, y = value, group = country)
             # dataLabels = list(
             #   enabled = TRUE,
             #   format = "{this.point.country}",
               # formatter= JS(
               #   paste0('function() {
               #         if(!this.point.noTooltip) {
               #           return this.series.name + "</b><br/>LE: <b>" + this.point.y + "<br/>Year: <b>"+this.point.x}
               #         return false;
               #         }'
               #   )
               # )
             #)
    ) %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text= ""))  %>% 
      hc_title(text=ind) %>%
      hc_tooltip(crosshairs = F, shared = F, useHTML=T) %>% 
      hc_plotOptions(enableMouseTracking = T) %>% 
      hc_chart(backgroundColor = "white") %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1000, sourceHeight=700,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) 
    
  })

  
  ## Heat map
  
  observe({
    x <- cod19 %>%
      # filter(country != input$iso3heat1) %>%
      arrange(country) %>%
      distinct(country) %>%
      pull(country)
    
    x2 <- c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
      "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , x)
    
    # x2[x2 != input$iso3heat1]
    x2<- setdiff(x2, input$iso3heat1)
    
    updateVirtualSelect("iso3heat2", label = "Select countries, areas or WHO regions to compare:", choices = x2, selected = c(x2[1], x2[2], x2[3]))
  })
  
  output$heatmap <- renderHighchart({
    
    # rank category
    heatdata <- heatdata %>% 
      filter(sex==input$sexheat & 
               country %in% c(input$iso3heat1, input$iso3heat2))

    # rank causes by country-specific death ranking
    cause_order <- heatdata %>%
      filter(country == input$iso3heat1) %>%
      arrange(-rank) %>% 
      pull(DIM_GHECAUSE_TITLE)
    
    heatdata$DIM_GHECAUSE_TITLE <- factor(heatdata$DIM_GHECAUSE_TITLE, levels = rev(cause_order))
    heatdata$country <- factor(heatdata$country, levels = c(input$iso3heat1, input$iso3heat2))
    
    heatdata %>% 
      hchart(type = "heatmap", hcaes(x=DIM_GHECAUSE_TITLE, y=country, value=rank), dataLabels = list(enabled = T, format = "{point.value}", style = list(color = "white"))) %>% 
      hc_colorAxis(
        minColor = "#EF3842",  
        # maxColor = "#F4A81D" # yellow
        # maxColor = "#F26829" # orange
        maxColor = "#009ADE" #"#80BC00" # green
      ) %>% 
      # hc_chart(backgroundColor = "white", spacingRight = 80) %>%
      hc_xAxis(title = list(text = ""), opposite = T,
               labels = list(
                 style = list(
                   textShadow=F,
                   fontSize = "13px"
                 )
               )) %>% 
      hc_yAxis(title = list(text = ""), reversed = T,
               labels = list(
                 style = list(
                   textShadow=F,
                   fontWeight = "bold",
                   fontSize = "14px"
                 )
               )) %>% 
      hc_legend(enabled = F) %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1000, sourceHeight=700,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) %>% 
      hc_tooltip(crosshairs = F, shared = F,
                 formatter= JS(
                   paste0('function() {
                   return this.point.country + "</b><br/>% of deaths: <b>" + this.point.prop_deaths
                       }'
                   )
                 )
      )
    
  })
  
  
  ## Comparative ranking
  codrankdatacomp1 <- reactive({
    
    if(input$yearrankcomp1==2021){   ###### CHANGE BACK TO 2019
      
      if(input$iso3comp1=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else if(input$iso3comp1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp1 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3comp1 & FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          mutate(region = 1) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
        
      }
    }else{
      if(input$iso3comp1=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else if(input$iso3comp1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp1 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3comp1 & FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          mutate(region = 1) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
        
      }
      
    }
  })
  
  codrankdatacomp2 <- reactive({
    
    if(input$yearrankcomp2==2021){ ######### CHANGE BACK TO 2019
      
      if(input$iso3comp2=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else if(input$iso3comp2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3comp2 & FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          mutate(region = 2) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
      }
      
    }else{
      
      if(input$iso3comp2=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else if(input$iso3comp2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3comp2 & FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>% 
          mutate(region = 2) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
      }
      
    }
  })
  
  codrankcomp1_graph <- reactive({
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    data_arrow <- codrankdatacomp1() %>%
      group_by(region) %>% 
      mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
             rank_deaths = rank(-prop_deaths, ties.method = "first")) %>%
      filter(rank_deaths < 11) %>%
      left_join(lev1, by = "FLAG_CAUSEGROUP")
    
    data_arrow$lev1_name <- factor(data_arrow$lev1_name, levels = lev1_causes)
    
    ggplot(data_arrow, aes(x=1, y=-rank_deaths)) +
      geom_label(aes(label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0.5, alpha=0.3, size=7, show.legend = F) +
      geom_label(aes(label = paste0(rank_deaths, ". ", DIM_GHECAUSE_TITLE), fill = lev1_name), hjust=0.5, alpha=0, size=7, label.size = NA, show.legend = F) +
      scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      theme_void() +
      labs(color = "", fill = "") +
      # scale_x_continuous(limits=c(0.95, 1.18)) +
      theme(plot.margin =  margin(0, 0, 0, 0, "cm")) 
  })
  
  codrankcomp2_graph <- reactive({
    
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    data_arrow <- codrankdatacomp2() %>%
      group_by(region) %>% 
      mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
             rank_deaths = rank(-prop_deaths, ties.method = "first")) %>%
      filter(rank_deaths < 11) %>%
      left_join(lev1, by = "FLAG_CAUSEGROUP")
    
    data_arrow$lev1_name <- factor(data_arrow$lev1_name, levels = lev1_causes)
    
    ggplot(data_arrow, aes(x=1, y=-rank_deaths)) +
      geom_label(aes(label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0.5, alpha=0.3, size=7, show.legend = F) +
      geom_label(aes(label = paste0(rank_deaths, ". ", DIM_GHECAUSE_TITLE), fill = lev1_name), hjust=0.5, alpha=0, size=7, label.size = NA, show.legend = F) +
      scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      theme_void() +
      labs(color = "", fill = "") +
      # scale_x_continuous(limits=c(0.95, 1.18))+
      theme(plot.margin =  margin(0, 0, 0, 0, "cm")) 
    
  })
  
  codrankforpdf <- reactive({ 
    patchwork::wrap_plots(
      codrankcomp1_graph() +
        ggtitle(paste0(input$iso3comp1, ", ", input$sexrankcomp1, ", ", input$yearrankcomp1)) + 
        theme(plot.title = element_text(hjust = 0.5, size = 12)), 
      codrankcomp2_graph() + 
        ggtitle(paste0(input$iso3comp2, ", ", input$sexrankcomp2, ", ", input$yearrankcomp2))+ 
        theme(plot.title = element_text(hjust = 0.5, size = 12)), 
      ncol = 2
      )
  })
  
  
  output$codrankcomp1 <- renderPlot({ codrankcomp1_graph() })
  
  output$codrankcomp2 <- renderPlot({ codrankcomp2_graph() })
  
  output$data_top10cod <- downloadHandler(
    filename = function() {
      paste("top10cod_compare", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        rbind(
          codrankdatacomp1() %>% 
            select(cause = DIM_GHECAUSE_TITLE, deaths_count = VAL_DEATHS_COUNT_NUMERIC) %>% 
            mutate(region = input$iso3comp1, sex = input$sexrankcomp2, year = input$yearrankcomp1), 
          codrankdatacomp2() %>% 
            select(cause = DIM_GHECAUSE_TITLE, deaths_count = VAL_DEATHS_COUNT_NUMERIC) %>% 
            mutate(region = input$iso3comp2, sex = input$sexrankcomp2, year = input$yearrankcomp2)
          ), 
        file, row.names = FALSE)
    }
  )
  
  output$jpeg_top10cod <- downloadHandler(
    filename = function() {
      paste("top10cod_compare", ".jpeg", sep = "")
    },
    content = function(file) {
      ggsave(codrankforpdf(), filename = file, width = 12, height = 8, units = "in", dpi = 100)
    }
  )
  
  output$pdf_top10cod <- downloadHandler(
    filename = function() {
      paste("top10cod_compare", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(codrankforpdf(), filename = file, width = 12, height = 8, units = "in")
    }
  )
  
  
  ## Comparative treemap
  comp1data <- reactive({
    if(input$yeartreecomp1==2021){
      if(input$iso3compt1=="Global"){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else{
        
        cod19 %>% filter(country == input$iso3compt1 & FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL")
        
      }
    }else{
      if(input$iso3compt1=="Global"){
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      }else{
        cod00 %>% filter(country == input$iso3compt1 & FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL")
      }
    }
    # }
  })
  comp1deaths <- reactive({
    if(input$yeartreecomp1==2021){
      if(input$iso3compt1=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        
        cod19 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3compt1 & FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }else{
      if(input$iso3compt1=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        
        cod00 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3compt1 & FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }
  })
  
  comp2data <- reactive({
    if(input$yeartreecomp2==2021){
      if(input$iso3compt2=="Global"){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else{
        
        cod19 %>% filter(country == input$iso3compt2 & FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL")
        
      }
    }else{
      if(input$iso3compt2=="Global"){
        
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else{
        
        cod00 %>% filter(country == input$iso3compt2 & FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL")
        
      }
    }

  })
  comp2deaths <- reactive({
    if(input$yeartreecomp2==2021){ 
      if(input$iso3compt2=="Global"){
        cod19 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        cod19 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else{
        cod19 %>%
          filter(FLAG_LEVEL == 0 & country == input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }else{
      if(input$iso3compt2=="Global"){
        cod00 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        cod00 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else{
        cod00 %>%
          filter(FLAG_LEVEL == 0 & country == input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE=="TOTAL") %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
      
    }
  })
  
  output$treemapcomp1 <- renderHighchart({
    
    country <- input$iso3compt1
    year <- input$yeartreecomp1
    deaths <- comp1deaths()
    
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    data_treemap <- comp1data() %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
    
    data_treemap$lev1_name <- factor(data_treemap$lev1_name, levels = lev1_causes)
    
    data_treemap <- data_treemap %>%
      mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
      mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " ")) %>% 
      mutate(deaths = round(VAL_DEATHS_COUNT_NUMERIC, digits = 0)) 
    
    test <- data_treemap %>% data_to_hierarchical(group_vars = c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#AACF7F", "#BE73AD")) 
    
    for (i in 1:4){
      test[[i]]$color <- case_when(
        test[[i]]$name == "Communicable, maternal, perinatal and nutritional conditions" ~ "#F6A27C",
        test[[i]]$name == "Noncommunicable diseases" ~ "#79B5E3",
        test[[i]]$name == "Injuries" ~ "#AACF7F",
        test[[i]]$name == "Other COVID-19 pandemic-related outcomes" ~ "#BE73AD"
      )
    }
    
    test %>% 
      # data_to_hierarchical(group_vars = c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#BE73AD", "#AACF7F")) %>% 
      hchart(type = "treemap", 
             allowTraversingTree = T,
             levelIsConstant = F,
             levels = list(
               list(level = 1, dataLabels = list(enabled = TRUE, format = "{point.name}<br>{point.value:,.0f}"), borderColor = "white", borderWidth = 2),
               list(level = 2, dataLabels = list(enabled = FALSE))
             )
      ) %>% 
      hc_title(text=paste0("Total deaths in ", year, ", ", country, ": ", format(deaths, big.mark = ",", scientific = F))) %>%
      hc_chart(backgroundColor = 'white')
  })
  
  output$treemapcomp2 <- renderHighchart({
    
    country <- input$iso3compt2
    year <- input$yeartreecomp2
    deaths <- comp2deaths()
    
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    data_treemap <- comp2data() %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
    
    data_treemap$lev1_name <- factor(data_treemap$lev1_name, levels = lev1_causes)
    
    data_treemap <- data_treemap %>%
      mutate(cause_fraction = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC)) %>%
      mutate(cause_title = paste(DIM_GHECAUSE_TITLE, sprintf("(%1.1f%%)", 100*cause_fraction), sep = " ")) %>% 
      mutate(deaths = round(VAL_DEATHS_COUNT_NUMERIC, digits = 0))
    
    test <- data_treemap %>% data_to_hierarchical(group_vars = c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#AACF7F", "#BE73AD")) 
    
    for (i in 1:4){
      test[[i]]$color <- case_when(
        test[[i]]$name == "Communicable, maternal, perinatal and nutritional conditions" ~ "#F6A27C",
        test[[i]]$name == "Noncommunicable diseases" ~ "#79B5E3",
        test[[i]]$name == "Injuries" ~ "#AACF7F",
        test[[i]]$name == "Other COVID-19 pandemic-related outcomes" ~ "#BE73AD"
      )
    }
    
    test %>% 
      # data_to_hierarchical(group_vars= c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#BE73AD", "#AACF7F")) %>% 
      hchart(type = "treemap", 
             allowTraversingTree = T,
             levelIsConstant = F,
             levels = list(
               list(level = 1, dataLabels = list(enabled = TRUE, format = "{point.name}<br>{point.value:,.0f}"), borderColor = "white", borderWidth = 2),
               list(level = 2, dataLabels = list(enabled = FALSE))
             )
      ) %>% 
      hc_title(text=paste0("Total deaths in ", year, ", ", country, ": ", format(deaths, big.mark = ",", scientific = F))) %>%    
      hc_chart(backgroundColor = 'white')
    
  })
  
  treemap_comp_graph_pdf <- reactive({
    
    # Selection 1
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    data_treemap <- comp1data() %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
    
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
        lev1_name=="Other pandemic related causes" ~ "#A6228C"
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
      labs(subtitle = paste0("Total deaths: ", format(comp1deaths(), big.mark = ",", scientific = F)),
           title = paste0(input$iso3compt1, ", ", input$sextreecomp1, ", ", input$yeartreecomp1))+
      theme(
        plot.margin = unit(c(0, 0, 0, 1.5), "cm"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
      )
    
    
    # 2000   

    data_treemap_2000 <- comp2data() %>% left_join(lev1, by = "FLAG_CAUSEGROUP") 
    
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
        lev1_name=="Other pandemic related causes" ~ "#A6228C"
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
      labs(subtitle = paste0("Total deaths: ", format(comp2deaths(), big.mark = ",", scientific = F)),
           title = paste0(input$iso3compt2, ", ", input$sextreecomp2, ", ", input$yeartreecomp2))+
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
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
      geom_text(aes(x = 4, y = 0.5, label = "Other pandemic related causes"), size=4.5, vjust = 1.1)+
      theme_void()+
      scale_y_continuous(limits = c(-1,2.5))
    
    final <- ggdraw() +
      draw_plot(patchwork::wrap_plots(tm2, tm1, ncol = 2), 0, 0.1, 1, 0.85) +
      draw_plot(legt, 0.02, 0.045, 0.95, 0.055)
    
    return(final)
  })
  
  output$data_treemap_bench <- downloadHandler(
    filename = function() {
      paste("treemap_compare", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        rbind(comp1data() %>% mutate(region = input$iso3compt1), 
              comp2data()%>% mutate(region = input$iso3compt1)), 
        file, row.names = FALSE)
    }
  )
  
  output$jpeg_treemap_bench <- downloadHandler(
    filename = function() {
      paste("treemap_compare", ".jpeg", sep = "")
    },
    content = function(file) {
      ggsave(treemap_comp_graph_pdf(), filename = file, width = 15, height = 7, units = "in", dpi = 100)
    }
  )
  
  output$pdf_treemap_bench <- downloadHandler(
    filename = function() {
      paste("treemap_compare", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(treemap_comp_graph_pdf(), filename = file, width = 15, height = 7, units = "in",)
    }
  )
  
  
  ## Countries with highest burden of causes of death
  
  observe({
    x <- cod19 %>% filter(FLAG_LEVEL== as.numeric(input$levelheatv2)) %>% distinct(DIM_GHECAUSE_TITLE) %>% pull()
    
    updateVirtualSelect("codheatv2", "Select cause of death:", choices = x, selected = x[1])
  })
  
  output$heatmapv2_year1 <- renderUI({
    
    var <- case_when(
      input$typeheatv2 == "Number of deaths" ~ "deaths",
      input$typeheatv2 == "Mortality rate" ~ "mort_rate"
    )
    name <- input$typeheatv2

    # rank category
    heatdatav2_year1 <- heatdatav2 %>% 
      filter(sex==input$sexheatv2 & 
               DIM_GHECAUSE_TITLE == input$codheatv2 &
               year == 2000) %>% 
      arrange(desc(get(var))) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number(),
             deaths = formatC(round(deaths, 0), big.mark=",", format = "d"),
             # text = str_wrap(paste0(rank, ". ", country, " - ", get(var)), 45),
             Value = get(var)) %>% 
      select(Country = country, Value) %>% 
      rename(!!name:=Value)
    
   heatdatav2_year1 %>% 
     flextable() %>% 
     bg(., j = 1:2, bg = "white", part = "all") %>% 
     align(., j=c(2), align = "center") %>% 
     align(., j=2, align = "center", part = "header") %>% 
     width(., j=1, width=7, unit = "cm") %>%
     width(., j=2, width=4, unit = "cm") %>%
     htmltools_value()

  }) %>% bindEvent(input$readyburden)
  
  output$heatmapv2_year2 <- renderUI({
    
    var <- case_when(
      input$typeheatv2 == "Number of deaths" ~ "deaths",
      input$typeheatv2 == "Mortality rate" ~ "mort_rate"
    )
    name <- input$typeheatv2
  
    # rank category
    heatdatav2_year2 <- heatdatav2 %>% 
      filter(sex==input$sexheatv2 & 
               DIM_GHECAUSE_TITLE == input$codheatv2 &
               year == 2021) %>% 
      arrange(desc(get(var))) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number(),
             deaths = formatC(round(deaths, 0), big.mark=",", format = "d"),
             # text = str_wrap(paste0(rank, ". ", country, " - ", get(var)), 45),
             Value = get(var)) %>% 
      select(Country = country, Value) %>% 
      rename(!!name:=Value)
    
    heatdatav2_year2 %>% 
      flextable() %>% 
      bg(., j = 1:2, bg = "white", part = "all") %>% 
      align(., j=c(2), align = "center") %>% 
      align(., j=2, align = "center", part = "header") %>% 
      width(., j=1, width=7, unit = "cm") %>%
      width(., j=2, width=4, unit = "cm") %>%
      bg(., j = 1:2, bg = "white", part = "body") %>% 
      htmltools_value()
    
  }) %>% bindEvent(input$readyburden)
  
  bump_cod_data <- reactive({
    
    var <- case_when(
      input$typeheatv2 == "Number of deaths" ~ "deaths",
      input$typeheatv2 == "Mortality rate" ~ "mort_rate"
    )
    
    heatdatav2_year1 <- heatdatav2 %>% 
      filter(sex==input$sexheatv2 & 
               DIM_GHECAUSE_TITLE == input$codheatv2 &
               year == 2000) %>% 
      arrange(desc(get(var))) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number()) 
    
    heatdatav2_year2 <- heatdatav2 %>% 
      filter(sex==input$sexheatv2 & 
               DIM_GHECAUSE_TITLE == input$codheatv2 &
               year == 2021) %>% 
      arrange(desc(get(var))) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number()) 
    
    df <- rbind(heatdatav2_year1, heatdatav2_year2)
    
    df_compare <- full_join(
      heatdatav2_year1 %>% select(rank1 = rank, country), 
      heatdatav2_year2 %>% select(rank2 = rank, country)) %>% 
      mutate(type = case_when(
        rank2<rank1 ~ "increase",
        rank2>rank1 ~ "decrease",
        rank2==rank1 ~ "same"
      ))
    
    df_<- df %>% 
      left_join(df_compare %>% select(country, type)) %>% 
      mutate(year = ifelse(year==2021, 2001, year)) 
    
    return(df_)
  })
  
  bump_cod_graph <- reactive({
    
    bump_cod_data() %>% 
      ggplot(aes(x = year, y = rev(rank), group = country, color = type)) +
      geom_bump(size = 1.5, show.legend = F, alpha = 0.7) +
      geom_point(size = 2, show.legend = F, alpha = 0.8) +
      scale_x_continuous(labels = c("2000", "2021"), breaks = c(2000, 2001))+
      scale_color_manual(
        values = c("decrease" = "#80BC00",
                   "increase" = "#F26829",
                   "same" = "#F4A81D")
      ) + 
      theme_void() +
      theme(plot.margin = unit(c(1.6,0,0,0), "cm"))
    
  })
  
  bump_cod_graph_pdf <- reactive({
    
    var <- case_when(
      input$typeheatv2 == "Number of deaths" ~ "deaths",
      input$typeheatv2 == "Mortality rate" ~ "mort_rate"
    )
    
    bump_cod_graph() +
      scale_x_continuous(limits = c(1999, 2002))+
      # theme(plot.margin = unit(c(0,0,0,0), "cm")) +
      geom_text(data = bump_cod_data() %>% filter(year == 2000),
                aes(x = year, y = rev(rank), label = paste0(country, " (", formatC(round(get(var), 1), big.mark=",", format = "d"), ")  ")),
                size = 4, hjust = 1, show.legend = F) +
      geom_text(data = bump_cod_data() %>% filter(year == 2001),
                aes(x = year, y = rev(rank), label = paste0("  ", country, " (", formatC(round(get(var), 1), big.mark=",", format = "d"), ")")),
                size = 4, hjust = 0, show.legend = F) +
      geom_text(aes(x = 2000, y = 21, label = paste0("2000")),
                size = 4, hjust = 0.75, show.legend = F) +
      geom_text(aes(x = 2001, y = 21, label = paste0("2021")),
                size = 4, hjust = 0, show.legend = F) +
      # theme(legend.position = "bottom") +
      ggtitle(paste0(input$typeheatv2, ": ", input$codheatv2, ", ", input$sexheatv2)) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
    
  })
  
  output$bump <- renderPlot({ bump_cod_graph() }) %>% bindEvent(input$readyburden)
  
  output$data_bump_cod <- downloadHandler(
    filename = function() {
      paste("topcountries_cod", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(bump_cod_data(), file, row.names = FALSE)
    }
  )
  
  output$jpeg_bump_cod <- downloadHandler(
    filename = function() {
      paste("topcountries_cod", ".jpeg", sep = "")
    },
    content = function(file) {
      ggsave(bump_cod_graph_pdf(), filename = file, width = 12, height = 8, units = "in", dpi = 100)
    }
  )
  
  output$pdf_bump_cod <- downloadHandler(
    filename = function() {
      paste("topcountries_cod", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(bump_cod_graph_pdf(), filename = file, width = 12, height = 8, units = "in")
    }
  )
  
  ## SDG Indicator progress between countries
  
  sdgy1 <- reactive({
    
    small_best <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(small_is_best) %>% pull()
    if(small_best==T){
      filtered_indicator_values %>% 
        filter(year==2018 & indicator_name==input$sdgind) %>% 
        arrange(desc(value)) %>% 
        mutate(value = round(value, 1)) %>% 
        filter(row_number()<21)
    }else{
      filtered_indicator_values %>% 
        filter(year==2018 & indicator_name==input$sdgind) %>% 
        arrange(value) %>% 
        mutate(value = round(value, 1)) %>% 
        filter(row_number()<21)
    }
  })
  output$sdgtablev2_y1 <- renderUI({
    name <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(units) %>% pull()
    
     sdgy1() %>% 
      select(Country = country, value) %>% 
      rename(!!name:=value) %>% 
      flextable() %>% 
      bg(., j = 1:2, bg = "white", part = "all") %>% 
      align(., j=c(2), align = "center") %>% 
      align(., j=2, align = "center", part = "header") %>% 
      width(., j=1, width=7, unit = "cm") %>%
      width(., j=2, width=3, unit = "cm") %>%
      bg(., j = 1:2, bg = "white", part = "body") %>% 
      htmltools_value()
    
  })
  
  sdgy2 <- reactive({
    
    small_best <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(small_is_best) %>% pull()
    if(small_best==T){
      filtered_indicator_values %>% 
        filter(year==2021 & indicator_name==input$sdgind) %>% 
        arrange(desc(value)) %>%
        mutate(value = round(value, 1)) %>% 
        filter(row_number()<21)
    }else{
      filtered_indicator_values %>% 
        filter(year==2021 & indicator_name==input$sdgind) %>% 
        arrange(value) %>% 
        mutate(value = round(value, 1)) %>% 
        filter(row_number()<21)
    }

  })
  output$sdgtablev2_y2 <- renderUI({
    name <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(units) %>% pull()
    
    sdgy2() %>% 
      select(Country = country, value) %>% 
      rename(!!name:=value) %>% 
      flextable() %>% 
      bg(., j = 1:2, bg = "white", part = "all") %>% 
      align(., j=c(2), align = "center") %>% 
      align(., j=2, align = "center", part = "header") %>% 
      width(., j=1, width=7, unit = "cm") %>%
      width(., j=2, width=3, unit = "cm") %>%
      bg(., j = 1:2, bg = "white", part = "body") %>% 
      htmltools_value()
    
  })
  
  sdgbump_data <- reactive({
    sdgy1 <- filtered_indicator_values %>% 
      filter(year==2018 & indicator_name==input$sdgind) %>% 
      arrange(desc(value)) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number()) 
    
    sdgy2 <- filtered_indicator_values %>% 
      filter(year==2021 & indicator_name==input$sdgind) %>% 
      arrange(desc(value)) %>% 
      filter(row_number()<21) %>% 
      mutate(rank = row_number()) 
    
    df <- rbind(sdgy1, sdgy2)
    
    df_compare <- full_join(
      sdgy1 %>% select(rank1 = rank, country), 
      sdgy2 %>% select(rank2 = rank, country)) %>% 
      mutate(type = case_when(
        rank2<rank1 ~ "increase",
        rank2>rank1 ~ "decrease",
        rank2==rank1 ~ "same"
      ))
    
    df_<- df %>% 
      left_join(df_compare %>% select(country, type))
    
    return(df_)
  })
  
  sdgbump_graph <- reactive({
    
    sdgbump_data() %>% 
      ggplot(aes(x = year, y = rev(rank), group = country, color = type)) +
      ggbump::geom_bump(size = 1.5, show.legend = F, alpha = 0.7) +
      geom_point(size = 2, show.legend = F, alpha = 0.8) +
      scale_color_manual(
        values = c("decrease" = "#80BC00",
                   "increase" = "#F26829",
                   "same" = "#F4A81D")
      ) + 
      theme_void() +
      theme(plot.margin = unit(c(1.6,0,0,0), "cm"))
    
  })
  
  sdgbump_graph_pdf <- reactive({
    
    sdgbump_graph() +
      scale_x_continuous(limits = c(2015, 2024))+
      # theme(plot.margin = unit(c(0,0,0,0), "cm")) +
      geom_text(data = sdgbump_data() %>% filter(year == 2018),
                aes(x = year, y = rev(rank), label = paste0(country, " (", formatC(round(value, 1), big.mark=",", format = "d"), ")   ")),
                size = 4, hjust = 1, show.legend = F) +
      geom_text(data = sdgbump_data() %>% filter(year == 2021),
                aes(x = year, y = rev(rank), label = paste0("  ", country, " (", formatC(round(value, 1), big.mark=",", format = "d"), ")")),
                size = 4, hjust = 0, show.legend = F) +
      geom_text(aes(x = 2018, y = 21, label = paste0("2018")),
                size = 4, hjust = 0.75, show.legend = F) +
      geom_text(aes(x = 2021, y = 21, label = paste0("2021")),
                size = 4, hjust = 0, show.legend = F) +
      # theme(legend.position = "bottom") +
      ggtitle(input$sdgind) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  })
  
  output$sdgbump <- renderPlot({ sdgbump_graph() })
  
  output$data_sdgbump <- downloadHandler(
    filename = function() {
      paste("topcountries_sdg", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sdgbump_data(), file, row.names = FALSE)
    }
  )
  
  output$jpeg_sdgbump <- downloadHandler(
    filename = function() {
      paste("topcountries_sdg", ".jpeg", sep = "")
    },
    content = function(file) {
      ggsave(sdgbump_graph_pdf(), filename = file, width = 10, height = 8, units = "in", dpi = 100)
    }
  )
  
  output$pdf_sdgbump <- downloadHandler(
    filename = function() {
      paste("topcountries_sdg", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(sdgbump_graph_pdf(), filename = file, width = 12, height = 8, units = "in")
    }
  )
  
  
  #################################################################################################################
  ##### Maternal Mortality ########
  #################################################################################################################
  
  # Map of MMR 
  output$mmrmap <- renderPlot({
    
    mmr_2020_afr <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" &
               Period==2020) %>% 
      select(ISO_3_CODE = SpatialDimValueCode, Value = FactValueNumeric, ParentLocationCode, Location)
    
    poly_data_afr <- poly %>% 
      mutate(region = stringr::str_remove(WHO_REGION, "O$")) %>% 
      filter(region=="AFR") %>%
      left_join(mmr_2020_afr, by = "ISO_3_CODE")
    
    ggplot() +
      geom_sf(data = poly_data_afr, aes(fill = Value), color = "black") +
      theme_void() +
      labs(fill = "") +
      scale_fill_distiller(palette = "RdYlBu",
                           limits = c(0,1200), 
                           breaks = c(70, 300, 600, 900, 1200),
                           name="",
                           na.value = "grey50",
                           direction = -1)+
      theme(
        legend.position = "top",
        legend.text = element_text(size = 18),
        legend.key.size = unit(1.5, "cm")
      )
    
  })
  
  # Map of time to reach MMR target
  output$mmrtarget <- renderPlot({
    
    mmr_roc <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026") %>% 
      filter(Period==2000 | Period==2020) %>% 
      select(year = Period, iso3= SpatialDimValueCode, country = Location, mmr = FactValueNumeric) %>% 
      arrange(year) %>% 
      pivot_wider(names_from = year, values_from = mmr) %>% 
      mutate(roc = (log(`2020`/`2000`)) / 20) %>% 
      mutate(n_years = (log(70/`2020`)) / roc) %>% 
      mutate(n_years = ifelse(`2020`<70, 0, n_years)) %>%
      mutate(n_years_limit = ifelse(roc<0 & n_years>100, 100, n_years)) %>%
      mutate(roc_pos = ifelse(roc>0, TRUE, FALSE))
    
    mmr_years <- poly %>% 
      mutate(region = stringr::str_remove(WHO_REGION, "O$")) %>% 
      filter(region=="AFR") %>%
      left_join(
        mmr_roc %>% select(iso3, country, roc, `2000`, `2020`, n_years, n_years_limit, roc_pos),
        by = c("ISO_3_CODE" = "iso3")) 
    
    ggplot() +
      geom_sf(data = mmr_years , 
              aes(fill=n_years_limit), color = "black") +
      scale_fill_distiller(palette = "RdYlBu",
                           limits = c(0,100), 
                           breaks = c(0, 25, 50, 75, 100), 
                           labels = c("0", "25", "50", "75", ">=100"),
                           name="",
                           na.value = "grey50",
                           direction = -1)+
      new_scale("fill") +
      geom_sf(data = mmr_years %>% filter(roc_pos==TRUE),
              aes(fill= roc_pos), color = "black", show.legend = TRUE) +
      scale_fill_manual(values = "grey50", labels = c(str_wrap("Countries with increasing MMR, 2000-2020", 20)), name=" ")+
      theme_void() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 18),
        legend.key.size = unit(1.5, "cm")
      )
  })
  
  # Annualized rate of Change
  output$aroc <- renderHighchart({
    
    mmr_rate <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" &
               Period>=2000 &
               Location %in% input$iso3aroc) %>% 
      select(year = Period, iso3= SpatialDimValueCode, country = Location, mmr = FactValueNumeric) %>% 
      arrange(year)
    
    countries <- unique(mmr_rate$iso3)
    
    mmr_rate2000 <- mmr_rate %>% 
      filter(year==2000 | year==2015) %>% 
      pivot_wider(names_from = year, values_from = mmr) %>% 
      mutate(roc = (log(`2015`/`2000`)) / 15)%>% 
      select(iso3, country, `2000-2015` =roc) 
    
    mmr_rate2020 <- mmr_rate %>% 
      filter(year==2016 | year==2020) %>% 
      pivot_wider(names_from = year, values_from = mmr) %>% 
      mutate(roc = (log(`2020`/`2016`)) / 5) %>% 
      select(iso3, country, `2016-2020` = roc) 
    
    mmr2020 <- mmr_rate %>% 
      filter(year==2020)
    
    # Maternal conditions
    mc <- cod19 %>% 
      filter(DIM_COUNTRY_CODE %in% countries &
               DIM_AGEGROUP_CODE=="TOTAL" &
               sex=="Both sexes") %>% 
      filter(DIM_GHECAUSE_CODE==0 | DIM_GHECAUSE_CODE==420) %>% 
      select(iso3 = DIM_COUNTRY_CODE, deaths = VAL_DEATHS_COUNT_NUMERIC, DIM_GHECAUSE_TITLE) %>% 
      pivot_wider(names_from = DIM_GHECAUSE_TITLE, values_from = deaths) %>% 
      mutate(percent_mc = (`Maternal conditions`/`All Causes`)*100) %>% 
      mutate(group_mc = case_when(percent_mc<1 ~ "<1%",
                                  percent_mc>=1 & percent_mc<=2 ~ "1-2%",
                                  percent_mc>2 ~ ">2%"))
    
    df <- mc %>% 
      left_join(mmr_rate2000) %>% 
      left_join(mmr_rate2020) %>% 
      left_join(mmr2020) %>% 
      mutate(`2000-2015` = round(`2000-2015`, 3),
             `2016-2020` = round(`2016-2020`, 3))
    
    df$group_mc <- factor(df$group_mc, levels = c("<1%", "1-2%", ">2%"))
    
    
    df %>% 
      hchart('scatter', hcaes(x = `2000-2015`, y = `2016-2020`, size = mmr, group = group_mc), 
             maxSize = "10%", color = c("#80BC00", "#009ADE","#EF3842"),
             dataLabels = list(
               enabled = TRUE,
               format = '{point.country}',
               # format = '<span style="color: ' + point.color + '">' + point.name + ': ' + point.y + '%</span>',
               color = '{this.point.color}',
               style = list(
                 textShadow=F,
                 fontSize = "14px"
               )
             )) %>% 
      hc_xAxis(title = list(text = "Annualized rate of change, MMR, 2000-2015"),
               min = -0.152, max = 0.03,
               plotLines = list(
                 list(color = "#252525",
                      width = 1,
                      value = 0)
               )) %>%
      hc_yAxis(title = list(text= "Annualized rate of change, MMR, 2016-2020"),
               min = -0.105, max = 0.105,
               plotLines = list(
                 list(color = "#252525",
                      width = 1,
                      value = 0)
               ))  %>% 
      # hc_plotOptions(
      #   scatter = list(showInLegend = TRUE)
      # ) %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = -0.145, y = 0.11, xAxis = 0, yAxis = 0), text = "Increasing MMR", shape = "square", style = list(fontSize ="12px")),
            list(point = list(x = -0.145, y = -0.12, xAxis = 0, yAxis = 0), text = "Decreasing MMR", shape = "square", style = list(fontSize ="12px")),
            list(point = list(x = 0.02, y = -0.12, xAxis = 0, yAxis = 0), text = "Increasing MMR", shape = "square", style = list(fontSize ="12px"))
          ),
          draggable = ''
        )
      ) %>%
      hc_legend(title = list(text = "Deaths due to maternal conditions, 2021", style = list(fontSize = "14px")),
                symbolWidth = 70,
                symbolHeight = 25,
                itemStyle = list(fontSize = "14px", color = "black")
      ) %>% 
      # hc_legend(
      #   # maxHeight=100,
      #   # width = "100%",
      #   # itemWidth= 100
      #   symbolWidth = 300,
      #   symbolHeight = 25
      #   # itemStyle = list(fontSize = "1.0em")
      # ) %>%
      hc_tooltip(crosshairs = F, shared = F, useHTML=T,
                 formatter= JS(
                   paste0('function() {
                  return this.point.country +
                  "</b><br/>MMR, 2020: " + this.point.mmr +
                  "</b><br/>MMR AROC, 2000-2015: " + this.point.x + 
                  "</b><br/>MMR AROC, 2016-2020: " + this.point.y +
                  "</b><br/>Deaths due to maternal conditions, 2021: " + this.point.group_mc
                          }'
                   )
                 )) %>% 
      hc_plotOptions(enableMouseTracking = T) %>% 
      hc_chart(backgroundColor = "white") %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1000, sourceHeight=700,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV"))))
    
  }) 
  
  
  # Benchmark Bar graph
  output$benchbar <- renderPlot({
    
    mmr_region_ <- mmr_region %>% 
      filter(Year==2020) %>% 
      mutate(mmr = as.numeric(str_extract(Maternal.mortality.ratio..per.100.000.live.births., "^[:digit:]+"))) %>% 
      select(WHO.region, Year, mmr) 
    
    mmr_ <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" &
               Location==input$iso3cp & 
               Period==2020) %>% 
      select(Year = Period, WHO.region = Location, mmr = FactValueNumeric) 
    
    mmr_top_reg_ <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" &
               Period==2020) %>% 
      arrange(Period, -FactValueNumeric) %>% 
      group_by(Period) %>% filter(row_number()==1) %>% ungroup()%>% 
      select(Year = Period, mmr = FactValueNumeric, WHO.region = Location)
    
    mmr_low_reg_ <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" &
               Period==2020) %>% 
      arrange(Period, FactValueNumeric) %>% 
      group_by(Period) %>% filter(row_number()==1) %>% ungroup() %>% 
      select(Year = Period, mmr = FactValueNumeric, WHO.region = Location)
    
    country_low <- mmr_low_reg_ %>% pull(WHO.region)
    country_high <- mmr_top_reg_ %>% pull(WHO.region)
    iso3_low <- data_mmr %>% filter(Location==country_low) %>% distinct(SpatialDimValueCode) %>% pull(SpatialDimValueCode)
    iso3_high <- data_mmr %>% filter(Location==country_high) %>% distinct(SpatialDimValueCode) %>% pull(SpatialDimValueCode)
    
    test <- rbind(mmr_, mmr_region_, mmr_low_reg_, mmr_top_reg_) 
    
    mmr_region_2000 <- mmr_region %>% 
      filter(Year==2000) %>% 
      mutate(mmr = as.numeric(str_extract(Maternal.mortality.ratio..per.100.000.live.births., "^[:digit:]+"))) %>% 
      select(WHO.region, Year, mmr) 
    
    mmr_2000 <- data_mmr %>% 
      filter(IndicatorCode == "MDG_0000000026" & Period==2000 &
               Location %in% c(input$iso3cp, country_low, country_high)
      ) %>% 
      select(Year = Period, WHO.region = Location, mmr = FactValueNumeric)   
    
    
    test3 <- rbind(test, mmr_2000, mmr_region_2000) %>% 
      arrange(Year, WHO.region) %>%
      mutate(rank = case_when(WHO.region== country_low ~ paste0(country_low, " (region lowest)"),
                              WHO.region== country_high ~ paste0(country_high, " (region highest)"))) %>%
      mutate(country = coalesce(rank, WHO.region)) %>% 
      arrange(country)
    
    country_low_ <- test3 %>% distinct(WHO.region,country) %>% filter(WHO.region==country_low) %>% pull(country)
    country_high_ <- test3 %>% distinct(WHO.region,country) %>% filter(WHO.region==country_high) %>% pull(country)
    
    test3$country <- factor(test3$country, levels = c(input$iso3cp, "Africa", "Global", country_low_, country_high_))
    
    test3 <- test3 %>% 
      mutate(mmr_ = round(mmr, digits = 0)) %>% 
      rename(mmr = mmr_, mmr_prev = mmr)
    diff <- test3 %>%
      select(Year, country, mmr) %>%
      pivot_wider(names_from = Year, values_from = mmr) %>%
      mutate(mmr = `2000` - `2020`,
             Year = 2000) %>%
      select(Year, country, mmr)
    
    test4 <- rbind(test3 %>% filter(Year==2020) %>% select(country, Year, mmr), diff)
    
    plot <- ggplot() +
      # geom_bar(data = test4, aes(x=country, y=mmr, color = country, fill = country), stat="identity", position = "stack", width = 0.5, alpha = 0, show.legend = FALSE, size = 1)+
      geom_bar(data = test3 %>% filter(Year==2000), aes(x=country, y=mmr, fill = country, color = country), stat="identity", width = 0.5, alpha=0.4, show.legend = FALSE) +
      geom_bar(data = test3 %>% filter(Year==2020), aes(x=country, y=mmr, fill = country, color = country), stat="identity", width = 0.5, alpha=0.7, show.legend = FALSE) +
      geom_point(data = test3 %>% distinct(country) %>% mutate(target=70), aes(x=country, y = target), shape = 108, size = 9, color = "#00205C")+
      geom_text(data = test3 %>% filter(Year==2020), aes(x=country, y=mmr, label = mmr), hjust = 1.3, fontface = "bold", size = 7)+
      geom_text(data = test3 %>% filter(Year==2000), aes(x=country, y=mmr, label = mmr), hjust = -0.8, size = 7)+
      coord_flip()+
      theme_classic() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 22),
        axis.title.x = element_text(size=22, margin = margin(t = 15, r = 0, b = 0, l = 0), color = "black"),
        axis.title.y = element_text(size=22, color = "black"),
        axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        plot.margin = margin(0, 0, 0, 50)
      ) +
      ylab("Maternal Mortality Ratio (per 100,000 live births)")+ 
      xlab("")+
      scale_x_discrete(limits=rev)+
      scale_y_continuous(breaks = c(0, 70, 500, 1000, 1500), limits = c(0, 1800))+
      scale_fill_manual(values = c("#F4A81D", "#80BC00", "#A6228C", "#009ADE", "#F26829")) +
      scale_color_manual(values = c("#F4A81D", "#80BC00", "#A6228C", "#009ADE", "#F26829")) 
    
    
    leg <- ggplot() +
      geom_tile(aes(x = 1, y = 1), fill = "#EF7C47")+
      geom_tile(aes(x = 4.4, y = 1), fill = "#EFA685", color = "#EF7C47", size = 6)+
      geom_text(aes(x = 2.6, y = 1, label = "MMR in 2020"), fontface = "bold", size=6)+
      geom_text(aes(x = 7.2, y = 1, label = "Change in MMR, 2000-2020"), size=6)+
      
      geom_tile(aes(x = 9.5, y = 1, width = 0.1), fill = "#00205C")+
      geom_text(aes(x = 11.1, y = 1, label = "SDG target on MMR"), size=6)+
      
      theme_void()+
      scale_x_continuous(limits = c(0, 13.5))
    
    ggdraw() +
      draw_plot(plot, 0, 0.1, 1, 0.9) +
      draw_plot(leg, 0.25, 0.02, 0.6, 0.07)
  }) %>% bindCache(input$iso3cp)
  
  
  # Trajectories
  output$mmrtraj <- renderPlot({
    
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
    
    final <- final_ %>% 
      filter(country==input$iso3cp) %>% 
      distinct() %>% 
      mutate(text_ = str_extract(scenario, "(?<=\\().+")) %>% 
      mutate(text = ifelse(str_detect(text_, "percentile"), 
                           paste0(round(mmr, digits = 0), " (Best ", text_),
                           paste0(round(mmr, digits = 0), " (", text_))) %>% 
      filter(text_!="60th percentile)" & text_!="70th percentile)" & text_!="80th percentile)") %>% 
      mutate(country_name = country)
    
    final_other <- final_ %>% 
      filter(country %in% input$iso3traj) %>%
      filter(str_detect(scenario, "Current rate of change")) %>% 
      select(year, mmr, WHO.region=country)
    
    max <- final %>% filter(str_detect(scenario, "Current")) %>% arrange(desc(mmr)) %>% filter(row_number()==1)
    max <- plyr::round_any(max$mmr, 10, f = ceiling)
    
    max <- ifelse(max<800, 800, max)
    
    colors <- c(
      "Current rate of change)" = "black",
      "Fastest rate of change in region)" = "#80BC00",
      "90th percentile)" = "#009ADE",
      "50th percentile)" = "#EF3842"
    )
    
    ggplot()+
      geom_line(data = mmr_region__, aes(x=year, y=mmr, group=WHO.region, linetype=WHO.region), show.legend = FALSE)+
      geom_line(data = final_other, aes(x=year, y=mmr, group=WHO.region), show.legend = FALSE)+
      geom_line(data = final, aes(x=year, y=mmr, group =text_, color = text_), show.legend = FALSE)+
      geom_text(data = final %>% filter(year==2030), 
                aes(x=year, y = mmr, color = text_, label= str_wrap(text, 28)), 
                show.legend = FALSE, size = 5.5, hjust = -0.05)+
      geom_text(data = mmr_region__ %>% filter(year==2001), 
                aes(x=year, y = mmr, label= WHO.region), 
                show.legend = FALSE, size = 5.5, vjust = 1.5)+
      geom_text(data = final_other %>% filter(year==2001), 
                aes(x=year, y = mmr, label= WHO.region), 
                show.legend = FALSE, size = 5.5, vjust = 1.5)+
      geom_text(data = final %>% filter(year==2001),
                aes(x=year, y = mmr, label=str_wrap(country_name, 10)),
                show.legend = FALSE, size = 5.5, vjust = 1.5)+
      geom_hline(yintercept = 70, color="#A6228C")+
      geom_label(aes(x=2005,y=70,label = as.character("SDG target: 70 per 100,000 live births")), show.legend = FALSE, 
                 size = 5.5, vjust = -0.1, family = "calibri", label.size = NA, color="#A6228C")+
      theme_classic()+
      theme(
        text = element_text(size = 14),
        axis.title.x = element_text(size=16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=16, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size = 12),
        plot.margin = margin(1,0,0.5,1, "cm")
        # legend.position = "bottom",
        # legend.margin=margin(t = 0, b=0, unit='cm')
      )+
      ylab("Maternal Mortality Ratio (per 100,000 live births)") +
      xlab("Year")+
      scale_color_manual(values = colors)+
      scale_linetype_manual(values = c("Global" = "dashed", "Africa" = "twodash"))+
      scale_y_continuous(expand = c(0,0), limits = c(0, max))+
      scale_x_continuous(limits = c(2000, 2037), labels = c(2000, 2010, 2020, 2030), breaks = c(2000, 2010, 2020, 2030))
    
  }) 
  # %>% bindCache(input$iso3cp)
  
  output$mmrtraj2 <- renderHighchart({
    
    final <- final_ %>% 
      filter(country==input$iso3cp) %>% 
      distinct() %>% 
      mutate(text_ = str_extract(scenario, "(?<=\\().+")) %>% 
      mutate(text = ifelse(str_detect(text_, "percentile"), 
                           paste0(round(mmr, digits = 0), " (Best ", text_),
                           paste0(round(mmr, digits = 0), " (", text_))) %>%
      mutate(text_ = str_remove(text_, "\\)$")) %>% 
      filter(text_!="60th percentile" & text_!="70th percentile" & text_!="80th percentile") %>% 
      mutate(country_name = country)
    
    mmr_region__ %>% 
      arrange(WHO.region, year) %>% 
      hchart('line', hcaes(x = year, y = mmr, group = WHO.region)
      ) %>% 
      hc_add_series(final %>% 
                      mutate(text = paste0(input$iso3cp, " trajectory using ", text_)), 
                    type = "line", hcaes(x = year, y = mmr, group = text)) %>% 
      # hc_add_series(final %>% filter(text_=="90th percentile)"), 
      #               type = "line", 
      #               hcaes(x = year, y = mmr), name = paste(input$iso3cp, "trajectory using best 90th percentile"), color = "green") %>%
      # hc_add_series(final %>% filter(text_=="50th percentile)"), 
      #               type = "line", 
      #               hcaes(x = year, y = mmr), name = paste(input$iso3cp, "trajectory using best 50th percentile"), color = "orange") %>%
      hc_xAxis(title = list(text = ""),
               plotLines = list(
                 list(color = "#252525",
                      width = 1,
                      value = 2020)
               )) %>%
      hc_yAxis(title = list(text= "Maternal Mortality Ratio (per 100,000 live births)"),
               min = 0,
               plotLines = list(
                 list(color = "#A6228C",
                      width = 2,
                      value = 70)
               )) %>%    
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 2020, y = 800, xAxis = 0, yAxis = 0), text = "Trajectories begin at 2020", shape = "square", style = list(fontSize ="14px")),
            list(point = list(x = 2001, y = 70, xAxis = 0, yAxis = 0), text = "SDG target: 70 per 100,000 live births", shape = "square", 
                 style = list(fontSize ="14px", color = "black"), backgroundColor = "white", borderColor = "#A6228C")
          ),
          draggable = ''
        )
      ) %>% 
      hc_title(text="") %>%
      hc_tooltip(crosshairs = F, shared = F, useHTML=T) %>% 
      hc_plotOptions(enableMouseTracking = T) %>% 
      hc_chart(backgroundColor = "white") %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1000, sourceHeight=700,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) 
    
  })
  
  
  # RMNCH
  output$rmnch <- renderHighchart({
    
    data = rmnch_est %>% filter(indicator=="rmnch") %>% filter(country==input$iso3cp) %>% mutate(median = round(median, 1))
    fill_var = "median"
    subtitle_name = "Median Percentage (%)"
    low_color = "#EF3842" 
    high_color = "#009ADE"
    
    max <- plyr::round_any(max(data[[fill_var]]), 10, f = ceiling)
    min <- plyr::round_any(min(data[[fill_var]]), 10, f = floor)
    
    map <- paste0("map", data %>% distinct(iso3) %>% pull())
    
    highchart() %>%
      hc_chart(backgroundColor = "white") %>% 
      hc_add_series_map(get(map), data , value = "median", joinBy = "region_", name = "Median Percentage (%)",
                        dataLabels = list(
                          enabled = TRUE,
                          format = "{point.region_:}",
                          style = list(
                            textShadow=F,
                            fontSize = "16px"
                          )
                        )) %>% 
      hc_colorAxis(
        minColor = low_color,
        maxColor = high_color,
        max = max,
        min = min,
        labels = list(style = list(fontSize = "16px", color = "black"))
        # width= '50%',
        # height= 100
      ) %>%
      hc_legend(
        # maxHeight=100,
        # width = "100%",
        # itemWidth= 100
        symbolWidth = 300,
        symbolHeight = 25
        # itemStyle = list(fontSize = "1.0em")
      ) %>% 
      hc_exporting(enabled = TRUE, sourceWidth=1200, sourceHeight=600,
                   buttons=list(contextButton=list(menuItems=c("downloadJPEG","downloadPDF", "downloadCSV")))) %>%
      # hc_subtitle(text = subtitle_name) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_tooltip(crosshairs = F, shared = F,
                 formatter= JS(
                   paste0('function() {
                   return this.point.region_ + "</b><br/>Coverage of RMNCH: " + this.point.value + "%"
                       }'
                   )
                 )
      )
    
    
  }) %>% bindCache(input$iso3cp)
  
  
  # Generate report
  reactive_filename_mmr <- reactive({
    paste0("mmr-country-report-", input$iso3cp, ".pdf")
  })
  
  reactive_country_name_mmr <- reactive({
    paste0(input$iso3cp)
  })
  
  output$download_button_mmr <- downloadHandler(
    
    filename = function() {
      reactive_filename_mmr()
    },
    content = function(file) {
      params <- list(
        country_name = reactive_country_name_mmr(),
        country_iso = country_table %>% filter(Title == input$iso3cp) %>% pull(Code),
        cod19 = cod19,
        cod00 = cod00,
        ledata = data %>% filter(GHOcode=="WHOSIS_000001"),
        country_table = country_table,
        data_mmr = data_mmr,
        mmr_region = mmr_region,
        final_ = final_
      )
      
      id <- showNotification(
        "Rendering MMR pdf...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render("country-report_mh_v3.Rmd", 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

