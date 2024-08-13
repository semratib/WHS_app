#################################################################################################################
## server
#################################################################################################################

server = function(input, output, session) { 

  
  #################################################################################################################
  
  # Figure sentences
  
  output$lehetext <- renderText({year <- 2021; paste0("Life expectancy at birth and healthy life expectancy, ", input$iso3le, ", 2000-", year, ".")})
  output$lecodtext <- renderText({year <- 2021; paste0("Life expectancy in ", year, " as a result of changes to the top 10 causes of death in  ", year, ", ", input$iso3le, ".")})
  output$ranktext <- renderText({year <- 2021; paste0("Change in rank for level 2 causes of death between 2000 and ", year, ", ", input$iso3, ".")})
  output$treemaptext <- renderText({year <- 2021; paste0("All-age cause of deaths in 2000 and ", year, ", ", input$iso3, ".")})
  output$sdgtext <- renderText({year <- 2021; paste0("Triple Billions indicator progress in ", input$iso3sdg, ", 2018-2030.")})
  output$heatmapv2text <- renderText({paste0("Compare the countries with the highest burden of ", input$codheatv2, " in and 2021.")})
  output$sdgv2text <- renderText({paste0("Countries with lowest performance in ", input$sdgind, " in 2018 and 2021.")})
  output$uhctext <- renderText({paste0("UHC tracer indicator progress in ",  input$iso3uhc, ", 2018-2030.")})
  
  #################################################################################################################
  
  # Life and health life expectancy
  output$lehe <- renderPlot ({
    
    exp <- data %>% 
      filter(GHOcode=="WHOSIS_000002" | GHOcode=="WHOSIS_000001") %>% 
      filter(year %in% c(2000, 2010, 2019, 2020, 2021)) %>%
      # filter(year %in% c(2000, 2010, 2019)) %>% 
      filter(country==input$iso3le) %>% 
      filter(sex %in% input$sex) %>% 
      select(country, year, sex, name, value)
    
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
    
    ggplot()+
      geom_ribbon(data = exp2, aes(x=year, ymax=`Life expectancy at birth (years)`, ymin=`Healthy life expectancy at birth (years)`), fill="grey", alpha=0.3) +
      geom_line(data = exp, aes(x=year, y=value, color=name), size = 1.2) +
      geom_point(data = exp, aes(x=year, y=value, color=name), size = 3, show.legend=F) +
      geom_text(data = exp %>% filter(name=="Life expectancy at birth (years)" & year!= 2020), 
                aes(x=year, y=value, label = round(value,1)), vjust=-1.5, size=4.5, color = "#009ADE", ) +
      geom_text(data = exp %>% filter(name=="Healthy life expectancy at birth (years)" & year!= 2020), 
                aes(x=year, y=value, label = round(value,1)), vjust=2, size=4.5, color ="#80BC00") +
      theme_classic()+
      ylab("")+ xlab("")+
      scale_y_continuous(limits = c(round(min, 0)-2,round(max, 0)+2)) +
      scale_x_continuous(breaks = c(2000, 2010, 2019, 2020, 2021)) +
      # scale_x_continuous(breaks = c(2000, 2010, 2019)) +
      scale_color_manual(values = c("Life expectancy at birth (years)" ="#009ADE",
                                    "Healthy life expectancy at birth (years)" = "#80BC00")) +
      facet_wrap(~sex) +
      theme(axis.text=element_text(size=14),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            strip.background = element_blank(),
            strip.text = element_text(size=14))

    
  }) 

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
  
  output$barcod <- renderPlot({
    
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
      DIM_GHECAUSE_TITLE==cod1_ ~ deaths*input$scalar1,
      DIM_GHECAUSE_TITLE==cod2_ ~ deaths*input$scalar2,
      DIM_GHECAUSE_TITLE==cod3_ ~ deaths*input$scalar3,
      DIM_GHECAUSE_TITLE==cod4_ ~ deaths*input$scalar4,
      DIM_GHECAUSE_TITLE==cod5_ ~ deaths*input$scalar5,
      DIM_GHECAUSE_TITLE==cod6_ ~ deaths*input$scalar6,
      DIM_GHECAUSE_TITLE==cod7_ ~ deaths*input$scalar7,
      DIM_GHECAUSE_TITLE==cod8_ ~ deaths*input$scalar8,
      DIM_GHECAUSE_TITLE==cod9_ ~ deaths*input$scalar9,
      DIM_GHECAUSE_TITLE==cod10_ ~ deaths*input$scalar10,
      .default = deaths)) %>% 
      ggplot()+
      geom_col(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths, fill=group), width = 0.5, size = 1, alpha=0.5)+ #fill = "#009ADE" show.legend = FALSE
      geom_text(aes(x=reorder(DIM_GHECAUSE_TITLE, deaths), y=new_deaths/2, label = format(round(new_deaths, digits = 0), big.mark=",")), color = "black", size = 5.5)+
      theme_void()+
      scale_fill_manual(values = c("Communicable, maternal, perinatal and nutritional conditions" = "#F26829", 
                                   "Noncommunicable diseases" = "#009ADE", 
                                   "Injuries" = "#80BC00",
                                   "Other pandemic related causes" = "#A6228C"))+
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
  
  output$codle <- renderHighchart({ 
    
     if(input$scalar1==1 &
        input$scalar2==1 &
        input$scalar3==1 &
        input$scalar4==1 &
        input$scalar5==1 &
        input$scalar6==1 &
        input$scalar7==1 &
        input$scalar8==1 &
        input$scalar9==1 &
        input$scalar10==1){
    
       exp <- data %>%
         filter(GHOcode=="WHOSIS_000001" & year %in% c(2000, 2005, 2010, 2019, 2020, 2021)) %>%
         # filter(GHOcode=="WHOSIS_000001") %>%
         filter(country==input$iso3le & sex==input$sexle) %>%
         select(year, name, sex, value) %>% 
         mutate(value = round(value, digits = 1))
    
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
                  format = "{point.value}"
                  )
                ) %>% 
         hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2019, 2020, 2021), labels = list(rotation=-45)) %>%
         # hc_xAxis(title = list(text = ""), tickPositions= c(2000, 2005, 2010, 2015, 2019), labels = list(rotation=-45)) %>% 
         hc_yAxis(title = list(text= ""), min = min-5, max = max+2)  %>%
         hc_tooltip(crosshairs = F, shared = F,
                    formatter= JS(
                      paste0('function() {
                      return this.series.name + "</b><br/>LE: <b>" + this.point.y + "<br/>Year: <b>"+this.point.x
                             }'
                             )
                      )
                    )
    
       # ggplot(data = exp, aes(x=year, y=value, color = type))+
       #   geom_line(size = 1.2, color = "#009ADE", show.legend = T) +
       #   geom_point(size = 3, show.legend=F, color = "darkgrey") +
       #   geom_text(aes(label = round(value,0)), vjust=-1.5, size=5.5, color = "black") +
       #  theme_classic()+
       #   ylab("")+ xlab("")+
       #   scale_y_continuous(limits = c(round(min, 0)-5,round(max, 0)+5)) +
       #  theme(axis.text=element_text(size=14),
       #         title = element_text(size = 14, hjust=0.5))

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
          filter(FLAG_TREEMAP==1 & sex==input$sexle & DIM_AGEGROUP_CODE!=101) %>% 
          group_by(DIM_GHECAUSE_TITLE, DIM_AGEGROUP_CODE, sex) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          ungroup() %>% 
          mutate(new_deaths = case_when(
            DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar1,
            DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar2,
            DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar3,
            DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar4,
            DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar5,
            DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar6,
            DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar7,
            DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar8,
            DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar9,
            DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar10,
            .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
          group_by(DIM_AGEGROUP_CODE, sex) %>% 
          summarise(new_deaths = sum(new_deaths)) %>% 
          ungroup() %>% 
          left_join(
            cod19 %>% 
              filter(DIM_GHECAUSE_CODE==0 & sex==input$sexle & DIM_AGEGROUP_CODE!=101) %>% 
              group_by(DIM_AGEGROUP_CODE, sex) %>% 
              summarise(ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
              ungroup()
          ) %>% 
          mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
          # mutate(DIM_AGEGROUP_CODE=str_remove(DIM_AGEGROUP_CODE, "^YEARS")) %>% 
          # mutate(age_start=as.numeric(str_extract(DIM_AGEGROUP_CODE, "^[:digit:]+")),
          #        age_end=as.numeric(str_extract(DIM_AGEGROUP_CODE, "[:digit:]+$")),
          #        age_end = ifelse(age_start==0 | age_start==85, age_end, age_end+1),
          #        age_end = ifelse(age_start==85, Inf, age_end)) %>% 
          mutate(age_start = DIM_AGEGROUP_CODE,
                 age_end = ifelse(DIM_AGEGROUP_CODE<5, 
                                  case_when(DIM_AGEGROUP_CODE==0 ~ 1,
                                            DIM_AGEGROUP_CODE==1 ~ 5),
                                  DIM_AGEGROUP_CODE+5),
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
              filter(FLAG_TREEMAP==1 & sex==input$sexle & region2==input$iso3le & DIM_AGEGROUP_CODE!=101) %>% 
              group_by(DIM_GHECAUSE_TITLE, DIM_AGEGROUP_CODE, sex) %>% 
              summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
              ungroup() %>% 
              mutate(new_deaths = case_when(
                DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar1,
                DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar2,
                DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar3,
                DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar4,
                DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar5,
                DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar6,
                DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar7,
                DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar8,
                DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar9,
                DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar10,
                .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
              group_by(DIM_AGEGROUP_CODE, sex) %>% 
              summarise(new_deaths = sum(new_deaths)) %>% 
              ungroup() %>% 
              left_join(
                cod19 %>% 
                  filter(DIM_GHECAUSE_CODE==0 & sex==input$sexle & region2==input$iso3le & DIM_AGEGROUP_CODE!=101) %>% 
                  group_by(DIM_AGEGROUP_CODE, sex) %>% 
                  summarise(ATTR_POPULATION_NUMERIC = sum(ATTR_POPULATION_NUMERIC)) %>% 
                  ungroup()
              ) %>% 
              mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
              mutate(age_start = DIM_AGEGROUP_CODE,
                     age_end = ifelse(DIM_AGEGROUP_CODE<5, 
                                  case_when(DIM_AGEGROUP_CODE==0 ~ 1,
                                            DIM_AGEGROUP_CODE==1 ~ 5),
                                  DIM_AGEGROUP_CODE+5),
                 age_end = ifelse(age_start==85, Inf, age_end)) %>% 
              mutate(year=yearle, 
                 sex=case_when(sex=="Both sexes" ~ "both",
                               sex=="Female" ~ "female",
                               sex=="Male" ~ "male")) %>%
              arrange(age_start) %>% 
              select(age_start, age_end, sex, year, mx) 
          
            
            } else {
              
              test <- cod19 %>% filter(country==input$iso3le & sex==input$sexle & DIM_AGEGROUP_CODE!=101)  
              
              code0 <- test %>%
                filter(FLAG_TREEMAP==1) %>% 
                mutate(new_deaths = case_when(
                  DIM_GHECAUSE_TITLE==cod1_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar1,
                  DIM_GHECAUSE_TITLE==cod2_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar2,
                  DIM_GHECAUSE_TITLE==cod3_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar3,
                  DIM_GHECAUSE_TITLE==cod4_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar4,
                  DIM_GHECAUSE_TITLE==cod5_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar5,
                  DIM_GHECAUSE_TITLE==cod6_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar6,
                  DIM_GHECAUSE_TITLE==cod7_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar7,
                  DIM_GHECAUSE_TITLE==cod8_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar8,
                  DIM_GHECAUSE_TITLE==cod9_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar9,
                  DIM_GHECAUSE_TITLE==cod10_ ~ VAL_DEATHS_COUNT_NUMERIC*input$scalar10,
                  .default = VAL_DEATHS_COUNT_NUMERIC)) %>% 
                group_by(DIM_COUNTRY_CODE, DIM_AGEGROUP_CODE, sex) %>% 
                summarise(new_deaths = sum(new_deaths)) %>% 
                ungroup() %>% 
                left_join(
                  test %>% 
                    filter(DIM_GHECAUSE_CODE==0) %>% 
                    select(DIM_COUNTRY_CODE, DIM_AGEGROUP_CODE, ATTR_POPULATION_NUMERIC)
                ) %>%
                mutate(mx = new_deaths/ATTR_POPULATION_NUMERIC) %>%
                mutate(age_start = DIM_AGEGROUP_CODE,
                       age_end = ifelse(DIM_AGEGROUP_CODE<5, 
                                        case_when(DIM_AGEGROUP_CODE==0 ~ 1,
                                                  DIM_AGEGROUP_CODE==1 ~ 5),
                                        DIM_AGEGROUP_CODE+5),
                       age_end = ifelse(age_start==85, Inf, age_end)) %>% 
                mutate(year=yearle,
                       sex=case_when(sex=="Both sexes" ~ "both",
                                     sex=="Female" ~ "female",
                                     sex=="Male" ~ "male")) %>% 
                arrange(age_start) %>% 
                select(age_start, age_end, sex, year, mx)
              
            }
      
      e0 <- gete0(code0) %>% mutate(type = "Potential")
      # e0_default <- gete0(default) %>% mutate(type = "Current") %>% mutate(value = floor(value))
      
      exp <- data %>% 
        filter(GHOcode=="WHOSIS_000001" & year %in% c(2000, 2005, 2010, 2019, 2020, 2021)) %>%
        # filter(GHOcode=="WHOSIS_000001") %>% 
        filter(country==input$iso3le & sex==input$sexle) %>% 
        mutate(type = "Current",
               sex=case_when(sex=="Both sexes" ~ "both",
                             sex=="Female" ~ "female",
                             sex=="Male" ~ "male")) %>% 
        select(year, sex, name, value, type)
      
      exp_ <- exp %>% 
        # rbind(e0_default) %>% 
        rbind(e0) %>%
        rbind(exp %>% filter(year==2020) %>% mutate(type = "Potential")) %>%
        # rbind(exp %>% filter(year==2015) %>% mutate(type = "Potential")) %>% 
        # rbind(exp %>% filter(year==2019) %>% mutate(type = "Current", year=2020)) %>%
        mutate(value = round(value, digits = 1)) %>% 
        mutate(noTooltip = ifelse(year==2020 & type=="Potential", T, F))
        # mutate(noTooltip = ifelse(year==2015 & type=="Potential", T, F)) 
      
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
                        format = "{point.value}"
                      )) %>%
        hc_add_series(data = exp_ %>% filter(type == "Current"), 'line', name = "Current", hcaes(x = year, y = value), color = "#009ADE",
                      dataLabels = list(
                        enabled = TRUE,
                        format = "{point.value}"
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
  
  #################################################################################################################
  
  # Arrow
  codrankdata <- reactive({
    
    req(input$iso3)
    
    if(input$iso3=="Global"){
      
      cod19 %>%
        filter(FLAG_LEVEL == 2 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        rbind(
          cod00 %>%
            filter(FLAG_LEVEL == 2 & sex==input$sexcod) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC))
        )
      
      }else if(input$iso3 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
      
      cod19 %>%
        filter(FLAG_LEVEL == 2 & region2==input$iso3 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        rbind(
          cod00 %>%
            filter(FLAG_LEVEL == 2 & region2==input$iso3 & sex==input$sexcod) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC))
        )
      
    }else{
      
      cod19 %>%
        filter(country == input$iso3 & FLAG_LEVEL == 2 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
        select(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC) %>% 
        rbind(
          cod00 %>%
            filter(country == input$iso3 & FLAG_LEVEL == 2 & sex==input$sexcod) %>% 
            select(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC)
        )
    }
    # return(test)
  })
  
  output$codrank <- renderPlot({
    
    year <- 2021
    comp_year <- 2000
    
    lev1 <- cod19 %>%
      filter(FLAG_LEVEL == 1) %>%
      select(FLAG_CAUSEGROUP, lev1_name = DIM_GHECAUSE_TITLE) %>%
      unique()
    
    # compute cause fractions and cause ranks
    data_arrow <- codrankdata() %>%
      group_by(DIM_YEAR_CODE) %>%
      mutate(prop_deaths = VAL_DEATHS_COUNT_NUMERIC / sum(VAL_DEATHS_COUNT_NUMERIC),
             rank_deaths = rank(-prop_deaths, ties.method = "first"),
      ) %>%
      select(FLAG_CAUSEGROUP, DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, prop_deaths, rank_deaths) %>%
      pivot_wider(id_cols = c(FLAG_CAUSEGROUP, DIM_GHECAUSE_TITLE), names_from = DIM_YEAR_CODE,
                  values_from = c(prop_deaths, rank_deaths)) %>%
      rename(rank_year1 = paste0("rank_deaths_", comp_year),
             rank_year2 = paste0("rank_deaths_", year)) %>%
      mutate(increasing_rank = rank_year1 < rank_year2) %>%
      filter(rank_year1<11 | rank_year2<11) %>% 
      left_join(lev1, by = "FLAG_CAUSEGROUP")
    
    data_arrow$lev1_name <- factor(data_arrow$lev1_name, levels = lev1_causes)
    
    # create plot
    ggplot(data_arrow, aes(x=0.2, xend=1, y=-rank_year1, yend=-rank_year2)) +
      geom_segment(aes(color=lev1_name, lty = increasing_rank), show.legend = F) +
      geom_label(aes(x = -1.1, y = -rank_year1, label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0, alpha=0.3, size=6) +
      geom_label(aes(x = 1.1, y = -rank_year2, label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0, alpha=0.3, size=6) +
      geom_label(aes(x = -1.1, y = -rank_year1, label = paste0(rank_year1, " ", DIM_GHECAUSE_TITLE), fill = lev1_name),
                 hjust=0, alpha=0, size=5.5, label.size = NA) +
      geom_label(aes(x = 1.1, y = -rank_year2, label = paste0(rank_year2, " ", DIM_GHECAUSE_TITLE), fill = lev1_name),
                 hjust=0, alpha=0, size=5.5, label.size = NA) +
      geom_label(aes(x=-1.1, y=0, label=comp_year), hjust=0, size = 6, label.size = NA) +
      geom_label(aes(x=1.1, y=0, label=year), hjust=0, size = 6, label.size = NA) +
      scale_color_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.margin = margin(t=5)) +
      guides(color=guide_legend(nrow=1, byrow=TRUE),
             fill=guide_legend(nro1=3, byrow=TRUE, override.aes = aes(label = "")),
             lty="none") +
      labs(color = "", fill = "") +
      scale_x_continuous(limits=c(-1.6, 2.7))
    
    
  }) #, height = 500, width = 1100
  
  
  # Treemap
  output$treemap <- renderPlot({
    
    year <- 2021
    comp_year <- 2000
    
    if(input$codswitch=="Level 1 causes"){
        
        if(input$iso3=="Global"){
          
          data_treemap <- cod19 %>%
            filter(FLAG_LEVEL == 1 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                      attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>% 
            ungroup() 
          
          data_treemap_2000 <- cod00 %>%
            filter(FLAG_LEVEL == 1 & sex==input$sexcod) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                      attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
            ungroup()
          
          total_deaths <- cod19 %>%
            filter(FLAG_LEVEL == 0 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>%
            group_by(DIM_YEAR_CODE) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
            pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
          
          total_deaths_2000 <- cod00 %>%
            filter(FLAG_LEVEL == 0 & sex==input$sexcod) %>%
            group_by(DIM_YEAR_CODE) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
            pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
          
        }else if(
          input$iso3 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
        ){
        
          data_treemap_2000 <- cod00 %>%
            filter(FLAG_LEVEL == 1 & region2==input$iso3 & sex==input$sexcod) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                      attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
            ungroup()
          
          data_treemap <- cod19 %>%
            filter(FLAG_LEVEL == 1 & region2==input$iso3 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
            group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                      attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
            ungroup()
          
          total_deaths <- cod19 %>%
            filter(FLAG_LEVEL == 0 & region2==input$iso3 & sex==input$sexcod& DIM_AGEGROUP_CODE==101) %>%
            group_by(DIM_YEAR_CODE) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
            pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
          
          total_deaths_2000 <- cod00 %>%
            filter(FLAG_LEVEL == 0 & region2==input$iso3 & sex==input$sexcod) %>%
            group_by(DIM_YEAR_CODE) %>% 
            summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
            pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
          
        }else{

          data_treemap <- cod19 %>%
            filter(country == input$iso3 & FLAG_LEVEL == 1 & sex==input$sexcod & DIM_AGEGROUP_CODE==101)
          
          data_treemap_2000 <- cod00 %>%
            filter(country == input$iso3 & FLAG_LEVEL == 1 & sex==input$sexcod)
          
          total_deaths <- cod19 %>%
            filter(country == input$iso3 & FLAG_LEVEL == 0 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>%
            pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()

          total_deaths_2000 <- cod00 %>%
            filter(country == input$iso3 & FLAG_LEVEL == 0 & sex==input$sexcod) %>%
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
          labs(title = paste0("Total deaths in ", year, ": ", format(total_deaths, big.mark = ",", scientific = F)))+
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
          labs(title = paste0("Total deaths in ", comp_year, ": ", format(total_deaths_2000, big.mark = ",", scientific = F)))+
          theme(
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.position = "bottom"
          )
        
        patchwork::wrap_plots(tm2, tm1, ncol = 2) 
        
      }else if(input$codswitch=="Level 2 causes"){
    
    if(input$iso3=="Global"){
      
      data_treemap_2000 <- cod00 %>%
        filter(FLAG_TREEMAP == 1 & sex==input$sexcod) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
        ungroup() %>% 
        mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      
      data_treemap <- cod19 %>%
        filter(FLAG_TREEMAP == 1 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>% 
        ungroup() %>% 
        mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      
      total_deaths <- cod19 %>%
        filter(FLAG_LEVEL == 0 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>%
        group_by(DIM_YEAR_CODE) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      
      total_deaths_2000 <- cod00 %>%
        filter(FLAG_LEVEL == 0 & sex==input$sexcod) %>%
        group_by(DIM_YEAR_CODE) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      
    }else if(
      input$iso3 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
    ){
      
      data_treemap_2000 <- cod00 %>%
        filter(FLAG_TREEMAP == 1 & region2==input$iso3 & sex==input$sexcod) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
        ungroup() %>% 
        mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      
      data_treemap <- cod19 %>%
        filter(FLAG_TREEMAP == 1 & region2==input$iso3 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>% 
        group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                  attr_pop = sum(ATTR_POPULATION_NUMERIC))%>% 
        ungroup() %>% 
        mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      
      total_deaths <- cod19 %>%
        filter(FLAG_LEVEL == 0 & region2==input$iso3 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>%
        group_by(DIM_YEAR_CODE) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      
      total_deaths_2000 <- cod00 %>%
        filter(FLAG_LEVEL == 0 & region2==input$iso3 & sex==input$sexcod) %>%
        group_by(DIM_YEAR_CODE) %>% 
        summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
        pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      
    }else{

      data_treemap <- cod19 %>%
        filter(country == input$iso3 & FLAG_TREEMAP == 1 & sex==input$sexcod & DIM_AGEGROUP_CODE==101)
      
      data_treemap_2000 <- cod00 %>%
        filter(country == input$iso3 & FLAG_TREEMAP == 1 & sex==input$sexcod)
      
      total_deaths <- cod19 %>%
        filter(country == input$iso3 & FLAG_LEVEL == 0 & sex==input$sexcod & DIM_AGEGROUP_CODE==101) %>%
        pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      
      total_deaths_2000 <- cod00 %>%
        filter(country == input$iso3 & FLAG_LEVEL == 0 & sex==input$sexcod) %>%
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
      labs(title = paste0("Total deaths in ", year, ": ", format(total_deaths, big.mark = ",", scientific = F)))+   ######### CHANGE BACK TO 2019
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
      labs(title = paste0("Total deaths in ", comp_year, ": ", format(total_deaths_2000, big.mark = ",", scientific = F)))+
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
      geom_text(aes(x = 1, y = 0.5, label = "Communicable, maternal, perinatal\nand nutritional conditions"), size=5, vjust = 1.1)+
      geom_text(aes(x = 2, y = 0.5, label = "Noncommunicable diseases"), size=5, vjust = 1.5)+
      geom_text(aes(x = 3, y = 0.5, label = "Injuries"), size=5, vjust = 1.5)+
      geom_text(aes(x = 4, y = 0.5, label = "Other pandemic related causes"), size=5, vjust = 1.5)+
      theme_void()+
      scale_y_continuous(limits = c(-1,2.5))
    
    ggdraw() +
      draw_plot(patchwork::wrap_plots(tm2, tm1, ncol = 2), 0, 0.1, 1, 0.85) +
      draw_plot(legt, 0.02, 0.035, 0.95, 0.055)
    
      }
    }) #, height = 580
      
  #################################################################################################################
  
  # Map
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
  
  ##################################################################################################################
  
  # Table
  observe({
    x <- annexdata3 %>%
      filter(name %in% input$indtab2) %>%
      filter(region_name %in% input$regtab2) %>%
      distinct(country) %>% pull()

    # Can use character(0) to remove all choices
    # if (is.null(x))
    #   x <- character(0)

    updateVirtualSelect("isotab2", label = "Select Country:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata3 %>%
      filter(name %in% input$indtab2) %>% 
      filter(region_name %in% input$regtab2) %>% 
      distinct(year) %>% arrange(desc(year)) %>% mutate(year = as.character(year)) %>%  pull()
    
    # Can also set the label and select items
    updateVirtualSelect("yeartab2", label = "Year:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata3 %>%
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
    x <- annexdata3 %>%
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
    annexdata3 %>%
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
  
  ## Comparative ranking
  codrankdatacomp1 <- reactive({
    
    if(input$yearrankcomp1==2021){   ###### CHANGE BACK TO 2019
      
      if(input$iso3comp1=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE==101) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else if(input$iso3comp1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp1 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE==101) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3comp1 & FLAG_LEVEL == 2 & sex==input$sexrankcomp1 & DIM_AGEGROUP_CODE==101) %>% 
          mutate(region = 1) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
        
      }
    }else{
      if(input$iso3comp1=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp1) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else if(input$iso3comp1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp1 & sex==input$sexrankcomp1) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 1)
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3comp1 & FLAG_LEVEL == 2 & sex==input$sexrankcomp1) %>% 
          mutate(region = 1) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
        
      }
      
    }
  })
  
  codrankdatacomp2 <- reactive({
    
    if(input$yearrankcomp2==2021){ ######### CHANGE BACK TO 2019
      
      if(input$iso3comp2=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE==101) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else if(input$iso3comp2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod19 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE==101) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3comp2 & FLAG_LEVEL == 2 & sex==input$sexrankcomp2 & DIM_AGEGROUP_CODE==101) %>% 
          mutate(region = 2) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
      }
      
    }else{
      
      if(input$iso3comp2=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & sex==input$sexrankcomp2) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else if(input$iso3comp2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", 
                                      "Region of the Americas", "South-East Asia Region", "Western Pacific Region")){
        
        cod00 %>%
          filter(FLAG_LEVEL == 2 & region2==input$iso3comp2 & sex==input$sexrankcomp2) %>% 
          group_by(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          mutate(region = 2)
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3comp2 & FLAG_LEVEL == 2 & sex==input$sexrankcomp2) %>% 
          mutate(region = 2) %>% 
          select(DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP, VAL_DEATHS_COUNT_NUMERIC, region)
      }
      
    }
  })
  
  output$codrankcomp1 <- renderPlot({
    
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
      geom_label(aes(label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0, alpha=0.3, size=6, show.legend = F) +
      geom_label(aes(label = paste0(rank_deaths, " ", DIM_GHECAUSE_TITLE), fill = lev1_name), hjust=0, alpha=0, size=6, label.size = NA, show.legend = F) +
      scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      theme_void() +
      labs(color = "", fill = "") +
      scale_x_continuous(limits=c(0.95, 1.18))
    
  })
  
  output$codrankcomp2 <- renderPlot({
    
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
      geom_label(aes(label = paste0(rep(" ", 57), collapse = ""), fill = lev1_name), hjust=0, alpha=0.3, size=6, show.legend = F) +
      geom_label(aes(label = paste0(rank_deaths, " ", DIM_GHECAUSE_TITLE), fill = lev1_name), hjust=0, alpha=0, size=6, label.size = NA, show.legend = F) +
      scale_fill_manual(values = c("#F26829", "#009ADE", "#80BC00"))+
      theme_void() +
      labs(color = "", fill = "") +
      scale_x_continuous(limits=c(0.95, 1.18))
    
  })
  
  
  ## Comparative treemap
  comp1data <- reactive({
    if(input$yeartreecomp1==2021){   ############ CHANGE BACK TO 2021
      if(input$iso3compt1=="Global"){

        cod19 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)


      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){

        cod19 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)


      }else{

        cod19 %>% filter(country == input$iso3compt1 & FLAG_TREEMAP == 1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101)

      }
    }else{
      if(input$iso3compt1=="Global"){
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp1) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt1 & sex==input$sextreecomp1) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
      }else{
        cod00 %>% filter(country == input$iso3compt1 & FLAG_TREEMAP == 1 & sex==input$sextreecomp1)
      }
    }
    # }
  })
  comp1deaths <- reactive({
    if(input$yeartreecomp1==2021){  ############ CHANGE BACK TO 2021
      if(input$iso3compt1=="Global"){
        
        cod19 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        
        cod19 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt1 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else{
        
        cod19 %>%
          filter(country == input$iso3compt1 & FLAG_LEVEL == 0 & sex==input$sextreecomp1 & DIM_AGEGROUP_CODE==101) %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }else{
      if(input$iso3compt1=="Global"){
        
        cod00 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp1) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else if(
        input$iso3compt1 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        
        cod00 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt1 & sex==input$sextreecomp1) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
        
      }else{
        
        cod00 %>%
          filter(country == input$iso3compt1 & FLAG_LEVEL == 0 & sex==input$sextreecomp1) %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }
  })
  
  comp2data <- reactive({
    if(input$yeartreecomp2==2021){ ############ CHANGE BACK TO 2021
      if(input$iso3compt2=="Global"){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        
        cod19 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else{
        
        cod19 %>% filter(country == input$iso3compt2 & FLAG_TREEMAP == 1 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101)
        
      }
    }else{
      if(input$iso3compt2=="Global"){
        
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & sex==input$sextreecomp2) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC)) %>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region")
      ){
        
        cod00 %>%
          filter(FLAG_TREEMAP == 1 & region2==input$iso3compt2 & sex==input$sextreecomp2) %>%
          group_by(DIM_YEAR_CODE, DIM_GHECAUSE_TITLE, FLAG_CAUSEGROUP) %>%
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC),
                    attr_pop = sum(ATTR_POPULATION_NUMERIC))%>%
          ungroup() %>%
          mutate(deaths_100k = (VAL_DEATHS_COUNT_NUMERIC/attr_pop)*100000)
        
        
      }else{
        
        cod00 %>% filter(country == input$iso3compt2 & FLAG_TREEMAP == 1 & sex==input$sextreecomp2)
        
      }
    }
    # }
  })
  comp2deaths <- reactive({
    if(input$yeartreecomp2==2021){ ############ CHANGE BACK TO 2021
      if(input$iso3compt2=="Global"){
        cod19 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        cod19 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else{
        cod19 %>%
          filter(FLAG_LEVEL == 0 & country == input$iso3compt2 & sex==input$sextreecomp2 & DIM_AGEGROUP_CODE==101) %>%
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }
    }else{
      if(input$iso3compt2=="Global"){
        cod00 %>%
          filter(FLAG_LEVEL == 0 & sex==input$sextreecomp2) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else if(
        input$iso3compt2 %in% c("African Region", "Eastern Mediterranean Region", "European Region", "Region of the Americas", "South-East Asia Region", "Western Pacific Region") 
      ){
        cod00 %>%
          filter(FLAG_LEVEL == 0 & region2==input$iso3compt2 & sex==input$sextreecomp2) %>%
          group_by(DIM_YEAR_CODE) %>% 
          summarise(VAL_DEATHS_COUNT_NUMERIC = sum(VAL_DEATHS_COUNT_NUMERIC)) %>% 
          pull(VAL_DEATHS_COUNT_NUMERIC) %>% round()
      }else{
        cod00 %>%
          filter(FLAG_LEVEL == 0 & country == input$iso3compt2 & sex==input$sextreecomp2) %>%
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

    data_treemap %>% 
      data_to_hierarchical(group_vars = c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#BE73AD", "#AACF7F")) %>% 
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

    data_treemap %>% 
      data_to_hierarchical(group_vars= c(lev1_name, cause_title), size_var = deaths, colors = c("#F6A27C", "#79B5E3", "#BE73AD", "#AACF7F")) %>% 
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

  
  ## LE graph comparative
  
  observe({
    x <- annexdata3 %>%
      filter(name==input$indgraph) %>%
      arrange(sort_country, country) %>% 
      distinct(country) %>%
      pull(country)
    
    updateVirtualSelect("iso3graph", label = "Select countries, areaa or WHO regions:", choices = x, selected = x[1])
  })
  
  observe({
    x <- annexdata3 %>%
      filter(name==input$indgraph & country %in% input$iso3graph) %>% 
      distinct(year) %>% 
      arrange(desc(year)) %>% 
      mutate(year = as.character(year)) %>% 
      pull()
    
    # Can also set the label and select items
    updateVirtualSelect("yeargraph", label = "Select years:", choices = x, selected = x)
  })
  
  observe({
    x <- annexdata3 %>%
      filter(name==input$indgraph & country %in% input$iso3graph) %>% 
      distinct(sex) %>% 
      pull()
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateRadioButtons(session, "sexgraph", label = "Sex:", choices = x, selected = x[1])
  })
  
  legraphdata <- reactive({
    if(input$indgraph %in% locinclude_ind){
      annexdata3 %>%
        filter(name==input$indgraph &
                 country %in% input$iso3graph &
                 year %in% as.numeric(input$yeargraph) &
                 sex == input$sexgraph &
                 location=="Both Rural and Urban") %>%
        arrange(year) %>%
        mutate(value = round(value, digits = 1)) %>%
        select(country, year, sex, name, value)
    }else{
    annexdata3 %>%
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
    ) %>% #, animation = FALSE
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
      hchart(type = "heatmap", hcaes(x=DIM_GHECAUSE_TITLE, y=country, value=rank), dataLabels = list(enabled = T, format = "{point.value}")) %>% 
      hc_colorAxis(
        minColor = "#EF3842",  
        # maxColor = "#F4A81D" # yellow
        # maxColor = "#F26829" # orange
        maxColor = "#009ADE" #"#80BC00" # green
      ) %>% 
      hc_chart(backgroundColor = "white", spacingRight = 80) %>% 
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
  
  
  ## Heat map version 2
  
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
      
    
    # ggplot(heatdatav2_high, aes(x=1, y=-rank)) +
    #   # geom_label(aes(label = paste0(rep(" ", 70), collapse = "")), hjust=0, alpha=0.3, size=6, show.legend = F) +
    #   geom_label(aes(label = text), hjust=0, alpha=0, size=6, show.legend = F,  color = "#EF3842", label.size = NA) +
    #   theme_void() +
    #   scale_x_continuous(limits=c(0.95, 1.22))

  })
  
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
      # arrange(desc(get(var))) %>% 
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
    
    # ggplot(heatdatav2_low, aes(x=1, y=-rank)) +
    #   # geom_label(aes(label = paste0(rep(" ", 70), collapse = "")), hjust=0, alpha=0.3, size=6, show.legend = F) +
    #   geom_label(aes(label = text), hjust=0, alpha=0, size=6, show.legend = F, color =  "#009ADE", label.size = NA) +
    #   theme_void() +
    #   scale_x_continuous(limits=c(0.95, 1.22))
    
  })
  
  output$bump <- renderPlot({
    
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
    
    df_ %>% 
      ggplot(aes(x = year, y = rev(rank), group = country, color = type)) +
      ggbump::geom_bump(size = 1.5, show.legend = F, alpha = 0.7) +
      geom_point(size = 2, show.legend = F, alpha = 0.8) +
      # geom_point(data = df_ %>% filter(year==2001), 
      #            aes(x = year, y = rev(rank), color = type), 
      #            size = 10, show.legend = F, alpha = 0.8, shape = "\u2192", hjust = 1) +
      scale_x_continuous(labels = c("2000", "2021"), breaks = c(2000, 2001))+
      scale_color_manual(
        values = c("decrease" = "#80BC00",
                   "increase" = "#F26829",
                   "same" = "#F4A81D")
        ) + 
      theme_void() +
      theme(plot.margin = unit(c(1.6,0,0,0), "cm"))
    
  })
  
  ## SDG Forecast Table
  
  output$sdgtable <- renderUI({
    
    table <- make_sdg_table(filtered_data = filtered_indicator_values, level = input$iso3sdg)
    
    ft <- flextable(table)
    # ft <- bg(ft, ~ ach=="Likely Achieve by 2030", ~ `2030`, bg = "#80BC00")
    # ft <- bg(ft, ~ ach=="Won't Achieve by 2030, but within 10% of target", ~ `2030`, bg = "#F4A81D")
    # ft <- bg(ft, ~ ach=="Won't Achieve by 2030", ~ `2030`, bg = "#F26829")
    ft <- bg(ft, bg = "white", part = "all")
    ft <- bg(ft, ~ ach=="Likely Achieve by 2030", ~ `2030`, bg = "#AACF7F")
    ft <- bg(ft, ~ ach=="Won't Achieve by 2030, but within 10% of target", ~ `2030`, bg = "#F5C46A")
    ft <- bg(ft, ~ ach=="Won't Achieve by 2030", ~ `2030`, bg = "#F6A27C")
    ft <- delete_columns(ft, j = "ach")
    ft <- width(ft, j =1, width=2, unit = "cm")
    ft <- width(ft, j =2, width=7, unit = "cm")
    ft <- width(ft, j =3, width=3, unit = "cm")
    ft <- width(ft, j =4:6, width=3, unit = "cm")
    ft <- width(ft, j =7, width=3.5, unit = "cm")
    ft <- align(ft, j=4:7, align = "center")
    ft <- align(ft, j=4:7, align = "center", part = "header")
    # ft <- footnote(ft, i= 1, j=6, value = as_paragraph("2030 values are colored based on whether the indicator is likely to achieve the target based on 2030 forecasts: green will reach target, yellow are within 10% of target, orange will not reach target."), ref_symbols = "*", part = "header")
    
    ft %>% htmltools_value()
  })
  
  ## SDG Indicator progress between countries
  
  sdgy1 <- reactive({
    
    small_best <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(small_is_best) %>% pull()
    if(small_best==T){
      filtered_indicator_values %>% 
        filter(year==2018 & indicator_name==input$sdgind) %>% 
        arrange(desc(value)) %>% 
        filter(row_number()<21)
    }else{
      filtered_indicator_values %>% 
        filter(year==2018 & indicator_name==input$sdgind) %>% 
        arrange(value) %>% 
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
      width(., j=2, width=5, unit = "cm") %>%
      bg(., j = 1:2, bg = "white", part = "body") %>% 
      htmltools_value()
    
  })
  
  sdgy2 <- reactive({
    
    small_best <- sdg_sort %>% filter(indicator_name==input$sdgind) %>% distinct(small_is_best) %>% pull()
    if(small_best==T){
      filtered_indicator_values %>% 
        filter(year==2021 & indicator_name==input$sdgind) %>% 
        arrange(desc(value)) %>% 
        filter(row_number()<21)
    }else{
      filtered_indicator_values %>% 
        filter(year==2021 & indicator_name==input$sdgind) %>% 
        arrange(value) %>% 
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
      width(., j=2, width=5, unit = "cm") %>%
      bg(., j = 1:2, bg = "white", part = "body") %>% 
      htmltools_value()
    
  })
  
  output$sdgbump <- renderPlot({
    
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
    
    df_ %>% 
      ggplot(aes(x = year, y = rev(rank), group = country, color = type)) +
      ggbump::geom_bump(size = 1.5, show.legend = F, alpha = 0.7) +
      geom_point(size = 2, show.legend = F, alpha = 0.8) +
      # geom_point(data = df_ %>% filter(year==2001), 
      #            aes(x = year, y = rev(rank), color = type), 
      #            size = 10, show.legend = F, alpha = 0.8, shape = "\u2192", hjust = 1) +
      scale_x_continuous(labels = c("2000", "2021"), breaks = c(2000, 2001))+
      scale_color_manual(
        values = c("decrease" = "#80BC00",
                   "increase" = "#F26829",
                   "same" = "#F4A81D")
      ) + 
      theme_void() +
      theme(plot.margin = unit(c(1.6,0,0,0), "cm"))
    
  })
  
  ## UHC Circle graph
  
  output$uhcgraph <- renderPlot({
    
    make_donut_uhc(filtered_data = filtered_indicator_values, level=input$iso3uhc, year_pie=2018)
    
  })
  
  ## Generate report
  reactive_filename_mmr <- reactive({
    paste0("country-report-", input$reportiso, ".pdf")
  })
  
  reactive_country_name <- reactive({
    paste0(input$reportiso)
  })
  
  output$download_button <- downloadHandler(
    
    filename = function() {
      reactive_filename_mmr()
    },
    content = function(file) {
      params <- list(
        country = reactive_country_name(),
        country_iso = country_table %>% filter(Title == input$reportiso) %>% pull(Code),
        cod19 = cod19,
        cod00 = cod00,
        ledata = data %>% filter(GHOcode=="WHOSIS_000001" & year==2021),
        sdg_data = filtered_indicator_values,
        country_table = country_table
      )
      
      id <- showNotification(
        "Rendering pdf...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render("rmd/country-report.Rmd", 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #################################################################################################################
  
}

