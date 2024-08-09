
##########################################################
## user interface
##########################################################  
div(style="max-width:14in;margin:auto;",
ui<-fluidPage(withMathJax(),
              useShinyjs(),
              includeCSS("www/app_theme.css"),
              theme = bs_theme(bg = "white", fg = "#00205C",
                               primary = "#00205C",
                               secondary = "#009ADE",
                               success = "#F26829"),
              tags$style(
              "h2 {margin-top: 0.5in; margin-bottom: 0.5in;}
              .nav-bar{font-size:35px;}"

),
              titlePanel(title = div(h1("Global Health Indicators", style="font-size:34px;margin-bottom:10px"), 
                                     h3("Data analytics platform by the Division of Data, Analytics and Delivery for Impact at WHO", style="font-size:20px"),
                                     div(h4("Development version/Temporary internal use only", style="font-size:18px"))
                                     )
                         ),
                tabsetPanel( 
                  type = "tabs",
                  selected = "Life Expectancy",
                  
                  tabPanel("Life Expectancy",
                           tags$br(),
                           fluidRow(virtualSelectInput("iso3le", div("Select country, area or WHO region:", style="font-size:18px"), 
                                                       choices =  iso3le , 
                                                       selected = "Global",
                                                       search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "40%",
                                                       # options = list(style = "{font-size:20px; color: #00205C;}"),
                                                       choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                    ),
                           tags$br(),
                           tabsetPanel(
                             tabPanel("Life and Healthy life expectancy",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(div(textOutput("lehetext"), style="font-size:20px")),
                                      fluidRow(checkboxGroupInput("sex", "",
                                                                  choices = c("Both sexes", "Female", "Male"),
                                                                  selected = c("Both sexes", "Female", "Male"),
                                                                  inline = TRUE,
                                                                  width = "700px")),
                                      fluidRow(plotOutput("lehe", height = "650px", width = "1200px")),
                                      tags$br(),
                                      ),
                             tabPanel("Life expectancy calculator",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(div(textOutput("lecodtext"), style="font-size:20px")),
                                      tags$br(),
                                      radioButtons("sexle", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                      tags$br(),
                                      fluidRow(
                                        column(3,
                                               fluidRow(div("Scale the mortality level by:", style="font-weight:bold")),
                                               tags$br(),
                                               fluidRow(sliderInput("scalar1", textOutput("cod1"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar2", textOutput("cod2"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar3", textOutput("cod3"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar4", textOutput("cod4"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar5", textOutput("cod5"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar6", textOutput("cod6"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar7", textOutput("cod7"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar8", textOutput("cod8"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar9", textOutput("cod9"), min = 0, max = 1, value = 1, ticks = F)),
                                               fluidRow(sliderInput("scalar10", textOutput("cod10"), min = 0, max = 1, value = 1, ticks = F))
                                        ),
                                        column(4, 
                                               fluidRow(div("Number of deaths:", style="font-weight:bold")),
                                               tags$br(),
                                               fluidRow(plotOutput("barcod", height = "1000px"))
                                        ),
                                        column(5, 
                                               fluidRow(div("Life expectancy at birth (years)", style="font-weight:bold")),
                                               tags$br(),
                                               highchartOutput("codle", height = "865px")
                                        )
                                      )
                             ),
                             type = "pills"
                          ),
                          tags$br(),
                          div("Based on World Health Statistics 2023 and Global Health Estimates 2021.", style="font-size:12px"),
                          hr()
                  ),

                  tabPanel("Cause of Death",
                           tags$br(),
                           fluidRow(virtualSelectInput("iso3", div("Select country, area or WHO region:", style="font-size:18px"), 
                                                       choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                    "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                       selected = "Global",
                                                       search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "40%",
                                                       choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))),
                           radioButtons("sexcod", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE, width = "100%"),
                           tags$br(),
                           tabsetPanel(
                             tabPanel("Ranking of cause of death",
                                      tags$br(),
                                      fluidRow(div(textOutput("ranktext"), style="font-size:20px")),
                                      tags$br(),
                                      fluidRow(plotOutput("codrank", height = "700px", width = "1300px") %>% withSpinner(type=1)),
                                      tags$br()
                             ),
                             tabPanel("Distribution of cause of death",
                                      tags$br(),
                                      fluidRow(div(textOutput("treemaptext"), style="font-size:20px")),
                                      radioButtons("codswitch", label = "", choices = c("Level 1 causes", "Level 2 causes"), selected = "Level 2 causes", inline = TRUE, width = "400px"),
                                      tags$br(),
                                      fluidRow(plotOutput("treemap", height = "600px") %>% withSpinner(type=1)),
                                      tags$br()
                             ),
                             type = "pills"
                           ),
                           tags$br(),
                           div("Based on World Health Statistics 2023 and Global Health Estimates 2021.", style="font-size:12px"),
                           hr()
                           ),

                  # tabPanel("Mapped Indicators",
                  #          tags$br(),
                  #          fluidRow(
                  #            column(7, 
                  #                   # selectInput("ind", "Select indicator:", choices = inds, selected = "Maternal mortality ratio (per 100 000 live births)", width = "700px")
                  #                   virtualSelectInput("ind", div("Select Indicator:", style="font-size:18px"), 
                  #                                      choices =  inds, 
                  #                                      selected = "Maternal mortality ratio (per 100 000 live births)",
                  #                                      search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "100%",
                  #                                      choicesOpt = rep_len("font-size: 16px; color: #00205C;", 57))
                  #                   ),
                  #            # column(7, sliderInput("yearmap", "Select year:", min = 2000, max = 2022, value = 2020, sep = "", width = "400px"))
                  #            column(5, virtualSelectInput("yearmap", div("Select year:", style="font-size:18px"), 
                  #                                         choices = c("2000", "2001", "2002", "2003", "2004", "2005",
                  #                                                     "2006", "2007", "2008", "2009", "2010", "2011",
                  #                                                     "2012", "2013", "2014", "2015", "2016", "2017", 
                  #                                                     "2018", "2019", "2020"), 
                  #                                         selected = c("2020"),
                  #                                         search = FALSE, multiple = FALSE, keepAlwaysOpen = FALSE,
                  #                                         choicesOpt = rep_len("font-size: 16px; color: #00205C;", 21)
                  #                                         )
                  #            )
                  #                   
                  #          ),
                  #          fluidRow(highchartOutput("map", height = "700px") %>% withSpinner(type=1)),
                  #          tags$br(),
                  #          div("Based on World Health Statistics 2024 and Global Health Estimates 2021.", style="font-size:12px"),
                  #          hr()
                  #          ),

                  tabPanel("Indicator Table",
                           tags$br(),
                           p(tabletext),
                           hr(),
                           fluidRow(
                             column(3,
                                    virtualSelectInput("regtab2", "Select WHO region:", choices = regions2, selected = regions2, multiple = TRUE, width = "100%"),
                                    br(),
                                    virtualSelectInput("isotab2", "Select country:", choices = isoreg2, selected = isoreg2, search = TRUE, multiple = TRUE, width = "100%"),
                                    br(),
                                    virtualSelectInput("indtab2", "Select indicator:", choices = ind, 
                                                       selected = c("UHC: Service coverage index"),
                                                       search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, width = "100%"),
                                    br(),
                                    virtualSelectInput("yeartab2", "Year:", choices = yeartab, selected = yeartab, multiple = TRUE, width = "100%"),
                                    br(),
                                    checkboxGroupInput("sextab2", "Sex:", choices = c("Female", "Male", "Both sexes"), selected = "Both sexes"),
                                    br(),
                                    checkboxGroupInput("loctab2", "Urban/Rural:", choices = c("Rural", "Urban", "Both Rural and Urban"), selected = "Both Rural and Urban")
                             ),
                             column(9,
                                    DTOutput("dt") %>% withSpinner(type=1)
                             )
                           ),
                           tags$br(),
                           div("Based on World Health Statistics 2023 and Global Health Estimates 2021.", style="font-size:12px"),
                           hr()
                   ),
                  tabPanel("Benchmarking",
                           tags$br(),
                           tabsetPanel(
                             tabPanel("Explore indicators over time",
                                      tags$br(),
                                      div("Compare indicators between countries, areas or WHO regions:", style="font-size:20px"),
                                      tags$br(),
                                      fluidRow(
                                        column(3,
                                               virtualSelectInput("indgraph", "Select indicator:", choices = ind,
                                                                  selected = c("Life expectancy at birth (years)"),
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = F, width = "100%"),
                                               br(),
                                               virtualSelectInput("iso3graph", "Select countries, areas or WHO regions:", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, width = "100%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               br(),
                                               virtualSelectInput("yeargraph", "Select years:", choices = yeartab,
                                                                  selected = yeartab,
                                                                  search = TRUE, multiple = TRUE, keepAlwaysOpen = F, width = "100%"),
                                               br(),
                                               radioButtons("sexgraph", "Sex:", choices = c("Female", "Male", "Both sexes"), selected = "Both sexes")
                                        ),
                                        column(9, highchartOutput("legraph", height = "900px", width = "1000px")%>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Top 10 causes of death",
                                      tags$br(),
                                      div("Compare the top 10 causes of death between two countries, areas or WHO regions:", style="font-size:20px"),
                                      fluidRow(
                                        column(6, align = "center",
                                               virtualSelectInput("iso3comp1", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "50%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sexrankcomp1", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yearrankcomp1", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               ),
                                        column(6, align = "center",
                                               virtualSelectInput("iso3comp2", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "50%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sexrankcomp2", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yearrankcomp2", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               )
                                      ),
                                      tags$br(),
                                      fluidRow(
                                        column(6, align = "center", plotOutput("codrankcomp1", height = "500px") %>% withSpinner(type=1)),
                                        column(6, align = "center", plotOutput("codrankcomp2", height = "500px") %>% withSpinner(type=1))
                                      ),
                                      tags$br(),
                             ),
                             tabPanel("Distribution of cause of death",
                                      tags$br(),
                                      div("Compare the distribution of cause of death between two countries, areas or WHO regions:", style="font-size:20px"),
                                      fluidRow(
                                        column(6, align = "center",
                                               virtualSelectInput("iso3compt1", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "50%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sextreecomp1", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yeartreecomp1", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               ),
                                        column(6, align = "center",
                                               virtualSelectInput("iso3compt2", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "50%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sextreecomp2", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yeartreecomp2", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               )
                                      ),
                                      tags$br(),
                                      fluidRow(
                                        column(6, highchartOutput("treemapcomp1", height = "600px") %>% withSpinner(type=1)),
                                        column(6, highchartOutput("treemapcomp2", height = "600px") %>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Heatmap of cause of death",
                                      tags$br(),
                                      div("Compare cause of death in 2021 between countries, areas or WHO regions:", style="font-size:20px"),
                                      tags$br(),
                                      fluidRow(
                                        column(3,
                                               virtualSelectInput("iso3heat1", "Select inital country, area or WHO region:",
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "100%", height = "100%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               virtualSelectInput("iso3heat2", "Select countries, areas or WHO regions to compare:",
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod),
                                                                  selected = c("Afghanistan", "Albania", "Algeria"),
                                                                  search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, width = "100%", height = "100%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sexheat", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F)
                                               ),
                                        column(9, highchartOutput("heatmap", height = "700px", width = "950px")%>% withSpinner(type=1))
                                        ),
                                      tags$br()
                                      ),
                             tabPanel("Top burden countries by disease",
                                      tags$br(),
                                      fluidRow(div(textOutput("heatmapv2text"), style="font-size:20px")),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               radioButtons("typeheatv2", label = "Select measure:", choices = c("Number of deaths", "Mortality rate"), selected = "Number of deaths", inline = F),
                                               # radioButtons("burdheatv2", label = "Rank:", choices = c("highest burden", "lowest burden"), selected = "highest burden", inline = T),
                                               sliderTextInput("levelheatv2", label = "Select cause of death level:", choices = c("1", "2", "3"), selected = "2"),
                                               virtualSelectInput("codheatv2", "Select cause of death:",
                                                                  choices = level2, 
                                                                  selected = "Infectious and parasitic diseases",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "100%", height = "100%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 130)),
                                               # radioButtons("yearheatv2", label = "Select year:", choices = c("2000", "2021"), selected = "2021", inline = F),
                                               radioButtons("sexheatv2", label = "Select sex:", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F)
                                               ),
                                        column(4, align = "center", 
                                               # div("Countries with the highest burden in 2021", style="font-size:18px"), 
                                               div("2000", style="font-size:18px"), 
                                               # plotOutput("heatmapv2_year1", height = "800px")%>% withSpinner(type=1)
                                               uiOutput("heatmapv2_year1")%>% withSpinner(type=1),
                                               style = "padding-left: 20px;"
                                               ),
                                        column(2, align = "center",
                                               # fluidRow(" "),
                                               fluidRow(plotOutput("bump", height = "690px", width = "235px")%>% withSpinner(type=1)),
                                               style = "padding-left: 0px;padding-right: 0px;"
                                               ),
                                        column(4, align = "center", 
                                               div("2021", style="font-size:18px"), 
                                               # plotOutput("heatmapv2_year2", height = "800px")%>% withSpinner(type=1)
                                               uiOutput("heatmapv2_year2")%>% withSpinner(type=1),
                                               style = "padding-right: 20px;"
                                               )
                                        ),
                                      tags$br()
                                      ),
                             tabPanel("SDG Forecast",
                                      tags$br(),
                                      fluidRow(
                                        virtualSelectInput("iso3sdg", "Select country, area or WHO region:",
                                                           choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                        "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3sdg_), 
                                                           selected = "Global",
                                                           search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "40%", height = "100%",
                                                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                      ),
                                      tags$br(),
                                      fluidRow(div(textOutput("sdgtext"), style="font-size:20px")),
                                      tags$br(),
                                      uiOutput("sdgtable"),
                                      tags$br()
                                      ),
                             tabPanel("SDG Progress",
                                      tags$br(),
                                      fluidRow(div(textOutput("sdgv2text"), style="font-size:20px")),
                                      tags$br(),
                                      fluidRow(
                                        virtualSelectInput("sdgind", "Select SDG indicator:",
                                                           choices =  indsdg_,
                                                           selected = "Adolescent/ Child Obesity",
                                                           search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "40%", height = "100%",
                                                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 40))
                                      ),
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(
                                        column(1),
                                        column(4, align = "center", 
                                               div("2018", style="font-size:18px"), 
                                               uiOutput("sdgtablev2_y1")%>% withSpinner(type=1),
                                               style = "padding-left: 20px;"
                                        ),
                                        column(2, align = "center",
                                               fluidRow(plotOutput("sdgbump", height = "690px", width = "235px")%>% withSpinner(type=1)),
                                               style = "padding-left: 0px;padding-right: 0px;"
                                        ),
                                        column(4, align = "center", 
                                               div("2021", style="font-size:18px"), 
                                               uiOutput("sdgtablev2_y2")%>% withSpinner(type=1),
                                               style = "padding-right: 20px;"
                                        ),
                                        column(1)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("UHC Forecasts",
                                      tags$br(),
                                      fluidRow(
                                        virtualSelectInput("iso3uhc", "Select country, area or WHO region:",
                                                           choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                        "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3sdg_), 
                                                           selected = "Global",
                                                           search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "40%", height = "100%",
                                                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                      ),
                                      tags$br(),
                                      fluidRow(div(textOutput("uhctext"), style="font-size:20px")),
                                      tags$br(),
                                      plotOutput("uhcgraph", height = "800px", width = "1200px") %>% withSpinner(type=1),
                                      tags$br()
                             ),
                             type = "pills"
                           ),
                           tags$br(),
                           div("Based on World Health Statistics 2023 and Global Health Estimates 2021.", style="font-size:12px"),
                           hr()
                  ),
                  tabPanel("Future Scenarios",
                           tags$br(),
                           tags$br(),
                           div("Under development.", style = "font-size:14px")
                  )
                  
)
)
)
