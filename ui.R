
##########################################################
## user interface
##########################################################  
# div(style="max-width:14in;margin:auto;",
# div(style="padding-left: 0.5in; padding-right: 0.5in;",
ui<-fluidPage(withMathJax(),
              useShinyjs(),
              includeCSS("www/app_theme.css"),
              theme = bs_theme(bg = "white", fg = "#00205C",
                               primary = "#00205C",
                               secondary = "#009ADE",
                               success = "#F26829"),
              tags$style(
              ".nav-bar{font-size:35px; border-radius:20px; padding-left: 0.3in;}"
              ),
              
              titlePanel(title = div(h1("Global Health Indicators", style="font-size:34px;margin-bottom:10px"), 
                                     h3("Data analytics platform by the Division of Data, Analytics and Delivery for Impact at WHO", style="font-size:20px")
                                     # div(h4("Development version/Temporary internal use only", style="font-size:18px"))
                                     )
                         ),
                tabsetPanel( 
                  type = "tabs",

                  tabPanel(div("Country profile", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
                           tags$br(), 
                             fluidRow(virtualSelectInput("iso3le", div("Select country, area or WHO region:", style="font-size:18px; font-weight:bold;"), 
                                                       choices =  iso3le , 
                                                       selected = "Global",
                                                       search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "30%",
                                                       choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                    ), 
                             tags$br(), 
                           tabsetPanel(
                             tabPanel("Life expectancy and Healthy life expectancy time-series",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("lehetext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      fluidRow(column(12, align = "center", 
                                                      checkboxGroupInput("sex", "",
                                                                  choices = c("Both sexes", "Female", "Male"),
                                                                  selected = c("Both sexes", "Female", "Male"),
                                                                  inline = TRUE,
                                                                  width = "700px"))),
                                      fluidRow(column(2),
                                               column(8, align = "center", plotOutput("lehe", height = "650px")),
                                               column(1,
                                                 downloadButton("data_le", "Download data"),
                                                 downloadButton("jpeg_le", "Save jpeg"),
                                                 downloadButton("pdf_le", "Save pdf")
                                               ),
                                               column(1)
                                               ),
                                      # fluidRow(column(12, align = "center", uiOutput("lehe", width = "1200px"))),
                                      tags$br()
                                      ),
                             tabPanel("Life expectancy calculator",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("lecodtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      radioButtons("sexle", label = div("Select sex:", style="font-weight:bold; font-size:18px"), choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               fluidRow(div("Scale the 2021 mortality level:", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               fluidRow(sliderInput("scalar1", textOutput("cod1"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar2", textOutput("cod2"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar3", textOutput("cod3"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar4", textOutput("cod4"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar5", textOutput("cod5"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar6", textOutput("cod6"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar7", textOutput("cod7"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar8", textOutput("cod8"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar9", textOutput("cod9"), min = 0, max = 1, value = 1, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar10", textOutput("cod10"), min = 0, max = 1, value = 1, ticks = F, width = "100%"))
                                        ),
                                        column(5, align = "center",
                                               fluidRow(div("Number of deaths", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               fluidRow(plotOutput("barcod", height = "1000px"))
                                        ),
                                        column(5, align = "center",
                                               fluidRow(div("Life expectancy at birth (years)", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               highchartOutput("codle", height = "865px")
                                        )
                                      )
                             ),
                             tabPanel("Life expectancy decomposition by cause of death",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("ledecomptext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        column(2),
                                        column(8, align = "center", 
                                               plotOutput("decomp", height = "700px") %>% withSpinner(type=1),
                                               tags$br(),
                                               "Life expectancy is presented as vertical lines: black is life expectancy in 2000, blue is life expectancy in 2021. Causes of death to the left of 2000 contributed to a decrease in life expectancy and causes of death to the right contributed to an increase in life expectany."),
                                        column(2)
                                        # column(1, downloadButton("data_ledecomp_country", "Download data"),
                                        #        downloadButton("jpeg_ledecomp_country", "Save jpeg"),
                                        #        downloadButton("pdf_ledecomp_country", "Save pdf"))
                                        ),
                                      tags$br()
                             ),
                             tabPanel("Change in rank of cause of death",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("ranktext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", radioButtons("sexcod2", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE, width = "100%"))),
                                      tags$br(),
                                      # fluidRow(plotOutput("codrank", height = "700px", width = "1300px") %>% withSpinner(type=1)),
                                      fluidRow(
                                        column(2), 
                                        column(8, plotOutput("codrank", height = "600px") %>% withSpinner(type=1)),
                                        column(1,
                                               downloadButton("data_arrow", "Download data"),
                                               downloadButton("jpeg_arrow", "Save jpeg"),
                                               downloadButton("pdf_arrow", "Save pdf")
                                        ),
                                        column(1)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Change in distribution of cause of death",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("treemaptext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               div(
                                                 sliderTextInput("codswitch", 
                                                                 label = div("Level of Cause of deaths:", style="font-weight:bold; font-size:18px"),
                                                                 choices = c("Level 1 causes", "Level 2 causes"), 
                                                                 selected = "Level 2 causes",  width = "80%"), 
                                                 style = "font-size:14px"
                                                 ),
                                               tags$br(),
                                               radioButtons("sexcod", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F, width = "100%"),
                                               tags$br(),tags$br(),tags$br(),
                                               downloadButton("data_treemap", "Download data"),tags$br(),
                                               downloadButton("jpeg_treemap", "Save jpeg"),tags$br(),
                                               downloadButton("pdf_treemap", "Save pdf")
                                               ), 
                                        column(10, align = "center",
                                               plotOutput("treemap", height = "700px") %>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Triple Billions Indicator Progress",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("sdgtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      uiOutput("sdgtable") %>% withSpinner(type=1),
                                      tags$br()
                             ),
                             tabPanel("UHC Indicator Progress",
                                      tags$br(),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", div(textOutput("uhctext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(column(2),
                                               column(12, align = "center", highchartOutput("uhcgraph", height = "800px") %>% withSpinner(type=1)),
                                               column(2)),
                                      tags$br()
                             ),
                             type = "pills"
                          ),
                          tags$br()
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
                  
                  tabPanel(div("Benchmarking", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
                           tags$br(),
                           tabsetPanel(
                             tabPanel("Explore indicators over time",
                                      tags$br(),
                                      div("Compare indicators between countries, areas or WHO regions:", style="font-size:20px; background-color:#e6e7e8; padding:0.1in"),
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
                                        column(9, highchartOutput("legraph", height = "900px")%>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Life expectancy decomposition",
                                      tags$br(),
                                      div("Compare life expectancy decomposition (2000 to 2021) by countries for level 2 causes of death.", style="font-size:20px; background-color:#e6e7e8; padding:0.1in;"),
                                      tags$br(),
                                      fluidRow(
                                        column(3,
                                               virtualSelectInput(
                                                 "iso3decomp_bench",
                                                 label = "Select countries, areas or WHO regions to compare:",
                                                 choices = list(
                                                   "African Region" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="AFR") %>% distinct(country) %>% pull(),
                                                   "Eastern Mediterranean Region" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="EMR") %>% distinct(country) %>% pull(),
                                                   "European Region" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="EUR") %>% distinct(country) %>% pull(),
                                                   "Region of the Americas" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="AMR") %>% distinct(country) %>% pull(),
                                                   "South-East Asia Region" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="SEAR") %>% distinct(country) %>% pull(),
                                                   "Western Pacific Region" = data %>% filter(GHOcode=="WHOSIS_000001" & region=="WPR") %>% distinct(country) %>% pull()
                                                 ),
                                                 selected = data %>% filter(GHOcode=="WHOSIS_000001" & region=="AFR") %>% distinct(country) %>% pull(),
                                                 search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, showValueAsTags = FALSE, width = "100%",
                                                 choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)
                                               ),
                                               br(),
                                               radioButtons("sexdecomp_bench", label = "Select sex:", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F),
                                               br(),
                                               actionButton("ready", "Show Results")
                                               ),
                                        column(9, plotOutput("decomp_bench", height = "1200px", width = "1300px")%>% withSpinner(type=1))
                                        ),
                                      tags$br()
                             ),
                             tabPanel("Top 10 causes of death",
                                      tags$br(),
                                      div("Compare the top 10 causes of death between two countries, areas or WHO regions:", style="font-size:20px; background-color:#e6e7e8; padding:0.1in;"),
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
                                      tags$br()
                             ),
                             tabPanel("Distribution of cause of death",
                                      tags$br(),
                                      div("Compare the distribution of cause of death between two countries, areas or WHO regions:", style="font-size:20px; background-color:#e6e7e8; padding:0.1in;"),
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
                                      div("Compare cause of death in 2021 between countries, areas or WHO regions:", style="font-size:20px; background-color:#e6e7e8; padding:0.1in;"),
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
                                        column(9, highchartOutput("heatmap", height = "800px")%>% withSpinner(type=1))
                                        ),
                                      tags$br()
                                      ),
                             tabPanel("Top burden countries by disease",
                                      tags$br(),
                                      fluidRow(div(textOutput("heatmapv2text"), style="font-size:20px; background-color:#e6e7e8; padding:0.1in;")),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               radioButtons("typeheatv2", label = "Select measure:", choices = c("Number of deaths", "Mortality rate"), selected = "Number of deaths", inline = F),
                                               tags$br(),
                                               sliderTextInput("levelheatv2", label = "Select cause of death level:", choices = c("1", "2", "3"), selected = "2"),
                                               tags$br(),
                                               virtualSelectInput("codheatv2", "Select cause of death:",
                                                                  choices = level2, 
                                                                  selected = "Infectious and parasitic diseases",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "100%", height = "100%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 130)),
                                               tags$br(),
                                               radioButtons("sexheatv2", label = "Select sex:", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F),
                                               tags$br(),
                                               actionButton("readyburden", "Show Results")
                                               ),
                                        column(1),
                                        column(3, align = "center", 
                                               div("2000", style="font-size:18px"), 
                                               uiOutput("heatmapv2_year1")%>% withSpinner(type=1),
                                               style = "padding-left: 20px;"
                                               ),
                                        column(2, align = "center",
                                               fluidRow(plotOutput("bump", height = "690px")%>% withSpinner(type=1)),
                                               style = "padding-left: 0px;padding-right: 0px;"
                                               ),
                                        column(3, align = "center", 
                                               div("2021", style="font-size:18px"), 
                                               uiOutput("heatmapv2_year2")%>% withSpinner(type=1),
                                               style = "padding-right: 20px;"
                                               ),
                                        column(1)
                                        ),
                                      tags$br()
                                      ),
                             tabPanel("Triple Billion Indicators",
                                      tags$br(),
                                      fluidRow(div(textOutput("sdgv2text"), style="font-size:20px; background-color:#e6e7e8; padding:0.1in;")),
                                      tags$br(),
                                      fluidRow(column(12, align = "center",
                                        virtualSelectInput("sdgind", "Select SDG indicator:",
                                                           choices =  indsdg_,
                                                           selected = "Adolescent/ Child Obesity",
                                                           search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "30%", height = "100%",
                                                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 40))
                                      )),
                                      tags$br(),
                                      fluidRow(
                                        column(2),
                                        column(3, align = "center", 
                                               div("2018", style="font-size:18px"), 
                                               uiOutput("sdgtablev2_y1")%>% withSpinner(type=1),
                                               style = "padding-left: 20px;"
                                        ),
                                        column(2, align = "center",
                                               fluidRow(plotOutput("sdgbump", height = "690px")%>% withSpinner(type=1)),
                                               style = "padding-left: 0px;padding-right: 0px;"
                                        ),
                                        column(3, align = "center", 
                                               div("2021", style="font-size:18px"), 
                                               uiOutput("sdgtablev2_y2")%>% withSpinner(type=1),
                                               style = "padding-right: 20px;"
                                        ),
                                        column(2)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Generate country report",
                                      tags$br(),
                                      fluidRow(
                                        virtualSelectInput("reportiso", "Select country:",
                                                           choices =  iso3cod, 
                                                           selected = "Afganistan",
                                                           search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "35%", height = "100%",
                                                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                      ),
                                      tags$br(),
                                      downloadButton("download_button", "Download disease burden graphs in one pdf"),
                                      tags$head(
                                        tags$style(
                                          HTML(".shiny-notification {position:fixed;top: calc(30%);left: calc(45%);}"
                                          )
                                        )
                                      ),
                                      tags$br(),
                                      tags$br(),
                                      tags$br()
                             ),
                             type = "pills"
                           ),
                           tags$br()
                  ),
                  tabPanel(div("Indicator Table", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
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
                           tags$br()
                  ),
                  tabPanel(div("Future Scenarios", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
                           tags$br(),
                           tags$br(),
                           div("Under development.", style = "font-size:14px")
                  )
                  
),
div("Based on World Health Statistics 2023 and Global Health Estimates 2021.", style="font-size:14px; color:white; background-color:#00205C; padding:0.3in"),
# tags$hr(style="color:#00205C; opacity:1;")
)
# )
