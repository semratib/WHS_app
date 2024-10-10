
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
              ".nav-bar{font-size:35px; border-radius:20px; padding-left: 0.3in;}",
              ".box {border: 1px solid #e6e7e8; background-color: #e6e7e8; padding: 20px; border-radius:20px;}",
              ".butt{background:white; border:1px solid #00205C;}"
              ),
              # tags$head(tags$style(HTML(".nav li {width: 25%;}"))),
              
              titlePanel(title = div(h1("Global Health Indicators", style="font-size:34px;margin-bottom:10px"), 
                                     h3("Data analytics platform by the Division of Data, Analytics and Delivery for Impact at WHO", style="font-size:20px")
                                     # div(h4("Development version/Temporary internal use only", style="font-size:18px"))
                                     )
                         ),
                tabsetPanel( 
                  type = "tabs",

                  tabPanel(div("Country profile", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
                           tags$br(), 
                           box(
                             fluidRow(
                               column(8, virtualSelectInput("iso3le", div("Select country, area or WHO region:", style="font-size:18px; font-weight:bold;"), 
                                                       choices =  iso3le , 
                                                       selected = "Global",
                                                       search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "50%",
                                                       choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))),
                               column(4,
                                      div(downloadButton("pdf_all", "Save pdf", class = "butt"), style="float:right; padding:15px"),
                                      div(downloadButton("jpeg_all", "Save jpeg", class = "butt"), style="float:right; padding:15px"),
                                      div(downloadButton("data_all", "Download data", class = "butt"), style="float:right; padding:15px"))
                               # column(4, downloadButton("download_button", "Download disease burden graphs in one pdf"))
                             ), width = 12),
                           # tags$head(
                           #   tags$style(
                           #     HTML(".shiny-notification {position:fixed;top: calc(30%);left: calc(45%);}"
                           #     )
                           #   )
                           # ),
                           # hr(),
                           tags$br(), 
                           navlistPanel(id = "pages",
                           # tabsetPanel(
                             tabPanel("Life expectancy and Healthy life expectancy time-series",
                                      fluidRow(column(12, align = "center", div(textOutput("lehetext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      fluidRow(column(12, align = "center", 
                                                      checkboxGroupInput("sex", "",
                                                                  choices = c("Both sexes", "Female", "Male"),
                                                                  selected = c("Both sexes", "Female", "Male"),
                                                                  inline = TRUE,
                                                                  width = "700px"))),
                                      fluidRow(
                                        # column(1,
                                        #        downloadButton("data_le", "Download data"), br(),br(),
                                        #        downloadButton("jpeg_le", "Save jpeg"), br(),br(),
                                        #        downloadButton("pdf_le", "Save pdf"), br(), br()
                                        # ),
                                        column(1),
                                        column(10, align = "center", plotOutput("lehe", height = "650px"),
                                               tags$a(href="https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-life-expectancy-and-healthy-life-expectancy", "See more: Life expectancy and Healthy life expectancy (who.int)", style = "float:right")
                                        ),
                                        column(1)
                                        ),
                                      # fluidRow(column(12, align = "center", uiOutput("lehe", width = "1200px"))),
                                      # tags$br(),
                                      # "See more: https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-life-expectancy-and-healthy-life-expectancy",
                                      tags$br()
                                      ),
                             tabPanel("Life expectancy calculator",
                                      fluidRow(column(12, align = "center", div(textOutput("lecodtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      radioButtons("sexle", label = div("Select sex:", style="font-weight:bold; font-size:18px"), choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               fluidRow(div("Scale the 2021 mortality level (%):", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               fluidRow(sliderInput("scalar1", textOutput("cod1"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar2", textOutput("cod2"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar3", textOutput("cod3"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar4", textOutput("cod4"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar5", textOutput("cod5"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar6", textOutput("cod6"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar7", textOutput("cod7"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar8", textOutput("cod8"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar9", textOutput("cod9"), min = 0, max = 100, value = 100, ticks = F, width = "100%")),
                                               fluidRow(sliderInput("scalar10", textOutput("cod10"), min = 0, max = 100, value = 100, ticks = F, width = "100%"))
                                               # br(), br(),
                                               # downloadButton("data_lecalc", "Download data"), br(),br(),
                                               # downloadButton("jpeg_lecalc", "Save jpeg"), br(),br(),
                                               # downloadButton("pdf_lecalc", "Save pdf")
                                        ),
                                        column(5, align = "center",
                                               fluidRow(div("Number of deaths, 2021", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               fluidRow(plotOutput("barcod", height = "1000px"))
                                        ),
                                        column(5, align = "center",
                                               fluidRow(div("Life expectancy at birth (years)", style="font-weight:bold; font-size:18px")),
                                               tags$br(),
                                               highchartOutput("codle", height = "865px")
                                        )
                                      ),
                                      br(),
                                     tags$a(href="https://github.com/ihmeuw-demographics/demCore/", "Methods: Paulson K, Nguyen G, Callender C, Pease S (2024). demCore: Core Functions for Demography. R package version 0.0.0.9000, https://github.com/ihmeuw-demographics/demCore/.", style = "float:right")
                             ),
                             tabPanel("Life expectancy decomposition by cause of death",
                                      fluidRow(column(12, align = "center", div(textOutput("ledecomptext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        # column(1, downloadButton("data_decomp", "Download data"), br(),br(),
                                        #        downloadButton("jpeg_decomp", "Save jpeg"), br(),br(),
                                        #        downloadButton("pdf_decomp", "Save pdf"), br(),br()
                                        # ),
                                        column(1),
                                        column(10, align = "center", 
                                               plotOutput("decomp", height = "700px") %>% withSpinner(type=1),
                                               "Life expectancy is presented as vertical lines: black is life expectancy in 2000, blue is life expectancy in 2021.",
                                               br(),
                                               "Causes of death to the left of 2000 contributed to a decrease in life expectancy and causes of death to the right contributed to an increase in life expectany."
                                               ),
                                        column(1)
                                        ),
                                      # fluidRow(
                                      #   column(3),
                                      #   column(6,  ),
                                      #   column(3)
                                      # ),
                                      tags$br(),
                                      tags$br(),
                                      tags$a(href="https://github.com/timriffe/DemoDecomp", "Methods: Riffe T (2024). DemoDecomp: Decompose Demographic Functions. R package version 	1.14.1, https://github.com/timriffe/DemoDecomp.", style = "float:right")
                             ),
                             tabPanel("Life expectancy decomposition over time",
                                      fluidRow(column(12, align = "center", div(textOutput("ledecomptext_time"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        # column(1, downloadButton("data_decomp_time", "Download data"), br(),br(),
                                        #        downloadButton("jpeg_decomp_time", "Save jpeg"), br(),br(),
                                        #        downloadButton("pdf_decomp_time", "Save pdf"), br(),br()
                                        # ),
                                        column(1),
                                        column(10, align = "center",
                                               radioButtons("sex_decomptime", label = "" , choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               br(), br(),
                                               plotOutput("decomp_time", height = "700px") %>% withSpinner(type=1)
                                        ),
                                        column(1)
                                      ),
                                      tags$br(),
                                      tags$br(),
                                      tags$a(href="https://github.com/timriffe/DemoDecomp", "Methods: Riffe T (2024). DemoDecomp: Decompose Demographic Functions. R package version 	1.14.1, https://github.com/timriffe/DemoDecomp.", style = "float:right")
                             ),
                             tabPanel("Change in rank of cause of death",
                                      fluidRow(column(12, align = "center", div(textOutput("ranktext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", radioButtons("sexcod2", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE, width = "100%"))),
                                      tags$br(),
                                      # fluidRow(plotOutput("codrank", height = "700px", width = "1300px") %>% withSpinner(type=1)),
                                      fluidRow(
                                        # column(1,
                                        #        downloadButton("data_arrow", "Download data"), br(),br(),
                                        #        downloadButton("jpeg_arrow", "Save jpeg"), br(),br(),
                                        #        downloadButton("pdf_arrow", "Save pdf"), br(),br()
                                        # ),
                                        column(1),
                                        column(10, plotOutput("codrank", height = "600px") %>% withSpinner(type=1),
                                               tags$a(href="https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-leading-causes-of-death", "See more: Global health estimates: Leading causes of death (who.int)", style = "float:right")
                                               ),
                                        column(1)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Change in distribution of cause of death",
                                      fluidRow(column(12, align = "center", div(textOutput("treemaptext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               div(
                                                 sliderTextInput("codswitch", 
                                                                 label = div("Level of cause of deaths:", style="font-weight:bold; font-size:18px"),
                                                                 choices = c("Level 1 causes", "Level 2 causes"), 
                                                                 selected = "Level 2 causes",  width = "80%"), 
                                                 style = "font-size:14px"
                                                 ),
                                               tags$br(),
                                               radioButtons("sexcod", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F, width = "100%")
                                               # tags$br(),tags$br(),tags$br(),
                                               # downloadButton("data_treemap", "Download data"), br(),br(),
                                               # downloadButton("jpeg_treemap", "Save jpeg"), br(),br(),
                                               # downloadButton("pdf_treemap", "Save pdf"), br(),br()
                                               ), 
                                        column(10, align = "center",
                                               plotOutput("treemap", height = "700px") %>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Triple Billions Indicator Progress",
                                      fluidRow(column(12, align = "center", div(textOutput("sdgtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(
                                        column(1
                                               # downloadButton("data_sdgtable", "Download as csv"), br(),br(),
                                               # downloadButton("jpeg_sdgtable", "Save image")
                                               # downloadButton("pdf_sdgtable", "Save pdf")
                                               ),
                                        column(10, align = "center",
                                               uiOutput("sdgtable") %>% withSpinner(type=1),
                                               plotOutput("sdgtable_legend", height = "100px", width = "800px"),
                                               tags$a(href = "https://www.who.int/publications/m/item/3b-forecasting-methods", "Methods: Triple Billions Forecasting (who.int)", style = "float:right")),
                                        column(1)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Triple Billions Indicator Contributions",
                                      fluidRow(column(12, align = "center", div(textOutput("TB_contrib_text"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(column(12, align = "center", radioButtons("TB", label = "", choices = c("UHC", "HPOP", "HEP"), selected = "UHC", inline = TRUE, width = "100%"))),
                                      tags$br(),
                                      fluidRow(
                                        # column(1,
                                        #        downloadButton("data_TBcontrib", "Download data"), br(),br(),
                                        #        downloadButton("jpeg_TBcontrib", "Save jpeg"), br(),br(),
                                        #        downloadButton("pdf_TBcontrib", "Save pdf"), br(),br()
                                        # ),
                                        column(1),
                                        column(10, align = "center",
                                               plotOutput("TBcontrib", height = "800px") %>% withSpinner(type=1),
                                               tags$a(href ="https://data.who.int/dashboards/global-progress/triple-billion)", "See more: Triple Billions Contributions (who.int)", style = "float:right"),br(),
                                               tags$a(href ="https://www.who.int/publications/m/item/3b-forecasting-methods", "Methods: Triple Billions Forecasting (who.int)", style = "float:right")),
                                        column(1)
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Triple Billions: UHC",
                                      fluidRow(column(12, align = "center", div(textOutput("uhctext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"))),
                                      tags$br(),
                                      fluidRow(column(1),
                                               column(10, align = "center", highchartOutput("uhcgraph", height = "800px") %>% withSpinner(type=1),
                                               tags$a(href ="https://www.who.int/publications/m/item/3b-forecasting-methods", "Methods: Triple Billions Forecasting (who.int)", style = "float:right")),
                                               column(1)),
                                      tags$br()
                             ),
                             tabPanel("Generate country report",
                                      tags$br(),
                                      downloadButton("download_button", "Download country report", class = "butt"),
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
                             widths = c(2, 10),
                             fluid = F,
                             well = F
                             # type = "pills"
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
                                      div("Compare indicators between countries, areas or WHO regions:", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
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
                                               radioButtons("sexgraph", "Sex:", choices = c("Female", "Male", "Both sexes"), selected = "Both sexes"),
                                               hr(),
                                               div("Note: To download the graph or its data, click menu icon at top right corner of the graph.", style = "font-style: italic; font-size: 16px;"),
                                               style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
                                        ),
                                        column(9, highchartOutput("legraph", height = "980px")%>% withSpinner(type=1))
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Life expectancy decomposition",
                                      tags$br(),
                                      div("Compare life expectancy decomposition (2000 to 2021) by countries for level 2 causes of death.", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
                                      tags$br(),
                                      fluidRow(
                                        column(3,
                                               div("Note: after selection, click 'Show Results' to present and refresh graph.", style = "font-style: italic; font-size: 16px;"),
                                               br(),
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
                                               actionButton("ready", "Show Results"),
                                               br(), br(), br(), br(),
                                               hr(),
                                               br(),
                                               downloadButton("data_decomp_bench", "Download data", class = "butt"),
                                               downloadButton("jpeg_decomp_bench", "Save jpeg", class = "butt"),
                                               downloadButton("pdf_decomp_bench", "Save pdf", class = "butt"),
                                               style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
                                               ),
                                        column(9, 
                                               plotOutput("decomp_bench", height = "1200px", width = "1300px")%>% withSpinner(type=1),
                                               tags$br(),
                                               "Life expectancy is presented as vertical lines: black is life expectancy in 2000, blue is life expectancy in 2021.",
                                               br(),
                                               "Causes of death to the left of 2000 contributed to a decrease in life expectancy and causes of death to the right contributed to an increase in life expectany."
                                               )
                                      ),
                                      tags$br()
                             ),
                             tabPanel("Top 10 causes of death",
                                      tags$br(),
                                      div("Compare the top level 2 causes of death between two countries, areas or WHO regions:", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
                                      tags$br(),
                                      fluidRow(
                                        column(2),
                                        column(4, align = "center",
                                               virtualSelectInput("iso3comp1", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "60%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sexrankcomp1", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yearrankcomp1", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               # style = "background-color: #e6e7e8; border-radius:20px; padding: 10px; margin:20px;"
                                        ),
                                        # column(2),
                                        column(4, align = "center",
                                               virtualSelectInput("iso3comp2", " ", 
                                                                  choices =  c("Global", "African Region", "Eastern Mediterranean Region", "European Region",
                                                                               "Region of the Americas", "South-East Asia Region", "Western Pacific Region" , iso3cod), 
                                                                  selected = "Global",
                                                                  search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "60%",
                                                                  choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                               radioButtons("sexrankcomp2", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = TRUE),
                                               sliderTextInput("yearrankcomp2", label = "", choices = c(2000, 2021), selected = 2021, width = "20%")
                                               # style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;  margin:20px;"
                                        ),
                                        column(2)
                                      ),
                                      tags$br(),
                                      fluidRow(
                                        column(2),
                                        column(4, align = "center", plotOutput("codrankcomp1", height = "500px") %>% withSpinner(type=1)),
                                        column(4, align = "center", plotOutput("codrankcomp2", height = "500px") %>% withSpinner(type=1)),
                                        column(2)
                                      ),
                                      # plotOutput("codrankcomp_leg"),
                                      fluidRow(column(12, align = "center", 
                                                      img(src='legend_top10.jpeg'))
                                               ),
                                      tags$br(),
                                      downloadButton("data_top10cod", "Download data", class = "butt"),
                                      downloadButton("jpeg_top10cod", "Save jpeg", class = "butt"), 
                                      downloadButton("pdf_top10cod", "Save pdf", class = "butt")
                             ),
                             tabPanel("Distribution of cause of death",
                                      tags$br(),
                                      div("Compare the distribution of cause of death between two countries, areas or WHO regions:", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
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
                                      tags$br(),
                                      tags$br(),
                                      downloadButton("data_treemap_bench", "Download data", class = "butt"),
                                      downloadButton("jpeg_treemap_bench", "Save jpeg", class = "butt"),
                                      downloadButton("pdf_treemap_bench", "Save pdf", class = "butt"),
                                      tags$br()
                             ),
                             tabPanel("Heatmap of cause of death",
                                      tags$br(),
                                      div("Compare level 2 causes of death in 2021 between countries, areas or WHO regions:", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
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
                                               radioButtons("sexheat", label = "", choices = c("Both sexes", "Female", "Male"), selected = "Both sexes", inline = F),
                                               hr(),
                                               div("Note: To download the graph or its data, click menu icon at top right corner of the graph.", style = "font-style: italic; font-size: 16px;"),
                                               style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
                                               ),
                                        column(9, highchartOutput("heatmap", height = "800px")%>% withSpinner(type=1))
                                        ),
                                      tags$br()
                                      ),
                             tabPanel("Top burden countries by disease",
                                      tags$br(),
                                      fluidRow(div(textOutput("heatmapv2text"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;")),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               div("Note: after selection, click 'Show Results' to present and refresh graph.", style = "font-style: italic; font-size: 16px;"),
                                               br(),
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
                                               actionButton("readyburden", "Show Results"),
                                               br(), br(), br(), br(),
                                               hr(),
                                               br(),
                                               downloadButton("data_bump_cod", "Download data", class = "butt"), br(), br(),
                                               downloadButton("jpeg_bump_cod", "Save jpeg", class = "butt"), br(), br(),
                                               downloadButton("pdf_bump_cod", "Save pdf", class = "butt"), br(), br(),
                                               style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
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
                                      fluidRow(div(textOutput("sdgv2text"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;")),
                                      tags$br(),
                                      # fluidRow(column(12, align = "center",
                                      #   virtualSelectInput("sdgind", "Select SDG indicator:",
                                      #                      choices =  indsdg_,
                                      #                      selected = "Adolescent/ Child Obesity",
                                      #                      search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "20%", height = "100%",
                                      #                      choicesOpt = rep_len("font-size: 16px; color: #00205C;", 40))
                                      # )),
                                      tags$br(),
                                      fluidRow(
                                        column(2,
                                               virtualSelectInput("sdgind", "Select SDG indicator:",
                                                                    choices =  indsdg_,
                                                                    selected = "Adolescent/ Child Obesity",
                                                                    search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "100%", height = "100%",
                                                                    choicesOpt = rep_len("font-size: 16px; color: #00205C;", 40)),
                                               br(), br(),br(), br(), br(), br(),br(), br(),
                                               hr(),
                                               br(),
                                               downloadButton("data_sdgbump", "Download data", class = "butt"), br(), br(),
                                               downloadButton("jpeg_sdgbump", "Save jpeg", class = "butt"), br(), br(),
                                               downloadButton("pdf_sdgbump", "Save pdf", class = "butt"), br(), br(),
                                               style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
                                               ),
                                        column(1),
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
                                        column(1)
                                      ),
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
                  nav_menu("Maternal Mortality",
                           tabPanel("Across African Region",
                                    tags$br(),
                                    fluidRow(
                                      column(6, align = "center",
                                             div("Maternal mortality ratio in African Region, 2020.", style="font-size:20px; background-color:#ddeff9; padding:15px; border-radius: 15px;"),
                                             br(),
                                             plotOutput("mmrmap", height = "750px") %>% withSpinner(type=1)
                                      ),
                                      column(6, align = "center",
                                             div("Number of years needed to achieve SDG target given the rate of improvement since 2000.", style="font-size:20px; background-color:#ddeff9; padding:15px; border-radius: 15px;"),
                                             br(),
                                             plotOutput("mmrtarget", height = "750px") %>% withSpinner(type=1)
                                      )
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    fluidRow(
                                      column(12, align = "center",
                                             div("Annualized rate of change in MMR in African Region, 2000–2015 and 2016–2020.", style="font-size:20px; background-color:#ddeff9; padding:15px; border-radius: 15px;")
                                      )
                                    ),
                                    tags$br(),
                                    fluidRow(
                                      column(3,
                                             virtualSelectInput("iso3aroc", "Select countries:", 
                                                                choices =  iso3afr_mmr, 
                                                                selected = iso3afr_mmr,
                                                                search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, width = "100%",
                                                                choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                             br(), br(),
                                             hr(),
                                             div("Note: To download the graph or its data, click menu icon at top right corner of the graph.", style = "font-style: italic; font-size: 16px;"),
                                             style = "background-color: #e6e7e8; border-radius:20px; padding: 20px;"
                                      ),
                                      column(9,
                                             highchartOutput("aroc", height = "750px") %>% withSpinner(type=1)
                                      )
                                    ),
                                    tags$br()
                           ),
                           tabPanel("By Country",
                                    tags$br(),
                                    fluidRow(
                                      column(12, align = "center",
                                        virtualSelectInput("iso3cp", div("Select country:", style="font-size:18px; font-weight:bold;"), 
                                                       choices =  c("Benin", "Burkina Faso", "Central African Republic", "Democratic Republic of the Congo", 
                                                                    "Guinea-Bissau", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "South Africa",
                                                                    "United Republic of Tanzania", "Zambia"), 
                                                       selected = "Benin",
                                                       search = TRUE, multiple = FALSE, keepAlwaysOpen = FALSE, width = "30%",
                                                       choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190)),
                                        style = "background-color: #e6e7e8; border-radius:20px; padding: 15px;"
                                        )
                                      ),
                                    tags$br(),
                                    navlistPanel(
                                      well = F, fluid = T, widths = c(2, 10),
                                    # tabsetPanel(
                                    #   type = "pills",
                                      tabPanel("Comparing MMR",
                                               div("Maternal Mortality Ratio, 2000 and 2020.", style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
                                               tags$br(),
                                               plotOutput("benchbar", height = "700px") %>% withSpinner(type=1)
                                      ),
                                      tabPanel("MMR Trajectories",
                                               fluidRow(
                                                 # column(2,
                                                 #        virtualSelectInput("iso3traj", "Select other countries to compare", 
                                                 #                           choices =  iso3afr_mmr, 
                                                 #                           selected = c("Algeria", "Angola"),
                                                 #                           search = TRUE, multiple = TRUE, keepAlwaysOpen = TRUE, width = "100%",
                                                 #                           choicesOpt = rep_len("font-size: 16px; color: #00205C;", 190))
                                                 # ),
                                                 # column(10,
                                                        div(textOutput("trajtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
                                                        br(),
                                                        highchartOutput("mmrtraj2", height = "750px") %>% withSpinner(type=1)
                                                 # )
                                               ),
                                               tags$br(),
                                      ),
                                      tabPanel("Sub-national RMNCH", 
                                               div(textOutput("rmnchtext"), style="font-size:20px; background-color:#ddeff9; padding:0.1in; border-radius: 15px;"),
                                               tags$br(),
                                               fluidRow(
                                                 column(2),
                                                 column(8, align = "center", highchartOutput("rmnch", height = "730px") %>% withSpinner(type=1)),
                                                 column(2)
                                               )
                                      ),
                                      tabPanel("Generate report",
                                               tags$br(),
                                               tags$br(),
                                               downloadButton("download_button_mmr", "Download Maternal Mortality report"),
                                               tags$head(
                                                 tags$style(
                                                   HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(40%);}"
                                                   )
                                                 )
                                               )
                                      )
                                    ),
                                    tags$br()
                                    )
                           )
                  # tabPanel(div("Future Scenarios", style="font-size:18px; padding-left:0.3in; padding-right:0.3in"),
                  #          tags$br(),
                  #          tags$br(),
                  #          div("Under development.", style = "font-size:14px"),
                  #          tags$br(),
                  #          tags$br()
                  # )
                  
),
div("Data source: Global Health Observatory and Global Health Estimates, 2021.", style="font-size:14px; color:white; background-color:#00205C; padding:0.3in"),
# tags$hr(style="color:#00205C; opacity:1;")
)
# )
