## About
This code creates a R Shiny dashboard that presents interavtive visualizations and trends in Global Health Estimates and health related SDG indicators for countries and regions.

## Code
1. "create_datasets.R" assembles all the data used in the dashbaord and stores it in "datasets.rda" file - dashbaord runs faster this way.
2. "global.R" sets up all the filter options used in the dashboard.
3. "ui.R" sets up the user interface: what the user sees and interacts with.
4. "server.R" defines the back-end logic: produces the visualizations and calculations as the user interacts with the dashboard.
5. "app.R" is the single entry point that combines both the user interface (UI) and the server logic so that the app can run.

## Data sources
- Global Health Estimates, cause of death: https://xmart-api-public.who.int/DEX_CMS/GHE_FULL_FOCUS
- Life & healthy life expectancy: https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-life-expectancy-and-healthy-life-expectancy
- Triple Billion: https://data.who.int/dashboards/global-progress/triple-billion?n=o
- Health-related SDG indicators: https://www.who.int/data/gho/data/indicators
