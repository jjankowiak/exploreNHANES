library(exploreNHANES)
library(dplyr)
source("modules/demographicsModule.R")
source("modules/activityModule.R")
source("modules/rawDataModule.R")

ui <- fluidPage(
  navbarPage(
    title = "Explore NHANES data",
    demographicsUI("demo"),
    activityUI("activ"),
    rawDataUI("raw")
  )
)

server <- function(input, output) {
  callModule(demographics, "demo")
  callModule(activity, "activ")
  callModule(rawData, "raw")
}

shinyApp(ui, server)
