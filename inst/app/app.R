library(shiny)
library(complexr)

shinyApp(
  ui     = complexr:::app_ui(),
  server = complexr:::app_server
)
