library(gmhdatahub)

ui <- gmhdatahub::app_ui
server <- gmhdatahub::app_server

shinyApp(ui, server)
