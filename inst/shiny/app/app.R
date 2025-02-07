library(gmhdatahub)

# decrypt_cfg_file("inst/shiny/app/")

Sys.setenv(R_CONFIG_FILE = "config.yml")

ui <- gmhdatahub::app_ui
server <- gmhdatahub::app_server

shinyApp(ui, server)
