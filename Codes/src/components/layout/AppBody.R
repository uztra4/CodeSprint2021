
# Source Theme
source('src/components/layout/AppTheme.R')


# Dashboard Body UI
#################################

widgetUI <- fluidRow(AppPagesUI('AppPages')) %>% {
  if (exists('AppConfig') && isFALSE(AppConfig$useAuthentication)) .
  else tagAppendChild(., loginUI("login", title = 'warehousesolutions'))
}

AppBodyUI <- dashboardBody(
  
  # Scripts
  shinyjs::useShinyjs(),
  AppTheme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  widgetUI
)




