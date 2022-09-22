
source('src/global.R')

# UI
################################

shinyUI(
  shinydashboardPlus::dashboardPagePlus(
    AppHeaderUI,
    AppLeftSideBarUI,
    AppBodyUI
  )
)








