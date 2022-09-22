


# Header UI
#################################

widgetUI <- tags$li(class = "dropdown") %>% {
  if (exists('AppConfig') && isFALSE(AppConfig$useAuthentication)) .
  else tagAppendChild(., logoutUI('logout'))
}

AppHeaderUI <- shinydashboardPlus::dashboardHeaderPlus(
  
  # Branding
  title = tagList(
    tags$a(href = '#top', class = "logo-lg main-logo", img(src = 'img/logo/logoLongSmall.png',width = "100px", height = "50px")),
    tags$a(href = '#top', class = "main-logo", img(src = 'img/logo/miniLogo.png',width = "50px", height = "50px"))
  ),
  
  # Logout
  widgetUI,
  
  # Right Sidebar
  enable_rightsidebar = F,
  rightSidebarIcon = "bars"
)




