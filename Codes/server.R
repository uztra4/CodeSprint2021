
shinyServer(function(input, output, session) {
  
  # Source global variables
  source('src/global.R')
  
  # Authentication
  logout_init <- callModule(logout, "logout", reactive(credentials()$user_auth))
  credentials <- callModule(login, "login", data = user_base, log_out = logout_init)
  
  
  # App Layout
  ###########################

  callModule(AppLeftSideBar, 'LeftSideBarContent', credentials = credentials)
  callModule(AppPages, 'AppPages', credentials = credentials)
 
})
















