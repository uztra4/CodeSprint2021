


# User Profile Widget
#################################

UserProfileWidgetUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('ProfileBadge'))
  )
}

UserProfileWidget <- function(input, output, session, credentials, ...) {
  
  ns <- session$ns
  
  output$ProfileBadge <- renderUI({
    req(credentials()$user_auth)
    
    sidebarUserPanel(
      credentials()$info$name,
      image = "img/usr/user_profile.png"
    )
  })
}









