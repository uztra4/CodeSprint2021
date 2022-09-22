


# Left Side Bar Module
#################################

AppLeftSideBarContentUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    UserProfileWidgetUI(ns('ProfileBadge')),
    uiOutput(ns('menu'))
  )
}

AppLeftSideBar <- function(input, output, session, credentials, ...) {
  
  ns <- session$ns
  
  callModule(UserProfileWidget, 'ProfileBadge', credentials = credentials)
  
  output$menu <- renderUI({
    req(credentials()$info$permissions)
    
    MenuUIList <- lapply(AppPageList, function(x) {
      x <- str_extract(x, '([^/]+$)')
      
      PageConfig <- get(paste0(x, 'PageConfig'))
      if (credentials()$info$permissions %in% PageConfig$permission && !isTRUE(PageConfig$disable))
        menuItem(PageConfig$title, tabName = str_to_lower(x), icon = icon(PageConfig$icon))
      else
        NULL
    })
    
    do.call(sidebarMenu, compact(MenuUIList))
  })
}

AppLeftSideBarUI <- dashboardSidebar(
  collapsed = T,
  
  AppLeftSideBarContentUI('LeftSideBarContent')
)


