


# Dashboard Pages Module
#################################

# AppPageList <- str_extract(list.files('src/pages/'), '[^.]+')

# AppPageList <- lapply(list.files('src/pages/'), function(x) {
# 
#   if (str_detect(x, '.(r|R)'))
#     return(str_extract(x, '[^.]+'))
# 
#   str_extract(list.files(paste0('src/pages/', x)), '[^.]+')
# })

AppPageList <- str_extract(list.files(paste0('src/pages/'), recursive = T), '[^.]+')

AppPagesUI <- function(id) {
  
  ns <- NS(id)

  PageUIList <- lapply(AppPageList, function(x) {
    x <- str_extract(x, '([^/]+$)')
    
    tabItem(tabName = str_to_lower(x), get(paste0(x, 'ModuleUI'))(ns(paste0(x, 'PageModule'))))
  })
  
  do.call(tabItems, compact(PageUIList))
}

AppPages <- function(input, output, session, credentials, ...) {

  ns <- session$ns
  
  observeEvent(credentials()$info$permissions, {
    lapply(AppPageList, function(x) {
      x <- str_extract(x, '([^/]+$)')
      
      PageConfig <- get(paste0(x, 'PageConfig'))
      if (credentials()$info$permissions %in% PageConfig$permission && !isTRUE(PageConfig$disable))
        callModule(get(paste0(x, 'Module')), paste0(x, 'PageModule'), credentials = credentials, ...)
    })
    
    # Hackish Way to Select Tab
    runjs('setTimeout(function() {$("#LeftSideBarContent-menu>ul>li:nth-child(1)>a").trigger("click");}, 100);')
  })
}










