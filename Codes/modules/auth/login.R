#' login UI module
#'
#' Shiny UI Module for use with \link{login}
#' 
#' Call via \code{loginUI("your_id")}
#'
#' @param id Shiny id
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#'
#' @return Shiny UI
#'
#' @export
library(RSQLite)
library(DBI)
loginUI <- function(id, title = "Please Log In", user_title = "User Name", pass_title = "Password", login_title = "Log in",
                    error_message = "Invalid username or password! guest password is guest", default_username = '', default_password = '') {
  ns <- shiny::NS(id)
  
  
	# ucon=dbConnect(RSQLite::SQLite(), sprintf("/db/userDB.sqlite"))
	# sql='select username from user_base'
	# user_base=dbGetQuery(ucon,sql)
	# dbDisconnect(ucon)
	
  usernameList=user_base$username
  default_username='w001'
  default_password='guest'
  returnClickJS <- '
    $(document).keyup(function(event) {
      if ($("#login-password").is(":focus") && (event.keyCode == 13)) {
          $("#login-button").click();
      }
    });
  '
  
  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
             
    tags$head(
      tags$script(HTML(returnClickJS)),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js", type = "text/javascript")
    ),
    
    shiny::wellPanel(
      
      shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
      shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title), value=default_username),
      shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title), value = default_password),
      
      shiny::div(
        style = "text-align: center;",
        shiny::actionButton(ns("button"), login_title, class = "btn-primary")
      ),
      
      shinyjs::hidden(
        shiny::div(id = ns("error"), style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center",
          shiny::tags$p(error_message)
        )
      )
    )
  )
}

#' login server module
#'
#' Shiny authentication module for use with \link{loginUI}
#'
#' Call via \code{shiny::callModule(login, "your_id", ...)}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param data data frame or tibble containing usernames, passwords and other user data
#' @param hashed Deprecated. shinyauthr now uses the sodium package for password hashing and decryption. If you have previously hashed your passwords with the digest package to use with shinyauthr please re-hash them with sodium for decryption to work. 
#' @param algo Deprecated
#' @param log_out [reactive] supply the returned reactive from \link{logout} here to trigger a user logout
#'
#' @return The module will return a reactive 2 element list to your main application. 
#'   First element \code{user_auth} is a boolean inditcating whether there has been
#'   a successful login or not. Second element \code{info} will be the data frame provided
#'   to the function, filtered to the row matching the succesfully logged in username. 
#'   When \code{user_auth} is FALSE \code{info} is NULL.
#' 
#' @importFrom rlang :=
#' 
#' @examples
#' \dontrun{
#'   user_credentials <- shiny::callModule(login, "login", 
#'                                         data = user_base,
#'                                         log_out = reactive(logout_init()))
#' }
#'
#' @export
login <- function(input, output, session, data, hashed, algo, log_out = NULL) {
  
  # shinyjs::runjs('$("body").addClass("sidebar-collapse");')
  
  if (exists('AppConfig') && isFALSE(AppConfig$useAuthentication)) {
    credentials <- shiny::reactiveValues(
      user_auth = T,
      info = as_tibble(list(
        'username' = 'demo',
        'password_hash' = sodium::password_store('demo'),
        'permissions' = 'demo',
        'name' = 'demo'
      ))
    )
    
    return(reactive({reactiveValuesToList(credentials)}))
  }
  
  credentials <- shiny::reactiveValues(user_auth = F, info = NULL)   

  shiny::observeEvent(log_out(), {
    credentials$user_auth <- F
    credentials$info <- NULL
    shiny::updateTextInput(session, "password", value = "")
  })
  
  shiny::observeEvent(credentials$user_auth, ignoreInit = F, {
    if (credentials$user_auth) {
      shinyjs::runjs('$("body").removeClass("sidebar-collapse");')
      shinyjs::hide(id = "panel")
      shinyjs::show(selector = '#logout-button')
    } else {
      shinyjs::runjs('$("body").addClass("sidebar-collapse");')
      shinyjs::show(id = "panel")
      shinyjs::hide(selector = '#logout-button')
    }
  })
  
  shiny::observeEvent(input$button, {
    
    # check for match of input username to username column in data
    row_username <- data %>%
      dplyr::filter(username == input$user_name) %>%
      pull(username)
    
    if (length(row_username)) {
      row_password <- data %>%
        dplyr::filter(username == input$user_name) %>%
        dplyr::pull(password_hash)
      password_match <- sodium::password_verify(row_password, input$password)
    } else {
      password_match <- F
    }
    
    # if user name row and password name row are same, credentials are valid
    if (length(row_username) == 1 && password_match) {
      credentials$user_auth <- T
      credentials$info <- data %>%
        dplyr::filter(username == input$user_name) %>%
        collect()
    } else {
      # if not valid temporarily show error message to user
      shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade"))
    }
    
  })
  
  # return reactive list containing auth boolean and user information
  shiny::reactive({
    shiny::reactiveValuesToList(credentials)
  })
  
}