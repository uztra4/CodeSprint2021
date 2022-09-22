
BBaseModuleUI <- function(id) {
	ns <- NS(id)
	fluidPage(
		tags$head(tags$link(rel="stylesheet", href="css/styles.css", type="text/css")
		,tags$script(src="fileUp.js"))
		,uiOutput(ns('mainUI'))
	)
}




BBaseModule <- function(input, output, session, credentials, ...) {
	######### Keep these ######################
	ns <- session$ns
	gAppendAccessLog(credentials()$info$username,getwd(),session$ns("name"),'','')
	
	username=credentials()$info$username
	userDbInfo=user_base[which(user_base$username==username),]
	permissions=userDbInfo$permissions
	
	###################################################
	### format(as.POSIXct(tn, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M")
	### as.numeric(timeStr)
	
	###################################################
	
	
	output$mainUI=renderUI({
		list(
			box(width=12
				, dateInput(ns('bookDate'),'Date',value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
			)
			,DTOutput(ns('mainResultUI'))
		)
	})
	output$mainResultUI = renderDT({
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		dockScheduleDfSub = dockScheduleDf[which(dockScheduleDf$haulierID==username), ]
		
		formatDTDisplay(dockScheduleDfSub,scrollY='1500px')
	})
	########################### Overview #############################
	
}


# Page Config
#################################
BBasePageConfig <- list(
  
  # Title for menu
  'title' = 'Haulier',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('haulier')
)
