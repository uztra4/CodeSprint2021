
DocModuleUI <- function(id) {
	ns <- NS(id)
	fluidPage(
		tags$head(tags$link(rel="stylesheet", href="css/styles.css", type="text/css")
		,tags$script(src="fileUp.js"))
		,uiOutput(ns('mainUI'))
	)
}




DocModule <- function(input, output, session, credentials, ...) {
	######### Keep these ######################
	ns <- session$ns
	gAppendAccessLog(credentials()$info$username,getwd(),session$ns("name"),'','')
	
	username=credentials()$info$username
	userDbInfo=user_base[which(user_base$username==username),]
	permissions=userDbInfo$permissions
	
	addResourcePath("pdf_folder","www/documentation")
	
	output$mainUI=renderUI({
		tabsetPanel(
			tabPanel('pdf'
				,uiOutput(ns('pdfUI'))
			)
			,tabPanel('video'
				,uiOutput(ns('video'))
			)
			,tabPanel('Theme PS3'
				,uiOutput(ns('scopeUI'))
			)
			# ,tabPanel('power point'
				# ,slickROutput(ns('slickr'),width="1200px")
			# )
			
		)
	})
	
	output$scopeUI=renderUI({
		imgDir='www/documentation'
		list(
			box(width=12,height='1000px'
				,renderImage({
					outfile <- sprintf("%s/problemStatement.png",imgDir)
					list(src = outfile,
						 contentType = "image/png",
						 #width = 800,
						 height = 1000,
						 alt = "PS3"
					)
				}, deleteFile=FALSE)
			)
			,box(width=12,height='1000px'
				,renderImage({
					outfile <- sprintf("%s/warehousePic.png",imgDir)
					list(src = outfile,
						 contentType = "image/png",
						 width = 800,
						 #height = 400,
						 alt = "Map"
					)
				}, deleteFile=FALSE)
			)
		)
	})
	
	output$pdfUI=renderUI({
		
		tags$iframe(src="pdf_folder/breakfastSearch.pdf",style="height:800px; width:100%;scrolling=yes")
	})
	
	output$video=renderUI({
		videoFn="www/breakfastSearch.mp4"
		list(
			HTML("<a href='https://youtu.be/fLSPUWmzXTM' target=new>Link to Youtube video</a><br />")
			,if (file.exists(videoFn))
			{
				tags$video(id='video',src="breakfastSearch.mp4",type = "video/mp4", width = "1080px", height = "480px",  controls = "controls",autoplay="0")
			}
		)
	})
	
	
	
}


# Page Config
#################################
DocPageConfig <- list(
  
  # Title for menu
  'title' = 'Documentation',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('haulier','warehouse')
)
