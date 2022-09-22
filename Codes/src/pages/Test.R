library(fullcalendarWidget)
library(fullcalendar)


TestModuleUI <- function(id) {
	ns <- NS(id)
	fluidPage(
		tags$head(tags$link(rel="stylesheet", href="css/styles.css", type="text/css")
		,tags$script(src="fileUp.js"))
		,uiOutput(ns('mainUI'))
	)
}




TestModule <- function(input, output, session, credentials, ...) {
	######### Keep these ######################
	ns <- session$ns
	gAppendAccessLog(credentials()$info$username,getwd(),session$ns("name"),'','')
	
	username=credentials()$info$username
	userDbInfo=user_base[which(user_base$username==username),]
	permissions=userDbInfo$permissions
	
	output$mainUI=renderUI({
		list(
			box(width=12
				,uiOutput(ns('shadesUI'))
			)
			,box(width=6,title='Load',solidHeader=T,status='primary',height='800px'
				,fullcalendarOutput(ns("loadCal"))
			)
			,box(width=6,title='Unload',solidHeader=T,status='primary',height='800px'
				,fullcalendarOutput(ns("unloadCal"))
			)
		)
	})
	warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
	colorList=c('#8CF74B','#C3F0A7','#C9E153','orange','red')
	colorValList=c(0,25,50,75,100)
	
	getDailyCapacity=function(fn,inputDate)
	{
		yr=year(inputDate)
		mth=month(inputDate)
		mthStartDate=as.Date(sprintf("%s-%s-01",yr,mth))
		warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
		capacitymap = setNames(warehouseDf$capacity, warehouseDf$warehouseID)
		df=read.csv(fn,header=T,stringsAsFactors=F)
		dailyOccupyDf=aggregate(Block~warehouseID+Date,df,length)
		dailyOccupyDf$capacity=capacitymap[as.character(dailyOccupyDf$warehouseID)]
		
		dailyOccupyDf$occupypc=round(dailyOccupyDf$Block/(dailyOccupyDf$capacity*48)*100,0)
		
		
		dateList=as.character(seq(mthStartDate,mthStartDate+31,by='day'))
		
		fullDf=NULL
		for (dt in dateList)
		{
			fullDf=rbind(fullDf,data.frame(Date=dt,warehouseID=unique(warehouseDf$warehouseID)))
		}
		
		fullDf=fullDf[which(month(fullDf$Date)==mth),]
		fullOccupyDf=merge(fullDf,dailyOccupyDf,by=c('warehouseID','Date'),all.x=T)
		fullOccupyDf$occupypc[which(is.na(fullOccupyDf$occupypc))]=0
		
		fullOccupyDf$color=colorList[1]
		fullOccupyDf$color[which(fullOccupyDf$occupypc>25)]=colorList[2]
		fullOccupyDf$color[which(fullOccupyDf$occupypc>50)]=colorList[3]
		fullOccupyDf$color[which(fullOccupyDf$occupypc>75)]=colorList[4]
		fullOccupyDf$color[which(fullOccupyDf$occupypc>=100)]=colorList[5]
		fullOccupyDf$start=fullOccupyDf$Date
		fullOccupyDf$title=fullOccupyDf$warehouseID
		
		
		return(fullOccupyDf)
	}
	
	
	output$shadesUI=renderUI({
		colorStr=unlist(lapply(1:length(colorValList), function(i) {
			sprintf("<td align='center' bgcolor='%s'>%s</td>",colorList[i],colorValList[i])
		}))
		HTML(sprintf("Legend<br /><table width=40%%><tr>%s</tr></table>",paste(colorStr,collapse="")))
	})
	
	
	output$loadCal <- renderFullcalendar({
		loadDaily=getDailyCapacity(dockScheduleFn,Sys.Date())
		
		
		# fullcalendar(
			# events = data.frame(
				# id = 1:2,
				# title = c("Event 1", input$text),
				# start = Sys.Date() + 0:1,
				# color = c("blue", "orange")
			# ),
			# options = list(header = list(
					# left = "",
					# center = "",
					# right = "prev,next"
				# )
			# )
			# , callbacks = list(
				# dayClick = DT::JS(
					# "function(date, jsEvent, view) {
					# alert('Clicked on: ' + date.format());}"
				# )
			# )
		# )
		fullcalendar(loadDaily)
	})
	
	output$unloadCal <- renderFullcalendar({
		unloadDaily=getDailyCapacity(dockScheduleFnUnload,Sys.Date())
		
		fullcalendar(unloadDaily)
	})
	
}


# Page Config
#################################
TestPageConfig <- list(
  
  # Title for menu
  'title' = 'Calendar View',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('haulier','warehouse')
)
