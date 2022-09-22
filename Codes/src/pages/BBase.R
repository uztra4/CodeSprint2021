
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
	
	##################Loading###################
	getDockSchSummary=function(haulierID)
	{
		timeDf=data.frame(Block = 0:47)
		timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
		timeMap=setNames(timeDf$display,as.character(timeDf$Block))
		
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		dockScheduleDf = dockScheduleDf[which(dockScheduleDf$haulierID == haulierID),]
		dockScheduleDf$display=timeMap[as.character(dockScheduleDf$Block)]
		maxBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,max)
		names(maxBlockDf)[which(names(maxBlockDf)=='Block')]='maxBlock'
		minBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,min)
		names(minBlockDf)[which(names(minBlockDf)=='Block')]='minBlock'
		dockSummary=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,length)
		dockSummary$hours=dockSummary$Block*0.5 
		dockSummaryAll=inner_join(inner_join(dockSummary,minBlockDf,by=c('warehouseID','containerID','haulierID','Date')),maxBlockDf,by=c('warehouseID','containerID','haulierID','Date'))
		dockSummaryAll$startTime=timeMap[as.character(dockSummaryAll$minBlock)]
		dockSummaryAll$endTime=timeMap[as.character(dockSummaryAll$maxBlock)]
		dockSummaryAll$endTime = format(as.POSIXct(sprintf('2020-01-01 %s',dockSummaryAll$endTime))+30*60,"%H:%M")
		return(dockSummaryAll)
	}
	
	
	
	###########Unloading##################
	getunLoadSummary=function(haulierID)
	{
		timeDf=data.frame(Block = 0:47)
		timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
		timeMap=setNames(timeDf$display,as.character(timeDf$Block))
		
		dockScheduleDf=read.csv(dockScheduleFnUnload,header=T,stringsAsFactors=F)
		dockScheduleDf = dockScheduleDf[which(dockScheduleDf$haulierID == haulierID),]
		dockScheduleDf$display=timeMap[as.character(dockScheduleDf$Block)]
		maxBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,max)
		names(maxBlockDf)[which(names(maxBlockDf)=='Block')]='maxBlock'
		minBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,min)
		names(minBlockDf)[which(names(minBlockDf)=='Block')]='minBlock'
		dockSummary=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,length)
		dockSummary$hours=dockSummary$Block*0.5 
		dockSummaryAll=inner_join(inner_join(dockSummary,minBlockDf,by=c('warehouseID','containerID','haulierID','Date')),maxBlockDf,by=c('warehouseID','containerID','haulierID','Date'))
		dockSummaryAll$startTime=timeMap[as.character(dockSummaryAll$minBlock)]
		dockSummaryAll$endTime=timeMap[as.character(dockSummaryAll$maxBlock)]
		dockSummaryAll$endTime = format(as.POSIXct(sprintf('2020-01-01 %s',dockSummaryAll$endTime))+30*60,"%H:%M")
		return(dockSummaryAll)
	}
	###################################################
	### format(as.POSIXct(tn, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M")
	### as.numeric(timeStr)
	
	###################################################
	
	
	output$mainUI=renderUI({
		list(
			box(width=12
				, dateInput(ns('bookDate'),'Date',value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
			)
			,uiOutput(ns('mainResultUI'))
		)
	})
	output$mainResultUI = renderUI({
		loadDf = getDockSchSummary(username)
		
		unloadDf = getunLoadSummary(username)
		tabsetPanel(
			tabPanel('Load'
				, renderDT(formatDTDisplay(loadDf[, c('warehouseID', 'containerID', 'Date', 'hours', 'startTime', 'endTime')],scrollY='1500px'))
			)
			, tabPanel('Unload'
				, renderDT(formatDTDisplay(unloadDf[, c('warehouseID', 'containerID', 'Date', 'hours', 'startTime', 'endTime')],scrollY='1500px'))
			)
		)
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
