
ABaseModuleUI <- function(id) {
	ns <- NS(id)
	fluidPage(
		tags$head(tags$link(rel="stylesheet", href="css/styles.css", type="text/css")
		,tags$script(src="fileUp.js"))
		,uiOutput(ns('mainUI'))
	)
}




ABaseModule <- function(input, output, session, credentials, ...) {
	######### Keep these ######################
	ns <- session$ns
	gAppendAccessLog(credentials()$info$username,getwd(),session$ns("name"),'','')
	
	username=credentials()$info$username
	userDbInfo=user_base[which(user_base$username==username),]
	permissions=userDbInfo$permissions
	
	#### Get time display block mapping
	timeDf=data.frame(Block = 0:47)
	timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
	timeMap=setNames(timeDf$display,as.character(timeDf$Block))
	
	######  global variables
	warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
	capacitymap = setNames(warehouseDf$capacity, warehouseDf$warehouseID)
	warehouseList =setNames(warehouseDf$warehouseID, warehouseDf$warehouseName)
	
	
	################################
	getDockSchSummary=function()
	{
		timeDf=data.frame(Block = 0:47)
		timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
		timeMap=setNames(timeDf$display,as.character(timeDf$Block))
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		dockScheduleDf$display=timeMap[as.character(dockScheduleDf$Block)]
		maxBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,max)
		names(maxBlockDf)[which(names(maxBlockDf)=='Block')]='maxBlock'
		minBlockDf=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,min)
		names(minBlockDf)[which(names(minBlockDf)=='Block')]='minBlock'
		dockSummary=aggregate(Block~warehouseID+containerID+haulierID+Date,dockScheduleDf,length)
		#dockSummary$Block = maxBlockDf$maxBlock - minBlockDf$minBlock + 1
		dockSummary$hours=dockSummary$Block*0.5
		dockSummaryAll=inner_join(inner_join(dockSummary,minBlockDf,by=c('warehouseID','containerID','haulierID','Date')),maxBlockDf,by=c('warehouseID','containerID','haulierID','Date'))
		dockSummaryAll$startTime=timeMap[as.character(dockSummaryAll$minBlock)]
		dockSummaryAll$endTime=timeMap[as.character(dockSummaryAll$maxBlock)]
		dockSummaryAll$endTime = format(as.POSIXct(sprintf('2020-01-01 %s',dockSummaryAll$endTime))+30*60,"%H:%M")
		return(dockSummaryAll)
	}
	
	
	
	###################################################
	
	output$mainUI=renderUI({
		list(
			box(width=12, title = "Loading", align = "center", status = "primary", solidHeader=T
				,inputPanel(
					actionButton(ns('bookingBtn'),HTML("Booking"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
					,actionButton(ns('cancelBookingBtn'),HTML("Cancel Booking"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
					,actionButton(ns('overviewBtn'),HTML("Overview"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
					,actionButton(ns('reportBtn'),HTML("Report"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
				)
			)
			,uiOutput(ns('mainResultUI'))
		)
	})
	
	########################### Cancel Booking ###############################

	observeEvent(input$cancelBookingBtn, {
		dockSummaryAll=getDockSchSummary()
		
		output$mainResultUI=renderUI({
			list(
				box(width=8
					,DTOutput(ns('cbDockScheduleDT'))
				)
				,box(width=4
					,uiOutput(ns('cbForm'))
				)
			)
		})
		
		output$cbDockScheduleDT=renderDT({
			dockSummaryAll$warehouseID=as.factor(dockSummaryAll$warehouseID)
			formatDTDisplay(dockSummaryAll, selectChoice='single')
			formatDTDisplay(dockSummaryAll[, c('warehouseID', 'containerID', 'haulierID', 'Date', 'hours', 'startTime', 'endTime' )], selectChoice='single')
		})
		
		output$cbForm=renderUI({
			cbDockScheduleDT_rows_selected=input$cbDockScheduleDT_rows_selected
			if (length(cbDockScheduleDT_rows_selected)>0)
			{
				dockSchSub=dockSummaryAll[cbDockScheduleDT_rows_selected,]
				
				box(width=12,title='Time slot Cancelation',solidHeader=T,status='primary'
					,HTML('Booking cancelation')
					,HTML(sprintf("Time slots: %s - %s",dockSchSub$startTime,dockSchSub$endTime))
					,selectInput(ns('cbDate'),'Booked Date',dockSchSub$Date,dockSchSub$Date)
					,selectInput(ns('cbWarehouseID'),'Warehouse',dockSchSub$warehouseID,dockSchSub$warehouseID)
					,selectInput(ns('cbContainerID'),'Container',dockSchSub$containerID,dockSchSub$containerID)
					,selectInput(ns('cbHaulierID'),'Haulier',dockSchSub$haulierID,dockSchSub$haulierID)
					,actionButton(ns('cbSubmit'),'Cancel')
				)
				
			}
		})
	})
	
	observeEvent(input$cbSubmit, {
		dockSummaryAll=getDockSchSummary()
		cbDockScheduleDT_rows_selected=input$cbDockScheduleDT_rows_selected
		if (length(cbDockScheduleDT_rows_selected)>0)
		{
			dockSchSub=dockSummaryAll[cbDockScheduleDT_rows_selected,]
			cbWarehouseID=input$cbWarehouseID
			cbContainerID=input$cbContainerID
			cbHaulierID=input$cbHaulierID
			cbDate=input$cbDate
			
			dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
			index=which(dockScheduleDf$containerID==cbContainerID & dockScheduleDf$haulierID==cbHaulierID & dockScheduleDf$warehouseID==cbWarehouseID & dockScheduleDf$Date==cbDate)
			
			if (length(index)>0)
			{
				canceledDf=dockScheduleDf[index,]
				dockScheduleDfSub=dockScheduleDf[-index,]
				write.table(canceledDf, dockScheduleCancelFn, sep=",", append=TRUE,col.names=FALSE,row.names=FALSE)
				write.csv(dockScheduleDfSub,dockScheduleFn,row.names=F)
			}
			
			
			output$mainResultUI=renderUI({
				list(
					box(width=12,title='Time slot Cancelation',solidHeader=T,status='primary'
						,HTML(sprintf("Booking canceled for:<br />Date: %s<br /> Timeslot: %s - %s<br />Warehouse: %s<br />ContainerID: %s<br />HaulierID: %s<br />",dockSchSub$Date,dockSchSub$startTime,dockSchSub$endTime,dockSchSub$warehouseID,dockSchSub$containerID,dockSchSub$haulierID))
					)
					,box(width=12,title='Recommended jobs to replace canceled slots',solidHeader=T,status='primary'
						,renderDT({
							jobsDf=read.csv(dockJobs,header=T,stringsAsFactors=F)
							jobsDfSub=jobsDf[which(jobsDf$hours<=dockSchSub$hours & jobsDf$warehouseID==dockSchSub$warehouseID),]
							formatDTDisplay(jobsDfSub,selectChoice='single')
						})
					)
				)
			})
			
			#output$cbDockScheduleDT=renderDT({
				# dockSummaryAll=getDockSchSummary()
				# dockSummaryAll$warehouseID=as.factor(dockSummaryAll$warehouseID)
				# formatDTDisplay(dockSummaryAll,selectChoice='single')
			# })
		}
	})
	
	
	########################### Report ###############################
	observeEvent(input$reportBtn, {
		warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
		capacitymap = setNames(warehouseDf$capacity, warehouseDf$warehouseID)
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		
		##### By date
		utilization = aggregate(Block~warehouseID+Date, dockScheduleDf, length)
		colnames(utilization)[3] = 'blockcnt'
		utilization$capacity = capacitymap[utilization$warehouseID]
		utilization$utilization = utilization$blockcnt/(utilization$capacity*48)*100
		
		####By block
		output$mainResultUI = renderUI({
			list(
				box(width=12,title='Warehouse Utilization Report',solidHeader=T,status='primary'
					,renderPlotly(plot_ly(utilization, x = ~Date, y = ~utilization, color = ~warehouseID, type= 'scatter', mode = 'lines+markers'))
				)
				, box(width=12,title='Warehouse Utilization Report',solidHeader=T,status='primary'
					, inputPanel(
						dateInput(ns('SDate'),'Start',value='2021-10-01',format("%Y-%m-%d"))
						, dateInput(ns('EDate'),'End',value='2021-10-11',format("%Y-%m-%d"))
					)
					, uiOutput(ns('hourChart'))
				)
				, box(width=12,title='Haulier UtilizationReport',solidHeader=T,status='primary'
					, inputPanel(
						dateInput(ns('hSDate'),'Start',value='2021-10-01',format("%Y-%m-%d"))
						, dateInput(ns('hEDate'),'End',value='2021-10-11',format("%Y-%m-%d"))
					)
					, uiOutput(ns('haulierChart'))
				)
			)
		})
		
		output$haulierChart=renderUI({
			hSDate=input$hSDate
			hEDate=input$hEDate
			dockSummaryAll=getDockSchSummary()
			dockSummaryAllSub=dockSummaryAll[which(dockSummaryAll$Date <=hEDate & dockSummaryAll$Date>=hSDate),]
			haulierSummary=aggregate(Block~warehouseID+haulierID,dockSummaryAllSub,sum)
			names(haulierSummary)[which(names(haulierSummary)=='Block')]='blockCnt'
			haulierSummary$hours=haulierSummary$blockCnt*0.5
			list(
				renderPlotly({
					plot_ly(haulierSummary,x=~haulierID,y=~hours,color=~warehouseID,type='bar')
				})
			)
		})

		output$hourChart = renderUI({
			SDate = input$SDate
			EDate = input$EDate
			dockScheduleDfSub = dockScheduleDf[which(dockScheduleDf$Date>=SDate & dockScheduleDf$Date <= EDate), ]
			timeDf=data.frame(Block = 0:47)
			timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
			timeMap=setNames(timeDf$display,as.character(timeDf$Block))
			dockScheduleDfSub$display=timeMap[as.character(dockScheduleDfSub$Block)]
			hourutil = aggregate(containerID~warehouseID+display ,dockScheduleDfSub, length)
			colnames(hourutil)[3] = 'jobCount'
			colnames(hourutil)[2] = 'Time'
			hourutil$capacity = capacitymap[hourutil$warehouseID]
			dayCnt=length(unique(dockScheduleDfSub$Date))
			hourutil$utilization = hourutil$jobCount/(hourutil$capacity)*dayCnt
			
			list(
				renderPlotly({
					plot_ly(hourutil,x=~Time,y=~utilization, color = ~warehouseID, type= 'scatter', mode = 'lines+markers')
				})
			)
		})
	})
	########################### Overview #############################

	observeEvent(input$overviewBtn, {
		output$mainResultUI = renderUI({
			box( width = 12,solidHeader=T,status='primary',title='Overview'
				,tabsetPanel(
					tabPanel('Overview by Date'
						,inputPanel(
							dateInput(ns('overviewDate'),'Date',value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
							,actionButton(ns('overviewDateBtn'), 'View')
						)
						, uiOutput(ns('overviewresult'))
					)
					,tabPanel('Details of bookings'
						,renderDT({
							dockSummaryAll=getDockSchSummary()
							dockSummaryAll$haulierID=as.factor(dockSummaryAll$haulierID)
							dockSummaryAll$warehouseID=as.factor(dockSummaryAll$warehouseID)
							formatDTDisplay(dockSummaryAll[, c('warehouseID', 'containerID', 'haulierID', 'Date', 'hours', 'startTime', 'endTime' )], selectChoice='single')
						})
					)
				)
			)
		})
		observeEvent(input$overviewDateBtn, {
			overviewDate = input$overviewDate
			warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
			overviewTimeSheet = data.frame(rbindlist(lapply(1:nrow(warehouseDf), function(i){
				warehouseID = warehouseDf$warehouseID[i]
				timeSheet = computetimeSheet(dockScheduleFn,warehouseID, overviewDate)
				timeSheet$warehouseID = warehouseID
				return(timeSheet)
			})))
			
			
			
			overviewTimeSheet$availClr='#A0F08D'
			overviewTimeSheet$availClr[which(overviewTimeSheet$avail<=5)]='#F6C156'
			overviewTimeSheet$availClr[which(overviewTimeSheet$avail==0)]='#F5A59F'
			overviewTimeSheet$avail=sprintf("<td align='center' bgcolor='%s'>%s</td>",overviewTimeSheet$availClr,overviewTimeSheet$avail)
			
			overviewspreadDf = spread(overviewTimeSheet[, c('warehouseID', 'display', 'avail')], key = 'warehouseID', value = 'avail')
			
			
			headerStr="<tr bgcolor='gray' align='center'><td width='20%%'>Timeslot</td>"
			for (cn in setdiff(colnames(overviewspreadDf),'display'))
			{
				headerStr=sprintf("%s<td align='center'>%s</td>",headerStr,cn)
			}
			headerStr=sprintf("%s</tr>",headerStr)
			
			htmlStr=paste(unlist(lapply(1:nrow(overviewspreadDf), function(i) {
				htmlStr=sprintf("<tr align='center'><td>%s</td>",overviewspreadDf$display[i])
				for (cn in setdiff(colnames(overviewspreadDf),'display'))
				{
					htmlStr=sprintf("%s%s",htmlStr,overviewspreadDf[i,cn])
				}
				htmlStr=sprintf("%s</tr>",htmlStr)
				return(htmlStr)
			})),collapse="")
			
			tblStr=sprintf("<table width=80%%>%s%s</table>",headerStr,htmlStr)
			
			
			
			overviewTimeSheet$jobCountClr='#fff'
			overviewTimeSheet$jobCountClr[which(overviewTimeSheet$jobCount>0)]='#A0F08D'
			overviewTimeSheet$jobCount=sprintf("<td align='center' bgcolor='%s'>%s</td>",overviewTimeSheet$jobCountClr,overviewTimeSheet$jobCount)
			overviewspreadDfBook = spread(overviewTimeSheet[, c('warehouseID', 'display', 'jobCount')], key = 'warehouseID', value = 'jobCount')
			
			
			htmlStrBook=paste(unlist(lapply(1:nrow(overviewspreadDfBook), function(i) {
				htmlStr=sprintf("<tr align='center'><td>%s</td>",overviewspreadDfBook$display[i])
				for (cn in setdiff(colnames(overviewspreadDfBook),'display'))
				{
					htmlStr=sprintf("%s%s",htmlStr,overviewspreadDfBook[i,cn])
				}
				htmlStr=sprintf("%s</tr>",htmlStr)
				return(htmlStr)
			})),collapse="")
			tblStrBook=sprintf("<table width=80%%>%s%s</table>",headerStr,htmlStrBook)
			
			
			output$overviewresult = renderUI({
				box(width=12
					,box(width=6,title='Available',solidHeader=T,status='primary'
						#formatDTDisplayTmp(overviewspreadDf,selectChoice='single')
						,HTML(sprintf("<b>Warehouse overview for %s</b><br />%s",overviewDate,tblStr))
					)
					,box(width=6,title='Bookings',solidHeader=T,status='primary'
						,HTML(sprintf("<b>Warehouse overview for %s</b><br />%s",overviewDate,tblStrBook))
					)
				)
			})
		})

		
	})
	
	
	################################ Booking ##########################
	observeEvent(input$bookingBtn, {
		output$mainResultUI=renderUI({
			box(width=12,solidHeader=T,align = 'center', status='primary',title='Booking'
				,uiOutput(ns('formUI'))
				,box(width=4, solidHeader=T,align = 'left', status='primary',title='Select Booking Time Slots'
					,DTOutput(ns('timeDT'))
				)
				,box(width=8,title='Booking Details',solidHeader=T,status='primary'
					,uiOutput(ns('bookingDetUI'))
				)
			)
		})
		warehouseList =setNames(warehouseDf$warehouseID, warehouseDf$warehouseName)
		output$formUI=renderUI({
			list(
				selectInput(ns('warehouseSel'), 'Warehouse', warehouseList, warehouseList[1])
				, dateInput(ns('bookDate'),'Date', value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
			, inputPanel(align = "center"
				, actionButton(ns('WHsubmitBtn'), 'Search')
				)
			)
		})
		
		output$bookingDetUI=renderUI({
			NULL
		})
		
		output$timeDT=renderDT({
			NULL
		})
	})
	
	observeEvent(input$WHsubmitBtn, {
		warehouseSel = input$warehouseSel
		bookDate = input$bookDate;
		timeSheet = computetimeSheet(dockScheduleFn,warehouseSel, bookDate)
		
		output$timeDT = renderDT({
			names(timeSheet)[which(names(timeSheet) == 'avail')] = 'Capacity'
			formatDTDisplayTmp(timeSheet[, c('Capacity'), drop = F],rownames=T,scrollY='1500px')
		})
		
		
		output$bookingDetUI = renderUI({
			timeDT_rows_selected = input$timeDT_rows_selected;
			if (length(timeDT_rows_selected)>0)
			{
				timeSheetSub = timeSheet[min(timeDT_rows_selected):max(timeDT_rows_selected), ]
				if (length(which(timeSheetSub$avail == 0)) > 0)
				{
					HTML(sprintf("<font colour = 'red'> %s is unavailable. Please book an available slot.</font>", paste(timeSheetSub$display[which(timeSheetSub$avail == 0)])))
					
				}else
				{
					starttime = min(timeSheetSub$display)
					endtime = max(timeSheetSub$display)
					endtime = format(as.POSIXct(sprintf('2020-01-01 %s',endtime))+30*60,"%H:%M")
					bookhours = ((max(timeSheetSub$Block)+1) - min(timeSheetSub$Block))*0.5
					list(
						HTML(sprintf('Booking time: %s - %s (%s hour(s))', starttime, endtime, bookhours))
						, textInput(ns('containerID'), 'Container ID', value = '')
						, textInput(ns('haulierID'), 'Haulier Company', value = '')
						, textInput(ns('remarks'), 'Remarks', value = '')
						, actionButton(ns('bookingDetBtn'), 'Submit')
					)
				}
			} else
			{
				HTML("Please highlight time start and time end from table on the left")
			}
		})
		
		
	})
	
	observeEvent(input$bookingDetBtn, {
		containerID = input$containerID
		haulierID = input$haulierID
		remarks = input$remarks
		warehouseSel = input$warehouseSel
		bookDate = input$bookDate;
		timeSheet = computetimeSheet(dockScheduleFn,warehouseSel, bookDate)
		
		timeDT_rows_selected = input$timeDT_rows_selected
		timeSheetSub = timeSheet[min(timeDT_rows_selected):max(timeDT_rows_selected), ]
		if (length(which(timeSheetSub$avail == 0)) > 0)
		{
			output$bookingDetUI = renderUI({
				HTML(sprintf("<font colour = 'red'> %s is unavailable. Please book an available slot.</font>", paste(timeSheetSub$display[which(timeSheetSub$avail == 0)])))
			})
		}else
		{
			newBookingDf = data.frame(warehouseID = warehouseSel, Block = timeSheetSub$Block, Date = bookDate, containerID = containerID, haulierID = haulierID, remarks = remarks)

			write.table(newBookingDf, dockScheduleFn, sep=",", append=TRUE,col.names=FALSE,row.names=FALSE)
			
			starttime = min(timeSheetSub$display)
			endtime = max(timeSheetSub$display)
			endtime = format(as.POSIXct(sprintf('2020-01-01 %s',endtime))+30*60,"%H:%M")
			bookhours = ((max(timeSheetSub$Block)+1) - min(timeSheetSub$Block))*0.5
			
			timeSheet = computetimeSheet(dockScheduleFn,warehouseSel, bookDate)
			output$timeDT = renderDT({
				formatDTDisplayTmp(timeSheet[, c('avail'), drop = F],rownames=T,scrollY='1500px')
			})
			output$bookingDetUI = renderUI({
				HTML(sprintf('Booking time: %s - %s (%s hour(s)). Your timing has been booked for %s under %s for Warehouse %s', starttime, endtime, bookhours, containerID, haulierID, warehouseSel))
			})
		}
	})
}


# Page Config
#################################
ABasePageConfig <- list(
  
  # Title for menu
  'title' = 'Warehouse Load',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('warehouse')
)