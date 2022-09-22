
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
	
	###################################################
	### format(as.POSIXct(tn, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M")
	### as.numeric(timeStr)
	computetimeSheet = function(warehouseSel, bookDate){
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
		dockScheduleDfSub = dockScheduleDf[which(dockScheduleDf$warehouseID == warehouseSel & dockScheduleDf$Date ==  bookDate),]
		
		timeDf=data.frame(Block = 0:47)
		timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
		
		if (nrow(dockScheduleDfSub)>0)
		{
			occupiedDocks = aggregate(containerID~Block, dockScheduleDfSub, length);
			colnames(occupiedDocks)[2] = 'jobCount';
		
			timeSheet = left_join(timeDf, occupiedDocks, by = 'Block');
		} else
		{
			timeSheet=timeDf
			timeSheet$jobCount=0
		}
		
		timeSheet$jobCount[which(is.na(timeSheet$jobCount))] = 0;
		timeSheet$avail = warehouseDf$capacity[which(warehouseDf$warehouseID == warehouseSel)] - timeSheet$jobCount 
		
		timeSheet$label = sprintf('%s (%s)', timeSheet$display, timeSheet$avail);
		#for display
		rownames(timeSheet) = timeSheet$display;
		
		return(timeSheet)
	}
	
	###################################################
	
	warehouseDf=reactive({
		warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
		return(warehouseDf)
	})
		
	output$mainUI=renderUI({
		list(
			box(width=12
				,inputPanel(
					actionButton(ns('bookingBtn'),HTML("Booking"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
					,actionButton(ns('overviewBtn'),HTML("Overview"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
					,actionButton(ns('reportBtn'),HTML("Report"),style="white-space: normal; text-align:center;  color: #fff;  background-color:#0000FF;  border-color: #2e6da4;   height:50px; width:150px;  font-size: 12px;")
				)
			)
			,uiOutput(ns('mainResultUI'))
		)
	})
	########################### Report ###############################
	observeEvent(input$reportBtn, {
		warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
		capacitymap = setNames(warehouseDf$capacity, warehouseDf$warehouseID)
		dockScheduleDf=read.csv(dockScheduleFn,header=T,stringsAsFactors=F)
		
		##### By date
		utilization = aggregate(Block~warehouseID+Date, dockScheduleDf, length);
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
						, dateInput(ns('EDate'),'End',value='2021-10-10',format("%Y-%m-%d"))
					)
					, uiOutput(ns('hourChart'))
				)
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
				,inputPanel(
					dateInput(ns('overviewDate'),'Date',value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
					,actionButton(ns('overviewDateBtn'), 'View')
				)
				, uiOutput(ns('overviewresult'))
			)
		})
		observeEvent(input$overviewDateBtn, {
			overviewDate = input$overviewDate
			warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
			overviewTimeSheet = data.frame(rbindlist(lapply(1:nrow(warehouseDf), function(i){
				warehouseID = warehouseDf$warehouseID[i]
				timeSheet = computetimeSheet(warehouseID, overviewDate)
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
			box(width=12,solidHeader=T,status='primary',title='Booking'
				,uiOutput(ns('formUI'))
				,box(width=2,solidHeader=T,status='primary',title='Select Booking Time Slots'
					,DTOutput(ns('timeDT'))
				)
				,box(width=10,title='Booking Details',solidHeader=T,status='primary'
					,uiOutput(ns('bookingDetUI'))
				)
			)
		})
		warehouseDf = warehouseDf();
		warehouseList =setNames(warehouseDf$warehouseID, warehouseDf$warehouseName);
		output$formUI=renderUI({
			inputPanel(
				selectInput(ns('warehouseSel'), 'Warehouse', warehouseList, warehouseList[1])
				, dateInput(ns('bookDate'),'Date',value=format(Sys.time(),"%Y-%m-%d"),format("%Y-%m-%d"))
				, actionButton(ns('WHsubmitBtn'), 'Search')
			)
		})
		observeEvent(input$WHsubmitBtn, {
			warehouseSel = input$warehouseSel;
			bookDate = input$bookDate;
			timeSheet = computetimeSheet(warehouseSel, bookDate)
			output$timeDT = renderDT({
				formatDTDisplayTmp(timeSheet[, c('avail'), drop = F],rownames=T,scrollY='1500px')
			})
			output$bookingDetUI = renderUI({
				timeDT_rows_selected = input$timeDT_rows_selected;
				if (length(timeDT_rows_selected)>0)
				{
					timeSheetSub = timeSheet[timeDT_rows_selected, ]
					if (length(which(timeSheetSub$avail == 0)) > 0)
					{
						HTML(sprintf("<font colour = 'red'> %s is unavailable. Please book an available slot.</font>", paste(timeSheetSub$display[which(timeSheetSub$avail == 0)])))
						
					}else
					{
						starttime = min(timeSheetSub$display)
						endtime = max(timeSheetSub$display)
						bookhours = (max(timeSheetSub$Block) - min(timeSheetSub$Block))*0.5
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
			
			observeEvent(input$bookingDetBtn, {
				containerID = input$containerID
				haulierID = input$haulierID
				remarks = input$remarks
				timeDT_rows_selected = input$timeDT_rows_selected
				timeSheetSub = timeSheet[timeDT_rows_selected, ]
				
				newBookingDf = data.frame(warehouseID = warehouseSel, Block = timeSheetSub$Block, Date = bookDate, containerID = containerID, haulierID = haulierID, remarks = remarks)

				write.table(newBookingDf, dockScheduleFn, sep=",", append=TRUE,col.names=FALSE,row.names=FALSE)
				
				starttime = min(timeSheetSub$display)
				endtime = max(timeSheetSub$display)
				bookhours = (max(timeSheetSub$Block) - min(timeSheetSub$Block))*0.5
				
				timeSheet = computetimeSheet(warehouseSel, bookDate)
				output$timeDT = renderDT({
					formatDTDisplayTmp(timeSheet[, c('avail'), drop = F],rownames=T,scrollY='1500px')
				})
				output$bookingDetUI = renderUI({
					HTML(sprintf('Booking time: %s - %s (%s hour(s)). Your timing has been booked for %s under %s for Warehouse %s', starttime, endtime, bookhours, containerID, haulierID, warehouseSel))
				})
			})

			
		})
	})
	
}


# Page Config
#################################
ABasePageConfig <- list(
  
  # Title for menu
  'title' = 'Warehouse',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('warehouse')
)