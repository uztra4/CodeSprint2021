#setwd('/srv/shiny-server/public/Portal/')
#
# Import Libraries
#################################


library(data.table)
library(DT)
library(sodium)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjqui)
library(jsonlite)
library(R.utils)
library(stringr)
library(tidyr)
library(stringi)
library(plotly)
library(tidyverse)
library(htmltools)
library(dplyr)
library(xlsx)
library(heatmaply)
library(formattable)
library(DBI)
library(readxl)
library(plyr)
library(lubridate)
library(officer)
library(flextable)
library(locatexec)

# App Data
################################# 


# App Config
AppConfig <- read_json("src/config.json")


createLink <- function(val,disp='Link') {
  #sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val,disp)
  sprintf('<a href="%s" target="_blank">%s</a>',val,disp)
}

Sys.setFileTime('/srv/shiny-server/public/EuChing/server.R', now())
Sys.setFileTime('/srv/shiny-server/public/EuChing/ui.R', now())
Sys.setFileTime('/srv/shiny-server/public/EuChing/pages/ABase.R', now())
Sys.setFileTime('/srv/shiny-server/public/EuChing/pages/UserManagement.R', now())

###########################################################################
warehouseFn='/srv/shiny-server/public/EuChing/data/warehouse.csv'
warehouseFnUnload='/srv/shiny-server/public/EuChing/data/warehouseUnload.csv'
#dockFnLoad='/srv/shiny-server/public/EuChing/data/dock.csv'
#dockFn='/srv/shiny-server/public/EuChing/data/dock.csv'
dockScheduleFn='/srv/shiny-server/public/EuChing/data/dockSchedule.csv'
dockScheduleFnUnload='/srv/shiny-server/public/EuChing/data/dockScheduleUnload.csv'


dockJobs='/srv/shiny-server/public/EuChing/data/input.csv'
dockJobsUnload='/srv/shiny-server/public/EuChing/data/inputUnload.csv'

dockScheduleCancelFn='/srv/shiny-server/public/EuChing/data/dockScheduleCancel.csv'
dockScheduleCancelFnUnload='/srv/shiny-server/public/EuChing/data/dockScheduleCancelUnoad.csv'

###########################################################################
###################################################
### format(as.POSIXct(tn, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M")
### as.numeric(timeStr)
computetimeSheet = function(fn, warehouseSel, bookDate){
	dockScheduleDf=read.csv(fn,header=T,stringsAsFactors=F)
	warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
	dockScheduleDfSub = dockScheduleDf[which(dockScheduleDf$warehouseID == warehouseSel & dockScheduleDf$Date ==  bookDate),]
	
	timeDf=data.frame(Block = 0:47)
	timeDf$display=format(as.POSIXct('2020-01-01 00:00')+30*60*timeDf$Block,"%H:%M")
	
	if (nrow(dockScheduleDfSub)>0)
	{
		occupiedDocks = aggregate(containerID~Block, dockScheduleDfSub, length);
		colnames(occupiedDocks)[2] = 'jobCount'
	
		timeSheet = left_join(timeDf, occupiedDocks, by = 'Block')
	} else
	{
		timeSheet=timeDf
		timeSheet$jobCount=0
	}
	
	timeSheet$jobCount[which(is.na(timeSheet$jobCount))] = 0;
	timeSheet$avail = warehouseDf$capacity[which(warehouseDf$warehouseID == warehouseSel)] - timeSheet$jobCount 
	
	timeSheet$label = sprintf('%s (%s)', timeSheet$display, timeSheet$avail)
	#for display
	rownames(timeSheet) = timeSheet$display
	
	return(timeSheet)
}

###########################################################################
formatDTDisplay=function(a,selectChoice='multiple',currencyCol=NULL,roundCol=NULL,roundDigit=2,pagelen=50,rowsSelected=NULL,buttons=FALSE,rowColorIndCol=NULL,rowColrMap=NULL,scrollY='500px',rownames=F)
{
	a= a %>% datatable(extensions = 'Buttons',
			selection=list(mode=selectChoice,selected=rowsSelected),
			rownames=rownames,
			filter='top',
			escape=FALSE,
		  options = list(
			pageLength = pagelen,
			scrollX=TRUE,
			scrollY=scrollY,
			dom = 'T<"clear">lBfrtip'
			,buttons = c('copy', 'excel', 'pdf', 'print')
			
		  )
		)
	
	if (!is.null(currencyCol))
	{
		a = a %>% formatCurrency(currencyCol, currency = "", interval = 3, mark = ",")
	} 

	if (!is.null(roundCol))
	{
		a = a %>% formatRound(roundCol,digits=roundDigit)
	}
	
	# if (!is.null(rowColorIndCol))
	# {
		# ## https://rstudio.github.io/DT/010-style.html
		
		# a = a %>% formatStyle(rowColorIndCol,target = 'row',backgroundColor = styleEqual(rowColrMap, rowColrMap))
	# }
	if (!is.null(rowColorIndCol))
	{
		#a = a %>% formatStyle(rowColorIndCol,target = 'row',backgroundColor = styleEqual(c(0,1), c('gray','yellow')))
		a = a %>% formatStyle(rowColorIndCol,backgroundColor = styleEqual(names(rowColrMap), rowColrMap))
	}
	

	return(a)
}
#########################################
formatDTDisplayTmp=function(a,selectChoice='multiple',currencyCol=NULL,roundCol=NULL,roundDigit=2,pagelen=50,rowsSelected=NULL,buttons=FALSE,rowColorIndCol=NULL,rowColrMap=NULL,scrollY='500px',rownames=F)
{
	a= a %>% datatable(extensions = 'Buttons',
			selection=list(mode=selectChoice,selected=rowsSelected),
			rownames=rownames,
			escape=FALSE,
		  options = list(
			pageLength = pagelen,
			scrollX=TRUE,
			scrollY=scrollY,
			dom = 'T<"clear">lBfrtip'
		  )
		)
	
	if (!is.null(currencyCol))
	{
		a = a %>% formatCurrency(currencyCol, currency = "", interval = 3, mark = ",")
	} 

	if (!is.null(roundCol))
	{
		a = a %>% formatRound(roundCol,digits=roundDigit)
	}
	
	# if (!is.null(rowColorIndCol))
	# {
		# ## https://rstudio.github.io/DT/010-style.html
		
		# a = a %>% formatStyle(rowColorIndCol,target = 'row',backgroundColor = styleEqual(rowColrMap, rowColrMap))
	# }
	if (!is.null(rowColorIndCol))
	{
		rowColrMap=setNames(c('red',rep('green',30)),0:30)
		#a = a %>% formatStyle(rowColorIndCol,target = 'row',backgroundColor = styleEqual(c(0,1), c('gray','yellow')))
		a = a %>% formatStyle(rowColorIndCol,backgroundColor = styleEqual(names(rowColrMap), rowColrMap))
	}
	

	return(a)
}

#################################
# App Initialization
#################################

###################################################################
###################################################################
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
## colors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:n]



# Modules
sourceDirectory('/srv/shiny-server/modulesEuChing/auth/', modifiedOnly = F)
user_base=gGetUserBase()


####################################################################
# Widgets
sourceDirectory('src/components/widgets', modifiedOnly = F)

# Pages
sourceDirectory('src/pages/', modifiedOnly = F, recursive = T)

# Page Router
source('src/components/layout/AppPages.R')

# App Layout
sourceDirectory('src/components/layout', modifiedOnly = F)

# Scripts

###################################################################
###################################################################

















