
############## Loading ####################
warehouseDf=read.csv(warehouseFn,header=T,stringsAsFactors=F)
st <- as.Date("2021-10-01")
en <- as.Date("2021-10-10")

scheduleDf=NULL
for (i in 1:nrow(warehouseDf))
{
	wid=warehouseDf$warehouseID[i]
	capacity=warehouseDf$capacity[i]
	for (d in format(seq(st, en,by='1 day'),"%Y-%m-%d"))
	{
		blockCnt=floor(runif(1, 0.5, 0.7)*capacity*44)
		
		
		blockEnd=0
		while (blockCnt>0)
		{
			blockBooked=floor(runif(1,3,7))
			blockStart=blockEnd
			blockEnd=blockStart+blockBooked-1
			cid=sprintf("c%03d",floor(runif(1,1,500)))
			if (blockEnd<=47)
			{
				scheduleDf=rbind(scheduleDf,data.frame(warehouseID=wid, Block=blockStart:blockEnd,Date=d, containerID=cid, haulierID='h001', remarks=''))
				blockCnt=blockCnt-blockBooked
			} else
			{
				blockEnd=0
			}
		}
	}
}

write.csv(scheduleDf,dockScheduleFn,row.names=F)




################## 
fn='/s3-bucket/shiny-server/public/EuChing/input.txt'
jobInput=read.csv(fn,header=T,stringsAsFactors = F,sep="\t")
wid='D'
capacity=8

jobAssignDf=NULL
utilizationDf=data.frame(block=0:47,utilization=0)
for (i in 1:nrow(jobInput))
{
	blocksNeeded=jobInput$hours[i]/0.5
	for (j in 1:(nrow(utilizationDf)-blocksNeeded))
	{
		flag=T
		for (k in j:(j+blocksNeeded-1))
		{
			if (utilizationDf$utilization[k]>=capacity)
			{
				flag=F
			}
		}
		
		if (flag==T)
		{
			for (k in j:(j+blocksNeeded-1))
			{
				utilizationDf$utilization[k]=utilizationDf$utilization[k]+1
			}
			jobAssignDf=rbind(jobAssignDf,data.frame(warehouseID='D',containerID=jobInput$containerID[i],Block=j:(j+blocksNeeded-1),haulierID=jobInput$haulierID[i],remarks='',Date='2021-10-11',LotID=''))
			break;
		}
		
	}
	
}




###################################################
############## Unoading ####################
warehouseDf=read.csv(warehouseFnUnLoad,header=T,stringsAsFactors=F)
st <- as.Date("2021-10-01")
en <- as.Date("2021-10-10")

scheduleDf=NULL
for (i in 1:nrow(warehouseDf))
{
	wid=warehouseDf$warehouseID[i]
	capacity=warehouseDf$capacity[i]
	for (d in format(seq(st, en,by='1 day'),"%Y-%m-%d"))
	{
		blockCnt=floor(runif(1, 0.5, 0.7)*capacity*44)
		
		
		blockEnd=0
		while (blockCnt>0)
		{
			blockBooked=floor(runif(1,3,7))
			blockStart=blockEnd
			blockEnd=blockStart+blockBooked-1
			cid=sprintf("c%03d",floor(runif(1,1,500)))
			if (blockEnd<=47)
			{
				scheduleDf=rbind(scheduleDf,data.frame(warehouseID=wid, Block=blockStart:blockEnd,Date=d, containerID=cid, haulierID='h001', remarks=''))
				blockCnt=blockCnt-blockBooked
			} else
			{
				blockEnd=0
			}
		}
	}
}

write.csv(scheduleDf,dockScheduleFnUnload,row.names=F)