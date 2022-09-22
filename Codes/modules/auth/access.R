userListFile='/srv/shiny-server/modulesEuChing/auth/userList.csv'
logfile='/srv/shiny-server/modulesMath/log/access.log'
#################################
# Access Log
#################################
gAppendAccessLog=function(userid,portal,page,remark1,remark2)
{
	DateTime=sprintf("%s",Sys.time())
	write.table(data.frame(username=userid,datetime=DateTime,portal=portal,page=page,remark1=remark1,remark2=remark2,stringsAsFactors=FALSE), logfile, sep=",", append=TRUE,col.names=FALSE,row.names=FALSE)
}


#################################
# Get Access Log
#################################
gGetAccessLog=function(portal)
{
	accessLog=read.csv(logfile,header=TRUE,stringsAsFactors=FALSE)
	accessLog=accessLog[which(accessLog$portal %in% portal),]
	return(accessLog)
}



#################################
# Add User
#################################
gAddUser=function(addusername,addpassword,addPermissions,addname,addemail,addStuBatch)
{
	password_hash=sapply(c(addpassword), sodium::password_store, USE.NAMES = F)
	write.table(data.frame(username=addusername,password_hash=password_hash,permissions=addPermissions,name=addname,email=addemail,batch=addStuBatch,stringsAsFactors=FALSE), userListFile, sep=",", append=TRUE,col.names=FALSE,row.names=FALSE)
}


#################################
# Delete User
#################################
gDeleteUser=function(delUsername)
{
	user_base=read.csv(userListFile,header=TRUE,stringsAsFactors=FALSE)
	user_base=user_base[which(user_base$username!=delUsername),]
	write.csv(user_base,file=userListFile,row.names=FALSE)
}


#################################
# change user permission
#################################
gChangePermission=function(uTypeSelect,usernameSelect)
{
	user_base=read.csv(userListFile,header=TRUE,stringsAsFactors=FALSE)
	user_base$permissions[which(user_base$username==usernameSelect)]=uTypeSelect
	write.csv(user_base,file=userListFile,row.names=FALSE)
}

#################################
# change permission
#################################
gChangePassword=function(usernameSelect,password1)
{
	user_base=read.csv(userListFile,hegGetUserBaseader=TRUE,stringsAsFactors=FALSE)
	user_base$password_hash[which(user_base$username==usernameSelect)]=sapply(c(password1), sodium::password_store, USE.NAMES = F)
	write.csv(user_base,file=userListFile,row.names=FALSE)
}

#################################
# change info
#################################
gChangeInfo=function(usernameSelect,nameTxt,emailTxt)
{
	user_base=read.csv(userListFile,header=TRUE,stringsAsFactors=FALSE)
	user_base$email[which(user_base$username==usernameSelect)]=emailTxt
	user_base$name[which(user_base$username==usernameSelect)]=nameTxt
	write.csv(user_base,file=userListFile,row.names=FALSE)
}


#################################
# User Log
#################################
gGetUserBase=function()
{
	
	user_base=read.csv(userListFile,header=TRUE,stringsAsFactors=FALSE)
	
	return(user_base)
}

