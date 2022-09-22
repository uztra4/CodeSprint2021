


UserManagementModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(width = 12, status = "primary"
		,tabsetPanel(
			tabPanel('UserList'
				,actionButton(ns('userRefresh'),'Refresh')
				,DTOutput(ns('user_base'))
			)
			,tabPanel('Change'
				,uiOutput(ns('ChangeForm'))
				,uiOutput(ns('infoChangeMsg'))
				,uiOutput(ns('uTypeChangeMsg'))
				,uiOutput(ns('addUMsg'))
				,uiOutput(ns('delUMsg'))
				,uiOutput(ns('passordChangeMsg'))
				,uiOutput(ns('ChangeFormDetails'))
			)
		)
    )
  )
}

UserManagementModule <- function(input, output, session, credentials, ...) {
  ######### Keep these ######################
  ns <- session$ns
	gAppendAccessLog(credentials()$info$username,getwd(),session$ns("name"),'','')
	
	permissionList=c('haulier','warehouse')
  ######### ######################
	userDf=reactive({
		input$userRefresh
		userDf=gGetUserBase()
		return(userDf)
	})
  
  output$user_base <- renderDT({
	userDf=userDf()
    formatDTDisplay(userDf[,c('username','name','permissions')])
  })
  
  
  output$ChangeForm=renderUI({
	userDf=userDf()
	userDbInfo=userDf[which(userDf$username==credentials()$info$username),]
	if (userDbInfo$permissions=='warehouse')
	{
		inputPanel(
			selectInput(ns('usernameSelect'),'Select User',userDf$username,userDbInfo$username)
		)
	} else
	{
		inputPanel(
			selectInput(ns('usernameSelect'),'Select User',userDbInfo$username,userDbInfo$username)
		)
	}
  })
  
  output$ChangeFormDetails=renderUI({
	userDf=userDf()
	usernameSelect=input$usernameSelect
	userDbInfo=userDf[which(userDf$username==usernameSelect),]
	uList=setdiff(userDf$username,credentials()$info$username)
	tabsetPanel(
		tabPanel('infoChange'
			,inputPanel(
				textInput(ns('nameTxt'),'Name',userDbInfo$name)
				,textInput(ns('emailTxt'),'email',userDbInfo$email)
				,actionButton(ns('infoChangeBtn'),'Submit')
			)
		)
		,tabPanel('password change'
			,inputPanel(
				textInput(ns('nameTxt'),'Name',userDbInfo$name)
				,passwordInput(ns('oldpassword'),'Old Password')
				,passwordInput(ns('password1'),'New Password')
				,passwordInput(ns('password2'),'Confirm New Password')
				,actionButton(ns('pwchangeBtn'),'Submit')
			)
		)
		,tabPanel('UserType change'
			,inputPanel(
				selectInput(ns('uTypeSelect'),'Permissions',permissionList,userDbInfo$permissions)
				,passwordInput(ns('oldpassword1'),'Old Password')
				,actionButton(ns('uTypeChangeBtn'),'Submit')
			)
		)
		,tabPanel('Add User'
			,inputPanel(
				textInput(ns('addusername'),'Username')
				,passwordInput(ns('addpassword'),'Password')
				,passwordInput(ns('addpassword1'),'Confirm Password')
				,textInput(ns('addname'),'Name')
				,selectInput(ns('addPermissions'),'Permissions',permissionList,'haulier')
				,textInput(ns('addemail'),'Email')
				,textInput(ns('addStuBatch'),'Batch')
				,passwordInput(ns('addAdminPw'),'Requestor Password (Adminstrator)')
				,actionButton(ns('uAddBtn'),'Submit')
			)
		)
		,tabPanel('Delete User'
			,inputPanel(
				selectInput(ns('delUsername'),'Delete username:',uList,uList[1])
				,passwordInput(ns('delAdminPw'),'Requestor Password (Adminstrator)')
				,actionButton(ns('uDelBtn'),'Submit')
			)
		)
	)
  })
  
  observeEvent(input$uDelBtn,{
	
	output$delUMsg=renderUI({
		userDf=userDf()
		delAdminPw=input$delAdminPw
		delUsername=input$delUsername
		reqInfo=userDf[which(userDf$username==credentials()$info$username),]
		if (reqInfo$permissions != 'warehouse')
		{
			msg=sprintf("%s: Requestor does not have admin rights",Sys.time())
		} else
		{
			if (!sodium::password_verify(reqInfo$password_hash, delAdminPw))
			{
				msg=sprintf("%s: Requestor password incorrect",Sys.time())
			} else
			{
				gDeleteUser(delUsername)
				msg=sprintf("Deleted user %s",delUsername,Sys.time())
			}
		}
		return(HTML(msg))
	})
	
  })
  
  observeEvent(input$uAddBtn,{
	userDf=userDf()
	addAdminPw=input$addAdminPw
	addusername=input$addusername
	addpassword=input$addpassword
	addpassword1=input$addpassword1
	addPermissions=input$addPermissions
	addemail=input$addemail
	addname=input$addname
	addStuBatch=input$addStuBatch
	reqInfo=userDf[which(userDf$username==credentials()$info$username),]
	output$addUMsg=renderUI({
		if (reqInfo$permissions != 'warehouse')
		{
			msg=sprintf("%s: Requestor does not have admin rights",Sys.time())
		} else
		{
			if (!sodium::password_verify(reqInfo$password_hash, addAdminPw))
			{
				msg=sprintf("%s: Requestor password incorrect",Sys.time())
			} else
			{
				if (addpassword!=addpassword1)
				{
					msg=sprintf("%s: Password and Confirm Password different",Sys.time())
				} else
				{
					if (addusername %in% userDf$username)
					{
						msg=sprintf("%s: username already exists",Sys.time())
					} else
					{
						gAddUser(addusername,addpassword,addPermissions,addname,addemail,addStuBatch)
						msg=sprintf("%s: new user %s added",addusername,Sys.time())
					}
				}
			}
		}
		return(HTML(msg))
	})
	
  })
  
  observeEvent(input$uTypeChangeBtn,{
	output$uTypeChangeMsg=renderUI({
		userDf=userDf()
		usernameSelect=input$usernameSelect
		oldpassword1=input$oldpassword1
		uTypeSelect=input$uTypeSelect
		userDbInfo=userDf[which(userDf$username==usernameSelect),]
		
		reqInfo=userDf[which(userDf$username==credentials()$info$username),]
		if (reqInfo$permissions=='warehouse')
		{
			if (!sodium::password_verify(reqInfo$password_hash, oldpassword1))
			{
				msg=sprintf("%s: Wrong password provided",Sys.time())
			} else
			{
				gChangePermission(uTypeSelect,usernameSelect)
				msg=sprintf("%s: Rights changed for %s to %s",Sys.time(),usernameSelect,uTypeSelect)
			}
		} else
		{
			msg=sprintf("%s: You have no admin rights",Sys.time())
		}
		
		return(HTML(msg))
	})
  })
  
  observeEvent(input$pwchangeBtn,{
	output$passordChangeMsg=renderUI({
		userDf=userDf()
		oldpassword=input$oldpassword
		usernameSelect=input$usernameSelect
		password1=input$password1
		password2=input$password2
		userDbInfo=userDf[which(userDf$username==usernameSelect),]
		if (!sodium::password_verify(userDbInfo$password_hash, oldpassword))
		{
			msg=sprintf('%s: Wrong old password',Sys.time())
		} else
		{
			if (password1==password2)
			{
				gChangePassword(usernameSelect,password1)
				msg=sprintf("%s: Password changed",Sys.time())
			} else
			{
				msg=sprintf("%s: Different password entered for Password and Confirm Password",Sys.time())
			}
		}
		return(HTML(msg))
	})
  })
  
  observeEvent(input$infoChangeBtn,{
	output$infoChangeMsg=renderUI({
		usernameSelect=input$usernameSelect
		emailTxt=input$emailTxt
		nameTxt=input$nameTxt
		
		gChangeInfo(usernameSelect,nameTxt,emailTxt)
		msg=sprintf("%s: Info changed for %s",Sys.time(),usernameSelect)
		
		
		return(HTML(msg))
	})
  })
}


# Page Config
#################################

UserManagementPageConfig <- list(
  
  # Title for menu
  'title' = 'UserManagement',
  
  # Icon for menu
  'icon' = 'dashboard',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('warehouse')
)


