library(shiny)
library(V8)
library(sodium)
library(openssl)
library(rJava)   #For sending an email from R
library(mailR)   #For sending an email from R
library(DBI)
library(pool)
library(RSQLite)
#DataBase
pool <- dbPool(drv = RSQLite::SQLite())

onStop(function() {
  poolClose(pool)
})
#Create table user in DB
dbExecute(pool, 'CREATE TABLE user (user_name TEXT, country TEXT, email TEXT, password TEXT)')

#Countries
countries.list <- read.table("www/countries.txt", header = FALSE, sep = "|",
                             stringsAsFactors = FALSE, quote = "",
                             col.names = c("abbr", "country"))
choice.country <- as.list(as.character(countries.list$country))
names(choice.country) <- countries.list$country

server <- function(input, output, session) {
  
  #####################################################################################
  ########################## Start LogIn ################################################
  #####################################################################################
  
  
  ## Initialize - user is not logged in
  #user_abu <- reactiveValues(login = FALSE, name = NULL, role = NULL, header = NULL)
  loggedIn <- reactiveVal(value = FALSE)
  user <- reactiveValues(name = NULL, id=NULL)
  
  #observeEvent will execute only if butLogin is pressed. observe is executed uniformly over time.# 
  #THis after pressing login#  
  observeEvent(input$butLogin, {
    #browser()  #: for debug mode test
    req(input$username, input$pwInp)  #Make sure username and passowrd are entered#
    query <- sqlInterpolate(pool,"select * from user where user_name=?user or email=?email;",user=input$username,email=input$username)
    
    user_data <- dbGetQuery(pool,query)
    
    if(nrow(user_data) > 0){ # If the active user is in the DB then logged in
      if(sha256(input$pwInp) == user_data[1, "password"]){
        
        user$name <- user_data[1, "user_name"]
        user$id <- user_data[1, "user_id"]
        loggedIn(TRUE)
        
        #print(paste("- User:", user$name, "logged in"))
        #removeModal()  ## remove the modal
        toggleModal(session, "window", toggle = "close")
        output$App_Panel <- renderUI({
          span(
            strong(paste("welcome", user$name, "|")), 
            actionLink(inputId = "logout", "Logout")
          )
          
        })
      }
    } else {
      loggedIn(FALSE)
      
    } 
  })
  
  
  output$login_status <- renderUI({
    if(input$butLogin == 0){
      return(NULL)
    } else {
      if(!loggedIn()){
        return(span("The Username or Password is Incorrect", style = "color:red"))
      }
    }
  })
  
  #For creating a new account#  
  
  observeEvent(input$create_account, {
    
    showModal(
      modalDialog(title = "Create an account", size = "m", 
                  textInput(inputId = "new_user", label = "Username"),
                  textInput(inputId = "new_email", label = "Email"),
                  selectizeInput(inputId = 'country', 'Country', choices = choice.country),
                  passwordInput(inputId = "new_pw", label = "Password"),
                  passwordInput(inputId = "new_pw_conf", label = "Confirm password"),
                  checkboxInput(inputId = "terms", label = a("I, agree for terms and conditions",target="_blank",href="Disclaimer-TermsandConditions.html")),
                  actionButton(inputId = "register_user", label = "Submit"),
                  #p(input$register_user),
                  uiOutput("register_status"),
                  footer = actionButton("dismiss_modal",label = "Dismiss")
                  
      )
    )
    
    register_user()
    
  })
  
  
  
  observeEvent(input$dismiss_modal,{
    removeModal()
  })
  
  
  register_user <- eventReactive(input$register_user, {
    
    
    if(!isTruthy(input$new_user) | !isTruthy(input$new_email) | !isTruthy(input$new_pw) ){
      return(span("Fill required information correctly", style = "color:red"))
    } 
    
    if (!isValidEmail(input$new_email)){
      return(span("Please provide a valid email address", style = "color:red"))
    }
    
    if (sha256(input$new_pw)!=sha256(input$new_pw_conf)){ 
      return(span("Entered passwords do not match.", style = "color:red"))
    }
    
    if (!input$terms){ 
      return(span("Please tick the box to show that you agree with terms and conditions", style = "color:red"))
    }
    
    
    query <- sqlInterpolate(pool,"select * from user where user_name=?user or email=?email;",user=input$new_user,email=input$new_email)
    
    users_data <- dbGetQuery(pool,query)
    
    #users_data <- DB_get_user(input$new_user)

    if(nrow(users_data) > 0){
      return(span("User already exists", style = "color:red"))
    }

    new_hash <- sha256(input$new_pw)
    new_user <- input$new_user
    
    dbExecute(pool,paste0("INSERT INTO user (user_name, country, email, password) values ","('",new_user,"','",input$country,"','",input$new_email,"','",new_hash,"')", ";"))
    
    print("- New user added to database")
    
    #Send an email to the newly regitered user. The email will provide him with username and password#    
    
    # isolate({send.mail(from = "....@gmail.com",
    #                    to = input$new_email,
    #                    subject = "Welcome to ... App",
    #                    body = HTML(paste(paste("Hi",new_user,","),
    #                                      "<p>Thank you for using https://test.com. Please find below your credentials for future reference:</p>",
    #                                      paste("Username:",new_user,"<br>"),
    #                                      paste("Password:",input$new_pw,"<br><br><br>"),
    #                                      paste("Best regards, <br><br>Test.com Team"))),
    #                    smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "...@gmail.com", passwd = "...", ssl = TRUE),
    #                    authenticate = TRUE,
    #                    html = TRUE,
    #                    send = TRUE)})
    # 
    
    return(span("Your registration was successful. An email with your credential is sent to the registred email adrress", style = "color:green"))
    
    loggedIn(FALSE)
  })
  
  
  output$register_status <- renderUI({
    if(input$register_user == 0){
      return(NULL)
    } else {
      isolate(register_user())
    }
  })
  
  observeEvent(input$logout, {
    user$name <- NULL
    user$id <- NULL
    loggedIn(FALSE)
    js$reset2()
    #stopApp()
    #print("- User: logged out")
  })
  
}  
