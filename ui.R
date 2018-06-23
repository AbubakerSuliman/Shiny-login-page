library(shiny)
library(shinyBS)
library(shinyjs)


#2. Main functions#
source("functions.R")
jsResetCode <- "shinyjs.reset2 = function() {history.go(0)}" # Define the js method that resets the page

ui <- fluidPage(
  ## Display login details 
  bsModalNoClose("window", "Window",
                 title="Log in",size='small',
                 textInput('username', 'Username'),
                 passwordInput('pwInp', 'Password'),
                 actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
                 uiOutput(outputId = "login_status"),hr(),
                 footer = h4(actionLink('create_account','Create an Account'),align='right'),
                 tags$head(tags$style("#window .modal-footer{display:none}
                                      .modal-header .close{display:none}"),
                           tags$script("$
                                       $(document).ready(function(){
                                       $('#window').modal();
                                       });
                                       ")
                           )),
  shinyjs::useShinyjs(),                                           # Include shinyjs in the UI
  shinyjs::extendShinyjs(text = jsResetCode),                      # Add the js code to the page
  navbarPage(collapsible=F,title="Log In App",
                        tabPanel("About")),
  HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
           parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\"><strong>",uiOutput("out_id"),"</strong></a></li><li class=\"disabled\"><a href=\"#\"><strong>",uiOutput('App_Panel'),"</strong></a></ul>' );</script>"))
  
)
  
  