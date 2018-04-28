
######################################
## Author:   Riti Kumari (sinhariti61@gmail.com)
## Date:     2018-02-13
## Title:    Socail Network Analysis 
## Purpose:  Analysis of EU Employee datasets
#            Analysis & visualization of employee and department level interaction 
#            Visualization of the degree centrality , in-degree, out-degree and betweeness
#            Depiction of the important persons of the organisation
#            Analysis of the horizaontality of the organization
######################################

library(shiny)
library(networkD3)
library(dplyr)

shinyUI(fluidPage(
  
  #titlePanel
  titlePanel("RitiKumari_Social_Network_Analysis"),
  br(),
  #sidebarLayout
  sidebarLayout
  (
    
    #sidebarPanel
    #Upload the file
    sidebarPanel(fileInput("file1","Please upload the Emmployee Email Data file in txt format"), 
                 fileInput("file2","Please upload the Department Data file in txt format"),
                 textInput("hopCount","Please input the number of connections for plotting",value="40"),
                 sliderInput("topN","Choose the Top N value for plotting : ", max=10,min=0, value =c(1)),
                 #sliderInput("receiverRange","Enter Receiver code range : ", max=1005,min=0, value =c(10,11))
                 br(),
                 br(),
                 #img(src="R.jpg", height=100, width=200)
                 img(src="Shiny-image.png", height=100, width=250)
    ),
    #mainPanel
    mainPanel
    (
      tabsetPanel(type="tab",
                  tabPanel("Preface",
                           tags$style("body{background-color:linen; color:brown}"),
                           br(),
                           h2("Social Network Analysis"),
                           br(),
                           p("The network was generated using email data from a large European research institution. 
                              We have anonymized information about all incoming and outgoing email between
                             members of the research institution. There is an edge (u, v) in the network if person
                             u sent person v at least one email. The e-mails only represent communication between
                             institution members (the core), and the dataset does not contain incoming messages
                             from or outgoing messages to the rest of the world."),
                           br(),
                           p("The dataset also contains ”ground-truth” community memberships of the nodes.
                             Each individual belongs to exactly one of 42 departments at the research institute.
                             This network represents the ”core” of the email-EuAll network, which also contains
                             links between members of the institution and people outside of the institution (although
                             the node IDs are not the same).")
                  ),
                  tabPanel("Data",
                           fluidRow(
                             br(),
                             column(4, h3("Employee Email Data"), dataTableOutput("tb1")),
                             column(4, h3("Department Data"), dataTableOutput("tb2"))
                           )),
                  navbarMenu( "Interaction" ,
                              tabPanel("Employee Interaction",
                                       #For making grid
                                       fluidRow(
                                         br(),
                                         column(4, h3("Senders Interaction"), dataTableOutput("sender")),
                                         column(4, h3("Receivers Interaction"), dataTableOutput("receiver"))
                                       )
                              ),
                              tabPanel("Department Interaction",
                                       fluidRow(column(4,h3("Department Interaction"), dataTableOutput("department"))))
                  ),
                  navbarMenu("Plot",
                             tabPanel("Connection Plot", h3("Connection Plot"), simpleNetworkOutput("plot")),
                             tabPanel("Department Interaction Plot", h3("Department Interaction Plot"),plotOutput("deptplot")),
                             tabPanel("Centrality Plot", h3("Centrality Plot"),forceNetworkOutput("centralityPlot")),
                             tabPanel("Betweenness Plot",h3("Betweenness Plot") ,forceNetworkOutput("betweennessPlot")),
                             tabPanel("In Degree Centrality Plot",h3("In Degree Centrality Plot"), forceNetworkOutput("inPlot"))
                  ),
                  navbarMenu("2-hop Connection" ,
                             tabPanel("Sender Connection",h3("Sender Connection Plot") ,simpleNetworkOutput("ShopConnection")),
                             tabPanel("Receiver Connection", h3("Receiver Connection Plot"),simpleNetworkOutput("RhopConnection"))
                  ),
                  tabPanel("Comuptation",
                           fluidRow(
                             br(),
                             column(4,h3("Degree Centrality"),dataTableOutput("DegreeCentrality")),
                             column(4,h3("In-Degree Centrality"),dataTableOutput("InCentrality")),
                             column(4,h3("BetweenNess"),dataTableOutput("Betweeness"))
                           )
                  )
      ),
      h5(strong(tags$footer("@ Shiny App", align ="center")))
    )
  )
  
  
))
