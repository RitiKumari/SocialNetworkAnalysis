
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
library(igraph)
library(networkD3)
library(dplyr)

shinyServer(function(input,output)
{
  # Reading the uploaded file EU_Core
  dataset = reactive({
    infile = input$file1  
    if (is.null(infile))
      return(NULL)
    infile_read = read.table(infile$datapath,sep = "\t", header=FALSE)
    colnames(infile_read) <- c ("Sender","Receiver")
    return(infile_read)
  })
  
  # Reading the uploaded file EU_Department
  dataset2 = reactive({
    infile = input$file2  
    if (is.null(infile))
      return(NULL)
    infile_read = read.table(infile$datapath,sep = "\t", header=FALSE)
    colnames(infile_read) <- c ("Code","Department")
    return(infile_read)
    
  })
  ############
  
  SenderFreqDataset = reactive({
    data_freq= as.data.frame(table(dataset()[,1]))
    names(data_freq)= c("Code", "Frequency")
    data_freq
  })
  
  ReceiverFreqDataset = reactive({
    data_freq= as.data.frame(table(dataset()[,2]))
    names(data_freq)= c("Code","Frequency")
    data_freq
  })
  
  ## Degree Centrality
  calcentrality <- reactive({
    if(is.null(dataset())){return ()}
    CoreData_df <- dataset()
    graph <- graph_from_data_frame(CoreData_df ,directed = T)
    
    #Making a data.frame from graph
    calDeg <- as.data.frame.character(V(graph))
    calDeg$Rowname <-row.names(calDeg)
    
    #calculating degree centrality
    calDeg$Degree <- igraph::degree(graph, mode="all")
    
    #sorting with respect to centrality and selecting the top ten nodes.
    calDeg <- calDeg[with(calDeg, order(-Degree)),]
    calDeg
  })
  ##################
  
  ##BetweenNess
  calbetween <-reactive({
    if(is.null(dataset())){return ()}
    CoreData_df <- dataset()
    graph <- graph_from_data_frame(CoreData_df ,directed = T)
    
    #Making a data.frame from graph
    calBet <- as.data.frame.character(V(graph))
    calBet$Rowname <-row.names(calBet)
    
    #calculating betweenness
    calBet$Betweenness <- igraph::betweenness(graph)
    
    #sorting with respect to betweenness and selecting the top ten nodes.
    calBet <- calBet[with(calBet, order(-Betweenness)),]
    calBet
  })
  
  #################
  
  ##Calculation of Indegree Centrality
  calindegree <- reactive({
    if(is.null(dataset())){return ()}
    CoreData_df <- dataset()
    graph <- graph_from_data_frame(CoreData_df ,directed = T)
    
    #Making a data.frame from graph
    calDeg <- as.data.frame.character(V(graph))
    calDeg$Rowname <-row.names(calDeg)
    
    #calculating degree centrality
    calDeg$Degree <- igraph::degree(graph, mode="in")
    
    #sorting with respect to centrality and selecting the top ten nodes.
    calInDeg <- calDeg[with(calDeg, order(-Degree)),]
    calInDeg
  })
  ################
  connectionCount <- reactive({input$hopCount})
  
  #Sending the output
  #First 50  rows for the first uploaded file
  output$tb1 <- renderDataTable({
    dataset()
  })
  
  #First 50  rows for the 2nd uploaded file
  output$tb2 <- renderDataTable({
    dataset2()
  })
  
  #Plotting the n/w
  output$plot <- renderSimpleNetwork({
    count <- as.numeric(connectionCount()[1])
    networkdata <- head(dataset(),count)
    simpleNetwork(networkdata, linkDistance = 50, charge = -30, fontSize = 15, fontFamily = "serif",
                  linkColour = "Black", nodeColour = "Red", opacity = 0.6, zoom = F)
  })
  ############
  
  
  #Counting the emails sent and displaying it in UI
  output$sender <- renderDataTable({
    data_freq= as.data.frame(table(dataset()[,1]))
    names(data_freq)= c("Employee Code","Emails Sent")
    data_freq
  })
  ############  
  
  #Counting the emails received and displaying it in UI
  output$receiver <- renderDataTable({
    data_freq= as.data.frame(table(dataset()[,2]))
    names(data_freq)= c("Employee Code","Emails Received")
    data_freq
  })
  ############
  
  #2-hop Sender Connection
  output$ShopConnection <- renderSimpleNetwork({
    data_freq= as.data.frame(table(dataset()[,1]))
    names(data_freq)= c("Sender","Frequency")
    
    #Ordering the dataset with frequency and taking the top 10 sender 
    data_freq = data_freq[with(data_freq, order(-Frequency)), ] 
    SenderList <- head(data_freq,input$topN)
    
    #Expansion of the SenderList
    SenderMailList <- filter(dataset(),Sender %in% SenderList$Sender)
    
    #1st hop
    FirstReceiver <- unique(SenderMailList$Receiver)
    
    SenderVec <- SenderList$Sender
    FinalSenderList <- append(SenderVec,FirstReceiver)
    
    #2nd hop
    SenderMailList <- filter(dataset(),Sender %in% FinalSenderList)
    SenderMailList <- unique(SenderMailList)
    #SenderMailList
    simpleNetwork(head(SenderMailList,250), linkDistance = 50, charge = -30, fontSize = 15, fontFamily = "serif",
                  linkColour = "Black", nodeColour = "Red", opacity = 0.6, zoom = F)
    
  })
  
  ############
  
  #2-hop Receiver Connection
  output$RhopConnection <- renderSimpleNetwork({
    data_freq= as.data.frame(table(dataset()[,2]))
    names(data_freq)= c("Receiver","Frequency")
    
    #Ordering the dataset with frequency and taking the top 10 Receiver 
    data_freq = data_freq[with(data_freq, order(-Frequency)), ] 
    ReceiverList <- head(data_freq,input$topN)
    
    #Expansion of the ReceiverList
    ReceiverMailList <- filter(dataset(),Receiver %in% ReceiverList$Receiver)
    
    #1st hop
    FirstSender <- unique(ReceiverMailList$Sender)
    
    ReceiverVec <- ReceiverList$Receiver
    FinalReceiverList <- append(ReceiverVec,FirstSender)
    
    #2nd hop
    ReceiverMailList <- filter(dataset(),Receiver %in% FinalReceiverList)
    ReceiverMailList <- unique(ReceiverMailList)
    simpleNetwork(head(ReceiverMailList,250),linkDistance = 50, charge = -30, fontSize = 15, fontFamily = "serif",
                  linkColour = "Black", nodeColour = "Red", opacity = 0.6, zoom = F)
    
  })
  
  ############
  
  #Calculating centrality
  output$DegreeCentrality <- renderDataTable({
    calcentrality()
  })
  
  #Plotting Degree Centrality
  
  output$centralityPlot <-renderForceNetwork({
    centralityData <- head(calcentrality()[,2],input$topN)
    
    #Expansion of the SenderList
    SenderMailList <- filter(dataset(),Sender %in% centralityData)
    
    #1st hop
    FirstReceiver <- unique(SenderMailList$Receiver)
    
    SenderVec <- centralityData
    FinalSenderList <- append(SenderVec,FirstReceiver)
    
    #2nd hop
    SenderMailList <- filter(dataset(),Sender %in% FinalSenderList)
    SenderMailList <- unique(SenderMailList)
    
    #Appending a column with value 1( to be used in force n/w value option)
    SenderMailList$One <- rep(1,nrow(SenderMailList))
    
    #ordering the department data
    deptData <- dataset2()
    deptData <- deptData[with(deptData, order(Code)),]
    
    #Plotting the Force N/w
    forceNetwork(Links =  head(SenderMailList,250) , Nodes = deptData , Source = "Sender",
                 Target = "Receiver",Value = "One",NodeID = "Code",
                 Group = "Department", opacity = 1, arrows = TRUE,
                 legend = TRUE, linkColour = "#666",  zoom = TRUE, linkDistance = 80, height=90,width=150,fontSize = 30,
                 bounded = FALSE, opacityNoHover = 0,
                 clickAction = NULL )
  })
  
  ###########
  
  #BetweenNess
  output$Betweeness <- renderDataTable({
    calbetween()
  })
  
  #Plotting Betweenness
  output$betweennessPlot <-renderForceNetwork({
    betweennessData <- head(calbetween()[,2],input$topN)
    
    #Expansion of the SenderList
    SenderMailList <- filter(dataset(),Sender %in% betweennessData)
    
    #1st hop
    FirstReceiver <- unique(SenderMailList$Receiver)
    
    SenderVec <- betweennessData
    FinalSenderList <- append(SenderVec,FirstReceiver)
    
    #2nd hop
    SenderMailList <- filter(dataset(),Sender %in% FinalSenderList)
    SenderMailList <- unique(SenderMailList)
    #simpleNetwork(head(SenderMailList,50))
    
    #Appending a column with value 1( to be used in force n/w value option)
    SenderMailList$One <- rep(1,nrow(SenderMailList))
    
    #ordering the department data
    deptData <- dataset2()
    deptData <- deptData[with(deptData, order(Code)),]
    
    #Plotting the Force N/w
    forceNetwork(Links =head(SenderMailList,250) , Nodes = deptData , Source = "Sender",
                 Target = "Receiver",Value = "One",NodeID = "Code",
                 Group = "Department", opacity = 1, arrows = TRUE,
                 legend = TRUE, linkColour = "#666",  zoom = TRUE, linkDistance = 80, height=90,width=150,fontSize = 30,
                 bounded = FALSE, opacityNoHover = 0,
                 clickAction = NULL )
  })
  
  ###########
  
  #Calculating In degree centrality
  output$InCentrality <- renderDataTable({
    calindegree()
  })
  
  ############
  
  #Plotting Betweenness
  output$inPlot <-renderForceNetwork({
    inDegreeData <- head(calindegree()[,2],10)
    
    #Expansion of the SenderList
    SenderMailList <- filter(dataset(),Sender %in% inDegreeData)
    
    #1st hop
    FirstReceiver <- unique(SenderMailList$Receiver)
    
    SenderVec <- inDegreeData
    FinalSenderList <- append(SenderVec,FirstReceiver)
    
    #2nd hop
    SenderMailList <- filter(dataset(),Sender %in% FinalSenderList)
    SenderMailList <- unique(SenderMailList)
    #simpleNetwork(head(SenderMailList,50))
    
    #Appending a column with value 1( to be used in force n/w value option)
    SenderMailList$One <- rep(1,nrow(SenderMailList))
    
    #ordering the department data
    deptData <- dataset2()
    deptData <- deptData[with(deptData, order(Code)),]
    
    #Plotting the Force N/w
    forceNetwork(Links =  head(SenderMailList,250) , Nodes = deptData , Source = "Sender",
                 Target = "Receiver",Value = "One",NodeID = "Code",
                 Group = "Department", opacity = 1, arrows = TRUE,
                 legend = TRUE, linkColour = "#666",  zoom = TRUE, linkDistance = 80, height=90,width=150,fontSize = 30,
                 bounded = FALSE, opacityNoHover = 0,
                 clickAction = NULL )
  })
   
  #Department Interaction
  output$department <- renderDataTable({
    
    mailData <- dataset()
    deptData <- dataset2()

    #Appending the sender department
    names(deptData)[1] <- names(mailData)[1]
    SenderDeptData <- left_join(mailData, deptData , by = c(names(mailData)[1],names(deptData)[1]))

    #Appending the receiver department
    names(SenderDeptData)[2] -> names(deptData)[1]
    EmpDeptData <- left_join(SenderDeptData, deptData , by = c(names(SenderDeptData)[2],names(deptData)[1]))
    names(EmpDeptData) <- c("Sender","Receiver","Sender_Department","Receiver_Department")
    
    DeptInteraction <- EmpDeptData[,3:4]
    
    DeptInteraction <- count(DeptInteraction,c("Sender_Department", "Receiver_Department"))
    #mail_department <- mail_department[!(mail_department$Sender_Department==mail_department$Receiver_Department),]
    #mail_department <- head(mail_department[order(mail_department$freq, decreasing = T),])
    DeptInteraction
    
    
  })
  
  ############
  
  
  #Department Interaction Plot
  output$deptplot <-renderPlot({
    
    mailData <- dataset()
    deptData <- dataset2()
    
    #Appending the sender department
    names(deptData)[1] <- names(mailData)[1]
    SenderDeptData <- left_join(mailData, deptData , by = c(names(mailData)[1],names(deptData)[1]))
    
    #Appending the receiver department
    names(SenderDeptData)[2] -> names(deptData)[1]
    EmpDeptData <- left_join(SenderDeptData, deptData , by = c(names(mailData)[2],names(deptData)[1]))
    names(EmpDeptData) <- c("Sender","Receiver","SenderDepartment","ReceiverDepartment")
    
    #DeptWiseInteraction 
    DeptInteraction <- unique(EmpDeptData[,3:4] %>% group_by(SenderDepartment,ReceiverDepartment))
    DeptInteraction <- as.matrix(DeptInteraction)

    graph <- graph_from_data_frame(head(DeptInteraction,50) ,directed = T)
    plot(graph)
    # plot.igraph(graph,edge.color="black",vertex.color="white", 
    #             vertex.label="SenderDepartment",vertex.size=20,vertex.label.cex=3, 
    #             vertex.label.color="black",edge.width=3,edge.arrow.size=1.2,edge.arrow.width=1.2)
  })
  ###############
  
})
