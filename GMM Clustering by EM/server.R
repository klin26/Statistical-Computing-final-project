library(mvtnorm)
library(fields)
library(plotly)
library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)

source("function.R")

# Define server logic
server = function(input, output, session) {
  
  # GMM
  
  filter_data = reactive({
    data = input$file
    if(is.null(data)){ return(NULL) }
    
    read.csv(data$datapath,header = T)
  })
  
  # output$ui_var = renderUI({
  #   
  #   if(is.null(input$file) == FALSE){
  #     return(
  #       list(
  #         CheckboxGroupInput(inputId = "var", "Select variables.", choices = names(filter_data())),#, selected = names(filter_data())[])
  #         tags$small(paste0("Note : Please select at least 2 variables."))
  #       )
  #     )
  #     }
  # })
  
  observe({
    updateCheckboxGroupInput(session, inputId = "var", choices = names(filter_data()))#, selected = names(filter_data())[])
  })
  
  GMMInput = eventReactive(input$GMMrun,{
    
    data = filter_data()
    # numeric_names = names(data)[sapply(data, is.numeric)]
    data_select = as.data.frame(data[,c(input$var)])
    k = as.numeric(input$k_select)
    par = EM(data_select,k)
    return(list(data=data_select,par=par))
    
  })
  
  output$data_tab = renderDataTable({
    
    data = GMMInput()$data
    out = GMMInput()$par
    print(round(data,7))
    
  })
  
  output$download_data_tab = downloadHandler(
    
    filename = function() { paste("Data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      data = GMMInput()$data
      DataSelect = as.data.frame(round(data,7))
      write.csv(DataSelect, file) }
  )
  
  # plotInput = eventReactive(input$GMMrun,{
  #   
  #   data = GMMInput()$data
  #   out = GMMInput()$par
  #   # n <- dim(data)[2]
  #   # par(mfrow=c(n,n), mar=c(3,3,1,1), mgp=c(1.6,0.6,0))
  #   plotEM(data,out)
  #   
  # })
  
  output$plot = renderPlot({
    
    data = GMMInput()$data
    out = GMMInput()$par
    # n <- dim(data)[2]
    # par(mfrow=c(n,n), mar=c(3,3,1,1), mgp=c(1.6,0.6,0))
    plotEM(data,out)
    
  })
  
  # output$download_plot = downloadHandler(
  #   
  #   filename = function() {paste("GMMclusteringplot-", Sys.Date(), ".png", sep="") },
  #   content = function(file) { 
  #     png(file, width = 800, height = 800) 
  #     plot(plotInput())
  #     dev.off()
  #   }
  #   
  # )
  
  output$pro_par = renderDataTable({
    
    data = GMMInput()$data
    out = GMMInput()$par
    print(round(out$par_table,7))
    
  })
  
  output$download_pro_par = downloadHandler(
    
    filename = function() { paste("GMMclusteringprocess-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      out = GMMInput()$par
      Parameter = as.data.frame(round(out$par_table,7))
      write.csv(Parameter, file) }
  )
  
  output$tsplot = renderPlot({
    
    data = GMMInput()$data
    out = GMMInput()$par
    ts.plotEM(data,out)
    
  })
  
  output$est_par = renderDataTable({
    
    data = GMMInput()$data
    out = GMMInput()$par
    finalParameter = as.data.frame(round(out$par_table[out$iter+1,],7))
    colnames(finalParameter) = c("parameter estimation")
    print(finalParameter)
    
  })
  
  output$download_est_par = downloadHandler(

    filename = function() { paste("GMMclusteringparameters-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      out = GMMInput()$par
      finalParameter = as.data.frame(round(out$par_table[out$iter+1,],7))
      colnames(finalParameter) = c("parameter estimation")
      write.csv(finalParameter, file) }
  )
  
  output$cluster_par = renderDataTable({
    
    data = GMMInput()$data
    out = GMMInput()$par
    clusteringIndex = data.frame(out$index_clustering)
    colnames(clusteringIndex) = c("clustering")
    print(clusteringIndex)
    
  })
  
  output$download_cluster_par = downloadHandler(

    filename = function() { paste("GMMclusteringindex-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      out = GMMInput()$par
      clusteringIndex = data.frame(out$index_clustering)
      colnames(clusteringIndex) = c("clustering")
      write.csv(clusteringIndex, file) }
  )
  
  # BIC
  
  filter_data_BIC = reactive({
    data = input$fileBIC
    if(is.null(data)){ return(NULL) }
    
    read.csv(data$datapath,header = T)
  })
  
  observe({
    updateCheckboxGroupInput(session, inputId = "varBIC", choices = names(filter_data_BIC()))#, selected = names(filter_data_BIC()))
  })
  
  BICInput = eventReactive(input$BICrun,{
    
    dataBIC = filter_data_BIC()
    dataBIC_select = as.data.frame(dataBIC[,c(input$varBIC)])
    kmin = as.numeric(input$k_min_select)
    kmax = as.numeric(input$k_max_select)
    return(list(data=dataBIC_select,kmin=kmin,kmax=kmax))
    
  })
  
  output$BICdata_tab = renderDataTable({
    
    data = BICInput()$data
    out = BICInput()$par
    print(round(data,7))
    
  })
  
  output$download_BICdata_tab = downloadHandler(
    
    filename = function() { paste("Data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      data = BICInput()$data
      DataSelect = as.data.frame(round(data,7))
      write.csv(DataSelect, file) }
  )
  
  output$BICplot = renderPlot({

    data = BICInput()$data
    kmin = BICInput()$kmin
    kmax = BICInput()$kmax
    BICplotEM(data,kmin,kmax)

  })
  
}