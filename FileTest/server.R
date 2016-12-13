library(shiny)

function(input, output) {
  
  fileData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    # read_csv(inFile$datapath,
    #          col_names = c("Object", "Non-Object", "Time", "Subject")) %>%
    #   mutate(Difference = Object - `Non-Object`) %>%
    #   gather(condition, amplitude,-Time,-Subject) %>%
    #   mutate(effectType = factor(if_else(condition == "Difference", "Difference", "Main")))
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    # inFile <- input$file1
    fileData()
    # if (is.null(inFile))
    #   return(NULL)
    # 
    # read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #          quote=input$quote)
  })
}
