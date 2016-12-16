# A Shiny App for exploration of ERPs to go along with my blog at
# http://craddm.github.io/
# Only for two conditions at present. Now allows uploading of own data.
#
# TO DO - allow specification of custom names on loading of file
# 
# Matt Craddock, 2016

# Load required libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(Rmisc)
library(Cairo)
library(shinythemes)
options(shiny.usecairo=T)

#Load and prepare example data

levCatGA <- read_csv("data/levCatObjNon.csv",
                     col_names = c("Object", "Non-Object", "Time", "Subject")) %>%
  mutate(Difference = Object - `Non-Object`) %>%
  gather(condition, amplitude, -Time, -Subject) %>%
  mutate(
    effectType = factor(if_else(
      condition == "Difference", "Difference", "Main"
      )),
    condition = factor(
      condition,
      levels = c("Object", "Non-Object", "Difference"),
      labels = c("Object", "Non-Object", "Difference")
    )
  )

levCatGA$Subject <- factor(levCatGA$Subject)
levCatGA$condition <- factor(levCatGA$condition)

ERPdata <- levCatGA

# Define UI for application that draws the ERPs
ui <- fluidPage(
  theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Exploring ERP plot options"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "whichData",
        label = h4("Choose dataset"),
        choices = list("LevCat" = 1, "Custom" = 2),
        selected = 1
      ),
      
      checkboxGroupInput(
        "GroupMeans",
        label = h4("Group effects"),
        choices = list("Condition means" = "Main",
                       "Difference wave" = "Difference"),
        inline = TRUE,
        selected = "Main"
      ),
      
      checkboxGroupInput(
        "indivEffects",
        label = h4("Individual effects"),
        choices = list("Main effects" = "Main",
                       "Difference waves" = "Difference"),
        inline = TRUE
      ),
      
      h4("Statistics"),
      checkboxInput("plotCIs",
                    label = "Plot confidence intervals",
                    value = FALSE),
      uiOutput("CIselect"),
      
      checkboxInput("plotSig",
                    label = "Plot significant timepoints",
                    value = FALSE),
      uiOutput("sigOptions"),
      
      sliderInput(
        "timeRange",
        label = "Time range to plot",
        min = (round(min(ERPdata$Time))),
        max = (round(max(ERPdata$Time))),
        value = c(-100, 400)
      ),
      
      checkboxInput("facetSel",
                    label = "Split conditions?",
                    value = FALSE)
    ),
    
    # Show the ERP plot
    mainPanel(
      plotOutput("ERPPlot"),
      conditionalPanel(
        condition = "input.whichData == 2",
         fileInput("customData", label = h4("Upload data"))
        #           textInput("col1", label = ("Col1"), value = "Enter text...")
      )
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output, session) {
  
  #First set up conditional UI elements
  output$CIselect <- renderUI({
    if (input$plotCIs){
      radioButtons("confInts", 
                   label = NULL, 
                   choices = list("Between-subject" = 1,
                                  "Within-subject" = 2, 
                                  "Both" = 3),
                   selected = 1,
                   inline = TRUE)
    }
  })
  
  output$sigOptions <- renderUI({
    if (input$plotSig){
      tagList(
        sliderInput("pOffset",
                    label = "P-value position offset",
                    min = -10,
                    max = 10,
                    value = 0,
                    width = "50%"),
        radioButtons("correctionType",
                     label = "Multiple comparison correction", 
                     choices = list("Uncorrected" = 1,
                                    "Bonferroni-Holm" = 2, 
                                    "FDR" = 3),
                     selected = 1,
                     inline = TRUE)
      )
    }
  })
  
  # Check if participant wants to upload a file, upload it and add column names if so.
  fileData <- reactive({
    inFile <- input$customData
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read_csv(inFile$datapath,
             col_names = c("Cond1", "Cond2", "Time", "Subject")) %>%
      mutate(Difference = Cond1 - Cond2) %>%
      gather(condition, amplitude, -Time, -Subject) %>%
      mutate(
        effectType = factor(if_else(
          condition == "Difference", "Difference", "Main"
        )),
        condition = factor(
          condition,
          levels = c("Cond1", "Cond2", "Difference"),
          labels = c("Cond1", "Cond2", "Difference")
        )
      )
  })
  
  dataInput <- reactive({
    if (is.null(input$customData)) {
      levCatGA
    } else if (is.null(input$whichData)) {
      levCatGA
    } else if (input$whichData == 1) {
      levCatGA
    } else if (input$whichData == 2) {
      fileData()
    } 
  })

  significanceTests <- reactive({
    if (input$plotSig == TRUE) {
      filter(dataInput(), condition != "Difference") %>%
        split(.$Time) %>%
        map(~t.test(amplitude~condition,paired = TRUE,data = .)) %>%
        map_dbl(.,"p.value") %>%
        data.frame(Time = unique(dataInput()$Time),p.value = .)
      }
  })
    
  allCIs <- reactive({
    WSCI <- filter(dataInput(), condition != "Difference") %>%
      split(.$Time) %>%
      map(
        ~ summarySEwithin(
          data = .,
          measurevar = "amplitude",
          withinvars = "condition",
          idvar = "Subject"
        )
      ) %>%
      map_df(extract) %>%
      mutate(
        Time = rep(unique(dataInput()$Time), each = 2),
        #Note, you'll have to change 2 to match the number of conditions,
        CItype = factor("WSCI"),
        CIclass = factor("Main")
      )
    
    BSCI <- filter(dataInput(), condition != "Difference") %>%
      split(.$Time) %>%
      map( ~ summarySE(
        data = .,
        measurevar = "amplitude",
        groupvars = "condition"
      )) %>%
      map_df(extract) %>%
      mutate(
        Time = rep(unique(dataInput()$Time), each = 2),
        #Note, you'll have to change 2 to match the number of conditions,
        CItype = factor("BSCI"),
        CIclass = factor("Main")
      )
    
    diffCI <- filter(dataInput(), condition == "Difference") %>%
      split(.$Time) %>%
      map( ~ summarySE(data = .,
                       measurevar = "amplitude")) %>%
      map_df(extract) %>%
      mutate(
        condition = "Difference",
        Time = unique(dataInput()$Time),
        CItype = factor("Difference"),
        CIclass = factor("Difference")
      ) %>%
      select(-.id)
    
    rbind(BSCI, WSCI, diffCI)
  })
  
  output$ERPPlot <- renderPlot({
    
    levCat.plot <-
      filter(
        dataInput(),
        effectType %in% input$indivEffects |
          effectType %in% input$GroupMeans 
      ) %>%
      ggplot(aes(Time, amplitude)) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
    
    if (!is.null(input$GroupMeans)) {
      
      ERPdata <- filter(dataInput(), effectType %in% input$GroupMeans)
      
      levCat.plot <- levCat.plot +
        stat_summary(
          data = ERPdata,
          fun.y = mean,
          geom = "line",
          size = 1,
          aes(colour = condition)
        )+ 
        scale_colour_discrete(limits = levels(ERPdata$condition))
      
      if (input$plotCIs == TRUE) {
        
        CIdata <-  filter(allCIs(), CIclass %in% input$GroupMeans)
        
        if (is.null(input$confInts)) {
          } else if (input$confInts == 1) {
          levCat.plot <- levCat.plot +
            geom_ribbon(
              data = filter(CIdata, CItype != "WSCI"),
              aes(
                ymin = amplitude - ci,
                ymax = amplitude + ci,
                colour = condition,
                fill = condition
              ),
              linetype = "dashed",
              alpha = 0.3
            ) +
            guides(fill = "none")
        } else if (input$confInts == 2) {
          levCat.plot <- levCat.plot +
            geom_ribbon(
              data = filter(CIdata, CItype != "BSCI"),
              aes(
                ymin = amplitude - ci,
                ymax = amplitude + ci,
                fill = condition,
                colour = condition
              ),
              linetype = "dashed",
              alpha = 0.3
            ) +
            guides(fill = "none")
        } else if (input$confInts == 3) {
          levCat.plot <- levCat.plot +
            geom_ribbon(
              data = filter(CIdata, CItype != "BSCI"),
              aes(
                ymin = amplitude - ci,
                ymax = amplitude + ci,
                colour = condition,
                fill = condition
              ),
              linetype = "dashed",
              alpha = 0.3
            ) +
            geom_ribbon(
              data = filter(CIdata, CItype == "BSCI"),
              aes(
                ymin = amplitude - ci,
                ymax = amplitude + ci,
                colour = condition
              ),
              fill = NA,
              linetype = "dotted",
              alpha = 0.3
            ) +
            guides(fill = "none")
        } else{
          levCat.plot <- levCat.plot +
            geom_ribbon(
              data = filter(CIdata, CItype != "WSCI"),
              aes(
                ymin = amplitude - ci,
                ymax = amplitude + ci,
                colour = condition,
                fill = condition
              ),
              linetype = "dashed",
              alpha = 0.3
            ) +
            guides(fill = "none")
        }
      }
    }
    
    #If user requests individual effects, plot them for whatever types they have currently plotted (i.e. main effects and differences)
    
    if (is.null(input$indivEffects) == FALSE) {
      ERPdata <- filter(dataInput(), effectType %in% input$indivEffects)
      levCat.plot <- levCat.plot +
        geom_line(data = ERPdata,
                  aes(
                    group = interaction(Subject, condition),
                    colour = condition,
                    alpha = 0.2
                  )) +
        guides(alpha = "none")
    }
   
    if (input$facetSel == TRUE) {
      levCat.plot <- levCat.plot +
        facet_wrap(~condition,drop = TRUE)
    }
    
    if (input$plotSig == TRUE) {
       pvals <- significanceTests() 
         
      if (is.null(input$correctionType)){
        pvals <- pvals %>% mutate(crit = 0+(.$p.value <= .05))
      } else if (input$correctionType == 1){
        pvals <- pvals %>% mutate(crit = 0+(.$p.value <= .05))
      } else if (input$correctionType == 2){
        pvals <- pvals %>% 
          mutate(
            p.value = p.adjust(.$p.value,"holm"),
            crit = 0+(p.value <= .05)
            )
      } else if (input$correctionType == 3){
        pvals <- pvals %>% 
          mutate(
            p.value = p.adjust(.$p.value,"BH"),
            crit = 0+(p.value <= .05)
          )
      }
         
       pvals$crit[pvals$crit == 0] <- NA
       
       if (is.null(input$pOffset)) {
         pvalOffset <- 0
       } else {
         pvalOffset <- input$pOffset
       }
       
       levCat.plot <- levCat.plot +
         geom_line(data = pvals,
                   aes(x = Time, y = crit-pvalOffset),
                   na.rm = TRUE,
                   size = 2)
    }
    
    levCat.plot +
      labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
      geom_vline(xintercept = 0,linetype = "dashed" )+
      geom_hline(yintercept = 0,linetype = "dashed")+
      theme(axis.text = element_text(size = 14),axis.title = element_text(size = 14),legend.text = element_text(size=14))+
      coord_cartesian(xlim = c(input$timeRange[1],input$timeRange[2]))
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

