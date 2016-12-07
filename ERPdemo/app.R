# A Shiny App for exploration of ERPs to go along with my blog at
# http://craddm.github.io/
# Only for two conditions at present.
# 
# Matt Craddock, 2016

library(shiny)
library(tidyverse)
library(reshape2)
library(magrittr)
library(Rmisc)
library(Cairo)
options(shiny.usecairo=T)


levCatGA <- read_csv("data/levCatObjNon.csv",
                     col_names = c("Object", "Non-Object", "Time", "Subject")) %>%
  mutate(Difference = Object - `Non-Object`) %>%
  gather(condition, amplitude,-Time,-Subject) %>%
  mutate(effectType = factor(if_else(condition == "Difference", "Difference", "Main")))

levCatGA$Subject <- factor(levCatGA$Subject)
levCatGA$condition <- factor(levCatGA$condition)

WSCI <- filter(levCatGA,condition != "Difference") %>%
  split(.$Time) %>%
  map(~summarySEwithin(data = .,
                       measurevar = "amplitude",
                       withinvars = "condition",
                       idvar = "Subject")) %>%
  map_df(extract) %>%
  mutate(
    Time = rep(unique(levCatGA$Time),each =2), #Note, you'll have to change 2 to match the number of conditions,
    CItype = factor("WSCI"),
    CIclass = factor("Main")
  )

BSCI <- filter(levCatGA,condition != "Difference") %>%
  split(.$Time) %>%
  map(~summarySE(data = .,
                 measurevar = "amplitude",
                 groupvars = "condition"
  )) %>%
  map_df(extract) %>%
  mutate(
    Time = rep(unique(levCatGA$Time),each =2), #Note, you'll have to change 2 to match the number of conditions,
    CItype = factor("BSCI"),
    CIclass = factor("Main")
  )

diffCI <- filter(levCatGA,condition == "Difference") %>%
  split(.$Time) %>%
  map(~summarySE(data = .,
                 measurevar = "amplitude"
  )) %>%
  map_df(extract) %>%
  mutate(
    condition = "Difference",
    Time = unique(levCatGA$Time),
    CItype = factor("Difference"),
    CIclass = factor("Difference")
  ) %>%
  select(-.id)

allCIs <- rbind(BSCI,WSCI,diffCI)
#allCIs$CIclass <- factor(allCIs$CIclass)
#allCIs$condition <- factor(allCIs$condition)

# Define UI for application that draws the ERPs
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring ERP plot options"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    
      checkboxGroupInput("GroupMeans", 
                         label = h4("Group effects"), 
                         choices = list("Condition means" = "Main",
                                        "Difference wave" = "Difference"),
                         inline = TRUE,
                         selected = "Main"),
      
      checkboxGroupInput("indivEffects", 
                         label = h4("Individual effects"), 
                         choices = list("Main effects" = "Main",
                                        "Difference waves" = "Difference"),
                         inline = TRUE
                         ),
      
      h4("Confidence intervals"),
      
      checkboxInput("plotCIs",
                    label = "Plot confidence intervals",
                    value = FALSE),
      
      radioButtons("confInts", 
                         label = NULL, 
                         choices = list("Between-subject" = 1,
                                        "Within-subject" = 2, 
                                        "Both" = 3),
                         inline = TRUE
                         ),
      
      sliderInput("timeRange",
                  label = "Time range to plot",
                  min = (round(min(levCatGA$Time))),
                  max = (round(max(levCatGA$Time))),
                  value = c(-100,400)
                  ),
      
      checkboxInput("facetSel",
                    label = "Split conditions?",
                    value = FALSE)),
    
    # Show the ERP plot
    mainPanel(
      plotOutput("ERPPlot")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  dataInput <- reactive({
    levCatGA <- filter(levCatGA,
                       Time > input$timeRange[1] & Time < input$timeRange[2]
    )
  })

  allCIinput <- reactive({
    allCIs <- filter(allCIs,
                     Time > input$timeRange[1] & Time < input$timeRange[2]
    )
  })
  
  output$ERPPlot <- renderPlot({
    
    levCat.plot <- dataInput() %>% 
      ggplot(aes(Time,amplitude))+
      scale_color_brewer(palette = "Set1")+
      theme_minimal()
    
    if (is.null(input$GroupMeans) == FALSE) {
      ERPdata <- filter(dataInput(), effectType %in% input$GroupMeans)
      
      levCat.plot <- levCat.plot +
        stat_summary(
          data = ERPdata,
          fun.y = mean,
          geom = "line",
          size = 1,
          aes(colour = condition)
        )
      
      if (input$plotCIs == TRUE) {
        CIdata <-  filter(allCIinput(), CIclass %in% input$GroupMeans)
        if (input$confInts == 1) {
          levCat.plot <- levCat.plot +
            geom_ribbon(
              data = filter(CIdata, CItype != "WSCI"),
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
              data = filter(CIdata,CItype != "BSCI"),
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
              data = filter(CIdata,CItype == "BSCI"),
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
        }
      }
    }
    
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
    
    levCat.plot +
      labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
      geom_vline(xintercept = 0,linetype = "dashed" )+
      geom_hline(yintercept = 0,linetype = "dashed")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

