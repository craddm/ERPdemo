#
# First attempt at creating a Shiny App for exploration of ERPs to go along with my blog at
# http://craddm.github.io/
#
# Matt Craddock, 2016

library(shiny)
library(reshape2)
library(ggplot2)
library(dplyr)
library(purrr)
library(magrittr)
library(Rmisc)
library(Cairo)
options(shiny.usecairo=T)

levCatGA <- read.csv("data/levCatObjNon.csv",header = FALSE)
names(levCatGA) <- c("Object","Non-Object","Time","Subject")
levCatGA$Subject <- as.factor(levCatGA$Subject)
levCatGA <- melt(levCatGA,id.vars = c("Subject","Time"))
names(levCatGA) <- c("Subject","Time","condition","amplitude")
WSCI <- levCatGA %>%
  split(.$Time) %>%
  map(~summarySEwithin(data = .,
                       measurevar = "amplitude",
                       withinvars = "condition",
                       idvar = "Subject")) %>%
  map_df(extract) %>%
  mutate(
    Time = rep(unique(levCatGA$Time),each =2) #Note, you'll have to change 2 to match the number of conditions
  )
BSCI <- levCatGA %>%
  split(.$Time) %>%
  map(~summarySE(data = .,
                 measurevar = "amplitude",
                 groupvars = "condition"
                 )) %>%
  map_df(extract) %>%
  mutate(
    Time = rep(unique(levCatGA$Time),each =2) #Note, you'll have to change 2 to match the number of conditions
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring ERP plot options"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput("plotCIs",
                      label = "Confidence intervals",
                      value = TRUE),
        selectInput("CItype", label = h4("CI type?"), 
                    choices = list("Between-subject" = 1, "Within-subject" = 2, "Both" = 3), 
                    selected = 1),
        checkboxInput("plotGroup",
                      label = "Plot group means?",
                      value = TRUE),
        checkboxInput("plotIndivs",
                      label = "Plot individuals?",
                      value = FALSE),
        sliderInput("timeRange",
                    label = "Time range to plot",
                    min = (round(min(levCatGA$Time))),
                    max = (round(max(levCatGA$Time))),
                    value = c(-100,400)),
        checkboxInput("facetSel",
                      label = "Split conditions?",
                      value = FALSE)
      ),
      
      # Show a plot of the generated distribution
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
  
  WSCIinput <- reactive({
    WSCI <- filter(WSCI,
                   Time > input$timeRange[1] & Time < input$timeRange[2]
                   )
    })
  BSCIinput <- reactive({
    BSCI <- filter(BSCI,
                   Time > input$timeRange[1] & Time < input$timeRange[2]
    )
  })
   output$ERPPlot <- renderPlot({
     levCat.plot <- dataInput() %>% 
       ggplot(aes(Time,amplitude))+
       scale_color_brewer(palette = "Set1")+
       theme_minimal()
     if (input$plotIndivs == TRUE) {
       levCat.plot <- levCat.plot +
         geom_line(aes(group = interaction(Subject,condition),colour = condition,alpha = 0.2))+
         guides(alpha= "none")
     }
     if (input$plotCIs == TRUE) {
       if (input$CItype == 1) {
       levCat.plot <- levCat.plot +
         geom_ribbon(data = BSCIinput(),
                     aes(ymin = amplitude-ci,
                         ymax = amplitude+ci,
                         fill = condition,
                         colour = condition),
                     linetype="dashed",
                     alpha = 0.3
                     )+
         guides(fill = "none")
       } else if (input$CItype == 2) {
         levCat.plot <- levCat.plot +
           geom_ribbon(data = WSCIinput(),
                       aes(ymin = amplitude-ci,
                           ymax = amplitude+ci,
                           fill = condition,
                           colour = condition),
                       linetype="dashed",
                       alpha = 0.3) +
           guides(fill = "none")
       } else if (input$CItype == 3) {
         levCat.plot <- levCat.plot +
           geom_ribbon(data = BSCIinput(),
                       aes(ymin = amplitude-ci,
                           ymax = amplitude+ci,
                           colour = condition),
                       fill = NA,
                       linetype="dotted",
                       alpha = 0.3)+
           geom_ribbon(data = WSCIinput(),
                       aes(ymin = amplitude-ci,
                           ymax = amplitude+ci,
                           fill = condition,
                           colour = condition),
                       linetype="dashed",
                       alpha = 0.3) +
           guides(fill = "none")
       }
     }
     
     if (input$plotGroup == TRUE) {
       levCat.plot <- levCat.plot + 
         stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = condition))
     }
     
     if (input$facetSel == TRUE) {
      levCat.plot <- levCat.plot +
         facet_wrap(~condition)
     }
     
     levCat.plot +
       labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
       geom_vline(xintercept = 0,linetype = "dashed" )+
       geom_hline(yintercept = 0,linetype = "dashed")
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

