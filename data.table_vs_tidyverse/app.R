#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
           sliderInput("select", min=100 , max=10000000, value=10000, label="select input"),
           # helpText("NULL"),
           # plotOutput("distPlot0"),
           helpText("data.table"),
           plotOutput("distPlot"),
           helpText("tidyverse"),
           plotOutput("distPlot2")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot0 <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     # x    <- faithful[, 2]
    #     
    #     # bins <- rnorm(10 ^ (input$bins), 100, 5)
    #     
    #     # draw the histogram with the specified number of bins
    #     # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #     # df_g = df1 %>% 
    #     #     sample_n(input$select) %>% 
    #     #     group_by(group) %>% 
    #     #     summarise(nrows = n(), 
    #     #               asum = sum(col1)
    #     #               ) %>%
    #     #     ungroup()
    #     
    #     # df_g = df2[sample(.N, input$select), .(asum=sum(col1), nrows=.N), by='group']
    #     # 
    #     df_g = data.frame(group = 'A', asum = 100)
    #     ggplot(df_g, aes(x = group, y = asum)) + 
    #         geom_bar(stat = 'identity') + 
    #         labs(title = str_interp('time: ${timenow}', list(timenow=Sys.time())))
    #     
    # })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        
        # bins <- rnorm(10 ^ (input$bins), 100, 5)

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        # df_g = df1 %>% 
        #     sample_n(input$select) %>% 
        #     group_by(group) %>% 
        #     summarise(nrows = n(), 
        #               asum = sum(col1)
        #               ) %>%
        #     ungroup()
        
        timestart = Sys.time()
        df_g = df2[sample(.N, input$select), .(asum=sum(col1), nrows=.N), by='group']
        
        timeend = Sys.time()
        timecost = difftime(timeend, timestart, units = 'secs')
        ggplot(df_g, aes(x = group, y = asum)) + 
            geom_bar(stat = 'identity') + 
            labs(title = str_interp(
              'timestart: ${timestart} data processing end: ${timeend}, time cost of data processing: ${timecost},
               timecost total ${timetotal}', 
            list(timestart = timestart, timeend = timeend, 
                 timecost = timecost, timetotal = difftime(Sys.time(), timestart, units='secs')
                                    ))
                 )

    })
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        
        # bins <- rnorm(10 ^ (input$bins), 100, 5)
        
        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
        timestart = Sys.time()
        df_g = df1 %>%
            sample_n(input$select) %>%
            group_by(group) %>%
            summarise(nrows = n(),
                      asum = sum(col1)
                      ) %>%
            ungroup()
        # df_g = df2[sample(.N, input$select), .(asum=sum(col1), nrows=.N), by='group']
        timeend = Sys.time()
        timecost = difftime(timeend, timestart, units = 'secs')
        ggplot(df_g, aes(x = group, y = asum)) + 
            geom_bar(stat = 'identity') + 
            labs(title = str_interp(
            'timestart: ${timestart} data processing end: ${timeend}, time cost of data processing: ${timecost},
               timecost total ${timetotal}', 
            list(timestart = timestart, timeend = timeend, 
                 timecost = timecost, timetotal = difftime(Sys.time(), timestart, units='secs')
            ))
          )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
