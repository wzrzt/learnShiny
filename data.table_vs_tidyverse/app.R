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
library(data.table)
# Define UI for application that draws a histogram
ui <- fluidPage(
           sliderInput("select", min=10^4 , max=10^8, value=10^4, label="select input"),
           # helpText("NULL"),
           # plotOutput("distPlot0"),
           h2("data.table"),
           plotOutput("distPlot"),
           h2("tidyverse"),
           plotOutput("distPlot2")
)


# n=10000000
# #n = as.integer(args[1])
# df1 = data.frame(
#   col1=runif(n),
#   c1  = runif(n),
#   c2 = runif(n),
#   group=c(rep("A", n/2), rep("B", n/2))
# )
# 
# df2 = as.data.table(df1)



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
        
        df2 = data.table(
          col1  = runif(input$select),
          c1    = runif(input$select),
          c2    = runif(input$select),
          group = rep(c("A", "B"), each = 2, length.out = input$select)
        )
        
        time_data_generated = Sys.time() 
        
        df_g = df2[, .(asum=sum(col1), nrows=.N), by='group']
        
        timeend = Sys.time()
        timecost = difftime(timeend, timestart, units = 'secs')
        ggplot(df_g, aes(x = group, y = asum)) + 
            geom_bar(stat = 'identity') + 
          labs(title = str_interp(
            'timestart: ${timestart} data processing end: ${timeend}, 
               time cost of data processing: ${timecost}, 
               time cost of data generating: ${timegenerate},
               time cost of data grouping: ${timegrouped},
               timecost total ${timetotal}', 
            list(timestart = timestart, timeend = timeend, 
                 timecost = timecost, 
                 timegrouped = difftime(timeend, time_data_generated, units = 'secs'),
                 timegenerate = difftime(time_data_generated, timestart, units = 'secs'), 
                 timetotal = difftime(Sys.time(), timestart, units='secs')
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
        
        df1 = data.frame(
            col1  = runif(input$select),
            c1    = runif(input$select),
            c2    = runif(input$select),
            group = rep(c("A", "B"), each = 2, length.out = input$select) # c(rep("A", input$select/2), rep("B", input$select/2))
        )
        
        time_data_generated = Sys.time() 
        
        df_g = df1 %>%
            # sample_n(input$select) %>%
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
            'timestart: ${timestart} data processing end: ${timeend}, 
               time cost of data processing: ${timecost}, 
               time cost of data generating: ${timegenerate},
               time cost of data grouping: ${timegrouped},
               timecost total ${timetotal}', 
            list(timestart = timestart, timeend = timeend, 
                 timecost = timecost, 
                 timegrouped = difftime(timeend, time_data_generated, units = 'secs'),
                 timegenerate = difftime(time_data_generated, timestart, units = 'secs'), 
                 timetotal = difftime(Sys.time(), timestart, units='secs')
            ))
          )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
