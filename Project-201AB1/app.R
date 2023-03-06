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
library(ggplot2)


dataset <- read_delim("nces330_20.csv") %>%
    filter(Expense == "Fees/Tuition")
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tutition By Year data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year Range",
                        min = 2013,
                        max = 2022,
                        value = c(2013, 2022)),
            checkboxGroupInput("variable", "Type of values to show",
                               c("General" = "General",
                                 "Private" = "Private",
                                 "Public In-State" = "Public In-State",
                                 "Public Out-of-State" = "Public Out-of-State"),
                               selected = c("General", "Private",
                                            "Public In-State",
                                            "Public Out-of-State"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        dataset2 <- dataset %>%
            filter(Year >= input$year[1], Year <= input$year[2])
        datamean <- dataset2 %>%
            group_by(Year) %>%
            summarise(mean = mean(Value)) %>%
            mutate(Type = "General")
        datasep <- dataset2 %>%
            group_by(Year, Type) %>%
            summarise(mean = mean(Value))
        dataUsing = rbind(datamean, datasep) %>%
            filter(Type %in% input$variable)
        print(input$variable)
        
        # dataUsing <- data.frame(Year = unique(datasep$Year),
        #                         General = datamean$mean,
        #                         Private = datasep[datasep$Type == "Private", ]$mean,
        #                         PublicOS = 
        #                             datasep[datasep$Type == "Public Out-of-State", ]$mean,
        #                         PublicIS = datasep[datasep$Type == "Public In-State", ]$mean)
        
        # draw the histogram with the specified number of bins
        ggplot(dataUsing, aes(x = Year, y = mean, group = Type)) +
            geom_line(aes(color = Type)) +
            geom_point(aes(color = Type)) + 
            ggtitle("Tuition over year") +
            xlab("Year") +
            ylab("Tuition") +
            scale_y_continuous(limits = c(0, max(dataUsing$mean))) +
            scale_x_continuous(breaks = seq(min(dataUsing$Year),
                                            max(dataUsing$Year), by = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
