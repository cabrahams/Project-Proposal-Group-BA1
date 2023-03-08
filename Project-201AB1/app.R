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

topprivate <- dataset %>% 
    group_by(State) %>% 
    filter(Type == "Private") %>% 
    summarise(avgcost = mean(Value)) %>% 
    arrange(desc(avgcost))

toppublicinstate <- dataset %>% 
    group_by(State) %>% 
    filter(Type == "Public In-State") %>% 
    summarise(avgcost = mean(Value)) %>% 
    arrange(desc(avgcost))

toppublicoutofstate <- dataset %>% 
    group_by(State) %>% 
    filter(Type == "Public Out-of-State") %>% 
    summarise(avgcost = mean(Value)) %>% 
    arrange(desc(avgcost))
    
# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabPanel("General Info",
                 br(),
                 h1("Dataset Information"),
                 p("The dataset being used is called",
                   strong("Average Cost of Undergraduate College by State."), 
                   "Collected by the", em("National Center of Education Statistics"), "Annual 
               Digest. Found on Kaggle - @kenmoretoast, updated a month ago. The 
               dataset focuses on average undergraduate tuition and fees and room 
               and board rates charged for full-time students in degree-granting 
               postsecondary institutions, by control and level of institution 
               and state or jurisdiction."), 
                 p("The dataset has ", nrow(dataset), "rows, and ", ncol(dataset), "columns."),
                 img(alt = "Tuition Graphic", 
                     src = "https://th.bing.com/th/id/R.f44fde31f669039af688c9de699da2da?rik=dCncQPN0qfHJrg&riu=http%3a%2f%2f614now.com%2fwp-content%2fuploads%2f2017%2f05%2ftuition-cash.jpg&ehk=iqnn7hEBQM9bbxFDF0uGiSBxhBPXvWd4bPi9CYwHCiQ%3d&risl=&pid=ImgRaw&r=0"),
                 h3("Target Audience"), 
                 p("Our target audience is prospective college students. 
                Prospective college students would be interested in 
                visualizing the cost of colleges based on their location (state), 
                so transforming this data about college costs into a friendly 
                and presentable way would be helpful for them."), 
                 h3("Research questions being answered: "), 
                 HTML("<ul>
                  <li>What state, on average, has the cheapest college tuition for 4-years, public universities?</li>
                  <li>Do private universities cost more than public universities?</li>
                  <li>What state, on average, has the most expensive college tuition for 4-years, public universities?</li>
                  <li>For both private and public universities, has the cost of college increased over time on average?</li>
                </ul>"),
                 p("The dataset we are using can be accessed", a("here.", 
                                                                 href = "https://www.kaggle.com/datasets/kfoster150/avg-cost-of-undergrad-college-by-state")), 
        ),
        
        tabPanel("Conclusion", 
                 h1("Overall Takeaways"), 
                 h3("Research questions we were answering: "), 
                 HTML("<ul>
                  <li>What state, on average, has the cheapest college tuition for 4-years, public universities, and 
                  what state has the most expensive?</li>
                  <li>Do private universities cost more than public universities?</li>
                  <li>For both private and public universities, has the cost of college increased over time on average?</li>
                </ul>"),
                 p("1. On average,", strong("Massachussetts and Vermont"), "are 2 of the most expensive states overall for college tuition.", strong("Wyoming, 
               and Idaho"), "are consistently among the cheapest states for college tuition."), 
                 p("2. Yes, private universities", em("do"), "cost more than public univerisites consistently from 2013 to 2021."),
                 p("3. Yes, college tuition in the US", em("has"), "gotten more expensive from 2013 to 2021, with the most expensive
               state (DC) gaining almost $8000 in total value during that time period."),
                 plotOutput("conclusion"), 
                 p("Comparing both DC's and Wyoming's tuition in 2013 and 2021, we can see a clear increase in overall tuition costs. 
               DC, being one of the most expensive states for tuition, has a much more pronouced increase, while Wyoming, one of the
               cheapest states, has a much smaller increase, but the increase is still clear. This data shows how expensive going to college
               in the United States is, and for our audience of prospective college students, seeing which states cost what can make or break 
               decisions about what school they end up at."),
                 p("Our dataset was very thorough, but was not overly complicated. There were only 5 columns/variables affecting the data, and 
               the main bulk of the rows came from the variation over time for each state for each type of school. Because the data was all numbers, 
               it was very unbiased, as it was just representing how the different expenses have changed over time. We feel as though the data will 
               do more good than harming certain groups, as it helps figure out what states best fit certain budgets or price ranges."),
                 p("To advance the project further, it would be interesting to compare this data with acceptance data with the same categories - see
               what states' schools or what type of schools overall are easier to get into, to again help prospective college students figure out
               what school in what state would be the best fit for them.")
        ),
        
        tabPanel("Average Tuition by type",
            titlePanel("Average Tuition by Type of College for Each State"),
             sidebarPanel(
                 fluidRow(
                     column(10,
                            radioButtons(inputId = "plot_type" ,
                                         label = "Select the type of college",
                                         choices = c("Public In-State", "Public Out-of-State", "Private"))
                     ),
                 )
             ),
             mainPanel(plotOutput("myplot"))
        ),
        tabPanel("5 Most Expensive 4-Year Colleges",
            sidebarLayout(
                sidebarPanel(fluidRow(uiOutput("checkboxtype"))),
                mainPanel(
                    tableOutput("table"),
                    textOutput("expensive")
                )
            )
        ),
        
        tabPanel("5 Cheapest 4-Year Colleges",
            sidebarLayout(
                sidebarPanel(fluidRow(uiOutput("checkboxcheap"))),
                mainPanel(
                    tableOutput("tablecheap"),
                    textOutput("cheap")
                )
            )
        ),

        tabPanel("Tutition by year",
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
            )
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$conclusion <- renderPlot({
        dc_wy_df <- subset(dataset, State %in% c("Vermont", "Wyoming") & Year %in% c("2013", "2021"))
        
        ggplot(dc_wy_df, aes(x = Year, y = Value, fill = State)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "College Tuition in DC and Wyoming in 2013 vs 2021",
                 x = "Year", y = "College Tuition", fill = "State") +
            scale_fill_manual(values = c("steelblue", "orange"))
    })
    
    output$myplot <- renderPlot({
        if (input$plot_type == "Public In-State") {
            ggplot(toppublicinstate, aes(reorder(State, avgcost))) +
                geom_bar(aes(weight = avgcost), fill = "blue", width = 0.75) +
                coord_flip() +
                ggtitle("The average cost of going to an in-state public college in each state") +
                xlab("State") +
                ylab("Average tuition per year in USD") +
                theme_bw(base_size = 10)
        } else if (input$plot_type == "Public Out-of-State") {
            ggplot(toppublicoutofstate, aes(reorder(State, avgcost))) +
                geom_bar(aes(weight = avgcost), fill = "red", width = 0.75) +
                coord_flip() +
                ggtitle("The average cost of going to an out-of-state public college in each state") +
                xlab("State") +
                ylab("Average tuition per year in USD") +
                theme_bw(base_size = 10)
        }
        else if (input$plot_type == "Private") {
            ggplot(topprivate, aes(reorder(State, avgcost))) +
                geom_bar(aes(weight = avgcost), fill = "orange", width = 0.75) +
                coord_flip() +
                ggtitle("The average cost of going to private college in each state") +
                xlab("State") +
                ylab("Average tuition per year in USD") +
                theme_bw(base_size = 10)
        }
    })

    output$table <- renderTable({
        dataset %>% 
            filter(Type %in% input$Type) %>% 
            arrange(desc(Value)) %>% 
            head(5)
    })
    
    output$tablecheap <- renderTable({
        dataset %>% 
            filter(Length == "4-year") %>% 
            filter(Type %in% input$TypeCheap) %>% 
            arrange(desc(Value)) %>% 
            tail(5)
    })
    
    output$checkboxtype <- renderUI({
        checkboxGroupInput("Type", "Choose Private or Public",
                           choices = unique(dataset$Type))
    })
    
    output$checkboxcheap <- renderUI({
        checkboxGroupInput("TypeCheap", "Choose Private or Public",
                           choices = unique(dataset$Type))
    })
    
    output$expensive <- renderText({
        paste("The table on top is a table of the 5 most expensive schools
            based on tuition in the Country while the bottom one is the top 5 cheapest, based 
              on Private, Public In-State, or Public Out-Of-State")
    })
    
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
