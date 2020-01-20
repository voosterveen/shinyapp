# load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(plyr)
scrubbed <- read_csv("scrubbed.csv")
scrubbed <- na.omit(scrubbed) # omit entries with NA's
scrubbed <- scrubbed %>% filter(`duration (seconds)` <= 40000) # not select extremely long observations, to make histograms more readable
scrubbed$country <- revalue(scrubbed$country, c("au"="Australia", "gb"="Great Britain", "us"="United States", "ca"="Canada")) # rename the countries

# Define UI 
ui <- fluidPage(
    # title
    titlePanel("UFO Sightings: Reports of unidentified flying object reports in the last century in four different countries"),
    # Sidebar with a input choice for country
    sidebarLayout(
        sidebarPanel(
            selectInput("countryinput",
                        "Country",
                        choices = c("Australia",
                                    "Canada",
                                    "Great Britain",
                                    "United States")),
            
        ),
        # Show the histogram and piechart
        mainPanel(
           plotOutput("hist"),
           br(), br(),
           plotOutput("piechart")
        )
    )
)

# Define server 
server <- function(input, output) {
    # first define histogram
    output$hist <- renderPlot({
        filtered <- scrubbed %>%
            filter(country == input$countryinput)
        ggplot(filtered, aes(`duration (seconds)`)) + geom_histogram(fill="blue4", bins=200) +
            ggtitle("Duration of reported observations")
    })
    # and define the piechart
    output$piechart <- renderPlot({
        filtered2 <- scrubbed %>%
            filter(country == input$countryinput)
        ggplot(filtered2, aes(x=factor(1), fill= shape)) +
        geom_bar(width=1) + coord_polar("y") +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.grid=element_blank(), axis.ticks = element_blank()) +
            ggtitle("Reported shapes of the observations")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
