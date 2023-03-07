library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
happiness <- read_delim("world-happiness-report-2021.csv.xls")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("World happiness in 2021"),
  tabsetPanel(
    tabPanel("About",
             checkboxInput("trend2", "Add Trend Line"),
             plotlyOutput("lastP"),
             h2("Project Overview"),
             p("The world happiness report provides a brife summary of how does different ",
               "factors affect a country's happiness level in 2021. With the results, the report will ",
               "clearly show the relationship between happiness and logged GDP per capita ",
               ", the relationship between happiness and healthy life expectancy. ",
               "We hope the report could help reader have ",
               "a more comprehensive understanding towards world happiness ",
               "and factors that influencing world happiness. Enjoy the report!"),
             h2("Data Source"),
             p("We will be working with the world happiness report data set made by the ",
               "Sustainable Development Solutions Network published on kaggle."),
             h2("Questions we are working with?"),
             p("What's the relationship between happiness and logged GDP per capita?"),
             p("What's the relationship between happiness and healthy life expectancy?")
    ),
    
    tabPanel("Happiness and logged GDP Per Capita", 
             sidebarLayout(
               sidebarPanel(
                 # select the region
                 selectInput("region", "Select the region:", 
                             choices = unique(happiness$`Regional indicator`),
                             selected = "Western Europe"),
                 # add a trend line or not
                 checkboxInput("trend", "Add Trend Line"),
                 radioButtons("color", "choose color" , choices = c("red","blue","cyan","black"))
                 
               ),
               mainPanel(
                 # the scatter plot output
                 plotlyOutput("p2"),
                 verbatimTextOutput("text2"))
             )),
    
    tabPanel("Happiness and Healthy life expectancy",
             sidebarLayout(
               sidebarPanel(
                 # select the  region
                 selectInput("region2", "Select the region:", 
                             choices = unique(happiness$`Regional indicator`), 
                             selected = "Western Europe")
                 
               ),
               mainPanel(
                 tableOutput("table"),  # table output
                 verbatimTextOutput("text"),  # render dynamic text
                 h4("Region with the highest average happiness score"),
                 tableOutput("table1"),  # table output
                 h4("Region with the lowest average happiness score"),
                 tableOutput("table2")  # table output
             ))   
    ),
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$p2 <- renderPlotly({
    # draw a scatter plot
    p <- happiness %>%
      filter(`Regional indicator`==input$region) %>%
      ggplot(aes(x=`Logged GDP per capita`, y=`Ladder score`)) + geom_point(col=input$color)
    if(input$trend) {
      # add a trend line
      p <- p + geom_smooth(method="lm", se=F, col=input$color)
    }
    
    p
  })
  
  output$lastP <- renderPlotly({
    p3 <- happiness %>%
      group_by(`Regional indicator`) %>%
      ggplot(aes(x=`Logged GDP per capita`, y=`Ladder score`, col=`Regional indicator`)) +
      geom_point()
    if(input$trend2) {
      # add a trend line
      p3 <- p3 + geom_smooth(method="lm", se=F)
    }
    ggplotly(p3)  # convert to plotly object
  })
  
  output$table <- renderTable({
    # wide to long
    happiness %>%
      filter(`Regional indicator`==input$region2) %>%
      select(`Country name`, `Ladder score`, `Healthy life expectancy`) %>%
      gather(key=Metric, value=value, -`Country name`) %>%
      # calcualte the statistic 
      group_by(Metric) %>%
      summarise(mean=mean(value),
                sd=sd(value), min=min(value), max=max(value), median=median(value)) %>%
      mutate(Metric=ifelse(Metric=="Ladder score", "Ladder score(unit)", "Healthy life expectancy/year"))
  })
  
  output$table1 <- renderTable({
    region <- happiness %>%
      group_by(`Regional indicator`) %>%
      summarise(value=mean(`Ladder score`)) %>%
      arrange(desc(value)) %>%
      head(1) %>%
      pull(`Regional indicator`)
    happiness %>%
      filter(`Regional indicator`==region) %>%
      select(`Regional indicator`,`Country name`, `Ladder score`, `Healthy life expectancy`)
  })
  
  output$table2 <- renderTable({
    region <- happiness %>%
      group_by(`Regional indicator`) %>%
      summarise(value=mean(`Ladder score`)) %>%
      arrange(value) %>%
      head(1) %>%
      pull(`Regional indicator`)
    happiness %>%
      filter(`Regional indicator`==region) %>%
      select(`Regional indicator`, `Country name`, `Ladder score`, `Healthy life expectancy`)
  })
  
  output$text <- renderText({
    # filter data
    df <- happiness %>%
      filter(`Regional indicator`==input$region2) 
    # calculate the correlation coefficient
    paste0("The selected region is ", input$region2, 
           "\nThe correlation coefficient between happiness and Healthy life expectancy is ", 
           round(cor(df$`Ladder score`, df$`Healthy life expectancy`), 2))
  })
  
  output$text2 <- renderText({
    # filter data
    df <- happiness %>%
      filter(`Regional indicator`==input$region) 
    # calculate the correlation coefficient
    paste0("The selected region is ", input$region, 
           "\nThe correlation coefficient between ladder score and Logged GDP per capita is ", 
           round(cor(df$`Ladder score`, df$`Logged GDP per capita`), 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

## I spent more than 12 hours on this ps
