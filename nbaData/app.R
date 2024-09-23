library(shiny)
library(ggplot2)
library(dplyr)

load("nba_data.RData")

df$Year <- as.numeric(as.character(df$Year))

ui <- fluidPage(
  h1("NBA"),
  tabsetPanel(
    
    tabPanel("Candidates",
             wellPanel(
               p(strong("Candidates for the MVP")),
               numericInput(inputId = "niCand",label = "Introduce the year",value = 1980,
                            min = 1980, max = 2021),
               actionButton(
                 inputId = "buttonCand",
                 label = "Show!"
               )
             ),
             plotOutput(outputId = "plotCand")
    ),
    tabPanel("Colleges",
             wellPanel(
               p(strong("Colleges MVP point")),
               sliderInput(inputId = "siYear", label = "Select the interval of years: ",
                           min = 1980, max = 2021, value = c(1980, 2021)),
               actionButton(
                 inputId = "buttonCollege",
                 label = "Show!"
               )
             ),
             plotOutput(outputId = "plotCollege")
    ),
    
    tabPanel("Team MVPs",
             wellPanel(
               p(strong("Teams MVP point")),
               sliderInput(inputId = "siYearT", label = "Select the interval of years: ",
                           min = 1980, max = 2021, value = c(1980, 2021)),
               actionButton(
                 inputId = "buttonTeam",
                 label = "Show!"
               )
             ),
             plotOutput(outputId = "plotTeam")
    ),
    
    tabPanel("Jordan vs LeBron",
             wellPanel(
               p(strong("Michael Jordan vs LeBron James statistics comparison")),
               selectInput(inputId = "selectStat", label = "Select Statistic", 
                           choices = c("PTS", "AST", "TRB", "TOV", "Share"), selected = "PTS"),
               actionButton(
                 inputId = "buttonPlayers",
                 label = "End debate!"
               )
             ),
             plotOutput(outputId = "plotPlayers")
    )
    
  )
  
  
)


server <- function(input, output) {
  observeEvent(input$buttonCand, {
    output$plotCand <- renderPlot(
      ggplot(df[df$Year == isolate({input$niCand}),], aes(x = Player, y = Share)) +
        geom_col(color="black", fill="red") +
        labs(title=paste("Share of MVP candidates in",isolate({input$niCand})), x="Player", y = "Share")
    )
  })
  observeEvent(input$buttonCollege, {
    df_mvp$Year <- as.numeric(as.character(df_mvp$Year))
    output$plotCollege <- renderPlot(
      ggplot(df_mvp[df_mvp$Year>=isolate({input$siYear[1]}) &
                      df_mvp$Year<=isolate({input$siYear[2]}), ], aes(Colleges))
      +geom_bar(fill = "red", color = "blue")
      + labs(title = "Colleges",x = "College", y = "Player")
    )
  })
  
  observeEvent(input$buttonTeam, {
    df_mvp$Year <- as.numeric(as.character(df_mvp$Year))
    output$plotTeam <- renderPlot(
      ggplot(df_mvp[df_mvp$Year>=isolate({input$siYearT[1]}) &
                      df_mvp$Year<=isolate({input$siYearT[2]}), ], aes(Team))
      +geom_bar(fill = "blue", color = "red")
      + labs(title = "Teams",x = "Team", y = "Player")
    )
  })
  
  observeEvent(input$buttonPlayers, {
    players_data <- df[df$Player %in% c("Michael Jordan", "LeBron James"), ]
    
    output$plotPlayers <- renderPlot({
      players_data %>%
        group_by(Player) %>%
        summarise(avg_stat = mean(get(input$selectStat))) %>%
        ggplot(aes(x = Player, y = avg_stat)) +
        geom_col(color = "yellow", fill = "orange") +
        labs(title = paste("Average", input$selectStat, "for Michael Jordan and LeBron James"),
             x = "Player", y = paste("Average ", input$selectStat))
    })
  })
  
  
  output$tableDf <- renderTable({
    datosOut <- df
    return(datosOut)
  })
  
  
  
}


shinyApp(ui = ui, server = server)