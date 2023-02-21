# Package names
packages <- c("tidyverse", "ggplot2", "shiny", "shinythemes", "DT")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
library(worldfootballR)


# 1.0 USER INTERFACE --------

ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                navbarPage(
                  "Football Stats",
                  tabPanel("League Standings",
                           sidebarPanel(
                             tags$h4("Data Slicers:"),
                             uiOutput("toCol")
                           ),
                           mainPanel(
                             h1(paste("Current Standing As Of", Sys.Date())),
                             dataTableOutput("leagueTable")
                           )),
                  tabPanel("Fixtures & Results", "This panel is currently empty"),
                  tabPanel("Football Power Index", "This panel is currently empty"),
                  tabPanel("Season Prediction", "This panel is currently empty")
                )
                )



# 2.0 SERVER --------
server <- function(input, output, session) {
  
  # READ THE LEAGUE STANDING TABLE FROM WORLDFOOTBALLR PACKAGE
  leagueTable <- fotmob_get_league_tables(country = c("ENG", "ESP", "GER", "FRA", "NED"),
                                          league_name = c("Premier League", "LaLiga", "1. Bundesliga", "Ligue 1", "Eredivisie")) %>%
    filter(table_type == "all")
  
  leagueTable$League <- if_else(grepl("premier-league", leagueTable$page_url, fixed = TRUE), "Premier League",
                                if_else(grepl("laliga", leagueTable$page_url, fixed = TRUE), "La Liga",
                                        if_else(grepl("1.-bundesliga", leagueTable$page_url, fixed = TRUE), "1. Bundesliga", 
                                                if_else(grepl("ligue-1", leagueTable$page_url, fixed = TRUE), "Ligue 1", "Eredivisie"))))
  
  
  leagueTableUpdated <- leagueTable %>%
    select(league_id, League, table_idx, table_name, table_short_name, table_id, table_played, table_wins, table_draws, table_losses,
           table_scores_str, table_goal_con_diff, table_pts, table_qual_color) %>%
    rename("League ID" = league_id, `Position` = table_idx, "Team" = table_name, "Team Abbr" = table_short_name, "Team ID" = table_id,
           "Matches" = table_played, "Wins" = table_wins, "Draws" = table_draws, "Loss" = table_losses, "Scored And Conceded" = table_scores_str,
           "Goal Difference" = table_goal_con_diff, "Points" = table_pts, "Coding Colour" = table_qual_color)
  
  output$toCol <- renderUI({
    league <- as.character(leagueTableUpdated[[2]])
    selectInput("league-breakdown", "League:", league)
  })
  
  output$leagueTable <- renderDataTable(
    leagueTableUpdatedShowCase <- leagueTableUpdated %>%
      select(Position, League, Team, Matches, Wins, Draws, Loss, `Scored And Conceded`, `Goal Difference`, Points) %>%
      arrange(League, Position)
  )
  
}

# 3.0 FUSE --------
shinyApp(ui, server)