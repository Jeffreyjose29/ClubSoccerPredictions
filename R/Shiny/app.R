# Package names
packages <- c("tidyverse", "ggplot2", "shiny", "shinythemes", "DT", "bslib", "dplyr", "png", "shinyWidgets", "gt", "gtExtras", "ggbump", "ggnewscale",
              "shinydashboard")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
library(worldfootballR)


# LOADING IN DATA -----------
leagueTable <- fotmob_get_league_tables(country = c("ENG", "ESP", "GER", "FRA", "NED"),
                                        league_name = c("Premier League", "LaLiga", "1. Bundesliga", "Ligue 1", "Eredivisie")) %>%
  filter(table_type == "all")
leagueTable$League <- if_else(grepl("premier-league", leagueTable$page_url, fixed = TRUE), "Premier League",
                              if_else(grepl("laliga", leagueTable$page_url, fixed = TRUE), "La Liga",
                                      if_else(grepl("1.-bundesliga", leagueTable$page_url, fixed = TRUE), "1. Bundesliga", 
                                              if_else(grepl("ligue-1", leagueTable$page_url, fixed = TRUE), "Ligue 1", "Eredivisie"))))
leagueTable <- leagueTable %>%
  select(league_id, League, table_idx, table_name, table_short_name, table_id, table_played, table_wins, table_draws, table_losses,
         table_scores_str, table_goal_con_diff, table_pts, table_qual_color) %>%
  rename("League ID" = league_id, `Position` = table_idx, "Team" = table_name, "Team Abbr" = table_short_name, "Team ID" = table_id,
         "Matches" = table_played, "Wins" = table_wins, "Draws" = table_draws, "Loss" = table_losses, "Scored And Conceded" = table_scores_str,
         "Goal Difference" = table_goal_con_diff, "Points" = table_pts, "Coding Colour" = table_qual_color)

leagueTable$League <- as.factor(leagueTable$League)
leagueTable$Team <- as.factor(leagueTable$Team)

# 1.0 USER INTERFACE --------

ui <- dashboardPage(
  skin = "red", 
  dashboardHeader(title = "Football In-Depth", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("1. League Standings", tabName = "leagueStandings", icon = icon("tools")),
      menuItem("2. Fixtures & Results", tabName = "fixtureResults", icon = icon("user")),
      menuItem("3. Transfers", tabName = "transfers", icon = icon("user")),
      menuItem("4. Football Power Index", tabName = "fpi", icon = icon("user")),
      menuItem("5. Season Predictor", tabName = "seasonPredictor", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("leagueStandings",
              selectInput("league", "Select A League:", leagueTable$League),
              selectInput("club", "Select A Club:", leagueTable$Team)
              ),
      tabItem("fixtureResults",
              selectInput("species", "Select a species", iris$Species),
              dataTableOutput("irisspecies"),
              downloadButton(outputId = "download", label = "Download .PDF"))
    )
  )
)

# 2.0 SERVER --------

server <- function(input, output) {
}


# 3.0 FUSE --------
shinyApp(ui, server)
