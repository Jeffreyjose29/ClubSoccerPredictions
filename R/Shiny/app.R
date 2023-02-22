# Package names
packages <- c("tidyverse", "ggplot2", "shiny", "shinythemes", "DT", "bslib", "dplyr", "png", "shinyWidgets", "gt", "gtExtras", "ggbump", "ggnewscale")

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

ui <- fluidPage(tags$link(href = "https://fonts.googleapis.com/css?family=Lato", rel = "stylesheet"),
                tags$style(HTML('
      * {
        font-family: Lato;
      })')), theme = bs_theme(version = 4, bootswatch = "minty"),
                
                navbarPage(
                  "Football Stats",
                  tabPanel("League Standings",
                           sidebarLayout(
                             sidebarPanel(
                               width = 2,
                               uiOutput("leagueLogo", align = "center"), # NEWLY ADDED THIS LINE
                               tags$h4("Data Slicers:"),
                               selectizeInput('LeaguePageTable', 'Select European League', choices = c("Select" = "", levels(leagueTable$League))),
                               selectizeInput('ClubPageTable', 'Select European Club', choices = c("Select" = "", levels(leagueTable$Team)))
                             ),
                             mainPanel(
                               width = 10,
                               h1(strong(paste("Current Standing As Of", Sys.Date())), align = "center"),
                               strong(dataTableOutput("table"))
                             ))),
                  tabPanel("Fixtures & Results", "This panel is currently empty"),
                  tabPanel("Football Power Index", "This panel is currently empty"),
                  tabPanel("Season Prediction", "This panel is currently empty")
                )
)



# 2.0 SERVER --------
server <- function(input, output, session) {
  LeagueTableReactive <- reactive({
    leagueTable %>%
      filter(League == input$LeaguePageTable) %>%
      select(Position, Team, Matches, Wins, Draws, Loss, `Scored And Conceded`, `Goal Difference`, Points, `Coding Colour`) %>%
      arrange(Position)
  })
  
  output$table <- renderDataTable({
    datatable(LeagueTableReactive()) %>%
      formatStyle('Coding Colour', target = 'row', color = styleEqual(c('#2AD572', '#0046A7', '#FF4646', '#02CCF0', '#FFD908', '#7FAEFF', '#FFA72F'), 
                                                                       c('#4284f5', '#35a852', '#eb4334', '#02CCF0', '#fb7b16', '#7FAEFF', '#fabc05'))) 
  }
  )
  
  output$leagueLogo <- renderUI({
    
    if(input$LeaguePageTable == "Premier League"){
      img(height = 240, width = 350, src = "https://download.logo.wine/logo/Premier_League/Premier_League-Logo.wine.png")
    }else if(input$LeaguePageTable == "La Liga"){
      img(height = 240, width = 240, src = "https://a2.espncdn.com/combiner/i?img=%2Fi%2Fleaguelogos%2Fsoccer%2F500%2F15.png")
    }else if(input$LeaguePageTable == "1. Bundesliga"){
      img(height = 240, width = 240, src = "https://www.fifplay.com/img/public/bundesliga-logo.png")
    }else if(input$LeaguePageTable == "Ligue 1"){
      img(height = 240, width = 210, src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5e/Ligue1.svg/1200px-Ligue1.svg.png")
    }else if(input$LeaguePageTable == "Eredivisie"){
      img(height = 200, width = 300, src = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Eredivisie_nieuw_logo_2017-.svg/1280px-Eredivisie_nieuw_logo_2017-.svg.png")
    }
    
  })
  
}

# 3.0 FUSE --------
shinyApp(ui, server)



# NOTES ----------
#2AD572 - Champions League
#0046A7 - Europa League
#FF4646 - Relegation
#02CCF0 - Conference League
#FFD908 - Champions League Qualifiers
#7FAEFF - Conference League
#FFA72F - Relegation Play-Offs