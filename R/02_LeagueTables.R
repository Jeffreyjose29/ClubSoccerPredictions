# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch", "hrbrthemes", "grid", "worldFootballR")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

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

?fotmob_get_league_tables


test <- fotmob_get_league_ids()
