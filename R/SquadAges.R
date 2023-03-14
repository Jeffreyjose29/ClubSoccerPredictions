# Package names
packages <- c("worldfootballR", "dplyr", "ggplot2", "lubridate", "tidyverse", "polite", "scales", "ggimage", "rvest", 
              "glue", "extrafont", "showtext", "ggrepel", "magick", "ggforce", "ggtext", "lubridate", 
              "cowplot", "patchwork", "rlang")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

team_urls <- tm_league_team_urls(country_name = "England", start_year = 2022)
arsenal_players_2022 <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-arsenal/startseite/verein/11/saison_id/2022")
arsenal_squad_stats <- tm_squad_stats(team_url = "https://www.transfermarkt.com/fc-arsenal/startseite/verein/11/saison_id/2022")

bio <- tm_player_bio(player_urls = "https://www.transfermarkt.com/aaron-ramsdale/profil/spieler/427568")
bio <- bio %>%
  select(player_name, date_of_birth, citizenship, current_club, joined, player_valuation, max_player_valuation, max_player_valuation_date)

for(i in 1:nrow(arsenal_squad_stats)){
  dummy <- tm_player_bio(player_urls = arsenal_squad_stats[i, 5]) %>%
    select(player_name, date_of_birth, citizenship, current_club, joined, player_valuation, max_player_valuation, max_player_valuation_date)
  bio <- rbind(bio, dummy)  
}

bio <- merge(bio, arsenal_squad_stats, by = "player_name")
bio <- bio %>%
  select(player_name, date_of_birth, citizenship, current_club, joined, player_valuation, max_player_valuation, max_player_valuation_date,
         minutes_played) %>%
  filter(current_club == "Arsenal FC") %>%
  distinct() %>%
  mutate(refresh_date = Sys.Date())
  
bio$refresh_date <- as.Date(bio$refresh_date, format = "%Y-%m-%d")
bio$date_of_birth <- as.Date(bio$date_of_birth, format = "%Y-%m-%d")
bio$joined <- as.Date(bio$joined, format = "%Y-%m-%d")


bio$age <- time_length(difftime(bio$refresh_date, bio$date_of_birth), "years")
bio$joined_to_now <- time_length(difftime(bio$refresh_date, bio$joined), "years")
bio$player <- paste0(substr(bio$player_name, 1, 1), ". ", sub("^\\S* ", "", bio$player_name))


num.players <- n_distinct(bio$player_name)
median.age <- median(bio$age)
avg.los <- round(mean(bio$joined_to_now), 2)


bio %>%
  ggplot(aes(x = age, y = minutes_played)) + geom_point(size = 3, shape = 16, colour = "#f00107") +
  geom_rect(aes(xmin = 23.17, xmax = 27.88, ymin = 0, ymax = 3100), fill = "#023473", alpha = 0.01) +
  geom_text(hjust=0.5, vjust=-0.8, label = bio$player) +
  labs(x = paste0("Current Age (As of ", max(bio$refresh_date), ")"),
       y = "Minutes Played (%)") + theme_minimal() 

