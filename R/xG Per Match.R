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

url <- "https://understat.com/match/18402"
match_data <- understat_match_shots(match_url = url)
match_data <- match_data %>%
  select(minute, home_away, home_team, away_team, xG) %>%
  mutate(team = if_else(home_away == "h", home_team, away_team)) %>%
  select(minute, home_away, team, xG)

maxMinutes <- max(match_data$minute)

minute_by_minute <- data.frame(minute = seq(1:90))
minute_by_minute <- minute_by_minute %>% add_row(minute = 0)

home <- match_data %>%
  filter(home_away == "h")
away <- match_data %>%
  filter(home_away == "a")

home <- merge(minute_by_minute, home, by = "minute", all.x = TRUE, all.y = TRUE)

home$test <- NA
for(i in 1:nrow(home)){
  if(home[i, 1] == 0){
    home[i, 5] <- 0.00
    prevXg <- home[i, 5]
    cumxG <- home[i, 5]
  }else{
    if(is.na(home[i, 4]) == TRUE){
      home[i, 5] <- cumxG
    }else{
      home[i, 5] <- home[i, 4] + cumxG
      cumxG <- home[i, 4] + cumxG
    }
  }
}

home <- home %>%
  select(minute, team, test) %>%
  mutate(team = "Arsenal F.C") %>%
  rename("xG" = test)


###########################################################################################

away <- merge(minute_by_minute, away, by = "minute", all.x = TRUE, all.y = TRUE)

away$test <- NA
for(i in 1:nrow(away)){
  if(away[i, 1] == 0){
    away[i, 5] <- 0.00
    prevXg <- away[i, 5]
    cumxG <- away[i, 5]
  }else{
    if(is.na(away[i, 4]) == TRUE){
      away[i, 5] <- cumxG
    }else{
      away[i, 5] <- away[i, 4] + cumxG
      cumxG <- away[i, 4] + cumxG
    }
  }
}

away <- away %>%
  select(minute, team, test) %>%
  mutate(team = "Manchester United F.C") %>%
  rename("xG" = test)


matches_data <- rbind(home, away)


matches_data %>%
  ggplot(aes(x = minute, y = xG, group = team, colour = team)) + geom_line(size = 3) + 
  labs(x = "", y = "Expected Goals") + theme_minimal() + theme(legend.position = "none") +
  scale_x_continuous(breaks= seq(0, 90, by = 5)) +
  theme(axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(face = "bold", size = 12)) +
  scale_colour_manual(values = c("#db0007", "#eab600"))

test <- understat_league_match_results("EPL", 2022)
test <- test %>%
  filter(match_id == 18402)

# Expected Points: xPts = 3 * p(win) + 1 * p(draw) + 0 * p(loss)

home_xpt <- (3 * test$forecast_win) + (1 * test$forecast_draw) + (0 * test$forecast_loss)
away_xpt <- (3 * (1 - test$forecast_win)) + (1 * (1 - test$forecast_draw)) + (0 * (1 - test$forecast_loss))


data <- understat_match_shots(match_url = url)

data %>%
  group_by(home_away) %>%
  summarise(meanxG = mean(xG, na.rm = TRUE))

match_shots <- understat_match_shots(match_url = url)