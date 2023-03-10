# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch", "hrbrthemes", "grid", "worldfootballR", "StatsBombR")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##################################### FUNCTIONS #######################################################################

fb_match_lineups <- function(match_url, time_pause=3) {
  # .pkg_message("Scraping lineups")
  main_url <- "https://fbref.com"
  time_wait <- time_pause
  get_each_match_lineup <- function(match_url, time_pause=time_wait) {
    pb$tick()
    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)
    match_page <- tryCatch(.load_page(match_url), error = function(e) NA)
    
    if(!is.na(match_page)) {
      match_date <- match_page %>% rvest::html_nodes(".venuetime") %>% rvest::html_attr("data-venue-date")
      
      lineups <- match_page %>% rvest::html_nodes(".lineup") %>% rvest::html_nodes("table")
      home <- 1
      away <- 2
      
      get_each_lineup <- function(home_away) {
        lineup <- lineups[home_away] %>% rvest::html_table() %>% data.frame()
        player_urls <- lineups[home_away] %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
        formation <- names(lineup)[1]
        is_diamond <- grepl("\\..$", formation)
        # on Windows, the diamond is coming through as utf-8, while on MacOS coming through as ".."
        if(grepl("u", formation, ignore.case = T)) {
          formation  <- formation %>% gsub("u\\..*", "", ., ignore.case = T) %>%stringr::str_extract_all(., "[[:digit:]]") %>% unlist() %>% paste(collapse = "-")
        } else {
          formation  <- formation %>% stringr::str_extract_all(., "[[:digit:]]") %>% unlist() %>% paste(collapse = "-")
        }
        if(is_diamond) {
          formation <- paste0(formation, "-diamond")
        }
        tryCatch( {team <- match_page %>% rvest::html_nodes("div+ strong a") %>% rvest::html_text() %>% .[home_away]}, error = function(e) {team <- NA})
        
        bench_index <- which(lineup[,1] == "Bench")
        
        suppressMessages(lineup <- lineup[1:(bench_index-1),] %>% dplyr::mutate(Starting = "Pitch") %>%
                           dplyr::bind_rows(
                             lineup[(bench_index+1):nrow(lineup),] %>% dplyr::mutate(Starting = "Bench")
                           ) )
        lineup <- lineup %>%
          dplyr::mutate(Matchday = match_date,
                        Team = team,
                        Formation = formation,
                        PlayerURL = player_urls)
        names(lineup) <- c("Player_Num", "Player_Name", "Starting", "Matchday", "Team", "Formation", "PlayerURL")
        all_tables <- match_page %>%
          rvest::html_nodes(".table_container")
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "summary$"))] %>%
          rvest::html_nodes("table")
        
        if(home_away == 1) {
          home_or_away <- "Home"
        } else {
          home_or_away <- "Away"
        }
        
        additional_info <- stat_df[home_away]%>% rvest::html_table() %>% data.frame()
        additional_info <- additional_info %>%
          .clean_match_advanced_stats_data() %>%
          dplyr::filter(!is.na(.data[["Player_Num"]])) %>%
          dplyr::bind_cols(Team=team, Home_Away=home_or_away, .) %>%
          dplyr::mutate(Player_Num = as.character(.data[["Player_Num"]]))
        
        if(any(grepl("Nation", colnames(additional_info)))) {
          additional_info <- additional_info %>%
            dplyr::select(.data[["Team"]], .data[["Home_Away"]], .data[["Player"]], .data[["Player_Num"]], .data[["Nation"]], .data[["Pos"]], .data[["Age"]], .data[["Min"]], .data[["Gls"]], .data[["Ast"]], .data[["CrdY"]], .data[["CrdR"]])
        } else {
          additional_info <- additional_info %>%
            dplyr::select(.data[["Team"]], .data[["Home_Away"]], .data[["Player"]], .data[["Player_Num"]], .data[["Pos"]], .data[["Age"]], .data[["Min"]], .data[["Gls"]], .data[["Ast"]], .data[["CrdY"]], .data[["CrdR"]])
        }
        
        
        lineup <- lineup %>%
          dplyr::mutate(Player_Num = as.character(.data[["Player_Num"]])) %>%
          dplyr::left_join(additional_info, by = c("Team", "Player_Name" = "Player", "Player_Num")) %>%
          dplyr::mutate(Home_Away = ifelse(is.na(.data[["Home_Away"]]), home_or_away, .data[["Home_Away"]])) %>%
          dplyr::select(.data[["Matchday"]], .data[["Team"]], .data[["Home_Away"]], .data[["Formation"]], .data[["Player_Num"]], .data[["Player_Name"]], .data[["Starting"]], dplyr::everything()) %>%
          dplyr::mutate(Matchday = lubridate::ymd(.data[["Matchday"]])) %>%
          dplyr::mutate(MatchURL = match_url)
        
        return(lineup)
      }
      
      all_lineup <- tryCatch(c(home, away) %>%
                               purrr::map_df(get_each_lineup), error = function(e) data.frame())
      if(nrow(all_lineup) == 0) {
        print(glue::glue("Lineups not available for {match_url}"))
      }
      
    } else {
      print(glue::glue("Lineups not available for {match_url}"))
      all_lineup <- data.frame()
    }
    
    return(all_lineup)
  }
  
  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))
  all_lineups <- match_url %>%
    purrr::map_df(get_each_match_lineup)
  return(all_lineups)
}
#######################################################################################################################

match_url <- "https://fbref.com/en/matches/0f67bdaa/Sporting-CP-Arsenal-March-9-2023-Europa-League"


# Getting the line-up for the match
match_lineup <- get_match_lineups(match_url = match_url) %>%
  mutate(Team = if_else(Team == "Arsenal", "Sporting CP", "Arsenal"))

match_summary <- fb_match_summary(match_url = match_url) %>%
  select(League, Match_Date, Home_Team, Home_Formation, Home_Score, Home_xG, Home_Yellow_Cards, Home_Red_Cards,
         Away_Team, Away_Formation, Away_Score, Away_xG, Away_Yellow_Cards, Away_Red_Cards, 
         Team, Home_Away, Event_Time, Is_Pens, Event_Half, Event_Type, Event_Players, Score_Progression, Penalty_Number)

match_report <- fb_match_report(match_url = match_url)

advanced_match_report <- fb_advanced_match_stats(match_url = match_url, stat_type = "possession", team_or_player = "team")
advanced_match_report <- fb_advanced_match_stats(match_url = match_url, stat_type = "summary", team_or_player = "team")
advanced_match_report_player <- fb_advanced_match_stats(match_url = match_url, stat_type = "summary", team_or_player = "player")
advanced_match_report <- fb_advanced_match_stats(match_url = match_url, stat_type = "possession", team_or_player = "player")


## Defemse: Tackle, Interceptions, Block (Top 5)
## Midfield: Touches, Passing Percentage, Carries (Top 5)
## Attacking: Shot Percentage (SoT / Total Shots), Att_Take_Ons, xg Expected (Top 5)

advanced_match_report_player %>%
  select(Team, Player, Tkl) %>%
  group_by(Team, Player) %>%
  summarise(maxTackles = max(Tkl, na.rm = TRUE)) %>%
  filter(Team == "Bayer Leverkusen") %>%
  top_n(4) %>%
  arrange(desc(maxTackles)) %>%
  ggplot(aes(x = reorder(Player, -maxTackles), y = maxTackles)) + geom_bar(stat = "identity", fill = "#A40218", colour = "#F8F8F8") +
  theme(plot.background = element_rect(fill = "#000000"),
        panel.background = element_rect(fill = "#000000",
                                        colour = "#000000",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#000000"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#000000"),
        axis.text = element_text(colour = "#F8F8F8"))



df <- get_advanced_match_stats(match_url=match_url,stat_type="possession")


