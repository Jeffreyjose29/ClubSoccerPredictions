
# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch", "hrbrthemes", "grid", "worldfootballR", "glue", "ggshakeR", "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

fb_player_scouting_report <- function(player_url, pos_versus, league_comp_name=NULL, time_pause=3) {
  
  main_url <- "https://fbref.com"
  
  # put sleep in as per new user agreement on FBref
  Sys.sleep(time_pause)
  
  player_page <- xml2::read_html(player_url)
  #player_page <- .load_page(player_url)
  
  player_name <- player_page %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_squish()
  
  main_cats <- player_page %>% rvest::html_nodes("#inner_nav") %>% rvest::html_nodes(".full.hasmore")
  span_names <- main_cats %>% rvest::html_nodes("span") %>% rvest::html_text()
  main_cats <- main_cats[grep("Scouting Report", span_names)]
  
  # scout_level1_url <- main_cats %>% rvest::html_nodes("ul li") %>% .[1] %>%
  #   rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
  
  scout_level1_url <- main_cats %>% rvest::html_nodes("ul li") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
  
  league_comp_names <- main_cats %>% rvest::html_nodes("ul li") %>% rvest::html_text() %>% stringr::str_squish()
  
  if(!is.null(league_comp_names)) {
    league_comp_name_regex <- league_comp_names %>% paste0(collapse = "|")
    
    if(!any(grep(league_comp_name_regex, league_comp_names))) {
      stop(glue::glue("'{league_comp_name}' not a valid value for `league_comp_name`. Please re-copy and paste the correct value(s)"))
    } else {
      scout_level1_url <- scout_level1_url[grep(league_comp_name_regex, league_comp_names)]
    }
  }
  
  
  all_scout_pos <- data.frame()
  
  for(each_scout_url in 1:length(scout_level1_url)) {
    Sys.sleep(time_pause)
    
    scout_pg <- xml2::read_html(scout_level1_url[each_scout_url])
    
    period <- scout_pg %>% rvest::html_nodes(".filter") %>% .[1] %>% rvest::html_elements("div") %>% rvest::html_text() %>%
      unique() %>% stringr::str_squish() %>% .[each_scout_url]
    
    # .pkg_message("Scraping full scouting report for {player_name} for period: {period}")
    
    outer <- scout_pg %>% rvest::html_nodes(".filter.switcher") %>% rvest::html_elements("div")
    
    if(length(outer) == 1) {
      pos_versus_idx <- 1
      versus <- outer %>% rvest::html_text() %>%
        stringr::str_squish()
    } else if (length(outer) > 1) {
      if(pos_versus != "primary") {
        pos_versus_idx <- 2
      } else {
        pos_versus_idx <- 1
      }
      
      versus <- outer %>% rvest::html_text() %>%
        stringr::str_squish() %>% .[pos_versus_idx]
    } else {
      stop(glue::glue("Full scouting report not available for {player_name}"))
    }
    
    
    scouting_all <- scout_pg %>% rvest::html_nodes("#all_scout_full") %>% rvest::html_nodes(".table_container")
    
    
    scout_pos <- scouting_all[pos_versus_idx] %>%
      rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()
    
    missing_idx <- scout_pos[,1] != ""
    scout_pos <- scout_pos[missing_idx,]
    
    names(scout_pos) <- scout_pos[1,]
    scout_pos <- scout_pos %>%
      dplyr::rename(Per90=.data[["Per 90"]])
    
    df <- data.frame(Statistic="Standard", Per90="Standard", Percentile="Standard")
    scout_pos <- rbind(df, scout_pos)
    scout_pos$stat_group <- NA_character_
    
    stat_names <- scout_pos$Statistic
    idx <- grep("Statistic", stat_names)
    
    stat_vct <- c()
    for(i in 1:length(idx)) {
      id <- idx[i]-1
      st <- stat_names[id]
      tryCatch(scout_pos[c(id:(idx[i+1]-2)), "stat_group"] <- st, error = function(e) scout_pos[c(id:nrow(scout_pos)), "stat_group"] <- st)
    }
    scout_pos$stat_group[is.na(scout_pos$stat_group)] <- st
    scout_pos <- scout_pos[-c(idx, idx-1), ]
    
    scout_pos <- scout_pos %>%
      dplyr::mutate(Player=player_name,
                    Versus=gsub("vs. ", "", versus),
                    Per90 = gsub("\\+", "", .data[["Per90"]]) %>% gsub("\\%", "", .) %>% gsub("\\,", "", .) %>% as.numeric(),
                    Percentile = as.numeric(.data[["Percentile"]])) %>%
      dplyr::select(.data[["Player"]], .data[["Versus"]], StatGroup=.data[["stat_group"]], dplyr::everything())
    
    mins_played <- scout_pg %>% rvest::html_nodes(".footer") %>% rvest::html_nodes("strong") %>%
      rvest::html_text() %>% gsub(" minutes", "", .) %>% as.numeric() %>% unique()
    
    scout_pos <- scout_pos %>%
      dplyr::mutate(BasedOnMinutes = mins_played)
    
    scout_pos <- scout_pos %>%
      dplyr::mutate(scouting_period = period)
    
    all_scout_pos <- dplyr::bind_rows(all_scout_pos, scout_pos)
  }
  
  return(all_scout_pos)
  
}


fb_teams_urls(league_url = "https://fbref.com/en/comps/9/Premier-League-Stats")
fb_player_urls("https://fbref.com/en/squads/19538871/Manchester-United-Stats" )

df <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/4d224fe8/Casemiro", pos_versus = "primary")


df_selected <- df %>%
  filter(scouting_period == "2022-2023 Premier League")

df_selected <- as.data.frame(df_selected)
df_selected1 <- df_selected %>%
  filter(Statistic %in% c("Progressive Passes", "Shot-Creating Actions", "Touches (Mid 3rd)", "Progressive Carries", "Pass Completion %", "Progressive Passes", "Passes Attempted",
                          "Tackles", "Tackles (Mid 3rd)", "Clearances", "Interceptions", "Passes Blocked", "Blocks",
                          "Goals + Assists", "xAG", "Shots on target %", "xG", "Progressive Carries",
                          "Fouls Drawn", "Fouls Committed", "Yellow Cards", "Red Cards"))



df_selected1 <- df_selected1 %>%
  arrange(Statistic, StatGroup) %>%
  mutate(rownum = row_number()) %>%
  filter(!rownum %in% c(7, 9, 12, 14, 16, 18, 25, 27, 29))

df_selected1 <- df_selected1 %>%
  mutate(stat = if_else(Statistic %in% c("Progressive Passes", "Shot-Creating Actions", "Touches (Mid 3rd)", "Progressive Carries", "Pass Completion %", "Progressive Passes", "Passes Attempted"), "Possession",
                        if_else(Statistic %in% c("Tackles", "Tackles (Mid 3rd)", "Clearances", "Interceptions", "Passes Blocked", "Blocks"), "Defense",
                                if_else(Statistic %in% c("Goals + Assists", "xAG", "Shots on target %", "xG", "Progressive Carries"), "Attack", "Miscellaneous")))
  )



temp <- (360/(nrow(df_selected1))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = nrow(df_selected1))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)                                          #rotate some lables back for readability...
ang <- abs(ang)

df_selected1$Statistic <- gsub(" ","\n",df_selected1$Statistic)


ggplot(df_selected1,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
           alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=4,color="white",show.legend = FALSE)+      #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
  scale_fill_manual(values=c("Possession" = "#41ab5d",                                   #choose colors to fill the pizza parts
                             "Attack" = "#fec44f",
                             "Defense" = "#de2d26",
                             "Miscellaneous" = "#1A78CF")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",   
       caption = "Source: StatsBomb Via FBref",     
       #remove legend title
       title = glue("{df_selected$Player[1]} (Manchester United F.C)"),
       subtitle = glue::glue("2022-2023 | Compared To Mid-Fielders Premier League 2022-2023 | Stats Per 90")) + #let the title be te name of the player                                                
  
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(size = 6, angle = ang),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        #plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.title = element_text(hjust=0.5, size = 30, colour = "#d50006"),
        plot.subtitle = element_text(hjust=0.5,size=16, colour = "#d50006"),
        plot.caption = element_text(hjust=0.5,size=16, colour = "#d50006"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2),
        legend.text = element_text(colour = "#d50006", size = 14),
        axis.text=element_text(size=12, colour = "#d50006")
  ) 
