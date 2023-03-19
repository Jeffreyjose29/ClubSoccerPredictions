# Packages
pacman::p_load(dplyr, tidyr, stringr, janitor, purrr,
               tibble, lubridate, glue, rlang, 
               rvest, polite,
               ggplot2, 
               gt, forcats, ggtext, extrafont, 
               understatr, ggsoccer)
## Load fonts
loadfonts(quiet = TRUE)

sessionInfo()



home_team = "Arsenal"
away_team = "Leicester"
# https://teamcolorcodes.com/
home_color = "#DB0007" 
away_color = "#003090"
  
home_team_logo <- "https://upload.wikimedia.org/wikipedia/en/thumb/5/53/Arsenal_FC.svg/1200px-Arsenal_FC.svg.png"
away_team_logo <- "https://upload.wikimedia.org/wikipedia/en/thumb/2/2d/Leicester_City_crest.svg/1200px-Leicester_City_crest.svg.png"

match_date <- "Aug. 14, 2023"
league_year <- "Premier League 2022-2023"
matchday <- 2
source_text <- "**Table**: Data Den (**Twitter**: @dataden_) | **Data**: understat.com"
  

match_id <- 18217
raw_match_df <- understatr::get_match_shots(match_id = match_id)

glimpse(raw_match_df)


shots_df <- raw_match_df %>% 
  ## 1. Take out 2 columns we don't really need.
  select(-h_goals, -a_goals) %>% 
  ## 2. Make sure the selected columns are set to numeric type.
  mutate(across(c(minute, xG, X, Y, 
                  player_id, match_id, season), as.numeric)) %>% 
  ## 3. If xG is `NA` then set it to 0.
  ## 4. Relabel the categories in "result", "situation", "lastAction", and "shotType" columns so they're more human-friendly and presentable.
  mutate(xG = if_else(is.na(xG), 0, xG),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           situation == "SetPiece" ~ "Set Piece",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           lastAction == "Standard" ~ NA_character_,
           lastAction == "HeadPass" ~ "Headed Pass",
           lastAction == "BlockedPass" ~ "Blocked Pass",
           lastAction == "OffsidePass" ~ "Offside Pass",
           lastAction == "CornerAwarded" ~ "Corner Awarded",
           lastAction == "Throughball" ~ "Through ball",
           lastAction == "SubstitutionOn" ~ "Subbed On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           shotType == "OtherBodyPart" ~ "Other",
           TRUE ~ shotType)) %>% 
  ## 5. Consolidate team name into a single column "team_name" based on the "h_a" column.
  mutate(team_name = case_when(
    h_a == "h" ~ h_team,
    h_a == "a" ~ a_team)) %>% 
  ## 6. Add team colors to the row depending on the team.
  mutate(team_color = if_else(team_name == h_team, home_color, away_color)) %>% 
  ## 7. Own Goal is set to the team that conceded it so swap it to the team that actually scored from it.
  mutate(team_name = case_when(
    result == "Own Goal" & team_name == home_team ~ away_team,
    result == "Own Goal" & team_name == away_team ~ home_team,
    TRUE ~ team_name)) %>% 
  ## 8. Set "team_name" as a factor variable.
  mutate(team_name = forcats::as_factor(team_name)) %>% 
  ## 9. Arrange the rows by `id` so that shots are in chronological order.
  arrange(id) %>% 
  ## 10. Separate "player" into two, then re-combine.
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho are listed without a last name "Tavares"
  ## so just add their name in again if NA
  mutate(player = if_else(is.na(player), firstname, player),
         ## 11. Set a new and cleaner ID for shots so that it starts at 1 and goes to `n`.
         id = row_number())


glimpse(shots_df)


## 1. Get minute of last shot
last_min <- shots_df$minute %>% unique() %>% last()

## 2. If last shot happened before 90th minute then change to 90
if (last_min < 90) {last_min <- 90}

## 3. Create index of every minute in the match
minute <- c(0:last_min)

## 4. Set team names in a list
team_name <- c(shots_df$h_team %>% unique(),
               shots_df$a_team %>% unique())

rollsum_df <- shots_df %>% 
  ## 5. Expand shots_df to include rows for every minute
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  ## 6. Change NAs to 0
  ## Apply rolling cumulative sum on xG
  mutate(xG = if_else(is.na(xG), 0, xG),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  ## 7. Change Player Labels (Not used for the table viz so can be ignored)
  mutate(rollsum_goal = rollsum + xG) %>% 
  ## for Minute == 0
  mutate(rollsum = if_else(is.na(rollsum), 0, rollsum),
         rollsum_goal = if_else(is.na(rollsum_goal), 0, rollsum_goal)) %>% 
  ## FOR THIS BLOGPOST // {gt} TABLE WE DON'T NEED MOST OF THESE COLUMNS
  ## We'll only use the shot order ID and the rolling sum of xG that we just calculated.
  filter(xG != 0.00) %>% 
  select(id, rollsum_goal)


match_url <- stringr::str_glue("https://understat.com/match/{match_id}")

match_page <- polite::bow(match_url)

team_stats_raw <- polite::scrape(match_page) %>% 
  html_nodes("div.scheme-block:nth-child(4)") %>% 
  html_text() %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% 
  str_squish()

## make sure that you set "home_team" and "away_team" in the beginning, exactly as they appear on understat.com
if (str_detect(home_team, " ") == TRUE | 
    str_detect(away_team, " ") == TRUE) {
  
  home_team_sp <- str_replace_all(home_team, " ", "-")
  away_team_sp <- str_replace_all(away_team, " ", "-")
  
  team_stats_raw <- team_stats_raw %>% 
    str_replace_all(., home_team, home_team_sp) %>% 
    str_replace_all(., away_team, away_team_sp)
}

home_team_sp <- str_replace_all(home_team, " ", "-")
away_team_sp <- str_replace_all(away_team, " ", "-")

team_stats <- team_stats_raw %>% 
  read.table(text = ., header = FALSE, sep = " ",
             col.names = c("var_name", "home", "away")) %>% 
  t() %>% 
  tibble::as_tibble(.name_repair = "minimal") %>% 
  janitor::row_to_names(row_number = 1) %>% 
  mutate_at(vars(-TEAMS), ~ as.numeric(.)) %>% 
  mutate(TEAMS = case_when(
    str_detect(TEAMS, home_team_sp) ~ home_team,
    str_detect(TEAMS, away_team_sp) ~ away_team,
    TRUE ~ TEAMS
  ))


## split team stats into "home" and "away"
home_stats <- team_stats[1,]

away_stats <- team_stats[2,]

## add colors based on defined variables for team's respective color
home_stats$home_team_color <- home_color
away_stats$away_team_color <- away_color

glimpse(team_stats)


glimpse(shots_df)
glimpse(rollsum_df)
glimpse(team_stats)


pitch_custom <- list(
  length = 587,
  width = 373,
  penalty_box_length = 101,
  penalty_box_width = 211,
  six_yard_box_length = 31,
  six_yard_box_width = 111,
  penalty_spot_distance = 66,
  goal_width = 45,
  origin_x = 0,
  origin_y = 0)

## create coords
match_df <- shots_df %>% 
  ## switch coordinates for vertical view
  mutate(
    x = case_when(
      h_a == "a" ~ X * 587,
      h_a == "h" ~ X * 587,
      TRUE ~ 0),
    y = case_when(
      h_a == "a" ~ Y * 373,
      h_a == "h" ~ Y * 373,
      TRUE ~ 0)) %>%
  ## edit result values
  mutate(result = case_when(
    result == "Goal" ~ "GOAL",
    result == "Own Goal" ~ "OWN GOAL",
    TRUE ~ result)) %>% 
  mutate(result = forcats::as_factor(result),
         result = forcats::fct_relevel(result, "GOAL", "Saved Shot",
                                       "On Post", "Blocked Shot", 
                                       "Missed Shots", "OWN GOAL"))


glimpse(match_df)


create_shotmap_basic <- function(df = data, team_name = team_name) {
  
  shotxG_map_raw <- 
    ggplot(df %>% filter(team_name == team_name), 
           aes(x = x, y = y)) +
    annotate_pitch(dimensions = pitch_custom) +
    ## all shots in grey and transparent
    geom_point(aes(x = x, y = y), color = "grey20", 
               size = 3, alpha = 0.3) +
    #scale_x_continuous(expand = c(0.01, 0)) +
    theme_pitch(aspect_ratio = 373/587) +
    coord_flip(xlim = c(280, 590), 
               ylim = c(10, 365)) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "pt"),
          #text = element_markdown(family = "Roboto Condensed"),
          legend.position = "none")    
  
  return(shotxG_map_raw)
}



match_df %>% tibble::as_tibble() %>% group_by(team_name) %>% nest()


match_df %>% 
  tibble::as_tibble() %>% 
  group_by(team_name) %>% 
  nest() %>% 
  head(1) %>% 
  unnest(cols = c(data))



## add blank shot map to all rows
match_df_basic_plot <- match_df %>% 
  tibble::as_tibble() %>% 
  ## nest by team
  group_by(team_name) %>% 
  nest() %>% 
  ## apply plots for all shots per team, for each row in their respective "data" column
  mutate(plot = map2(data, team_name, create_shotmap_basic)) %>% 
  ungroup()


glimpse(match_df_basic_plot)

match_df_basic_plot <- match_df_basic_plot %>% 
  unnest(cols = "data") %>% 
  arrange(id)

glimpse(match_df_basic_plot)


add_xG_shot <- function(x, y, xG, team_color, plot) {
  shotxG_map_point <- 
    plot +
    # specific shot point in black and bold
    geom_point(x = x, y = y, aes(fill = team_color),
               size = 12, stroke = 3, shape = 21) +
    scale_fill_identity() +
    #scale_x_continuous(expand = c(0.01, 0)) +
    ## label for shot point
    geom_label(x = 318, y = 186.5, 
               color = "black", size = 20,
               fill = "white", family = "Roboto Slab",
               label = glue::glue("{xG %>% round(digits = 2)} xG"))
  
  return(shotxG_map_point)
}


## map plot to df again with a shot point for each row/plot
dfdfdf <- match_df_basic_plot %>% 
  ## shot-per-row, using 'plot' as base pass along the 'x', 'y' coordinates and xG value
  ## to "add_xG_shot()` function for each row. 
  ## have empty 'ggplot' column for gt plot-insertion purposes
  mutate(complete_plot = pmap(list(x, y, xG, team_color, plot), add_xG_shot),
         ggplot = NA) %>% 
  select(-plot) %>% 
  left_join(rollsum_df, by = "id")


dfdfdf$complete_plot[[1]]
dfdfdf$complete_plot[[4]]


glimpse(dfdfdf)


## data creation for actual table
match_shots_table_df <- dfdfdf %>% 
  select(minute, team_name, result, xG, firstname, player, 
         ggplot, complete_plot, rollsum = rollsum_goal,
         situation, type = shotType, player_assisted, lastAction) %>% 
  ## player name labels, clean "lastAction"
  mutate(player_name = paste(firstname, player),
         lastAction = if_else(lastAction == "None", NA_character_, lastAction),
         xG = xG %>% round(digits = 2),
         rollsum = rollsum %>% round(digits = 2)) %>% 
  ## NAs as blanks
  mutate(across(where(is.character), ~ replace_na(., ""))) %>% 
  ## take out extraneous name vars and move to after team name
  select(-firstname, -player) %>% 
  relocate(player_name, .after = team_name) 

glimpse(match_shots_table_df)


soccer_ball <- "<span style='color:white;font-size:25px'>&#9917;</span>"


match_gt_xG_timeline <- 
  gt(match_shots_table_df) %>% 
  tab_header(   
    title = gt::html(glue::glue("<p><img src='{home_team_logo}' alt='home_team_logo' style='float:left;height:190px;'><span style='font-size:35px'>({home_stats$xG} xG)</span> <b style='color:{home_stats$home_team_color}; font-size:55px'>{home_stats$TEAMS}</b> <span style='font-size:70px'>{home_stats$GOALS} - </span><span style='font-size:70px'>{away_stats$GOALS}</span> <b style='color:{away_stats$away_team_color}; font-size:55px'>{away_stats$TEAMS}</b> <span style='font-size:35px'>({away_stats$xG} xG)</span><img src='{away_team_logo}' alt='away_team_logo' style='float:right;height:190px;'> 
</p><span style='font-size:40px'>{league_year}: Matchday {matchday} ({match_date})</span>")))


match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## title style
  tab_style(
    style = list(
      cell_text(
        font = "Roboto Slab",
        align = "center",
        weight = "bold",
        color = "#000000"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  ## column style
  tab_style(
    style = list(
      cell_text(font = "Roboto Slab", align = "center", 
                size = "xx-large", weight = "bold"),
      cell_borders(sides = c("left", "right"), 
                   color = "grey20", weight = px(2))
    ),
    locations = list(
      cells_column_labels(everything())
    )
  ) 


match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## HOME TEAM
  tab_style(
    style = cell_fill(color = home_color),
    locations = cells_body(
      rows = team_name == home_team)
  ) %>% 
  ## AWAY TEAM
  tab_style(
    style = cell_fill(color = away_color),
    locations = cells_body(
      rows = team_name == away_team)
  ) %>% 
  ## all cell text
  tab_style(
    style = cell_text(color = "white", align = "center", size = "x-large",
                      font = "Roboto Condensed", weight = "bold"),
    locations = cells_body(
      columns = TRUE) 
  ) 


match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## add Goal result emoji by pasting in the emoji next to the 'result' text
  text_transform(
    locations = cells_body(
      columns = vars(result),
      rows = result %in% c("GOAL", "OWN GOAL")),
    fn = function(x) paste(x, soccer_ball)
  ) 



match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## add plots into the empty 'ggplot' column
  ## use `ggplot_image()` function and set height and aspect ratio
  text_transform(
    locations = cells_body(columns = vars(ggplot)),
    fn = function(x) {
      map(match_shots_table_df$complete_plot, ggplot_image, height = px(150), aspect_ratio = 2)
    }
  )


match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## Rename cols
  cols_label(
    minute = "Minute", team_name = "Team", player_name = "Player", 
    result = "Result", xG = "xG", rollsum = "Cumulative xG", 
    ggplot = "Shot Map", situation = "Situation", 
    type = "Shot Type", lastAction = "Assist Action", player_assisted = "Assist Player"
  ) %>% 
  cols_align("center")


match_gt_xG_timeline <- 
  match_gt_xG_timeline %>% 
  ## general table options
  tab_options(
    column_labels.border.top.color = "grey",
    column_labels.border.top.width= px(5),
    column_labels.border.bottom.color = "grey",
    column_labels.border.bottom.width= px(5),
    data_row.padding = px(15),
    source_notes.font.size = 20
  ) %>% 
  tab_source_note(source_note = md(source_text)) %>% 
  cols_hide(vars(complete_plot, xG))



match_gt_xG_timeline
