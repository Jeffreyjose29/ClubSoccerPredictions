################################################################################
## PROJECT: PEP GUARDIOLA MANAGERIAL ELO RATING
## DESCRIPTION: THIS SCRIPT PLOTS THE ELO RATING OF THE THREE CLUBS PEP
##              GUARDIOLA HAS MANAGED THROUGHOUT HIS CAREER ALONG WITH ANY KEY
##              HIGHLIGHTS IN THE CLUB'S HISTORY DURING HIS TENURE
## DEVELOPER: JEFFREY JOSE
## DATE: 12TH JUNE 2023
################################################################################


################################################################################
## SECTION 1: LOAD AND INSTALL PACKAGES
## DESCRIPTION: LOAD AND INSTALL PACKAGES NECESSARY FOR THE PROJECT
################################################################################
packages <- c("dplyr", "tidyverse", "StatsBombR", "ggplot2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


################################################################################
## SECTION 2: READ IN CLUB ELO RATING .CSV FILE AND FORMAT ACCORDINGLY
## DESCRIPTION: READ IN THE SEPARATE .CSV FOR EACH CLUBS ELO RATING HISTORY
##              AND FORMAT SO THAT THEY ARE ALL THE SAME. COMBINE TOGETHER TO
##              CREATE 1 DATAFRAME IN THE END.
## NOTE: EACH CLUBS ELO RATING PERIOD IS STRIPPED TO WHEN PEP WAS MANAGING THEM.
################################################################################

setwd("H:/Personal Work/05 Github/01 Code Review/ClubSoccerPredictions/Data")

# Read each club's elo rating historical file (clubelo.com)
barcelona <- read.csv("barcelona_elo_rating.csv", header = TRUE)
bayern_munich <- read.csv("bayern_munich_elo_rating.csv", header = TRUE)
man_city <- read.csv("manchester_city_elo_rating.csv", header = TRUE)

# Format the date column to actually be a 'DATE'
barcelona$From <- as.Date(barcelona$From, format = "%Y-%m-%d")
barcelona$To <- as.Date(barcelona$To, format = "%Y-%m-%d")

bayern_munich$From <- as.Date(bayern_munich$From, format = "%Y-%m-%d")
bayern_munich$To <- as.Date(bayern_munich$To, format = "%Y-%m-%d")

man_city$From <- as.Date(man_city$From, format = "%Y-%m-%d")
man_city$To <- as.Date(man_city$To, format = "%Y-%m-%d")

# Filter the data for each club to only include periods where Pep was manager
barcelona <- barcelona %>%
  mutate(Team = "Barcelona F.C") %>%
  filter(From >= "2008-07-01" & To <= "2012-06-30")

bayern_munich <- bayern_munich %>%
  mutate(Team = "Bayern Munich F.C") %>%
  filter(From >= "2013-07-01" & To <= "2016-06-30")

man_city <- man_city %>%
  mutate(Team = "Manchester City") %>%
  filter(From >= "2016-07-01")

# Combine all three club's datasets into one
elo_rating <- rbind(barcelona, bayern_munich, man_city)


################################################################################
## SECTION 3: PLOT THE ELO RATING OF EACH CLUB WITH SIGNIFICANT EVENTS
## DESCRIPTION: PLOT A LINE GRAPH OF THE ELO RATING OF EACH CLUB OVER PEP'S
################################################################################

elo_rating %>%
  ggplot(aes(x = From, y = Elo, group = Team, colour = Team)) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_color_manual(values = c("#a60042", "#dd0129", "#6cafe0")) +
  labs(
    y = "Club Elo Rating",
    x = "",
    title = "Pep Guardiola's Managerial History",
    subtitle = "After creating a name for himself with a mega team in Barcelona and conquering the La Liga, Guardiola has given FC Bayern Munich & Manchester City FC the experience and masterclass to extend their dominance in the Bundesliga & English Premier League.",
    caption = "Created By: @dataden_\nSource: clubelo.com"
  ) +
  theme(
    legend.position = "none", axis.title.y = element_text(size = 14, colour = "#131313"),
    axis.text = element_text(size = 12, colour = "#333232"),
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", colour = "#161616"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, colour = "#131313"),
    plot.caption = element_text(colour = "#131313", size = 9, hjust = 0.5)
  ) +
  
  # Hard setting the elo rating boundaries for the y-axis
  scale_y_continuous(limits = c(1800, 2200)) +
  
  # Drawing the underline below each club's name
  annotate("segment", x = as.Date("2008-07-01"), xend = as.Date("2012-06-30"), y = 2175, yend = 2175, colour = "#a60042", size = 2) +
  annotate("segment", x = as.Date("2013-07-01"), xend = as.Date("2016-06-30"), y = 2175, yend = 2175, colour = "#dd0129", size = 2) +
  annotate("segment", x = as.Date("2016-07-01"), xend = as.Date(max(elo_rating$From)), y = 2175, yend = 2175, colour = "#6cafe0", size = 2) +
  
  # Writing out each club's name with the appropriate club colour
  geom_text(aes(x = as.Date("2010-07-01"), y = 2183, label = "FC Barcelona"), colour = "#a60042", size = 7) +
  geom_text(aes(x = as.Date("2015-01-01"), y = 2183, label = "FC Bayern München"), colour = "#dd0129", size = 7) +
  geom_text(aes(x = as.Date("2020-01-01"), y = 2183, label = "Manchester City FC"), colour = "#6cafe0", size = 7) +
  
  ## Label Annotation - Annotating key highlights in the pep's stint at the club
  ### Barcelona FC
  geom_label(aes(x = as.Date("2009-05-13"), y = 1900, label = "Copa del Rey"), colour = "#131313") +
  geom_label(aes(x = as.Date("2009-05-27"), y = 2025, label = "UEFA Champions League"), colour = "#131313") +
  geom_label(aes(x = as.Date("2011-05-28"), y = 1980, label = "UEFA Champions League"), colour = "#131313") +
  geom_label(aes(x = as.Date("2011-08-17"), y = 2020, label = "Supercopa"), colour = "#131313") +
  geom_label(aes(x = as.Date("2011-12-18"), y = 2125, label = "FIFA Club World Cup"), colour = "#131313") +
  geom_label(aes(x = as.Date("2012-05-25"), y = 2025, label = "Copa del Rey"), colour = "#131313") +
  
  ### FC Bayern Munich
  geom_label(aes(x = as.Date("2013-08-30"), y = 2000, label = "UEFA Super Cup"), colour = "#131313") +
  geom_label(aes(x = as.Date("2014-05-17"), y = 2120, label = "DFB-Pokal"), colour = "#131313") +
  geom_label(aes(x = as.Date("2016-05-04"), y = 2100, label = "UEFA Champions League Semi-Final"), colour = "#131313") +
  geom_label(aes(x = as.Date("2016-05-21"), y = 1975, label = "DFB-Pokal"), colour = "#131313") +
  
  ### Manchester City
  geom_label(aes(x = as.Date("2018-04-15"), y = 2050, label = "Premier League Champions (100 Points)"), colour = "#131313") +
  geom_label(aes(x = as.Date("2019-05-18"), y = 1940, label = "FA Cup"), colour = "#131313") +
  geom_label(aes(x = as.Date("2021-05-29"), y = 2070, label = "UEFA Champions League Final"), colour = "#131313")


##########################################################################################
################################# IGNORE THIS PART LOL ###################################
##########################################################################################

## ARSENAL ELO-RATING
arsenal_elo_rating <- read.csv("arsenal_elo_rating.csv", header = TRUE)
arsenal_elo_rating$From <- as.Date(arsenal_elo_rating$From, format = "%Y-%m-%d")
arsenal_elo_rating$To <- as.Date(arsenal_elo_rating$To, format = "%Y-%m-%d")

arsenal_elo_rating <- arsenal_elo_rating %>%
  select(Club, Rank, From, Elo) %>%
  filter(From >= "2000-01-01")

arsenal_elo_rating %>%
  ggplot(aes(x = From, y = Elo, group = 1)) +
  geom_line(colour = "#e21824", size = 1) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14, colour = "#000000"),
    plot.title = element_text(size = 40)
  )