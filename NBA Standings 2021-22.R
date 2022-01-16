##### 2021-22 NBA Standings Table #####
#### By: Stephan Teodosescu ####
### January 2022 ###

library(tidyverse)
library(nbastatR)
library(teamcolors)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(crosstalk)
library(rvest)


##### Define Themes #####
# match 538's table theme for the reactable table
theme_538 <- function() {
    reactable::reactableTheme(
        searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
        style = list(
            fontFamily = "Chivo"
        ),
        headerStyle = list(
            "&:hover[aria-sort]" = list(
                background = "hsl(0, 0%, 80%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
                background = "#555",
                color = "#FFF"
            ),
            borderColor = "#333"
        ),
        borderColor = "#CDCDCD"
    )
}

# load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

##### Load Data #####

# Load NBA game logs from nbastatR
df_team_logs <- game_logs(seasons = 2022, result_types = "team", season_types = "Regular Season")

# Load team stats data from BREF.com. This loads multiple data frames into your environment at once
bref_teams_stats(seasons = 2022)

# Read in games data from nbastatR
nba_standings <- nbastatR::standings(seasons = 2022) %>%
    select(slugSeason:TeamSlug, recordOverall) %>% 
    mutate(slugTeam = case_when(
        teamName == "Clippers" ~ "LAC", 
        teamName == "Nuggets" ~ "DEN", 
        TRUE ~ slugTeam
    ))


# get team logos from the nbastatR package
logos <- nba_teams() %>% 
    filter(isNonNBATeam == 0) %>% 
    select(nameTeam, slugTeam, urlThumbnailTeam) %>% 
    mutate(nameTeam = case_when(
        nameTeam == "Portland Trail Blazers" ~ "Portland Trailblazers",
        nameTeam == "Los Angeles Clippers" ~ "LA Clippers",
        TRUE ~ nameTeam
    ))

# get nba team colors 
tc <- teamcolors %>% 
    filter(league == "nba") %>% select(name, primary:quaternary)


# Get url for scraping data from vegasinsider and scrape tables on the page using rvest
vegasURL <- "https://www.vegasinsider.com/nba/odds/futures/"

vi_raw <- vegasURL %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)

vegas_odds <- vi_raw %>% 
    .[8] %>% #extract the element from the list containing the finals odds table that is returned from the html table
    as.data.frame()

names(vegas_odds) <- vegas_odds[1,] #clean up column names

vegas_odds <- vegas_odds %>% 
    slice(-1)

vegas_odds$Odds <- str_remove(vegas_odds$Odds, "[+]")
vegas_odds$Odds <- str_remove(vegas_odds$Odds, "[-]")

vegas_odds <- vegas_odds %>% 
    as_tibble() %>% 
    type_convert() %>% 
    mutate(implied_odds = 1-Odds/(Odds+100)) %>% 
    mutate(Team = case_when(
        Team == "Los Angeles Clippers" ~ "LA Clippers", 
        TRUE ~ Team
    ))


# join datasets together
df_nba <- left_join(vegas_odds, nba_standings, by = c("Team" = "nameTeam"))

df_nba <- df_nba %>% 
    mutate(pct_diff = pctWinTeam - implied_odds) #calcualte performance relative to Vegas expectations

df_nba <- left_join(df_nba, logos, by = c("Team" = "nameTeam")) #joining logos

df_nba <- df_nba %>% 
    mutate(rank = row_number()) %>% 
    relocate(rank)



##### Make Reactable Table #####

tbl_react <- df_nba %>% 
    select(rank, urlThumbnailTeam, Team, nameConference, nameDivison, recordOverall, slugStreakCurrent, pctWinTeam, pct_diff, implied_odds) %>% 
    reactable(
          theme = theme_538,
          columnGroups = list(
              colGroup(name = "According to Vegas", 
                       columns = c("pct_diff", "implied_odds"))
          ),
          showSortIcon = TRUE,
          searchable = TRUE,
          defaultSorted = c("implied_odds","pctWinTeam"),
          defaultSortOrder = "desc",
          language = reactableLang(
              searchPlaceholder = "SEARCH FOR A TEAM..."),
          defaultPageSize = 100,
          columns = list(
              rank = colDef(name = "Rank",
                            align = "left",
                            maxWidth = 50),
              Team = colDef(minWidth = 200,
                                 name = "Team",
                                 align = "left"),
              nameConference = colDef(align = "left",
                                name = "Conference"),
              nameDivision = colDef(align = "left",
                                  name = "Division"),
              pctWinTeam = colDef(align = "right",
                                  name = "Win %",
                                  style = list(borderRight = "2px solid #555")),
              slugStreakCurrent = colDef(align = "right",
                               name = "Streak"),
              recordOverall = colDef(align = "center",
                                     name = "Record"),
              pct_diff = colDef(align = "right",
                                name = "vs Expectation",
                                format =  colFormat(digits = 3)),
              implied_odds = colDef(align = "right",
                                    name = "Winning NBA Title",
                                    style = color_scales(df_nba, colors = paletteer::paletteer_d(
                                        palette = "ggsci::amber_material")),
                                    format =  colFormat(percent = TRUE, digits = 0)),
              
              ### add logos using embed_img()
              urlThumbnailTeam = colDef(
                  name = "",
                  maxWidth = 70,
                  align = "center",
                  cell = embed_img(height = "30", width = "30")
              )),
          pagination = FALSE,
          compact = TRUE, 
          borderless = FALSE, 
          striped = FALSE,
          fullWidth = FALSE, 
          defaultColDef = colDef(align = "center", minWidth = 100)
)



# Create titles, subtitles, and captions
title <- "2021-22 NBA Standings"
subtitle_1 <- "The table shows actual wins versus expected wins and also shows a ten game rolling average of margin of victory/defeat through January 9th, 2022. Expected wins is derived from points scored and points allowed. The rolling average is derived from the average +/- points differential from the preceding ten games. The dotted line shows the average across all ten game intervals for the season."
subtitle_2 <- "Over the first ten games, the Cleveland Cavaliers, for example, were outscored by an average of .5 points per game, their minimum over any ten game stretch of the year. From games 22 to 31, they outscored opponents by an average of 16 points per game, their maximum for the year.  The average across all stretches of ten game intervals has been +6.6 points per game."  
caption <- "Stephan Teodosescu @steodosescu | Source: Basketball-Reference via {nbastatR}"

ima <- here::here("nba_logo.png")
