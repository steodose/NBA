##### NBA Standings App #####
## By: Stephan Teodosescu

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyscreenshot)

library(tidyverse)
library(nbastatR)
library(teamcolors)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(crosstalk)
library(glue)
library(rvest)
library(janitor)


##### Define Themes #####

#Fetch current date for updating visualizations
update_date <- format.Date(Sys.Date(), "%b. %d, %Y")

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

# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

temppal <- c("#36a1d6", "#76b8de", "#a0bfd9", "#ffffff", "#d88359", "#d65440", "#c62c34")


##### Load Data #####

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



# Get each team's Net Rating and other advances stats from basketball-reference
url <- "https://www.basketball-reference.com/leagues/NBA_2022.html"

bref_tables <- url %>% 
    read_html() %>% 
    html_table()

df_tm_net_rating <- bref_tables[[11]]

df_tm_net_rating <- df_tm_net_rating %>% 
    row_to_names(row_number = 1) %>% 
    clean_names()

df_tm_net_rating <- head(df_tm_net_rating, - 1) #delete league average row at the bottom

df_tm_net_rating <- df_tm_net_rating %>% 
    mutate(team = case_when(
        team == "Los Angeles Clippers" ~ "LA Clippers",
        TRUE ~ team
    ))

df_tm_net_rating <- df_tm_net_rating %>% 
    select(team, pw, pl, mov, o_rtg, d_rtg, n_rtg, attend_g) 


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

#clean Vegas data

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


## join datasets together
df_nba <- left_join(vegas_odds, nba_standings, by = c("Team" = "nameTeam"))

df_nba <- df_nba %>% 
    mutate(pct_diff = pctWinTeam - implied_odds) #calcualte performance relative to Vegas expectations

df_nba <- left_join(df_nba, logos, by = c("Team" = "nameTeam")) #joining logos

df_nba <- df_nba %>% 
    mutate(rank = row_number()) %>% 
    relocate(rank)

# join advanced stats scraped data from BREF
df_nba <- left_join(df_nba, df_tm_net_rating, by = c("Team" = "team"))

# Select variables we want to use in the final table
df_nba <- df_nba %>% 
    select(rank, urlThumbnailTeam, Team, nameConference, recordOverall, 
           slugStreakCurrent, pw, pl, mov, o_rtg, d_rtg, n_rtg, attend_g, pctWinTeam, pct_diff, 
           implied_odds)




##### Define UI for application #####
ui <- tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/nba-logo.png"), #Getting the NBAlogo in the browser window
    
    navbarPage(theme = shinytheme("cosmo"), # Navbar theme at the top
               tags$head(tags$style(HTML('.navbar-static-top {background-color: #3d195b;}',
                                         '.navbar-default .navbar-nav>.active>a {background-color: #3d195b;}'))), #NBA blue theme for navbar
               
               title = tags$div(img(src="nba-logo.png", height=28), "2021-22 NBA"),
               tabPanel("Standings", icon = icon("sort"), 
                        h1("2021-22 NBA Standings and Stats"),
                        glue("Welcome to our EPL projections and probabilities page where you will find each squad’s projected points total, goal differential, average finish, and its probability of Champions League qualification or relegation. These odds are based on 10,000x simulations of the remainder of the current season. The data are refreshed on Mondays after the week's matches have concluded. Last updated {update_date}."),
                        reactableOutput("table_standings"),
                        screenshotButton(id = "table_standings")
               ),
               
               ### About tab
               tabPanel("About", icon = icon("info-circle"),
                        fluidRow(
                            column(8,
                                   includeMarkdown("About.md")
                            )
                        )
               ),
               
               # Footer for web app
               hr(style = "border-color: #cbcbcb;"),
               fluidRow(
                   column(9,
                          p("App created by ", tags$a(href = "https://steodose.github.io/steodosescu.github.io/", 'Stephan Teodosescu', target = '_blank'),", October 2021", HTML("&bull;"),
                            "Find the code on Github:", tags$a(href = "https://github.com/steodose/NBA", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 100%"),
                          p("Questions? Comments? Reach out on Twitter", tags$a(href = "https://twitter.com/steodosescu", tags$i(class = 'fa fa-twitter', style = 'color:#1DA1F2'), target = '_blank'), style = "font-size: 100%"),
                          p(tags$em("Last updated: January 2022"), style = 'font-size:85%'))),
               windowTitle = "2021-22 NBA"
    ) #navbarPage bracket
) #END UI function


###### Define server logic #####
server <- function(input, output) {

    output$table_standings <- renderReactable({
        reactable(df_nba,
            theme = theme_538,
            columnGroups = list(
                colGroup(name = "According to Vegas", 
                         columns = c("pct_diff", "implied_odds")),
                colGroup(name = "Pythagorean Wins and Losses", 
                         columns = c("pw", "pl"))
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
                              maxWidth = 60),
                Team = colDef(minWidth = 200,
                              name = "Team",
                              align = "left"),
                nameConference = colDef(align = "left",
                                        name = "Conf.",
                                        minWidth = 80),
                pctWinTeam = colDef(align = "right",
                                    name = "Win %",
                                    style = list(borderRight = "2px solid #555")),
                slugStreakCurrent = colDef(align = "right",
                                           style = list(borderRight = "2px solid #555"),
                                           name = "Streak"),
                pw = colDef(align = "right",
                            name = "PW"),
                pl = colDef(align = "right",
                            name = "PL"),
                mov = colDef(align = "right",
                             name = "MOV"),
                o_rtg = colDef(align = "right",
                               cell = color_tiles(nba_df,
                                                  temppal,
                                                  number_fmt=scales::number_format()), 
                               format =  colFormat(digits = 1),
                               name = "Offensive Rating"),
                d_rtg = colDef(align = "right",
                               name = "Defense Rating"),
                n_rtg = colDef(align = "right",
                               name = "Net Rating"),
                attend_g = colDef(align = "right",
                                  minWidth = 110,
                                  name = "Attendance per Game"),
                recordOverall = colDef(align = "center",
                                       name = "Record"),
                pct_diff = colDef(align = "right",
                                  name = "vs Expectation",
                                  minWidth = 110,
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
