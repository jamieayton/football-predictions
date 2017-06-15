


# 1. Setup ----------------------------------------------------------------

# global options
options(stringsAsFactors = FALSE)

# packages
library('tidyverse')
library('rvest')

# dir
project_wd <- getwd()





# 2. Scrape Functions ---------------------------------------------------

get_predictions <- function(url){
  # webpage
  webpage <- read_html(url)
  Sys.sleep(2.5+1*runif(1))
  
  # date
  match_date <- webpage %>% 
    html_nodes("#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-top > td.date > div") %>% 
    html_text()
  
  # teams
  hometeam <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-top > td.team") %>% 
    html_attr('data-str')
  awayteam <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-bottom > td.team") %>% 
    html_attr('data-str')
  
  # probabilities
  home_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-top > td.prob") %>% 
    html_text()
  draw_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-top > td.prob.tie-prob > div") %>% 
    html_text()
  away_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div > table > tbody > tr.match-bottom > td.prob") %>% 
    html_text()
  
  # adjust 'home_prob'
  home_prob <- home_prob[seq(1, length(home_prob), by=2)]
  
  # clean and combine to dataframe
  predictions <- tibble(
    match_date,
    hometeam,
    awayteam,
    home_prob = as.numeric(gsub("*%*<*", "", home_prob))/100,
    draw_prob = as.numeric(gsub("*%*<*", "", draw_prob))/100,
    away_prob = as.numeric(gsub("*%*<*", "", away_prob))/100,
    scrape_datetime = as.character(Sys.time())
  )
  
  # return
  predictions
  
}





get_results <- function(url){
  # webpage
  webpage <- read_html(url)
  Sys.sleep(2.5+1*runif(1))
  
  # date
  match_date <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-top > td.date") %>%
    html_text()
  
  # teams
  hometeam <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-top > td.team") %>% 
    html_attr('data-str')
  
  awayteam <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-bottom > td.team") %>% 
    html_attr('data-str')
  
  # probabilities
  home_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-top > td.prob") %>% 
    html_text()
  draw_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-top > td.prob.tie-prob") %>% 
    html_text()
  away_prob <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div > table > tbody > tr.match-bottom > td.prob") %>% 
    html_text()
  
  # adjust 'home_prob'
  home_prob <- home_prob[seq(1, length(home_prob), by=2)]
  
  
  # goal models
  AdjG_home <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(2) > td:nth-child(2)") %>% 
    html_text()
  AdjG_away <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(2) > td:nth-child(3)") %>% 
    html_text()
  
  ShotExG_home <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(3) > td:nth-child(2)") %>% 
    html_text()
  ShotExG_away <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(3) > td:nth-child(3)") %>% 
    html_text()
  
  NonShotExG_home <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr.moves > td:nth-child(2)") %>% 
    html_text()
  NonShotExG_away <- webpage %>% 
    html_nodes(css="#matches-table-wrapper > div.games-container.completed > div") %>% 
    html_nodes(css="div.additional-info-container > table > tbody > tr.moves > td:nth-child(3)") %>% 
    html_text()
  
  
  # clean and combine to dataframe
  previous_results <- tibble(
    match_date, 
    hometeam, 
    awayteam,home_prob = as.numeric(gsub("*%*<*", "", home_prob))/100, 
    draw_prob = as.numeric(gsub("*%*<*", "", draw_prob))/100, 
    away_prob = as.numeric(gsub("*%*<*", "", away_prob))/100,
    AdjG_home = as.numeric(AdjG_home), 
    AdjG_away = as.numeric(AdjG_away), 
    ShotExG_home = as.numeric(ShotExG_home), 
    ShotExG_away = as.numeric(ShotExG_away), 
    NonShotExG_home = as.numeric(NonShotExG_home), 
    NonShotExG_away = as.numeric(NonShotExG_away), 
    scrape_datetime = as.character(Sys.time()) 
  )
  
  # return
  previous_results
  
}




# 3. Scrape Data ----------------------------------------------------------

# create urls to be scraped
base_url <- "https://projects.fivethirtyeight.com/soccer-predictions/"

urls_list <- tibble(
  league_code = c("E0", "SP1", "D1", "I1", "F1", "CL1", "MLS"), 
  url = paste0(base_url, c("", "la-liga/", "bundesliga/", "serie-a/", "ligue-1/", "champions-league/", "mls/"))
)


urls_list



