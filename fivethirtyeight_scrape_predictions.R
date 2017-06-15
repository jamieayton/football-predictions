


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
  Sys.sleep(2.5+1*runif(1))
  webpage <- read_html(url)
  
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
    scrape_datetime = Sys.time()
  )
  
  # return
  predictions
  
}



