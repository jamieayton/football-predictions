


# 1. Setup ----------------------------------------------------------------

# global options
options(stringsAsFactors = FALSE)

# packages
library('tidyverse')
library('rvest')

# dir
project_wd <- getwd()





# 2. Scrape Functions ---------------------------------------------------

get_upcoming_games <- function(url){
  # webpage
  webpage <- read_html(url)
  Sys.sleep(2.5+1*runif(1))
  
  # base_css
  base_css <- webpage %>% html_nodes(css="#matches-table-wrapper > div.games-container.upcoming > div")
  
  if (length(base_css) == 0){
    
    # output empty tibble
    predictions <- tibble(
      stage = NA, 
      match_date = NA,
      hometeam = NA,
      awayteam = NA,
      home_prob = NA,
      draw_prob = NA,
      away_prob = NA,
      scrape_datetime = as.character(Sys.time())
    )
    
  } else {
  
  # nodes to map through
  css_i <- paste0("#matches-table-wrapper > div.games-container.upcoming > div:nth-child(", seq_along(base_css), ")")
  
  # stage
  stage <- map_chr(
    css_i, 
    function(x) if(webpage %>% html_nodes(css=x) %>% html_attr('class') %>% grepl("match-container", .)){
      NA
    } else {
      webpage %>% html_nodes(css=x) %>% html_text()
    }
  )
  
  # date
  match_date <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.date") %>% 
      html_text() %>% ifelse(length(.) == 0, NA, .)
  )
  
  # teams
  hometeam <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.team") %>% 
      html_attr('data-str') %>% ifelse(length(.) == 0, NA, .)
  )
  awayteam <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-bottom > td.team") %>% 
      html_attr('data-str') %>% ifelse(length(.) == 0, NA, .)
  )
  
  # probabilities
  home_prob <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.prob") %>% 
      html_text() %>% ifelse(length(.) == 0, NA, .)
  )
  draw_prob <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.prob.tie-prob") %>% 
      html_text() %>% ifelse(length(.) == 0, NA, .)
  )
  away_prob <- map_chr(
    css_i, 
    function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="tbody > tr.match-bottom > td.prob") %>% 
      html_text() %>% ifelse(length(.) == 0, NA, .)
  )
  
  
  
  # clean and combine to dataframe
  predictions <- tibble(
    stage,
    match_date,
    hometeam,
    awayteam,
    home_prob = as.numeric(gsub("*%*<*", "", home_prob))/100,
    draw_prob = as.numeric(gsub("*%*<*", "", draw_prob))/100,
    away_prob = as.numeric(gsub("*%*<*", "", away_prob))/100,
    scrape_datetime = as.character(Sys.time())
  )
  
  # apply stage to rows below
  for (i in seq(1, nrow(predictions))){
    predictions$stage[i] <- ifelse(is.na(predictions$stage[i]) & i > 1, predictions$stage[i-1], predictions$stage[i])
  }
  
  # drop NAs
  predictions <- predictions %>% drop_na()

  }
  
  
  # return
  predictions
  
}





get_completed_games <- function(url){
  # webpage
  webpage <- read_html(url)
  Sys.sleep(2.5+1*runif(1))
  
  # base_css
  base_css <- webpage %>% html_nodes(css="#matches-table-wrapper > div.games-container.completed > div")
  
  if (length(base_css) == 0){
    
    # output empty tibble
    previous_results <- tibble(
      stage = NA, 
      match_date = NA, 
      hometeam = NA, 
      awayteam = NA,
      home_prob = NA, 
      draw_prob = NA, 
      away_prob = NA, 
      AdjG_home = NA, 
      AdjG_away = NA, 
      ShotExG_home = NA, 
      ShotExG_away = NA, 
      NonShotExG_home = NA, 
      NonShotExG_away = NA, 
      scrape_datetime = as.character(Sys.time())
    )
    
    
  } else {
    
    # nodes to map through
    css_i <- paste0("#matches-table-wrapper > div.games-container.completed > div:nth-child(", seq_along(base_css), ")")
    
    # stage
    stage <- map_chr(
      css_i, 
      function(x) if(webpage %>% html_nodes(css=x) %>% html_attr('class') %>% grepl("match-container", .)){
        NA
      } else {
        webpage %>% html_nodes(css=x) %>% html_text()
      }
    )
    
    # date
    match_date <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.date") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    
    # teams
    hometeam <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.team") %>% 
        html_attr('data-str') %>% ifelse(length(.) == 0, NA, .)
    )
    awayteam <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-bottom > td.team") %>% 
        html_attr('data-str') %>% ifelse(length(.) == 0, NA, .)
    )
    
    # probabilities
    home_prob <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.prob") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    draw_prob <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="table > tbody > tr.match-top > td.prob.tie-prob") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    away_prob <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="tbody > tr.match-bottom > td.prob") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    
    # goal models
    AdjG_home <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(2) > td:nth-child(2)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    AdjG_away <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(2) > td:nth-child(3)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    ShotExG_home <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(3) > td:nth-child(2)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    ShotExG_away <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr:nth-child(3) > td:nth-child(3)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    NonShotExG_home <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr.moves > td:nth-child(2)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )
    NonShotExG_away <- map_chr(
      css_i, 
      function(x) webpage %>% html_nodes(css=x) %>% html_nodes(css="div.additional-info-container > table > tbody > tr.moves > td:nth-child(3)") %>% 
        html_text() %>% ifelse(length(.) == 0, NA, .)
    )

    
    # clean and combine to dataframe
    previous_results <- tibble(
      stage,
      match_date, 
      hometeam, 
      awayteam,
      home_prob = as.numeric(gsub("*%*<*", "", home_prob))/100, 
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
    
    # apply stage to rows below
    for (i in seq(1, nrow(previous_results))){
      previous_results$stage[i] <- ifelse(is.na(previous_results$stage[i]) & i > 1, previous_results$stage[i-1], previous_results$stage[i])
    }
    
    # drop NAs
    previous_results <- previous_results %>% drop_na()
    
    
  }
  
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


upcoming_games <- map(urls_list$url, get_upcoming_games)
completed_games <- map(urls_list$url, get_completed_games)



