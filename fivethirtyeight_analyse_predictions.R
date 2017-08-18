


# 1. Setup ----------------------------------------------------------------

# global options
options(stringsAsFactors = FALSE)

# packages
library('tidyverse')
library('stringr')
library('rvest')

# dir
project_dir <- file.path(getwd())
data_dir <- file.path(paste0(getwd(), "/fivethirtyeight/"))



# 2. Import Files ---------------------------------------------------------

# import data from last season
completed_games <- read_csv(file.path(data_dir, paste0("completed_games", " - ", "2017-06-16", ".csv")))
upcoming_games <- read_csv(file.path(data_dir, paste0("upcoming_games", " - ", "2017-06-16", ".csv")))

# football data
football_data <- read_csv(file.path(project_dir, paste0("football_data", ".csv")))



# 3. Parse Data -----------------------------------------------------------

# parse data to match football_data leagues
completed_games <- completed_games %>% 
  select(stage, match_date, hometeam, awayteam, home_prob, draw_prob, away_prob, home_goals, away_goals, league_code) %>% 
  filter(league_code %in% c("E0", "D1", "F1", "I1", "SP1")) %>% 
  drop_na() %>% 
  mutate(FTR = case_when(home_goals > away_goals ~ "H", home_goals == away_goals ~ "D", home_goals < away_goals ~ "A")) %>% 
  mutate(total_prob = home_prob + draw_prob + away_prob) %>% 
  filter(is.na(total_prob) == FALSE)

# filter football_data
football_data <- football_data %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, PSH, PSD, PSA, BbMxH, BbMxD, BbMxA, BbAvH, BbAvD, BbAvA) %>% 
  filter(league_code %in% c("E0", "D1", "F1", "I1", "SP1")) %>% 
  filter(season_code == "16-17")

# exclusions list
exclusions_football_data <- football_data %>% 
  filter(is.na(PSH + PSD + PSA) | is.na(BbMxH + BbMxD + BbMxA) | is.na(BbAvH + BbAvD + BbAvA))

# NAs football_data
football_data <- football_data %>% 
  drop_na()

# filter completed games for NAs in football_data
completed_games <- completed_games %>% 
  filter(!((hometeam == "Metz" & awayteam == "Lyon") | (hometeam == "Bastia" & awayteam == "Rennes"))) %>% 
  filter(!((hometeam == "Fiorentina" & awayteam == "Pescara") | (hometeam == "Roma" & awayteam == "Genoa") | (hometeam == "Sampdoria" & awayteam == "Napoli")))


# check equivalence of matches & leagues
stopifnot(
  setequal(
    completed_games %>% 
      group_by(league_code) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)), 
    football_data %>% 
      group_by(league_code) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count))
  ) == TRUE
)


# football_data NA values

# # A tibble: 5 x 3
# HomeTeam AwayTeam bookmaker_code
# <chr>    <chr>          <chr>
# 1       Metz     Lyon             PS
# 2     Bastia   Rennes             PS
# 3 Fiorentina  Pescara             PS
# 4       Roma    Genoa             PS
# 5  Sampdoria   Napoli             PS



# 4. True Odds Methods ----------------------------------------------------

# odds_original
odds_original <- football_data %>% 
  gather(
    ., "bookmaker_code","odds_value", which(colnames(.) %in%  c("BbMxH", "BbMxD", "BbMxA", "BbAvH", "BbAvD", "BbAvA", "PSH", "PSD", "PSA"))
  ) %>% 
  mutate(
    odds_type = substring(bookmaker_code, nchar(bookmaker_code)), 
    bookmaker_code = substring(bookmaker_code, 1, nchar(bookmaker_code)-1)
  ) %>% 
  mutate(
    odds_value = as.numeric(odds_value)
  ) %>% 
  spread(
    ., odds_type, odds_value
  ) %>% 
  mutate(
    overround = 1/H + 1/D + 1/A, 
    favourite = if_else(H <= A, "home", "away")
  ) %>% 
  gather(
    ., odds_type, original_odds, which(colnames(.) %in% c("H", "D", "A"))
  ) %>% 
  rename("odds_value" = "original_odds") %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, overround, favourite, odds_type, odds_value)


# proportional: simply (1/original_odds) / overround
odds_proportional <- odds_original %>% 
  spread(odds_type, odds_value) %>% 
  mutate(
    H = 1/((1/H)/overround), # re-define odds values
    D = 1/((1/D)/overround), 
    A = 1/((1/A)/overround)
  ) %>% 
  mutate(overround = 1/H + 1/D + 1/A) %>% 
  gather(odds_type, odds_value, which(colnames(.) %in% c("H", "D", "A"))) %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, overround, favourite, odds_type, odds_value)


# solver function to output parameter c
ratio_odds_solver <- function(odds_1, odds_2, odds_3, overround){
  
  # check inputs are all numeric, are finite & length == 1
  stopifnot(all(length(odds_1) == 1, length(odds_2) == 1, length(odds_3) == 1, length(overround) == 1))
  stopifnot(all(is.finite(odds_1), is.finite(odds_2), is.finite(odds_3), is.finite(overround)))
  
  # adjust odds to probabilities
  p <- 1/odds_1
  q <- 1/odds_2
  r <- 1/odds_3
  
  # solver function: minimise 'x + y - v' to find c such that x + y = v
  find_c <- function(c){
    abs((c*p/(1-p+c*p)) + (c*q/(1-q+c*q)) + (c*r/(1-r+c*r)) - overround)
  }
  
  # use optim to solve with lower bound to prevent -ve roots
  optimised_solver <- optim(
    par = c(1), fn = find_c, method = "Brent", lower = 1e-03, upper = 1e+03
  )
  
  # output parameter from optim
  optimised_solver$par[1]
}

# find c for each then calculate odds
odds_ratio <- odds_original %>% 
  spread(odds_type, odds_value) %>% 
  mutate(
    c = pmap_dbl(list(.$H, .$D, .$A, rep(1, nrow(.))), ratio_odds_solver)  # overround = 1 provided as this is our desired value that 'c' corresponds to
  ) %>% 
  gather(
    odds_type, original_odds, which(colnames(.) %in% c("H", "D", "A"))  # use original_odds to distinguish between original odds and new odds created
  ) %>% 
  mutate(
    original_prob = 1/original_odds, 
    ratio_prob = c*original_prob/(1-original_prob+c*original_prob), 
    ratio_odds = 1/ratio_prob
  ) %>% 
  select(
    league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, favourite, odds_type, ratio_odds
  ) %>% 
  spread(odds_type, ratio_odds) %>% 
  mutate(
    overround = 1/H + 1/D + 1/A
  ) %>% 
  gather(
    odds_type, odds_value, which(colnames(.) %in% c("H", "D", "A"))
  ) %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, overround, favourite, odds_type, odds_value)


# solver function to simply output required 'lambda'
power_odds_solver <- function(odds_1, odds_2, odds_3){
  
  # check inputs are all numeric & length == 1
  stopifnot(all(is.numeric(odds_1), is.numeric(odds_2), is.numeric(odds_3)))
  stopifnot(all(length(odds_1) == 1, length(odds_2) == 1, length(odds_3) == 1))
  
  # adjust odds to probabilities
  p <- 1/odds_1
  q <- 1/odds_2
  r <- 1/odds_3
  
  # find lambda function
  find_lambda <- function(lambda){
    abs(p^(lambda) + q^(lambda) + r^(lambda) - 1)  # desired overround of value 1L
  }
  
  # parameter range
  min_par <- log(1/3)/log(min(p,q,r))
  max_par <- log(1/3)/log(max(p,q,r))
  
  # use optim to solve with lower bound to prevent -ve roots
  optimised_solver <- optimize(find_lambda, c(min_par, max_par), tol = 0.00001)
  
  # output parameter from optim
  optimised_solver$minimum[1]
  
}

# find lambda for each then calculate odds
odds_power <- odds_original %>% 
  spread(odds_type, odds_value) %>% 
  mutate(
    lambda = pmap_dbl(list(.$H, .$D, .$A), power_odds_solver)
  ) %>% 
  gather(
    odds_type, original_odds, which(colnames(.) %in% c("H", "D", "A"))  # re-name odds_value to original_odds to distinguish between new created odds
  ) %>% 
  mutate(
    original_prob = 1/original_odds, 
    power_prob = original_prob^(lambda), 
    power_odds = 1/power_prob
  ) %>% 
  select(
    league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, favourite, odds_type, power_odds
  ) %>% 
  spread(odds_type, power_odds) %>% 
  mutate(
    overround = 1/H + 1/D + 1/A
  ) %>% 
  gather(
    odds_type, odds_value, which(colnames(.) %in% c("H", "D", "A"))   # re-name power_odds to odds_value
  ) %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, overround, favourite, odds_type, odds_value)

       

# 5. Combine Odds Methods -------------------------------------------------

# combine
odds_methods <- bind_rows(
    odds_original %>% mutate(odds_method = "odds_original"), 
    odds_power %>% mutate(odds_method = "odds_power"), 
    odds_proportional %>% mutate(odds_method = "odds_proportional"), 
    odds_ratio %>% mutate(odds_method = "odds_ratio")
  ) %>% 
  select(league_code, season_code, Date, HomeTeam, AwayTeam, FTR, bookmaker_code, overround, favourite, odds_method, odds_type, odds_value)

# rm created df
rm(odds_original, odds_power, odds_proportional, odds_ratio)

# rm functions
rm(ratio_odds_solver, power_odds_solver)




# 6. Combine Completed Games ----------------------------------------------

# adjust format
completed_games <- completed_games %>% 
  mutate(
    season_code = "16-17", 
    bookmaker_code = "fivethirtyeight", 
    favourite = if_else(home_prob > away_prob, "home", "away"), 
    H = 1/home_prob, 
    D = 1/draw_prob, 
    A = 1/away_prob, 
    odds_method = "odds_original"
  ) %>% 
  select(
    league_code, season_code, Date = match_date, HomeTeam = hometeam, AwayTeam = awayteam, FTR, bookmaker_code, overround = total_prob, 
    favourite, odds_method, H, D, A
  ) %>% 
  gather(odds_type, odds_value, which(colnames(.) %in% c("H", "D", "A")))


# add proportional
completed_games_proportional <- completed_games %>% 
  mutate(odds_method = "odds_proportional", odds_value = 1/((1/odds_value)/overround))


# combine to existing data
odds_methods <- bind_rows(
  odds_methods, completed_games, completed_games_proportional
)




# 7. RPS Values -----------------------------------------------------------

# add rps component
rps_comparison <- odds_methods %>% 
  spread(odds_type, odds_value) %>% 
  mutate(
    rps_H_component = (if_else(FTR == "H", 1, 0) - 1/H)^2, 
    rps_D_component = (if_else(FTR == "D", 1, 0) - 1/D)^2, 
    rps_A_component = (if_else(FTR == "A", 1, 0) - 1/A)^2
  ) %>% 
  mutate(
    rps = (rps_H_component + rps_D_component + rps_A_component)*1/(3-1)
  )


# compare rps values
rps_summary <- rps_comparison %>% 
  group_by(odds_method, bookmaker_code) %>% 
  summarize(
    count = n(), 
    rps = mean(rps), 
    rps_H_component = mean(rps_H_component), 
    rps_D_component = mean(rps_D_component), 
    rps_A_component = mean(rps_A_component)
  ) %>% 
  arrange(rps)


# rps comparison
rps_summary %>% 
  filter(odds_method == "odds_original")



# 8. Cleanup --------------------------------------------------------------

# rm dir
rm(project_dir, data_dir)

# rm df
rm(completed_games, completed_games_proportional)
rm(exclusions_football_data, football_data)
rm(odds_methods)
rm(rps_comparison, rps_summary)
rm(upcoming_games)



# 9. Summary --------------------------------------------------------------

# summary:
#
# fivethrityeight odds are less accurate than bookmaker odds across the board - even when adjusted for the rounding error (overround = 0.99 | 1.01)
#


