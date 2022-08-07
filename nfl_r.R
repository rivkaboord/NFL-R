library(tidyverse)
library(nflfastR)
library(nflreadr)
library(gt)
library(ggimage)

## load 2021 play by play data
pbp <- load_pbp(seasons = most_recent_season())

pbp %>% glimpse()

library(nflverse)
library(forcats)

## load Elijah Moore receiving plays 2021
e_moore_pass_plays_2021 <- pbp %>% 
    filter(posteam == "NYJ" & play_type == "pass" & receiver == "E.Moore") %>% glimpse()

## filter Moore pass plays to only reception-relevant info
e_moore_pass_plays_2021_trimmed <- 
    e_moore_pass_plays_2021 %>% 
    select(receiver, shotgun, pass_length, pass_location, air_yards, yards_after_catch, first_down,
           complete_pass, incomplete_pass, interception, receiving_yards, third_down_converted,
           third_down_failed, fourth_down_converted, fourth_down_failed, cp,
           cpoe, epa, air_epa, yac_epa, comp_air_epa, comp_yac_epa, qb_epa, xyac_epa,
           xyac_mean_yardage, xyac_median_yardage, xyac_success, xyac_fd, xpass, pass_oe)

e_moore_pass_plays_2021_trimmed %>% 
    select(receiver, complete_pass, air_yards, yards_after_catch, first_down, cp, cpoe, epa, air_epa, yac_epa)

## filter out NA data from Moore receiving data
e_moore_pass_plays_2021_trimmed_no_na <- 
    e_moore_pass_plays_2021_trimmed %>% 
    mutate_all(~replace(., is.na(.), 0))

e_moore_avg_analytics_epa <-
    e_moore_pass_plays_2021_trimmed_no_na %>% 
    summarize(avg_air_yards_per_target = mean(air_yards),
              avg_yards_after_catch    = mean(yards_after_catch),
              qb_cp_on_target          = mean(cp),
              qb_cpoe_on_target        = mean(cpoe),
              total_epa                = sum(epa),
              total_air_epa            = sum(air_epa),
              total_yac_epa            = sum(yac_epa),
              first_down_pct           = (sum(first_down) / sum(complete_pass) * 100) %>% round(digits = 2)
              )

e_moore_analytics_by_pass_location <- 
    e_moore_pass_plays_2021_trimmed_no_na %>% 
    select(pass_length, pass_location, complete_pass, air_yards, yards_after_catch, first_down, interception, cp, cpoe, epa, air_epa, xpass, pass_oe) %>% 
    filter(!pass_location == 0) %>% 
    group_by(pass_location) %>% 
    summarize(avg_air_yds         = mean(air_yards),
              avg_yds_after_catch = mean(yards_after_catch),
              total_ints          = sum(interception),
              avg_cp              = mean(cp)*100,
              avg_cpoe            = mean(cpoe),
              avg_epa_per_play    = mean(epa),
              total_first_downs   = sum(first_down),
              first_down_pct      = (total_first_downs / sum(complete_pass) * 100) %>% round(digits = 2) 
              )

e_moore_analytics_by_pass_location

pbp %>% 
    filter(passer_player_name == "Z.Wilson" & pass_attempt == 1) %>% 
    select(down, ydstogo, ydsnet, yards_gained, shotgun, pass_length, pass_location, air_yards, yards_after_catch)

## Reading in standings from nflfastr ----

home <- nflfastR::fast_scraper_schedules(2012:2021) %>%
    filter(game_type == 'REG') %>% 
    select(game_id, season, week, home_team, home_result) %>% 
    rename(team = home_team,
           result = home_result)

away <- nflfastR::fast_scraper_schedules(2012:2021) %>%
    filter(game_type == 'REG') %>% 
    select(game_id, season, week, away_team, home_result) %>% 
    rename(team = away_team,
           result = home_result) %>% 
    mutate(result = -result)

# converting scores to wins and losses
results <- bind_rows(home, away) %>% 
    arrange(week) %>% 
    mutate(win = case_when(
        result > 0 ~ 1,
        result < 0 ~ 0,
        result == 0 ~ 0.5
        )
    )

results

# binding wins to teams over a specific season
team_wins <- results %>% 
    
    group_by(team, season) %>% 
    
    summarize(
        wins = sum(win == 1),
        losses = sum(win == 0),
        ties = sum(win == 0.5)
    ) %>% 
    
    ungroup() %>%
    
    arrange(season) %>% 
    mutate(win_pct = wins / (wins + losses) %>% round(3))

team_wins

## Trying to aggregate 2012-21 standings with imported data from Pro Football Reference ----

library(rvest)

# afc_standings_2021 <- read_html("https://www.pro-football-reference.com/years/2021/") %>% 
#     html_element("table") %>% 
#     html_table()
# 
# afc_standings_2021$`W-L%` <- afc_standings_2021$`W-L%` %>% as.numeric()
# 
# afc_standings_2021_cleaned <-
#     afc_standings_2021[-c(1,6,11,16),] %>% 
#     select(Tm, W, L, "T", 'W-L%') %>% 
#     mutate(W = as.numeric(W),
#            L = as.numeric(L),
#            "T" = as.numeric("T") %>% replace_na(replace = 0),
#            Tm = Tm %>%
#                str_replace("\\+", "") %>%
#                str_replace("\\*", "") %>% 
#                word(-1))
# 
# afc_standings_2021_cleaned

# Reading in standings from Pro Football Reference ----
# tables_2021 <- read_html("https://www.pro-football-reference.com/years/2021/") %>%
#     html_table(fill = TRUE)
# 
# afc_2021 <- tables_2021[[1]]
# nfc_2021 <- tables_2021[[2]]
# 
# afc_2021$`W-L%` <- afc_2021$`W-L%` %>% as.numeric()
# 
# afc_2021_cleaned <- afc_2021[-c(1,6,11,16),] %>%
#     select(Tm, W, L, "T", 'W-L%') %>%
#     mutate(W = as.numeric(W),
#            L = as.numeric(L),
#            "T" = as.numeric("T") %>% replace_na(replace = 0),
#            Tm = Tm %>%
#                str_replace("\\+", "") %>%
#                str_replace("\\*", "") %>%
#                word(-1))
# 
# afc_2021_cleaned
# 
# nfc_2021$`W-L%` <- nfc_2021$`W-L%` %>% as.numeric()
# 
# nfc_2021_cleaned <- nfc_2021[-c(1,6,11,16),] %>%
#     select(Tm, W, L, "T", 'W-L%') %>%
#     mutate(W = as.numeric(W),
#            L = as.numeric(L),
#            "T" = as.numeric("T") %>% replace_na(replace = 0),
#            Tm = Tm %>%
#                str_replace("\\+", "") %>%
#                str_replace("\\*", "") %>%
#                word(-1))
# 
# nfc_2021_cleaned
# 
# nfl_2021 <- afc_2021_cleaned %>%
#     full_join(nfc_2021_cleaned)
# 
# nfl_2021

# standings_finder  <- function(year) {
# 
#     tables_yearly <- read_html(paste0("https://www.pro-football-reference.com/years/", year)) %>%
#         html_table(fill = TRUE) %>% 
#         mutate(year = {{ year }} )
# 
#     return(tables_yearly)
# }
# 
# nfl_standings_12_21 <- map_df(2012:2021, standings_finder)
# 
# nfl_standings_12_21 <- nfl_standings_12_21[-seq(1, nrow(nfl_standings_12_21), by = 5) ,]
# # 
# nfl_standings_12_21

## Importing DVOA data ----

library(readr)

# read_dvoa() function to batch import dvoa data ----
read_dvoa <- function(year, game_facet) {
    tables_yearly <- read_csv(
        paste0("C://Documents and Settings/rivki/Documents/Programming and Data Science/R/NFL/DVOA stats/2012-21 DVOA by team/", year, 
               " Team DVOA Ratings ", game_facet, ".csv")
    ) %>% 
        mutate(year = year)
    
    return(tables_yearly)
}

dump(list = c('read_dvoa'), file = 'C://Documents and Settings/rivki/Documents/Programming and Data Science/R/NFL/read_dvoa.r', append = TRUE)

# importing defensive dvoa data using read_dvoa() ----

defense_dvoa_12_21 <-  
    map_df(.x = 2012:2021, game_facet = "Defense", .f = read_dvoa) %>% 
    
    select(1:10, 17:21) %>% 
    rename(team                     = 'Team',
           total_def_dvoa_rank      = 'Total DVOA Rank',
           total_def_dvoa           = 'Total DVOA',
           prev_year_rank           = 'Prev. Year Rank',
           weighted_def_dvoa_rank   = 'Weighted DVOA Rank',
           weighted_def_dvoa        = 'Weighted DVOA',
           pass_def_dvoa_rank       = 'Pass DVOA Rank',
           pass_def_dvoa            = 'Pass DVOA',
           rush_def_dvoa_rank       = 'Rush DVOA Rank',
           rush_def_dvoa            = 'Rush DVOA',
           variance_def_rank        = 'Variance Rank',
           schedule_rank            = 'Schedule Rank',
           schedule                 = 'Schedule') %>% 
    select(team, year, everything())

offense_dvoa_12_21 <- 
    map_df(.x = 2012:2021, game_facet = "Offense", .f = read_dvoa) %>%
    
    select(1:10, 17:21) %>% 
    rename(team                     = 'Team',
           total_off_dvoa_rank      = 'Total DVOA Rank',
           total_off_dvoa           = 'Total DVOA',
           prev_year_off_rank       = 'Prev. Year Rank',
           weighted_off_dvoa_rank   = 'Weighted DVOA Rank',
           weighted_off_dvoa        = 'Weighted DVOA',
           pass_off_dvoa_rank       = 'Pass DVOA Rank',
           pass_off_dvoa            = 'Pass DVOA',
           rush_off_dvoa_rank       = 'Rush DVOA Rank',
           rush_off_dvoa            = 'Rush DVOA',
           variance_off_rank        = 'Variance Rank',
           schedule_rank            = 'Schedule Rank',
           schedule                 = 'Schedule') %>% 
    select(team, year, everything())

team_dvoa_12_21 <- map_df(.x = 2012:2021, game_facet = "Overall", .f = read_dvoa) %>% 
    rename(team = Team) %>% 
    select(team, year, everything())

offense_dvoa_12_21
defense_dvoa_12_21
team_dvoa_12_21

full_dvoa_12_21_joined <- offense_dvoa_12_21 %>% 
    group_by(team, year) %>% 
    inner_join(defense_dvoa_12_21 %>% group_by(team, year), by = c('team', 'year')) %>% 
    ungroup()

record_off_def_dvoa_12_21 <- team_wins %>% 
    rename(year = season) %>% 
    group_by(team, year) %>% 
    inner_join(full_dvoa_12_21_joined, by = c("team", "year")) %>% 
    ungroup()

record_off_def_dvoa_12_21

# different rush defense DVOA scenarios and resulting win totals
record_off_def_dvoa_12_21 %>% 
    select(wins, rush_def_dvoa_rank) %>% 
    filter(rush_def_dvoa_rank > 24) %>% 
    arrange(desc(wins)) %>% 
    summarize(
        avg_wins = mean(wins),
        median_wins = median(wins),
        IQR(wins)
    )

record_off_def_dvoa_12_21 %>% 
    select(wins, pass_def_dvoa_rank) %>% 
    filter(pass_def_dvoa_rank > 24) %>% 
    arrange(desc(wins)) %>% 
    summarize(
        avg_wins = mean(wins),
        median_wins = median(wins),
        IQR(wins)
    )

record_off_def_dvoa_12_21 %>% 
    select(wins, rush_def_dvoa_rank, pass_def_dvoa_rank) %>% 
    filter(pass_def_dvoa_rank <= 24 & pass_def_dvoa_rank >= 9) %>% 
    arrange(desc(wins)) %>% 
    summarize(
        avg_wins = mean(wins),
        median_wins = median(wins),
        IQR(wins)
    )

record_off_def_dvoa_12_21 %>% 
    select(wins, losses, rush_def_dvoa_rank, pass_def_dvoa_rank, pass_off_dvoa_rank, rush_off_dvoa_rank) %>% 
    filter(rush_def_dvoa_rank > 16 & rush_def_dvoa_rank <= 24 & wins >= 8 & wins < 10) %>% 
    arrange(desc(wins)) %>% 
    summarize(
        avg_pass_def_dvoa_rank = mean(pass_def_dvoa_rank),
        avg_pass_off_dvoa_rank = mean(pass_off_dvoa_rank),
        avg_rush_off_dvoa_rank = mean(rush_off_dvoa_rank)
    )

record_off_def_dvoa_12_21 %>% 
    select(wins, losses, rush_def_dvoa_rank) %>% 
    filter(rush_def_dvoa_rank > 24 & wins >= losses)

# Aggregating EPA data for 2012:2021 seasons

pbp_2 <- load_pbp(2012:2021) %>%
    filter(rush == 1 | pass == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>%
    select(season, posteam, pass, defteam, epa)

team_epa_offense_12_21 <- pbp_2 %>%
    group_by(posteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>%
    pivot_wider(names_from = pass, values_from = epa) %>%
    rename(off_pass_epa = `1`, off_rush_epa = `0`,
           team = posteam) %>% 
    ungroup()

team_epa_defense_12_21 <- pbp_2 %>%
    group_by(defteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>%
    pivot_wider(names_from = pass, values_from = epa) %>%
    rename(def_pass_epa = `1`, def_rush_epa = `0`,
           team = defteam) %>% 
    ungroup()

team_epa_off_def_wins_12_21 <- team_epa_offense_12_21 %>% 
    group_by(team, season) %>% 
    inner_join(team_epa_defense_12_21 %>% group_by(team, season), by = c("team", "season")) %>% 
    inner_join(team_wins, by = c("team", "season")) %>%
    ungroup() %>% 
    group_by(season) %>% 
    mutate(off_rush_epa_rank = rank(off_rush_epa),
           off_pass_epa_rank = rank (off_pass_epa),
           def_rush_epa_rank = rank(-def_rush_epa),
           def_pass_epa_rank = rank(-def_pass_epa)) %>% 
    ungroup()

team_epa_off_def_wins_12_21 %>% 
    select(def_rush_epa_rank, def_pass_epa_rank, off_rush_epa_rank, off_pass_epa_rank, wins, losses) %>% 
    filter(def_rush_epa_rank < 24 & def_rush_epa_rank > 16 & wins >= 10) %>% 
    summarize(mean_off_rush_epa_rank = mean(off_rush_epa_rank),
              mean_off_pass_epa_rank = mean(off_pass_epa_rank),
              mean_def_pass_epa_rank = mean(def_pass_epa_rank))

team_epa_off_def_wins_12_21 %>% 
    select(def_pass_epa_rank, wins) %>% 
    filter(def_pass_epa_rank < 24 & def_pass_epa_rank > 9) %>% 
    summarize(mean_wins = mean(wins))

## Importing Defensive Redzone Data ----

library(readxl)

read_redzone_def <- function(year) {
    
    redzone_def_data <- 
        read_excel(path = paste0("C://Documents and Settings/rivki/Documents/Programming and Data Science/R/NFL/Red Zone Defense 2012-21/",
                                 year, ".xlsx"),
                   skip = 1, n_max = 32) %>%  
        mutate(season = {{ year }})
}

def_conv_pct_12_21 <- map_df(2012:2021, read_redzone_def)

redzone_def_ranked_12_21 <- def_conv_pct_12_21 %>% 
    select(Tm, season, RZAtt, RZTD, RZPct) %>% 
    rename(team = Tm) %>% 
    group_by(season) %>% 
    mutate(rz_rank = rank(RZPct)) %>%
    arrange(rz_rank) %>% 
    ungroup()

redzone_def_ranked_12_21

team_abbrev_key <- redzone_def_ranked_12_21 %>% 
    distinct(team) %>% 
    bind_cols(
        team_key = c('DEN', 'SF', 'HOU', 'CAR', 'PIT', 'ARI', 'IND', 'SEA', 'DET', 'CHI',
                     'CIN', 'KC', 'STL', 'NO', 'DAL', 'ATL', 'NE', 'BAL', 'OAK', 'PHI', 'GB',
                     'NYJ', 'NYG', 'TEN', 'CLE', 'MIA', 'TB', 'SD', 'JAX', 'BUF', 'WAS', 'MIN',
                     'LAR', 'LAC', 'WFT', 'LVR')
    )

team_redzone_def_wins_12_21 <- 
    
    redzone_def_ranked_12_21 %>% 
    inner_join(team_abbrev_key, by = c("team")) %>% 
    select(-team) %>% 
    rename(team = team_key) %>% 
    select(team, everything()) %>% 
    
    inner_join(team_wins, by = c("team", "season")) %>% 
    inner_join(full_dvoa_12_21_joined %>% rename(season = year),
               by = c("team", "season"))


team_redzone_def_wins_12_21 %>% 
    select(rz_rank, wins, rush_def_dvoa_rank, pass_def_dvoa_rank,
           rush_off_dvoa_rank, pass_off_dvoa_rank) %>% 
    filter(rush_def_dvoa_rank < 24 & rush_def_dvoa_rank > 9) %>% 
    summarize(avg_rz_rank = mean(rz_rank))

team_redzone_def_wins_12_21 %>% 
    select(rz_rank, wins, losses, rush_def_dvoa_rank, pass_def_dvoa_rank,
           rush_off_dvoa_rank, pass_off_dvoa_rank) %>% 
    filter(rush_def_dvoa_rank > 24 & wins >= 10) %>% 
    summarize(avg_rz_rank = mean(rz_rank))

# # get espnscrapeR
# install.packages("remotes")
# remotes::install_github("jthomasmock/espnscrapeR")

## Looking at Jets' 4th down plays

pbp %>% 
    filter(down == 4 & posteam == "NYJ" & (play_type == "run" | play_type == "pass")) %>% 
    group_by(play_type) %>% 
    summarize(avg_yds_to_go = mean(ydstogo),
              avg_field_position = mean(yardline_100))

pbp %>% 
    filter(down == 4 & posteam == "NYJ" & (play_type == "punt" | play_type == "field_goal")
           & yardline_100 >= 50 & ydstogo <= 5)
