library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

## Creating visualization for Team EPA and QB Aggressiveness ----

pbp <- load_pbp(2020:2021)

pbp %>% head()

pbp %>% 
    select(posteam, defteam, season, down, ydstogo, rush, pass, yards_gained) %>% 
    head()

## get the column names
names(pbp)

## number of rows in dataset
nrow(pbp)

# pbp %>% View()

pbp_rp <- pbp %>% 
    filter(rush == 1 | pass == 1) %>% 
    filter(!is.na(epa))

pbp_rp %>%
    filter(posteam == "DAL") %>% 
    group_by(passer_player_name) %>%
    filter(!is.na(passer_player_name)) %>% 
    summarize(passes = n(),
              avg_epa = mean(epa)) %>% 
    filter(passes >= 10) %>% 
    arrange(-avg_epa)

offenses_20 <- pbp_rp %>% 
    filter(season == 2020) %>% 
    group_by(posteam) %>% 
    summarize(epa_20 = mean(epa))

offenses_21 <- pbp_rp %>% 
    filter(season == 2021) %>% 
    group_by(posteam) %>% 
    summarize(epa_21 = mean(epa))

offenses_all <- offenses_20 %>% 
    left_join(offenses_21, by = "posteam")

offenses_all <- offenses_all %>% 
    left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

offenses_all %>% 
    ggplot(aes(x = epa_20,
               y = epa_21)) +
    geom_hline(yintercept = mean(offenses_all$epa_21), linetype = "dashed") +
    geom_vline(xintercept = mean(offenses_all$epa_20), linetype = "dashed") +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    theme_bw() +
    labs(x = "Offensive EPA/Play 2020",
         y = "Offensive EPA/Play 2021",
         title = "Offensive EPA/Play 2020 vs. 2021",
         caption = "by Rivka Boord @ Jets X-Factor") +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

ggsave('off-epa-20-21.png', width = 14, height = 10, dpi = "retina")

pbp_rp <- pbp_rp %>% 
    mutate(yards_past_sticks = air_yards - ydstogo)

qb_agg <- pbp_rp %>% 
    filter(!is.na(yards_past_sticks)) %>% 
    filter(down %in% c(3, 4)) %>% 
    group_by(passer_player_name) %>% 
    summarize(passes = n(),
              avg_yps = mean(yards_past_sticks),
              team_abbr = last(posteam)) %>% 
    filter(passes >= 70) %>% 
    left_join(teams_colors_logos, by = "team_abbr")

qb_agg %>% 
    ggplot(aes(avg_yps, fct_reorder(passer_player_name, avg_yps))) +
    geom_bar(aes(fill = team_color, color = team_color2),
             stat = "identity", alpha = 0.8) +
    theme_bw() +
    scale_color_identity(aesthetics = c("fill", "color")) +
    labs(x = "Average Late-Down Yards Past Sticks",
         y = "Passer Name",
         title = "How Aggressive Each Quarterback is on Late Downs",
         subtitle = "2020-21, minimum of 70 passes to qualify") +
    theme(legend.position = "none",
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          panel.grid.major.y = element_line(size = 0.1))

qb_gt <- qb_agg %>% 
    mutate(avg_yps = round(avg_yps, 2)) %>% 
    arrange(-avg_yps) %>% 
    mutate(rank = row_number()) %>% 
    select(rank, passer_player_name, avg_yps)

gt_save <- qb_gt %>% 
    gt() %>% 
    cols_label(passer_player_name = "Passer",
         avg_yps = "Late-Down Yards Past Sticks") %>% 
    tab_header(title = "Late-Down Yards Past Sticks")

gtsave(gt_save, "gt_save.png")

## Creating a Linear Model Using NFLFastR ----

library(ggrepel)

## Reading in standings from nflfastr ----

home <- nflfastR::fast_scraper_schedules(seasons = 1999:2021) %>%
    filter(game_type == 'REG') %>% 
    select(game_id, season, week, home_team, home_result) %>% 
    rename(team = home_team,
           result = home_result)

away <- nflfastR::fast_scraper_schedules(1999:2021) %>%
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
        ties = sum(win == 0.5),
        point_diff = sum(result)
    ) %>% 
    
    ungroup() %>%
    
    arrange(season) %>% 
    mutate(win_pct = wins / (wins + losses) %>% round(3))

pbp_2 <- load_pbp(1999:2021) %>%
    filter(rush == 1 | pass == 1, season_type == "REG",
           !is.na(posteam), !is.na(epa), posteam != "") %>% 
    select(season, posteam, pass, defteam, epa)

offense <- pbp %>% 
    group_by(posteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>% 
    pivot_wider(names_from = pass, values_from = epa) %>% 
    rename(off_pass_epa = '1', off_rush_epa = '0')

defense <- pbp %>% 
    group_by(defteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>% 
    pivot_wider(names_from = pass, values_from = epa) %>% 
    rename(def_pass_epa = '1', def_rush_epa = '0')

team_wins <- team_wins %>% 
    mutate(
        team = case_when(
            team == 'OAK' ~ 'LV',
            team == 'SD' ~ 'LAC',
            team == 'STL' ~ 'LA',
            TRUE ~ team
        )
    )

data <- team_wins %>% 
    left_join(offense, by = c('team' = 'posteam', 'season')) %>% 
    left_join(defense, by = c('team' = 'defteam', 'season'))

data
