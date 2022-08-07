library(tidyverse)
library(nflfastR)

pbp_21 <- load_pbp(2021)

nyj_pos_21 <- pbp_21 %>% 
    filter(
        posteam == 'NYJ',
        !play_type == 'kickoff',
        !play_type == 'punt',
        !play_type == 'no_play',
        !play_type == 'field_goal',
        !play_type == 'extra_point',
        !play_type == 'qb_kneel',
        !play_type == 'qb_spike'
        )

nyj_first_half_play_calling <- 
    nyj_pos_21 %>% 
    filter(qtr == 1 | qtr == 2) %>% 
    mutate(
        distance_type =
        case_when(
            ydstogo <= 3 ~ '<= 3 ydstogo',
            ydstogo > 3 & ydstogo <= 6 ~ '4-6 ydstogo',
            ydstogo >= 7 ~ '>=7 ydstogo'
        )
    ) 

nyj_first_half_pc_and_success <- nyj_first_half_play_calling %>%
    select(distance_type, ydstogo, yards_gained, everything()) %>%
    mutate(
        result = case_when(
                yards_gained >= ydstogo ~ '1D',
                yards_gained < ydstogo ~ 'next down',
                TRUE ~ 'error'
            )
    ) %>% 
    group_by(down, distance_type, play_type, result) %>%
    count() %>%
    ungroup()

nyj_pos_21 %>% 
    filter(qtr == 3 | qtr == 4) %>% 
    mutate(
        distance_type =
            case_when(
                ydstogo <= 3 ~ '<= 3 ydstogo',
                ydstogo > 3 & ydstogo <= 6 ~ '4-6 ydstogo',
                ydstogo >= 7 ~ '>=7 ydstogo'
            )
    ) %>%
    select(distance_type, everything()) %>%
    group_by(play_type) %>%
    count() %>% 
    View()



nyj_pos_21 %>% 
    filter(down == 3 & game_half == 'Half1' & play_type == 'pass') %>% 
    select(defteam, side_of_field, quarter_seconds_remaining, qtr, down, goal_to_go, time, yrdln, ydstogo, desc,
           play_type, yards_gained, shotgun, qb_scramble, air_yards, pass_location) %>% 
    mutate(result = 
        case_when(
            air_yards >= ydstogo ~ 'threw past the sticks',
            air_yards < ydstogo ~ 'did not throw past the sticks',
            TRUE ~ 'sack'
        )
    ) %>% 
    group_by(result) %>% 
    count()

nyj_first_half_pc_and_success %>%
    select(distance_type, ydstogo, yards_gained, everything()) %>%
    filter(down == 3 & game_half == 'Half1') %>% 
    mutate(
        first_down_gained =
            case_when(
                yards_gained >= ydstogo ~ 'Y',
                yards_gained < ydstogo ~ 'N',
                TRUE ~ 'error'
        ),
        past_sticks = 
            case_when(
                air_yards >= ydstogo ~ 'threw past the sticks',
                air_yards < ydstogo ~ 'did not throw past the sticks',
                TRUE ~ 'sack'
                )
    ) %>% 
    group_by(down, past_sticks, first_down_gained) %>% 
    count()

# goal-to-go situations
nyj_pos_21 %>% 
    filter(game_half == 'Half1' & goal_to_go == 1) %>% 
    mutate(
        distance_type =
            case_when(
                ydstogo <= 3 ~ '<= 3 ydstogo',
                ydstogo > 3 & ydstogo <= 6 ~ '4-6 ydstogo',
                ydstogo >= 7 ~ '>=7 ydstogo'
            )
    ) %>% 
    group_by(down, distance_type, play_type, touchdown) %>% 
    count()

nyj_series_close_21 <- nyj_pos_21 %>% 
    filter(score_differential >= -8 & game_seconds_remaining > 300) %>% 
    select(week, series, play_type, series_success, yards_gained, ydstogo)

nyj_series_close_21 %>% 
    filter(lead(x = series, n = 1) != series)

series_breakdown <- nyj_series_close_21 %>% 
    group_by(week, series) %>% 
    summarize(play_selection = toString(play_type)) %>% 
    ungroup() %>% 
    separate(
        play_selection, into = c('play_1', 'play_2', 'play_3', 'play_4', 'play_5'), sep = ', '
        )

series_breakdown %>% 
    filter(play_1 == 'run' & play_2 == 'run') %>% 
    count()

series_breakdown %>% 
    group_by(play_1) %>% 
    count()

first_down_play_calling <- nyj_series_close_21 %>% 
    filter(lag(x = series, n = 1) != series) 

first_down_play_calling_run <- 
    first_down_play_calling %>% 
    filter(play_type == 'run') %>% 
    mutate(
        success_rate =
            case_when(
                yards_gained >= (ydstogo * 0.4) ~ 'success',
                yards_gained < (ydstogo * 0.4) ~ 'failure',
        )
    )

first_down_play_calling_pass <- 
    first_down_play_calling %>% 
    filter(play_type == 'pass')

first_down_play_calling_run %>% 
    group_by(success_rate) %>% 
    count()

series_breakdown %>% 
    inner_join(first_down_play_calling_run, by = c('week', 'series')) %>% 
    filter(play_1 == 'run' & play_2 == 'run') %>% 
    group_by(success_rate) %>% 
    count()

    