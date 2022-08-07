library(nflfastR)
library(tidyverse)

# some Corey Davis stats ----
pbp <- load_pbp(2017:2021)

c_davis_plays_21 <- 
    pbp %>% 
    filter(season == 2021 & posteam == 'NYJ' & play_type == "pass" & grepl('84-C.Davis', desc) & season_type == 'REG')

c_davis_plays_21 %>% 
    group_by(pass_location) %>% 
    count()

c_davis_plays <- 
    pbp %>% 
    filter(
        (posteam == 'NYJ' | posteam == 'TEN') & play_type == "pass" & grepl('84-C.Davis', desc) & season_type == 'REG'
    )

c_davis_complete_passes <- 
    c_davis_plays %>% 
    filter(
        !grepl('incomplete', desc) & !grepl('INTERCEPTED', desc)
    )
    

c_davis_complete_passes %>% 
    group_by(pass_location) %>% 
    count()

# 

pbp_2 <- load_pbp(2019:2021)

qwilliams_95 <- pbp_2 %>% 
    filter(defteam == 'NYJ' & grepl('95-Q.Williams', desc))
