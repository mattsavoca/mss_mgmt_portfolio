# Install Packages and Dependencies ------
source("./dependencies_and_themes.R")
source("./helpers.R")

# Extract Data -------
SEASONS = 2017:2022
nfl_partic_raw = nflreadr::load_participation(seasons = SEASONS)
nfl_pbp_raw = nflreadr::load_pbp(seasons = SEASONS)
nfl_playcallers_raw_fromURL = nflreadr::csv_from_url("https://raw.githubusercontent.com/samhoppen/NFL_public/main/data/all_playcallers.csv")
nfl_playcallers_raw = nfl_playcallers_raw_fromURL %>% filter(season %in% SEASONS)
nfl_rosters_raw = nflreadr::load_rosters(seasons = SEASONS)



# Transform and Load ------
nfl_partic = nfl_partic_raw %>%
  separate(offense_personnel, into = c("n_rb", "n_te", "n_wr"),
           sep = ", ",
           convert = TRUE) %>% 
  separate(defense_personnel, into = c("n_dl", "n_lb", "n_db"),
           sep = ", ",
           convert = TRUE) %>% 
  mutate(
    offense_formation = tolower(offense_formation),
    n_rb = quick_parse_number(n_rb),
    n_te = quick_parse_number(n_te),
    n_wr = quick_parse_number(n_wr),
    offense_personnel_code = paste0(as.character(n_rb), as.character(n_te)),
    n_dl = quick_parse_number(n_dl),
    n_lb = quick_parse_number(n_lb),
    n_db = quick_parse_number(n_db))



nfl_pbp = nfl_pbp_raw %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, posteam_playcaller = off_play_caller), 
            by = c("season", "posteam" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, defteam_playcaller = def_play_caller), 
            by = c("season", "defteam" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, home_off_play_caller = off_play_caller), 
            by = c("season", "home_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, away_off_play_caller = off_play_caller), 
            by = c("season", "away_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, home_def_play_caller = def_play_caller), 
            by = c("season", "home_team" = "team", "game_id")) %>%
  left_join(nfl_playcallers_raw %>% select(season, team, game_id, away_def_play_caller = def_play_caller), 
            by = c("season", "away_team" = "team", "game_id")) %>%
  left_join(nfl_partic %>% select(
    game_id = nflverse_game_id,
    play_id,
    posteam = possession_team,
    posteam_formation = offense_formation,
    posteam_personnel = offense_personnel_code,
    n_rb,
    n_te,
    n_wr,
    n_dl,
    n_lb,
    n_db,
    posteam_players = offense_players,
    posteam_n_players = n_offense,
    defteam_players= defense_players,
    defteam_n_players = n_defense
  )) %>%
  generate_primary_passers() %>%
  left_join(nfl_rosters_raw %>% select(season, home_team = team, home_primary_passer_name = full_name, home_primary_passer_id = gsis_id)) %>%
  left_join(nfl_rosters_raw %>% select(season, away_team = team, away_primary_passer_name = full_name, away_primary_passer_id = gsis_id)) %>%
  mutate(
    posteam_primary_passer_name = case_when(is.na(home_primary_passer_id) ~ as.character(NA), 
                                            home_team == posteam ~ home_primary_passer_name, 
                                            away_team == posteam ~ away_primary_passer_name, 
                                            T ~ as.character(NA)),
    posteam_primary_passer_id = case_when(is.na(home_primary_passer_id) ~ as.character(NA), 
                                          home_team == posteam ~ home_primary_passer_id, 
                                          away_team == posteam ~ away_primary_passer_id, 
                                          T ~ as.character(NA)),
    home_team_players = if_else(posteam == home_team, posteam_players, defteam_players),
    home_team_n_players = if_else(posteam == home_team, posteam_n_players, defteam_n_players),
    away_team_players = if_else(posteam == away_team, posteam_players, defteam_players),
    away_team_n_players = if_else(posteam == away_team, posteam_n_players, defteam_n_players)
  ) 
  
# Data Visualization -------
## Playcaller/QB Combo Top EPA/Play -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), play == 1) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    min_season = min(season),
    max_season = max(season),
    seasons = paste0(as.character(min_season), " - ", as.character(max_season)),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(n >= 150) %>%
  top_n(12, epa) %>% 
  arrange(-epa) %>% 
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.05)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Reid-Mahomes Tandem Dominates EPA per Play",
    subtitle = "All Team Plays in Games with Playercaller/QB Combo, 2017-2022 (Min. 150 Plays)",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )

## Playcaller/QB Combo Top EPA/Dropback -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), qb_dropback == 1) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(n >= 100) %>%
  top_n(12, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.05)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Dropback",
    subtitle = "All Dropbacks in Games with Playercaller/QB Combo 2017-2022 (Min. 150 Plays)",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )


## Playcaller/QB Combo Top EPA/Drive -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), qb_dropback == 1) %>%
  mutate(
    playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name),
    unique_drive_id = paste0(game_id,"_drive",fixed_drive)) %>%
  group_by(playcaller_qb_combo, posteam, unique_drive_id) %>%
  summarize(
    plays = n(),
    epa = sum(epa, na.rm = T)
  ) %>% 
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    drives = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>%
  filter(drives >= 60, epa > 0) %>%
  top_n(12, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.05)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Drive",
    subtitle = "All Drives in Games with Playercaller/QB Combo 2017-2022",
    y = "Playcaller-QB Combo",
    x = "EPA/Drive"
  )

## Playcaller/QB Combo Top Non-QB Rushes -------
nfl_pbp %>% 
  filter(!is.na(posteam_primary_passer_name), play == 1,
         play_type == "run" | qb_scramble == 1 
         ) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  select(game_id, play_id, season, week, home_team, away_team, posteam, epa, rusher_id, passer_id,playcaller_qb_combo) %>%
  left_join(nfl_rosters_raw %>% 
              select(season, posteam = team,rusher_position = position, rusher_name = full_name, rusher_id = gsis_id)) %>%
  left_join(nfl_rosters_raw %>% 
              select(season, posteam = team,passer_position = position, passer_name = full_name, passer_id = gsis_id)) %>%
  mutate(rusher_position = if_else(!is.na(passer_position), passer_position, rusher_position),
         rusher_name = if_else(!is.na(passer_name), passer_name, rusher_name),
         rusher_id = if_else(!is.na(passer_id), passer_id, rusher_id),
         ) %>%
  select(-starts_with("passer_")) %>%
  filter(rusher_position != "QB") %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(
    n = n(),
    epa = mean(epa, na.rm = T)
  ) %>% 
  ungroup() %>% 
  arrange(-epa) %>% 
  filter(n >= 150) %>%
  top_n(12, epa) %>% 
  arrange(-epa) %>%
  mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, epa)) %>% 
  ggplot()+
  aes(epa, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
  geom_col()+
  geom_nfl_logos(x = -0.002, width = 0.05)+
  nflplotR::scale_fill_nfl() +
  labs(
    title = "Payercaller-QB Combo EPA per Rush",
    subtitle = "All Non-QB Rushes in Games with Playercaller/QB Combo 2017-2022 (Min. 75 Team Att.)",
    y = "Playcaller-QB Combo",
    x = "EPA/Play"
  )

## Playcaller/QB Combo Top Rushes Per Game -----
  nfl_pbp %>% 
    filter(!is.na(posteam_primary_passer_name), play == 1,
           play_type == "run" | qb_scramble == 1,
    ) %>%
    mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
    select(game_id, play_id, season, week, home_team, away_team, posteam, epa, rusher_id, passer_id,playcaller_qb_combo) %>%
    left_join(nfl_rosters_raw %>% 
                select(season, posteam = team,rusher_position = position, rusher_name = full_name, rusher_id = gsis_id)) %>%
    left_join(nfl_rosters_raw %>% 
                select(season, posteam = team,passer_position = position, passer_name = full_name, passer_id = gsis_id)) %>%
    mutate(rusher_position = if_else(!is.na(passer_position), passer_position, rusher_position),
           rusher_name = if_else(!is.na(passer_name), passer_name, rusher_name),
           rusher_id = if_else(!is.na(passer_id), passer_id, rusher_id),
    ) %>%
    select(-starts_with("passer_")) %>%
    #filter(rusher_position != "QB") %>%
    group_by(playcaller_qb_combo, posteam) %>%
    summarize(
      n = n(),
      games = n_distinct(game_id),
      rushes_g = n/games,
      epa = mean(epa, na.rm = T)
    ) %>% 
    ungroup() %>% 
    arrange(-rushes_g) %>% 
    filter(n >= 75) %>%
    top_n(25, rushes_g) %>% 
    arrange(-rushes_g) %>%
    mutate(playcaller_qb_combo = reorder(playcaller_qb_combo, rushes_g)) %>% 
    ggplot()+
    aes(rushes_g, playcaller_qb_combo, fill = posteam, team_abbr = posteam)+
    geom_col()+
    geom_nfl_logos(x = 0.1, width = 0.025)+
    nflplotR::scale_fill_nfl() +
    labs(
      title = "Do Rushes Per Game Equate to QB Quality?",
      subtitle = "Rushes/Game, 2017-2022 (Min. 75 Att.)",
      y = "Playcaller-QB Combo",
      x = "Rushes/Game"
    )
    
    
    
    
## Playcaller/QB Dropback and Rush Efficiency  -----
passing_epa = nfl_pbp %>%
  filter(!is.na(posteam_primary_passer_name), play == 1, qb_dropback == 1) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name),) %>%
  group_by(playcaller_qb_combo, posteam) %>%
  summarize(n_dropbacks = n(),
            dropback_epa = mean(epa, na.rm = T)) %>%
  ungroup()

passing_epa %>% glimpse()

#ggplotly(
nfl_pbp %>%
  filter(!is.na(posteam_primary_passer_name),
         play == 1,
         play_type == "run",
         #| qb_scramble == 1,
         #season == 2022
         ) %>%
  mutate(
    playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name),
    posteam_playcaller_lastname = map_chr(posteam_playcaller, get_last_name),
    posteam_primary_passer_lastname = map_chr(posteam_primary_passer_name, get_last_name),
    p_q_combo = paste0(
      posteam_playcaller_lastname,
      "-",
      posteam_primary_passer_lastname
    )
  ) %>% 
  select(
    game_id,
    play_id,
    season,
    week,
    home_team,
    away_team,
    posteam,
    epa,
    rusher_id,
    passer_id,
    contains("combo")
  ) %>%
  left_join(
    nfl_rosters_raw %>%
      select(
        season,
        posteam = team,
        rusher_position = position,
        rusher_name = full_name,
        rusher_id = gsis_id
      )
  ) %>%
  left_join(
    nfl_rosters_raw %>%
      select(
        season,
        posteam = team,
        passer_position = position,
        passer_name = full_name,
        passer_id = gsis_id
      )
  ) %>%
  mutate(
    rusher_position = if_else(!is.na(passer_position), passer_position, rusher_position),
    rusher_name = if_else(!is.na(passer_name), passer_name, rusher_name),
    rusher_id = if_else(!is.na(passer_id), passer_id, rusher_id),
  ) %>%
  select(-starts_with("passer_")) %>%
  filter(rusher_position != "QB") %>%
  group_by(p_q_combo, posteam) %>%
  summarize(
    playcaller_qb_combo = playcaller_qb_combo[1],
    min_season = min(season),
    max_season = max(season),
    rushes = n(),
    games = n_distinct(game_id),
    rushes_g = rushes / games,
    rush_epa = mean(epa, na.rm = T),
    .groups = "drop"
  ) %>%
  left_join(passing_epa) %>%
  mutate(
    total_opps = n_dropbacks + rushes,
    pass_epa_ntile = percent_rank(dropback_epa),
    rush_g_ntile = percent_rank(rushes_g),
    rush_epa_ntile = percent_rank(rush_epa),
    adj_rush_epa_ntile = if_else(max_season < 2022, as.numeric(NA), rush_epa_ntile),
    adj_pass_epa_ntile = if_else(max_season < 2022, as.numeric(NA), pass_epa_ntile)
  ) %>%
  filter(total_opps >= 150) %>% 
  ggplot() +
  aes(
    pass_epa_ntile,
    rush_epa_ntile,
    size = total_opps,
    fill = posteam,
    team_abbr = posteam,
    label = p_q_combo
  ) +
  geom_point(color = "grey", alpha = .4) +
  geom_nfl_logos(aes(x = adj_pass_epa_ntile, y = adj_rush_epa_ntile), width = 0.04) +
  #geom_point()+
  #scale_fill_nfl(type = "primary")+
  geom_vline(xintercept = .5, lty = 2) +
  geom_hline(yintercept = .5, lty = 2) +
  theme(legend.position = "none") +
  labs(
    y = "Non-QB Rush EPA/Play (Percentile)",
    x = "Dropback EPA/Play, Incl. Scrambles (Percentile)",
    title = "McDaniel-Tagovailoa & Shanahan-Purdy Tops in Run/Pass Efficiency",
    subtitle = "All Rushes and Dropbacks, 2022 Season (Min. 150 Plays)"
  ) +
  ggrepel::geom_text_repel(aes(x = adj_pass_epa_ntile, y = adj_rush_epa_ntile), size = 3, force_pull = .5)

    
## Playercaller/QB Efficiency by Formation ------
nfl_pbp %>%
  filter(!is.na(posteam_primary_passer_name), qb_dropback == 1) %>%
  mutate(playcaller_qb_combo = paste0(posteam_playcaller, "-", posteam_primary_passer_name)) %>%
  group_by(playcaller_qb_combo, posteam, posteam_personnel) %>%
  summarize(n = n(),
            epa = sum(epa, na.rm = T),
            .groups = "drop") %>%
  mutate(personnel = if_else(n <= 20, "other", posteam_personnel)) %>%
  group_by(playcaller_qb_combo, posteam, personnel) %>%
  summarize(n = sum(n),
            epa = mean(epa, na.rm = T),
            .groups = "drop") %>%
  filter(playcaller_qb_combo == "Andy Reid-Patrick Mahomes")

