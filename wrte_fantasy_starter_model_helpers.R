# Skill DF (Rolling Metrics) -----
skill_ep_preprocess = function(df){
  df %>% 
    filter(week <= 17) %>%
    transmute(
      year,
      week,
      player_id,
      team_id,
      position,
      week_id = as.numeric(paste0(year,ifelse(week<10,"0",""),week)),
      fp = ppg,
      fpoe = diff,
      tdve_rec = receiving_tds - expected_receiving_tds,
      etd_rec = expected_receiving_tds,
      tdve_ru = rushing_tds - expected_rushing_tds,
      etd_ru = expected_rushing_tds,
      yve_rec =  receiving_yards - expected_receiving_yards,
      ey_rec = expected_receiving_yards,
      yve_ru = rushing_yards - expected_rushing_yards,
      ey_ru = expected_rushing_yards,
      recve = receptions - expected_receptions,
      pos2 = if_else(position == "RB", "RB", "REC"),
    ) %>% 
    group_by(pos2, week_id) %>%
    mutate(pos2_rk = min_rank(-fp)) %>%
    group_by(week_id) %>%
    mutate(flex_rk = min_rank(-fp)) %>%
    arrange(week_id) %>%
    group_by(player_id) %>%
    mutate(player_gm = rank(week_id),
           max_player_gm = max(player_gm, na.rm = T)) %>%
    ungroup() %>%
    mutate(is_hit = if_else(flex_rk <= 24, 1, 0),
           is_game = 1) %>%
    arrange(player_id, player_gm) %>%
    group_by(player_id) %>%
    mutate(
      roll_fp  = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_fpoe  = slide_index(.x = fpoe, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_tdve_rec  = slide_index(.x = tdve_rec, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_etd_rec  = slide_index(.x = etd_rec, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_tdve_ru  = slide_index(.x = tdve_ru, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_etd_ru  = slide_index(.x = etd_ru, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_yve_rec  = slide_index(.x = yve_rec, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_ey_rec  = slide_index(.x = ey_rec, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_yve_ru  = slide_index(.x = yve_ru, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_ey_ru  = slide_index(.x = ey_ru, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      roll_recve  = slide_index(.x = recve, .i = player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      ###
      roll2_fp  = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_fpoe  = slide_index(.x = fpoe, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_tdve_rec  = slide_index(.x = tdve_rec, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_etd_rec  = slide_index(.x = etd_rec, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_tdve_ru  = slide_index(.x = tdve_ru, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_etd_ru  = slide_index(.x = etd_ru, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_yve_rec  = slide_index(.x = yve_rec, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_ey_rec  = slide_index(.x = ey_rec, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_yve_ru  = slide_index(.x = yve_ru, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_ey_ru  = slide_index(.x = ey_ru, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      roll2_recve  = slide_index(.x = recve, .i = player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      ###
      roll3_fp  = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_fpoe  = slide_index(.x = fpoe, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_tdve_rec  = slide_index(.x = tdve_rec, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_etd_rec  = slide_index(.x = etd_rec, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_tdve_ru  = slide_index(.x = tdve_ru, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_etd_ru  = slide_index(.x = etd_ru, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_yve_rec  = slide_index(.x = yve_rec, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_ey_rec  = slide_index(.x = ey_rec, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_yve_ru  = slide_index(.x = yve_ru, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_ey_ru  = slide_index(.x = ey_ru, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      roll3_recve  = slide_index(.x = recve, .i = player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      ####
      roll4_fp  = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_fpoe  = slide_index(.x = fpoe, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_tdve_rec  = slide_index(.x = tdve_rec, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_etd_rec  = slide_index(.x = etd_rec, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_tdve_ru  = slide_index(.x = tdve_ru, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_etd_ru  = slide_index(.x = etd_ru, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_yve_rec  = slide_index(.x = yve_rec, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_ey_rec  = slide_index(.x = ey_rec, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_yve_ru  = slide_index(.x = yve_ru, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_ey_ru  = slide_index(.x = ey_ru, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      roll4_recve  = slide_index(.x = recve, .i = player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      ####
      roll5_fp  = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_fpoe  = slide_index(.x = fpoe, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_tdve_rec  = slide_index(.x = tdve_rec, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_etd_rec  = slide_index(.x = etd_rec, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_tdve_ru  = slide_index(.x = tdve_ru, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_etd_ru  = slide_index(.x = etd_ru, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_yve_rec  = slide_index(.x = yve_rec, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_ey_rec  = slide_index(.x = ey_rec, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_yve_ru  = slide_index(.x = yve_ru, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_ey_ru  = slide_index(.x = ey_ru, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      roll5_recve  = slide_index(.x = recve, .i = player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      # target variables
      # fp_wkn1 = lead(fp, 1, order_by = player_gm),
      # hit_wkn1 = lead(is_hit, 1, order_by = player_gm),
      hit_weeks  = slide_index(.x = is_hit, .i = player_gm, .f = ~sum(.x),.after = 8) %>% as.numeric() - is_hit,
      num_weeks = slide_index(.x = is_game, .i = player_gm, .f = ~sum(.x) ,.after = 8) %>% as.numeric()- 1,
      hit_rate = hit_weeks/num_weeks
      # fp_next8 = slide_index(.x = fp, .i = player_gm, .f = ~mean(.x),.after = 8) %>% as.numeric(),
    ) %>%
    ungroup()
}


get_skill_player_dict = function(skill_ep_df, qb_ep_df){
  qb = qb_ep_df %>%
    mutate(week_id = as.numeric(paste0(year,ifelse(week<10,"0",""),week))) %>%
    select(qb_player_id = player_id, quarterback = player, team_id, team, week_id) %>%
    distinct()
  
  skill = skill_ep_df %>%
    mutate(week_id = as.numeric(paste0(year,ifelse(week<10,"0",""),week))) %>% 
    select(player_id, player, team_id, team, week_id) %>%
    distinct() %>%
    left_join(qb)
  
  return(skill)
}

# QB DF (Rolling Metrics) -----
skill_ep_qbdf_preprocess = function(qb_df){
  qb_df %>% 
    filter(week <= 17) %>%
    transmute(
      year,
      week,
      player_id,
      team_id,
      qb_fp = ppg,
      qb_fpoe = diff,
      week_id = as.numeric(paste0(year,ifelse(week<10,"0",""),week)),
      qb_fpve_pa = passing_ppg - passing_expected_ppg,
      qb_xfp_pa = passing_expected_ppg,
      qb_tdve_pa = passing_tds - expected_passing_tds,
      qb_etd_pa = expected_passing_tds,
      qb_yve_pa = passing_yards - expected_passing_yards,
      qb_ey_pa = expected_passing_yards,
      qb_cmpve_pa = completions - expected_completions,
      qb_ecmp_pa = expected_completions,
      qb_intve_pa = interceptions - expected_interceptions,
      qb_eint_pa = expected_interceptions,
      qb_tdve_ru = designed_rush_tds - expected_designed_rush_tds,
      qb_etd_ru = expected_designed_rush_tds,
      qb_yve_ru = designed_rush_yards  - expected_designed_rush_yards,
      qb_ey_ru = expected_designed_rush_yards,
      qb_tdve_scrm = scramble_tds - expected_scramble_tds,
      qb_etd_scrm = expected_scramble_tds,
      qb_yve_scrm = scramble_yards  - expected_scramble_yards,
      qb_ey_scrm = expected_scramble_yards) %>% 
    arrange(week_id) %>%
    group_by(player_id) %>%
    mutate(
      qb_player_gm = rank(week_id)
    ) %>% 
    ungroup() %>%
    arrange(player_id, qb_player_gm) %>%
    group_by(player_id) %>%
    mutate(
      qb_roll_fp  = slide_index(.x = qb_fp, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_fpoe  = slide_index(.x = qb_fpoe, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_tdve_pa  = slide_index(.x = qb_tdve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_etd_pa  = slide_index(.x = qb_etd_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_ey_pa  = slide_index(.x = qb_ey_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_tdve_ru  = slide_index(.x = qb_tdve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_ecmp_pa  = slide_index(.x = qb_ecmp_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_eint_pa  = slide_index(.x = qb_eint_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_tdve_scrm  = slide_index(.x = qb_tdve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_etd_ru  = slide_index(.x = qb_etd_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_ey_ru  = slide_index(.x = qb_ey_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_etd_scrm  = slide_index(.x = qb_etd_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_ey_scrm  = slide_index(.x = qb_ey_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_yve_scrm  = slide_index(.x = qb_yve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_yve_pa  = slide_index(.x = qb_yve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_yve_ru  = slide_index(.x = qb_yve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_cmpve  = slide_index(.x = qb_cmpve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      qb_roll_intve  = slide_index(.x = qb_intve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 1) %>% as.numeric(),
      ###
      qb_roll2_fp  = slide_index(.x = qb_fp, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_fpoe  = slide_index(.x = qb_fpoe, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_tdve_pa  = slide_index(.x = qb_tdve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_etd_pa  = slide_index(.x = qb_etd_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_ey_pa  = slide_index(.x = qb_ey_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_tdve_ru  = slide_index(.x = qb_tdve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_ecmp_pa  = slide_index(.x = qb_ecmp_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_eint_pa  = slide_index(.x = qb_eint_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_tdve_scrm  = slide_index(.x = qb_tdve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_etd_ru  = slide_index(.x = qb_etd_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_ey_ru  = slide_index(.x = qb_ey_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_etd_scrm  = slide_index(.x = qb_etd_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_ey_scrm  = slide_index(.x = qb_ey_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_yve_scrm  = slide_index(.x = qb_yve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_yve_pa  = slide_index(.x = qb_yve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_yve_ru  = slide_index(.x = qb_yve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_cmpve  = slide_index(.x = qb_cmpve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      qb_roll2_intve  = slide_index(.x = qb_intve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 2) %>% as.numeric(),
      ###
      qb_roll3_fp  = slide_index(.x = qb_fp, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_fpoe  = slide_index(.x = qb_fpoe, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_tdve_pa  = slide_index(.x = qb_tdve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_etd_pa  = slide_index(.x = qb_etd_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_ey_pa  = slide_index(.x = qb_ey_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_tdve_ru  = slide_index(.x = qb_tdve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_ecmp_pa  = slide_index(.x = qb_ecmp_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_eint_pa  = slide_index(.x = qb_eint_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_tdve_scrm  = slide_index(.x = qb_tdve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_etd_ru  = slide_index(.x = qb_etd_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_ey_ru  = slide_index(.x = qb_ey_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_etd_scrm  = slide_index(.x = qb_etd_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_ey_scrm  = slide_index(.x = qb_ey_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_yve_scrm  = slide_index(.x = qb_yve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_yve_pa  = slide_index(.x = qb_yve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_yve_ru  = slide_index(.x = qb_yve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_cmpve  = slide_index(.x = qb_cmpve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      qb_roll3_intve  = slide_index(.x = qb_intve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 3) %>% as.numeric(),
      ###
      qb_roll4_fp  = slide_index(.x = qb_fp, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_fpoe  = slide_index(.x = qb_fpoe, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_tdve_pa  = slide_index(.x = qb_tdve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_etd_pa  = slide_index(.x = qb_etd_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_ey_pa  = slide_index(.x = qb_ey_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_tdve_ru  = slide_index(.x = qb_tdve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_ecmp_pa  = slide_index(.x = qb_ecmp_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_eint_pa  = slide_index(.x = qb_eint_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_tdve_scrm  = slide_index(.x = qb_tdve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_etd_ru  = slide_index(.x = qb_etd_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_ey_ru  = slide_index(.x = qb_ey_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_etd_scrm  = slide_index(.x = qb_etd_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_ey_scrm  = slide_index(.x = qb_ey_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_yve_scrm  = slide_index(.x = qb_yve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_yve_pa  = slide_index(.x = qb_yve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_yve_ru  = slide_index(.x = qb_yve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_cmpve  = slide_index(.x = qb_cmpve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      qb_roll4_intve  = slide_index(.x = qb_intve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 4) %>% as.numeric(),
      ###
      qb_roll5_fp  = slide_index(.x = qb_fp, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_fpoe  = slide_index(.x = qb_fpoe, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_tdve_pa  = slide_index(.x = qb_tdve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_etd_pa  = slide_index(.x = qb_etd_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_ey_pa  = slide_index(.x = qb_ey_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_tdve_ru  = slide_index(.x = qb_tdve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_ecmp_pa  = slide_index(.x = qb_ecmp_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_eint_pa  = slide_index(.x = qb_eint_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_tdve_scrm  = slide_index(.x = qb_tdve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_etd_ru  = slide_index(.x = qb_etd_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_ey_ru  = slide_index(.x = qb_ey_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_etd_scrm  = slide_index(.x = qb_etd_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_ey_scrm  = slide_index(.x = qb_ey_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_yve_scrm  = slide_index(.x = qb_yve_scrm, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_yve_pa  = slide_index(.x = qb_yve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_yve_ru  = slide_index(.x = qb_yve_ru, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_cmpve  = slide_index(.x = qb_cmpve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric(),
      qb_roll5_intve  = slide_index(.x = qb_intve_pa, .i = qb_player_gm, .f = ~mean(.x),.before = 5) %>% as.numeric()
    ) %>%
    ungroup()
}