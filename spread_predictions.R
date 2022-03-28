# Calculate predicted spreads
spreads <- EPA_regression_data %>% 
  mutate(spread_reg = round(b0 
         + b_home_off_epa * home_off_epa 
         + b_home_def_epa * home_def_epa 
         + b_away_off_epa * away_off_epa
         + b_away_def_epa * away_def_epa,
         digits = 1)) %>% 
  mutate(spread_tree = if_else(home_off_epa <= .015, 
                               if_else(away_off_epa >= -.005, -3.3, 2.2),
                               if_else(away_off_epa >= -.055, 3.7, 9.7))) %>%
  mutate(pick = if_else(spread_reg < spread_line & spread_tree < spread_line, "home",
                        if_else(spread_reg > spread_line & spread_tree > spread_line, "away", "none"))) %>% 
  mutate(bet_result = if_else(pick == "none", 0,
                              if_else(pick == "home" & home_margin_to_spread > 0, 1,
                                      if_else(pick == "away" & home_margin_to_spread < 0, 1,
                                              if_else(home_margin_to_spread == 0, .5, 0)))))

spreads$spread_tree2 <- predict(model_class, spreads)


bet_results <- spreads %>%
  group_by(pick) %>% 
#  filter(pick != "none") %>% 
  summarise(success_rate = mean(bet_result), 
            game_count = n_distinct(game_id))
