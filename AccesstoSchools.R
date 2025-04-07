colleges_4yr <- colleges %>% filter(fouryear == "True")
hist(colleges_4yr$success, xlab = "Fraction of Kids with Parents from the Bottom Income Quintile", main = "Distribution of Success among 4-year colleges")
colleges_4yr %>% ggplot(aes(pct_stem, success)) 
+ geom_point(na.rm = TRUE, alpha = 0.1) 
+ geom_smooth(method = "lm", 
              se = FALSE, formula = "y ~ x", 
              na.rm = TRUE) + 
  stat_summary_bin(fun='mean', 
                   bins=20, color='blue', size=2, geom='point', na.rm = TRUE) + 
  labs(x = "% of STEM majors", 
       y = "Success", 
       title = "presence of STEM majors vs students' success")
success_pct_stem <- lm(formula = success ~ pct_stem, data = colleges_4yr) 
summary(success_pct_stem)
colleges_4yr %>% ggplot(aes(sat_avg, success)) + 
  geom_point(na.rm = TRUE, alpha = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, 
              formula = "y ~ x", na.rm = TRUE) + 
  stat_summary_bin(fun='mean', 
                   bins=20, 
                   color='blue', size=2, geom='point', na.rm = TRUE) + 
  labs(x = "Average SAT Score", 
       y = "Success", 
       title = "Success vs. Average SAT Score among 4 Year Colleges")
  success_sat <- lm(formula = success ~ sat_avg, data = colleges_4yr) 
summary(success_sat)
success_pct_stem_and_sat <- lm(formula = success ~ pct_stem + sat_avg, 
                               data = colleges_4yr) 
summary(success_pct_stem_and_sat)
pub_NY <- colleges %>% filter(type == 1 & state == "NY")
success_sticker_price_and_female <- lm(formula = success ~ sticker_price + 
                                         female, data = colleges_4yr) 
summary(success_sticker_price_and_female)