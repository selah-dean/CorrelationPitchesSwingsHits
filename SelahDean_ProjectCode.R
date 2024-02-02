#Selah Dean 
#Project - Pitch Type and Location Effects on Swings and Hits

library(infotheo)
library(tidyverse)
library(ggpubr)
library(colorspace)


chart <- read.csv("Fall2023PitchingData.csv")
chart <- chart %>% select(-X)

character_cols <- names(chart %>% select_if(negate(is.numeric)))
chart <- chart %>% mutate_at(character_cols, ~ ifelse(. %in% c(" ", "-", ""), NA, .))

chart <- chart %>% drop_na(S.B)

chart <- chart %>% mutate(Result = toupper(str_replace_all(Result, " ", "")))

chart <- chart %>%
  mutate(Result=case_when(
    Result == "3-JUN" ~ "6-3", 
    Result == "1-MAR" ~ "3-1",
    Result =="3-APR" ~ "4-3",
    Result=="3-MAY" ~ "5-3", 
    Result == "3-FEB" ~ "2-3", 
    Result=="3-JAN" ~ "1-3",
    Result == "" ~ NA, 
    TRUE ~ Result
  ))

chart <- chart %>%
  mutate(Result_Descrip = case_when(
    str_detect(Result, "K") ~ "strikeout",
    str_detect(Result, "1B") ~ "single",
    str_detect(Result, "2B") ~ "double",
    str_detect(Result, "3B") ~ "triple",
    str_detect(Result, "HR") ~ "homerun",
    str_detect(Result, "BB") ~ "walk",
    str_detect(Result, "HBP") ~ "hitbypitch",
    str_detect(Result, "-") ~ "groundout",
    str_detect(Result, "F") ~ "flyout",
    str_detect(Result, "DP") ~ "groundout",
    str_detect(Result, "L") ~ "lineout",
    str_detect(Result, "P") ~ "popout",
    str_detect(Result, "E") ~ "error",
    str_detect(Result, "FC") ~ "groundout",
    str_detect(Result, "U") ~ "groundout",
    TRUE ~ NA  # Handle other cases
  ))

chart <- chart %>%
  mutate(Hit = case_when(
    Result_Descrip %in% c("single", "double", "triple", "homerun") ~ TRUE,
    Result_Descrip %in% c("strikeout", "groundout", "flyout", "linout", "popout", "error") ~ FALSE,
    TRUE ~ NA
  ))

chart <- chart %>%
  mutate(Swing = case_when(
    S.B %in% c("Ball", "Called Strike") ~ FALSE,
    S.B %in% c("Foul", "Swinging Strike") ~ TRUE,
    TRUE ~ NA
  ))



chart <- chart %>%
  rowwise() %>%
  mutate(Zone = case_when(
    (Strike.Zone.Side >= -9 && Strike.Zone.Side<= -3 && 
       Strike.Zone.Height>=34 && Strike.Zone.Height<=42) ~ 1, 
    (Strike.Zone.Side >=-3 && Strike.Zone.Side<= 3 &&
       Strike.Zone.Height>=34 && Strike.Zone.Height<=42) ~ 2,
    (Strike.Zone.Side >= 3 && Strike.Zone.Side<= 9 &&
       Strike.Zone.Height>=34 && Strike.Zone.Height<=42) ~ 3,
    (Strike.Zone.Side >= -9 && Strike.Zone.Side<= -3 &&
       Strike.Zone.Height>=26 && Strike.Zone.Height<=34) ~ 4,
    (Strike.Zone.Side >= -3 && Strike.Zone.Side<= 3 &&
       Strike.Zone.Height>=26 && Strike.Zone.Height<=34) ~ 5,
    (Strike.Zone.Side >= 3 && Strike.Zone.Side<= 9 &&
       Strike.Zone.Height>=26 && Strike.Zone.Height<=34) ~ 6,
    (Strike.Zone.Side >= -9 && Strike.Zone.Side<= -3 &&
       Strike.Zone.Height>=18 && Strike.Zone.Height<=26) ~ 7,
    (Strike.Zone.Side >= -3 && Strike.Zone.Side<= 3 &&
       Strike.Zone.Height>=18 && Strike.Zone.Height<=26) ~ 8,
    (Strike.Zone.Side >= 3 && Strike.Zone.Side<= 9 &&
       Strike.Zone.Height>=18 && Strike.Zone.Height<=26) ~ 9,
    ((Strike.Zone.Side< -9 && Strike.Zone.Height> 30) |
       (Strike.Zone.Side<=0 && Strike.Zone.Height >42)) ~ 11,
    ((Strike.Zone.Side>9 && Strike.Zone.Height> 30) |
       (Strike.Zone.Side>=0 && Strike.Zone.Height >42)) ~ 12,
    ((Strike.Zone.Side< -9 && Strike.Zone.Height < 30) |
       (Strike.Zone.Side<=0 && Strike.Zone.Height <18)) ~ 13,
    ((Strike.Zone.Side>9 && Strike.Zone.Height< 30) |
       (Strike.Zone.Side>=0 && Strike.Zone.Height <18)) ~ 14,
    TRUE ~ NA
  ))

players_to_analyze <- c()

for (i in 1:length(players_to_analyze)){
  player <- chart %>% filter(Name == players_to_analyze[i])
  
  SB_type_table <- table(player$S.B, player$Type)
  SB_type_test <- fisher.test(SB_type_table, hybrid = TRUE)
  SB_type_test

  swing_type_table <- table(player$Swing, player$Type)
  swing_type_test <- fisher.test(swing_type_table)
  swing_type_test

  zone_swing_table <- table(player$Swing, player$Zone)
  zone_swing_test <- fisher.test(zone_swing_table, simulate.p.value=TRUE)
  zone_swing_test

  hit_type_table <- table(player$Hit, player$Type)
  hit_type_test <- fisher.test(hit_type_table)
  hit_type_test

  hit_zone_table <- table(player$Hit, player$Zone)
  hit_zone_test <- fisher.test(hit_zone_table)
  hit_zone_test

  result_type_table <- table(player$Result_Descrip, player$Type)
  result_type_test <- fisher.test(result_type_table, simulate.p.value = TRUE)
  result_type_test

  result_zone_table <- table(player$Result_Descrip, player$Zone)
  result_zone_test <- fisher.test(result_zone_table, simulate.p.value = TRUE)
  result_zone_test

  swing_strike_table <- table(player$Swing, player$Is.Strike)
  swing_strike_test <- fisher.test(swing_strike_table)
  swing_strike_test

  type_strike_table <- table(player$Type, player$Is.Strike)
  type_strike_test <- fisher.test(type_strike_table)
  type_strike_test

  SB_type <- ggtexttable(SB_type_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "S/B Call vs Pitch Type", padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(SB_type_test$p.value ,5)), face="italic", size=10)

  swing_type <- ggtexttable(swing_type_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Swing vs Pitch Type", padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(swing_type_test$p.value,5)), face="italic", size=10)

  zone_swing <- ggtexttable(zone_swing_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Swing vs Pitch Location (Zone)", padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(zone_swing_test$p.value,5)), face="italic", size=10)

  hit_type <- ggtexttable(hit_type_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Hit vs Pitch Type",padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(hit_type_test$p.value,5)), face="italic", size=10)

  hit_zone <- ggtexttable(hit_zone_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Hit vs Pitch Location (Zone)",padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(hit_zone_test$p.value,5)), face="italic", size=10)

  result_type <- ggtexttable(result_type_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Play Result vs Pitch Type",padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(result_type_test$p.value,5)), face="italic", size=10)

  result_zone <- ggtexttable(result_zone_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Play Result vs Pitch Location (Zone)", padding = unit(1, "line"),hjust=-0.45) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(result_zone_test$p.value,5)), face="italic", size=10)

  swing_strike <- ggtexttable(swing_strike_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Swing vs Strike", padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(swing_strike_test$p.value,5)), face="italic", size=6)

  type_strike <- ggtexttable(type_strike_table, theme = ttheme("light", rownames.style = rownames_style(face="bold"))) %>%
    tab_add_title(text = "Pitch Type vs Strike",padding = unit(1, "line")) %>%
    tab_add_footnote(text = paste0("Fisher's Exact Test p-value: ", round(type_strike_test$p.value,5)), face="italic", size=6)



  text = paste0("Contingency Tables \n for Player ", i)

  title_plot <- ggplot() +
    annotate("text", x = 4, y = 25, size=8, fontface="bold", label = text) +
    theme_void()

  row1 <- ggarrange(title_plot, SB_type, nrow=1, ncol=2, align="hv",widths = c(1,1))
  row2 <- ggarrange(swing_type, zone_swing, nrow=1, ncol=2, align="hv",widths = c(1, 1))
  swing_tables <- ggarrange(row1, row2, nrow=2, ncol=1, align="hv", heights = c(2, 1))
  ggsave(paste0("Figures/swing_conttable_player", i, ".jpg"), swing_tables, width = 10.5, height=4)

  row1 <- ggarrange(NULL,title_plot, NULL, nrow=1, ncol=3, align="hv", widths=c(1,3,1))
  row2 <- ggarrange(swing_strike, type_strike, nrow=1, ncol=2,align="hv", widths=c(1,1))
  general_tables <- ggarrange(row1, row2, nrow = 2, ncol=1, align = "hv", heights = c(1,2))
  ggsave(paste0("Figures/general_conttable_player", i, ".jpg"), general_tables, width = 6, height=4)

  row1 <- ggarrange(NULL,title_plot, NULL, nrow=1, ncol=3, align="hv", widths=c(1,1,1))
  row2 <- ggarrange(hit_type, hit_zone, nrow = 1, ncol =2, align="hv",widths = c(1, 1))
  row3 <- ggarrange(result_type, result_zone, nrow = 1, ncol =2, align="hv",widths = c(1, 1))
  hit_tables <- ggarrange(row1, row2, row3, nrow=3, ncol=1, align="hv", heights = c(1, 1, 3))
  ggsave(paste0("Figures/hit_conttable_player", i, ".jpg"),hit_tables, width = 10, height=7)

  cols <- c("Fastball" = "red2","Curveball" = "blue", "Changeup" = "darkorange", "Slider" = "purple", "Splitter" = "deeppink")
  
  scatter <- ggplot(player, aes(x=Strike.Zone.Side, y=Strike.Zone.Height)) + 
    xlab("Strike Zone Side (in)") + ylab("Strike Zone Height (in)") + ggtitle("Strike Zone") +
    geom_rect(xmin=-30, xmax=30, ymin=0, ymax=60, color="grey93", fill="grey93", alpha=0.1) + 
    geom_rect(xmin=-20, xmax=20, ymin=6, ymax=54, color="lightgoldenrod1", fill="lightgoldenrod1", alpha=0.1) +
    geom_rect(xmin=-13.3, xmax=13.3, ymin=14, ymax=46, color="lightsalmon", fill="lightsalmon", alpha=0.1) +
    geom_rect(xmin=-6.7, xmax=6.7, ymin=22, ymax=38, color="plum2", fill="plum2", alpha=0.1) +
    geom_rect(xmin=-9, xmax = 9, ymin = 18, ymax=42, color="limegreen", fill="transparent", linewidth=1.5, linetype="dashed") +
    geom_point(aes(color=Type), size=2.5) + labs(color = "Pitch Type") + 
    theme(panel.background = element_rect(color = "white", fill = "white"), 
          plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    guides(color = guide_legend(nrow = 2)) + 
    xlim(-30, 30) + ylim(0,60) + scale_color_manual(values = cols)
  
  in_zone <- data.frame(x1 = c(-9, -9, -9, -3, -3, -3, 3, 3, 3), x2=c(-3,-3, -3, 3, 3, 3, 9, 9, 9), y1 = c(34, 26, 18, 34, 26, 18, 34, 26, 18), y2 = c(42, 34, 26, 42, 34, 26, 42, 34, 26), Zone=c(1, 4, 7, 2, 5, 8, 3, 6, 9))
  out_zone <- data.frame(x = c(-12, -12, 0, 0, -9, -9, 0, 12, 12, 9, 9, 0, 9, 12, 12, 0, 0, 9, 0, 0, -12, -12, -9, -9), y=c(30, 45, 45, 42, 42, 30, 45, 45, 30, 30, 42, 42, 30, 30, 15, 15, 18, 18, 18, 15, 15, 30, 30, 18), Zone=c(11,11,11,11,11,11,12,12,12,12,12,12,14,14,14,14,14,14,13,13,13,13,13,13))
  out_zone_loc <- data.frame(x = c(-10, 10, 10, -10), y = c(43, 43, 16, 16), Zone = c(11, 12, 14, 13))
  
  
  swing <- player %>%
    filter(!is.na(Swing)) %>%
    group_by(Zone) %>%
    summarise(swing_percentage = sum(Swing)/(sum(Swing)+sum(!Swing))) %>% drop_na(Zone)
  
  in_zone <- in_zone %>% left_join(swing, by="Zone") %>% replace(is.na(.), "NA")
  out_zone <- out_zone %>% left_join(swing, by="Zone") %>% replace(is.na(.), "NA")
  out_zone_loc <- out_zone_loc %>%left_join(swing, by="Zone") %>% replace(is.na(.), "NA")
  
  ba <- player %>% 
    filter(!is.na(Hit)) %>%
    group_by(Zone) %>%
    summarise(batting_avg = sum(Hit)/(sum(Hit)+sum(!Hit))) %>% drop_na(Zone)
  
  in_zone <- in_zone %>% left_join(ba, by="Zone") %>% replace(is.na(.), -1)
  out_zone <- out_zone %>% left_join(ba, by="Zone") %>% replace(is.na(.), -1)
  out_zone_loc <- out_zone_loc %>%left_join(ba, by="Zone") %>% replace(is.na(.), -1)
  
  max_swing_perc <- max(swing$swing_percentage)
  min_swing_perc <- min(swing$swing_percentage)
  swing_int <- (max_swing_perc - min_swing_perc)/6
  
  max_ba <- max(ba$batting_avg)
  min_ba <- min(ba$batting_avg)
  ba_int <- (max_ba - min_ba)/6
  
  fill_colors <- c("black", "steelblue4", "steelblue3", "steelblue2", "lightsteelblue1", "mistyrose", "red2")
  
  fill_colors_labels <- c("black"= "black","steelblue4"= "steelblue4","steelblue3"= "steelblue3",
                          "steelblue2"= "steelblue2","lightsteelblue1"= "lightsteelblue1",
                          "mistyrose"= "mistyrose","red2"= "red2")

  
  in_zone <- in_zone %>% mutate(swing_fill = 
                                   cut(swing_percentage, breaks = c(-Inf, (min_swing_perc-swing_int), 
                                                                    min_swing_perc + swing_int, 
                                                                    min_swing_perc + 2*swing_int, 
                                                                    min_swing_perc + 3*swing_int, 
                                                                    min_swing_perc + 4*swing_int, 
                                                                    min_swing_perc + 5*swing_int, 
                                                                    Inf), labels = fill_colors), 
                                 swing_label = if_else(swing_percentage == -1, "NA", paste0(round(swing_percentage,4)*100, "%")), 
                                ba_fill = 
                                  cut(batting_avg, breaks = c(-Inf, (min_ba-ba_int), 
                                                                   min_ba + ba_int, 
                                                                   min_ba + 2*ba_int, 
                                                                   min_ba + 3*ba_int, 
                                                                   min_ba + 4*ba_int, 
                                                                   min_ba + 5*ba_int, 
                                                                   Inf), labels = fill_colors), 
                                ba_label = if_else(batting_avg == -1, "NA", format(round(batting_avg,3), nsmall = 3)))
  
  out_zone <- out_zone %>% mutate(swing_fill = 
                                  cut(swing_percentage, breaks = c(-Inf, (min_swing_perc-swing_int), 
                                                                   min_swing_perc + swing_int, 
                                                                   min_swing_perc + 2*swing_int, 
                                                                   min_swing_perc + 3*swing_int, 
                                                                   min_swing_perc + 4*swing_int, 
                                                                   min_swing_perc + 5*swing_int, 
                                                                   Inf), labels = fill_colors), 
                                  ba_fill = 
                                    cut(batting_avg, breaks = c(-Inf, (min_ba-ba_int), 
                                                                min_ba + ba_int, 
                                                                min_ba + 2*ba_int, 
                                                                min_ba + 3*ba_int, 
                                                                min_ba + 4*ba_int, 
                                                                min_ba + 5*ba_int, 
                                                                Inf), labels = fill_colors))
  out_zone_loc <- out_zone_loc %>% mutate(swing_label = if_else(swing_percentage==-1, "NA", paste0(round(swing_percentage,4)*100, "%")), 
                                          ba_label = if_else(batting_avg==-1, "NA", format(round(batting_avg,3), nsmall = 3)))
  
  
  swing_zone_plot <- ggplot() + 
    scale_x_continuous(name="x") + scale_y_continuous(name = "y") + ggtitle("Strike Zone with Swing Percentage") + theme_void() + 
    theme(legend.position="none", plot.title=element_text(hjust=0.5)) + 
    geom_rect(data = in_zone, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax=y2, fill=swing_fill), color = "grey", alpha = 0.5) + 
    geom_polygon(data = out_zone, mapping = aes(x =x, y=y, group = Zone, fill=swing_fill), color = "grey", alpha = 0.5) + 
    geom_text(data=in_zone, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=swing_label), size=4) + 
    geom_text(data = out_zone_loc, aes(x=x, y=y, label=swing_label), size =4) + 
    scale_fill_manual(values = fill_colors_labels)
    
  
  ba_zone_plot <- ggplot() + 
    scale_x_continuous(name="x") + scale_y_continuous(name = "y") + ggtitle("Strike Zone with Batting Average") + theme_void() + 
    theme(legend.position="none", plot.title=element_text(hjust=0.5)) + 
    geom_rect(data = in_zone, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax=y2, fill=ba_fill), color = "grey", alpha = 0.5) + 
    geom_polygon(data = out_zone, mapping = aes(x =x, y=y, group = Zone, fill=ba_fill), color = "grey", alpha = 0.5) + 
    geom_text(data=in_zone, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=ba_label), size=4) + 
    geom_text(data = out_zone_loc, aes(x=x, y=y, label=ba_label), size =4) + 
    scale_fill_manual(values = fill_colors_labels)
  
  strike_zone_plots <- ggarrange(scatter, swing_zone_plot, ba_zone_plot, nrow=1, ncol=3)
  strike_zone_plots <- annotate_figure(strike_zone_plots, top = text_grob(paste0("Strike Zone Plots for \nPlayer ", i), face = "bold", size = 14))
  ggsave(paste0("Figures/strike_zone_player", i, ".jpg"), width = 12, height=6)
}

zone_label_plot <- ggplot() + 
  scale_x_continuous(name="Strike Zone Side (in)") + scale_y_continuous(name = "Strike Zone Height (in)") + ggtitle("Strike Zone - Zone Labels") + 
  theme(panel.background = element_rect(color = "white", fill = "white"), legend.position="none",plot.title = element_text(hjust = 0.5)) +
  geom_rect(data = in_zone, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax=y2), fill="white" ,color = "black", alpha = 0.5) + 
  geom_polygon(data = out_zone, mapping = aes(x =x, y=y, group = Zone), fill="white",color = "black", alpha = 0.5) + 
  geom_text(data=in_zone, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=Zone), size=4) + 
  geom_text(data = out_zone_loc, aes(x=x, y=y, label=Zone), size =4) 

ggsave("Figures/zone_label_plot.jpg", zone_label_plot, width=5, height=6)


