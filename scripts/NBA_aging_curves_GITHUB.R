# If needed install.packages
# install.packages("tidyverse")
# install.packages("ggthemes")
# install.packages("ggrepel")
# install.packages("gt")

library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gt)


#1) Loading the data

NBA_2021_to_1997 <- read.csv("data/NBA_2021_to_1997.csv")

#2) Combine the data - Skip

#3) Fixing outliers in rate statistic qualifiers and age

#3A) Fix the players who do not meet the rate statistic qualifiers: https://www.basketball-reference.com/about/rate_stat_req.html
#Per BBallRef, 1500MP is the qualifer other than 98-99 lockout season where it is 915
  
#there are 11,702 player seasons
NBA_2021_to_1997 %>% filter(MP >1499)
#1500 or better drops our number to 4,454. Meaning we only use 38% of the data
  
NBA_2021_to_1997 %>% filter(MP >914)
#915 MP or better increases the number to 6,648. Meaning we use 57% of the data. Seems reasonable 
  
NBAdata_minquals <- NBA_2021_to_1997 %>% filter(MP >914)
  
#3B) Fix the age outliers
#First the older guys
  
NBAdata_minquals %>% filter(Age >36)
#118 player seasons older than 36
  
NBAdata_minquals %>% filter(Age >37)
#59 player seasons older than 37. 
  
#Somewhat arbitrary but I am going to cut it at players younger than 36. LEaving us with 6,444 seasons. 
NBAdata_minquals %>% filter(Age <37)
  
#Now the younger seasons
NBAdata_minimums %>% filter(Age <19)
# 2 players 18 (Tracy and Kobe) so those are east cuts
  
NBAdata_minimums %>% filter(Age <20)
#72 players under 20, so 70 excluding kobe and tracy. That seems like enough to me to include it?
  
NBAdata_minimums <- NBAdata_minquals %>% filter(Age >18, Age<37)
  
#3C) Fix the positions
NBAdata_minimums %>% select(Pos) %>% distinct()
  
NBAdata_minimums <- NBAdata_minimums %>% mutate(Pos = case_when(Pos == "SG-SF" ~ "SG",
                                                                  Pos == "SF-SG" ~ "SF",
                                                                  Pos == "PG-SG" ~ "PG",
                                                                  Pos == "C-PF" ~ "C",
                                                                  Pos == "SG-PG" ~ "SG",
                                                                  Pos == "PF-C" ~ "PF",
                                                                  Pos == "SF-PF" ~ "SF",
                                                                  Pos == "PF-SF" ~ "PF",
                                                                  Pos == "SG-PF" ~ "SG",
                                                                  Pos == "PG-SF" ~ "PG",
                                                                  TRUE ~ Pos))
  
  
#4) Calculate Aging curves using delta method
#4A) Group by Player seasons and calculate the change in metric each season, then group by Ages, then average those changes for each age
  
is.nan.tibble <- function(x) {do.call(cbind, lapply(x, is.nan))}
  
# 
# NBA_delta <- NBAdata_minimums %>% group_by(PlayerID) %>% 
#   summarise(Age, 
#             USG_CNG = (USG.-lead(USG.)),
#             TRB_CNG = (TRB.-lead(TRB.)),
#             FTr_CNG = (FTr-lead(FTr)),
#             TS_CNG = (TS.-lead(TS.)),
#             VORP_CNG = (VORP-lead(VORP)),
#             PER_CNG = (PER- lead(PER)),
#             BLK_CNG = (BLK.- lead(BLK.))) %>%
#   group_by(Age) %>% summarise(Avg_USG_CNG = mean(USG_CNG, na.rm = TRUE),
#                               Avg_TRB_CNG = mean(TRB_CNG, na.rm = TRUE),
#                               Avg_FTr_CNG = mean(FTr_CNG, na.rm = TRUE),
#                               Avg_TS_CNG = mean(TS_CNG, na.rm = TRUE),
#                               Avg_VORP_CNG = mean(VORP_CNG, na.rm = TRUE),
#                               Avg_PER_CNG = mean(PER_CNG, na.rm = TRUE),
#                               Avg_BLK_CNG = mean(BLK_CNG, na.rm = TRUE))
# 
# NBA_delta[is.nan.tibble(NBA_delta)] <- 0
# NBA_delta
  
#ALTERNATE WITH WEIGHTED AVERAGE Calculate Aging curves using delta method
#4A) Group by Player seasons and calculate the change in metric each season, then group by Ages, then average those changes for each age
  
NBA_delta <- NBAdata_minimums %>% group_by(PlayerID) %>%
    summarise(Age,MP,
              USG_CNG = (USG.-lead(USG.)),
              TRB_CNG = (TRB.-lead(TRB.)),
              FTr_CNG = (FTr-lead(FTr)),
              TS_CNG = (TS.-lead(TS.)),
              VORP_CNG = (VORP-lead(VORP)),
              PER_CNG = (PER- lead(PER)),
              BLK_CNG = (BLK.- lead(BLK.))) %>%
    group_by(Age) %>% summarise(Avg_USG_CNG = weighted.mean(USG_CNG, na.rm = TRUE, w = MP),
                                Avg_TRB_CNG = weighted.mean(TRB_CNG, na.rm = TRUE, w = MP),
                                Avg_FTr_CNG = weighted.mean(FTr_CNG, na.rm = TRUE, w = MP),
                                Avg_TS_CNG = weighted.mean(TS_CNG, na.rm = TRUE, w = MP),
                                Avg_VORP_CNG = weighted.mean(VORP_CNG, na.rm = TRUE, w = MP),
                                Avg_PER_CNG = weighted.mean(PER_CNG, na.rm = TRUE, w = MP),
                                Avg_BLK_CNG = weighted.mean(BLK_CNG, na.rm = TRUE, w = MP))
  
  
  
NBA_delta[is.nan.tibble(NBA_delta)] <- 0
NBA_delta
  
  
#4B) Now need to "chain" those averages together 
  
# NBA_delta_chain <- NBA_delta %>% mutate(USG_Chain = cumsum(Avg_USG_CNG),
#                      TRB_Chain = cumsum(Avg_TRB_CNG),
#                      FTr_Chain = cumsum(Avg_FTr_CNG),
#                      TS_Chain = cumsum(Avg_TS_CNG),
#                      VORP_Chain = cumsum(Avg_VORP_CNG),
#                      PER_Chain = cumsum(Avg_PER_CNG),
#                      BLK_Chain = cumsum(Avg_BLK_CNG)) %>% 
#   select(Age, USG_Chain:BLK_Chain)
  
#ALTERNATE WITH WEIGHTED AVERAGE Calculate Aging curves using delta method
#4B) Now need to "chain" those averages together
  
NBA_delta_chain <- NBA_delta %>% mutate(USG_Chain = cumsum(Avg_USG_CNG),
                                          TRB_Chain = cumsum(Avg_TRB_CNG),
                                          FTr_Chain = cumsum(Avg_FTr_CNG),
                                          TS_Chain = cumsum(Avg_TS_CNG),
                                          VORP_Chain = cumsum(Avg_VORP_CNG),
                                          PER_Chain = cumsum(Avg_PER_CNG),
                                          BLK_Chain = cumsum(Avg_BLK_CNG)) %>%
    select(Age, USG_Chain:BLK_Chain)
  
NBA_delta_chain

#4C) Tactical Questions: When are player's approaching peak or peaking 
#4C) Is this different for different types of players? -> Jump to 6C for Positions


# VORP TABLE

AgingCurve_GT_VORP <- NBA_delta_chain %>% select(Age, VORP_Chain) %>% 
  pivot_wider(names_from = Age, values_from = VORP_Chain) %>% 
  gt() %>% tab_header(title = "NBA Players Peak Between Ages 24 and 28", 
                      subtitle = "Cumulative Avg. Change in VORP by Age") %>% 
  tab_footnote(footnote = "NBA Players from 1996-97 to 2020-21 | Source: Basketball Reference",
               locations = cells_column_labels(1)) %>%
  tab_spanner(label = "Ascending", columns = (1:5)) %>% 
  tab_spanner(label = "Peak", columns = 6:10) %>% 
  tab_spanner(label = "Descending", columns = 11:18) %>%
  fmt_number(columns =  1:18, decimals = 1) %>%
  tab_style(
    style = list(cell_fill(color = scales::alpha("dark green", 0.7)),
                 cell_text(color = "white", weight = "bold")), locations = list(cells_body(columns = 6:10))) 

AgingCurve_GT_VORP

#5) Let us visualize what we have calculated

#5A) VORP Aging Curve

AgingCurve_Plot_VORP <- ggplot(data = NBA_delta_chain, aes(x = Age, y = VORP_Chain))+geom_smooth()+
  geom_label_repel(data = NBA_delta_chain %>% filter(Age == 20 | Age == 25|Age == 36),
                   aes(label = paste(paste("Age:", Age), sep = " | ", paste("cVORP: ", round(VORP_Chain, 1))))) +
  labs(title = "NBA Players Peak Between Ages 24 and 28",
       x = "Age",y = "Cumulative Avg. Change in VORP", 
       caption ="NBA Players from 1996-97 to 2020-21 | Source: Basketball Reference")+
  theme_economist_white()

AgingCurve_Plot_VORP

#5B) What about a player specific VORP Aging Curve?


#5B) Delta Method
NBA_delta_Players <- NBA_2021_to_1997 %>% arrange(PlayerID, Age) %>%
  group_by(Player, PlayerID) %>%
  summarise(Age, USG_CNG = (USG.-lag(USG.)), 
            TRB_CNG = (TRB.-lag(TRB.)),
            FTr_CNG = (FTr-lag(FTr)),
            TS_CNG = (TS.-lag(TS.)),
            VORP_CNG = (VORP-lag(VORP)),
            PER_CNG = (PER- lag(PER))) %>%
  replace_na(list(USG_CNG=0, TRB_CNG=0, FTr_CNG=0, TS_CNG=0, VORP_CNG=0, PER_CNG=0))

NBA_delta_Players[is.nan.tibble(NBA_delta_Players)] <- 0
NBA_delta_Players

#5B) Chain

NBA_players_Chain <- NBA_delta_Players %>% 
  group_by(Player, PlayerID) %>% summarise(Age, USG_Chain = cumsum(USG_CNG),
                                           TRB_Chain = cumsum(TRB_CNG),
                                           FTr_Chain = cumsum(FTr_CNG),
                                           TS_Chain = cumsum(TS_CNG),
                                           VORP_Chain = cumsum(VORP_CNG),
                                           PER_Chain = cumsum(PER_CNG)) %>% 
  select(Age, USG_Chain:PER_Chain)

NBA_players_Chain %>% group_by(PlayerID, Player) %>% summarise(max = max(VORP_Chain)) %>% arrange(-max)
NBA_players_Chain %>% filter(Player=="Dirk Nowitzki")
  

#5C Visualize 

NBA_players_Chain %>% filter(Player=="Dirk Nowitzki") %>% 
  select(Age, VORP_Chain) %>% 
  pivot_wider(names_from = Age, values_from = VORP_Chain) %>% 
  gt() %>% tab_header(title = "Dirk Nowitzki peaked at 27", 
                      subtitle = "Cumulative Avg. Change in VORP by Age") %>% 
  tab_footnote(footnote = "Source: Basketball Reference",
               locations = cells_column_labels(1)) %>%
  fmt_number(columns =  1:18, decimals = 1)

ggplot(data = NBA_players_Chain %>% filter(Player=="Dirk Nowitzki"), aes(x = Age, y = VORP_Chain))+
  geom_smooth()+
  geom_label_repel(data = NBA_players_Chain %>% filter(Player=="Dirk Nowitzki", Age == 21 | Age == 27|Age == 40),
                   aes(label = paste(paste("Age:", Age), sep = " | ", paste("cVORP: ", round(VORP_Chain, 1))))) +
  labs(title = "Dirk Nowitzki Peaked at 27",
       x = "Age",y = "Cumulative Avg. Change in VORP", 
       caption ="Source: Basketball Reference")+
  theme_economist_white()


#6) Breaking down  by position
#6A) Delta Method

Pos_levels <- c("PG", "SG", "SF", "PF", "C")

NBA_delta_Pos <- NBAdata_minimums %>% group_by(PlayerID, Pos) %>% 
  summarise(Age, MP, 
            USG_CNG = (USG.-lead(USG.)),
            TRB_CNG = (TRB.-lead(TRB.)),
            FTr_CNG = (FTr-lead(FTr)),
            TS_CNG = (TS.-lead(TS.)),
            VORP_CNG = (VORP-lead(VORP)),
            PER_CNG = (PER- lead(PER)),
            BLK_CNG = (BLK.- lead(BLK.))) %>%
  group_by(Age, Pos) %>% summarise(Avg_USG_CNG = weighted.mean(USG_CNG, na.rm = TRUE, w=MP),
                                   Avg_TRB_CNG = weighted.mean(TRB_CNG, na.rm = TRUE, w=MP),
                                   Avg_FTr_CNG = weighted.mean(FTr_CNG, na.rm = TRUE, w=MP),
                                   Avg_TS_CNG = weighted.mean(TS_CNG, na.rm = TRUE, w=MP),
                                   Avg_VORP_CNG = weighted.mean(VORP_CNG, na.rm = TRUE, w=MP),
                                   Avg_PER_CNG = weighted.mean(PER_CNG, na.rm = TRUE, w=MP),
                                   Avg_BLK_CNG = weighted.mean(BLK_CNG, na.rm = TRUE, w=MP)) %>%
  mutate(Pos = factor(Pos, levels = Pos_levels))

NBA_delta_Pos[is.nan.tibble(NBA_delta_Pos)] <- 0
NBA_delta_Pos 


#6B) Chain them together
NBA_delta_Pos_chain <- NBA_delta_Pos %>% 
  group_by(Pos) %>% 
  mutate(USG_Chain = cumsum(Avg_USG_CNG),
         TRB_Chain = cumsum(Avg_TRB_CNG),
         FTr_Chain = cumsum(Avg_FTr_CNG),
         TS_Chain = cumsum(Avg_TS_CNG),
         VORP_Chain = cumsum(Avg_VORP_CNG),
         PER_Chain = cumsum(Avg_PER_CNG),
         BLK_Chain = cumsum(Avg_BLK_CNG)) %>% 
  select(Age, USG_Chain:BLK_Chain) %>% arrange(Age, Pos)


#6C) Tactical Questions: When are player's approaching peak or peaking (By Position)


# NBA_delta_Pos_chain %>% select(Age, Pos, VORP_Chain) %>% 
#   pivot_wider(names_from = Pos, values_from = VORP_Chain) %>% select(Age, PG, SG, SF, PF, C) %>%
#   gt() %>% fmt_number(columns = c(PG, SG, SF, PF, C)) %>%
#   tab_header(title = "Cumulative VORP Change by Position", 
#              subtitle = "NBA Players from 1996-97 to 2020-21") %>% 
#   data_color(columns = c(PG, SG, SF, PF, C), colors = scales::col_numeric(c("#f87274", "#ffeb84", "#63be7b"),domain = NULL))

NBA_delta_Pos_chain %>% select(Age, VORP_Chain) %>% 
  pivot_wider(names_from = Age, values_from = VORP_Chain) %>% 
  gt() %>% fmt_number(columns =  1:19, decimals = 1) %>% tab_header(title = "Cumulative VORP Change by Position", 
                                                                    subtitle = "Cumulative Avg. Change in VORP by Age") %>% 
  tab_footnote(footnote = "NBA Players from 1996-97 to 2020-21 | Source: Basketball Reference",
               locations = cells_column_labels(1:2))

#7) Visualizing by position 

ggplot(data = NBA_delta_Pos_chain, aes(x = Age, y = VORP_Chain), color = NBA_delta_Pos_chain$Pos) +
  geom_smooth(data = NBA_delta_Pos_chain %>% filter(Pos == "PG" |Pos == "SG" | Pos == "SF"| Pos == "PF"| Pos == "C"), aes(color = Pos))+
  labs(title = "Positions Peak at Different Ages",
       x = "Age",y = "Cumulative Avg. Change in VORP", caption ="NBA Players from 1996-97 to 2020-21 | Source: Basketball Reference") + 
  theme_economist_white() 
