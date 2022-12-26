traditional <- 
  read_csv("teams_traditional.csv") %>%
  select(TEAM, OREB, DREB)
opponent <- 
  read_csv("teams_opponent.csv") %>%
  select(TEAM, `OPP\nOREB`, `OPP\nDREB`)
logos <-
  read_csv("https://raw.githubusercontent.com/jeremydumalig/DataBank/master/logos.csv") %>%
  select(TEAM, URL)

teams <- 
  merge(traditional, 
        opponent,
        by='TEAM') %>%
  mutate(`ORB%` = 100 * OREB / (OREB + `OPP\nDREB`),
         `DRB%` = 100 * DREB / (DREB + `OPP\nOREB`),
         `d_ORB%` = round(`ORB%` - mean(`ORB%`), 1),
         `d_DRB%` = round(`DRB%` - mean(`DRB%`), 1)) %>%
  select(TEAM, `ORB%`, `DRB%`, `d_ORB%`, `d_DRB%`) %>%
  merge(logos,
        by='TEAM')

teams %>%
  ggplot(aes(x=`d_ORB%`,
             y=`d_DRB%`)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_image(aes(image=URL),
             size=0.1,
             stat='identity') +
  labs(title="Who are the best and worst rebounding teams?",
       subtitle="NBA Regular Season | Through December 25, 2022",
       caption="* Compared to League Average",
       x="Offensive Rebound Rate* (ORB%)",
       y="Defensive Rebound Rate* (DRB%)") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    plot.title = element_text(size=18,
                              face="bold"),
    plot.subtitle = element_text(size=14),
    plot.caption = element_text(size=10))