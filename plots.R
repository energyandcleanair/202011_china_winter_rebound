plots.compare_past_years <- function(m, poll, folder=NULL){

  m.365 <- m %>%
    filter(poll==!!poll) %>%
    rcrea::utils.rolling_average("day",365,"value")

  m.365.last <- m.365 %>%
    filter(lubridate::yday(date)==lubridate::yday(max(date))) %>%
    mutate(year=lubridate::year(date),
           region_name=tools::toTitleCase(region_name))

  (p <- ggplot(m.365.last) +
    geom_bar(stat="identity", aes(x=year, y=value, fill=factor(year, ordered=T))) +
    facet_wrap(~region_name) +
    rcrea::theme_crea() +
    labs(title="PM2.5 levels in selected Chinese cities",
         subtitle=paste("365-running day average on",strftime(max(m.365$date),"%d %B")),
         caption="Source: CREA based on MEE",
         y="µg/m3",
         x=NULL) +
    scale_fill_brewer(name=NULL))


  if(!is.null(folder)){
    ggsave(file.path(folder, paste0(poll,"_year_comparison.png")), p, width=14, height=12)
  }

  return(p)

}


plots.targets <- function(targetmeans.Q1, targetmeans.Q4, m.keyregions){

m <- m.keyregions %>%
  filter(!is.na(region_id)) %>%
  rcrea::utils.running_average(365) %>%
  mutate(type="Observations") %>%
  select(region_id, date, value, type)

t1 <-targetmeans.Q1 %>%
  mutate(date=as.Date("2020-03-31")) %>%
  group_by(region_id=keyregion2018, date) %>%
  summarise_at("value", mean, na.rm=T) %>%
  mutate(type="Target")

t4 <-targetmeans.Q4 %>%
  mutate(date=as.Date("2020-12-31")) %>%
  group_by(region_id=keyregion2018, date) %>%
  summarise_at("value", mean, na.rm=T) %>%
  mutate(type="Target")

lv <- m %>%
  filter(date==max(date)) %>%
  select(region_id, date, value, type) %>%
  bind_rows(t4) %>%
  mutate(type="Target")

(p <- ggplot(m, aes(date, value, col=type, linetype=type)) +
  geom_line(size=1) +
  geom_point(data=bind_rows(t1,t4)) +
  geom_line(data=lv, size=1) +
  facet_wrap(~region_id) +
  rcrea::theme_crea() +
  scale_x_date(limits=as.Date(c("2020-01-01","2020-12-31")),
               breaks=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 month"),
               date_labels="%b %Y"
               ) +
  scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0,.05))) +
  theme(legend.position = 'bottom',
        panel.grid.minor.x = element_line(colour = "grey90")) +
  scale_linetype_discrete(name='', guide = guide_legend(ncol=2)) +
  scale_color_manual(name='', values=c('black', 'darkred')) +

  labs(title="Are key regions on track?",
       subtitle="Rolling 12-month mean PM2.5 level and path to 2019-2020 winter PM2.5 target",
       y="PM2.5 concentration [µg/m3]",
       x=NULL))

d <- file.path(dir_results_plots, "regional", "EN")
dir.create(d, showWarnings = F, recursive = T)
ggsave(file.path(d, "target_regional.png"),
       width=10,
       height=10)
}

#WIP
plots.quarter_anomalies <- function(m.dew.regional){

  m.plot <- m.dew.regional %>%
    filter(process_id=="anomaly_gbm_lag1_city_mad",
           date>="2020-01-01") %>%# Anomaly in absolute terms
    mutate(Q=gsub("\\.","Q",as.character(lubridate::quarter(date, with_year = T)))) %>%
    group_by(poll, region_id, process_id, Q) %>%
    summarize_at("value", mean, na.rm=T)

  polls <- unique(m.plot$poll)
  for(poll in polls){
    p <-  ggplot(m.plot %>% filter(poll)) +
      geom_bar(stat="identity", aes(Q, value)) +
      facet_wrap(poll~region_id) +
      theme_crea() +
      labs(
        y="Anomaly [µg/m3]",
        x=NULL
      )
  }
}
