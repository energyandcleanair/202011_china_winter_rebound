plots.compare_past_years <- function(m, poll, folder=NULL, width=14, height=12, dpi=100, ...){

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
           y=expression(paste(mu, "g/m3")),
           x=NULL) +
      scale_fill_brewer(name=NULL))


  if(!is.null(folder)){
    ggsave(file.path(folder, paste0(poll,"_year_comparison.png")), p, width=width, height=height, dpi=dpi, ...)
  }

  return(p)

}

plots.targets_ts <- function(targetmeans.Q1, targetmeans.Q4, m.keyregions, nrow=2, width=7.5,height=7.5, dpi=270, ...){

  m <- m.keyregions %>%
    filter(!is.na(region_id)) %>%
    rcrea::utils.running_average(365) %>%
    mutate(type="Observations") %>%
    select(region_id, date, value, type)

  t1 <-targetmeans.Q1 %>%
    mutate(date=as.Date("2021-03-31")) %>%
    group_by(region_id=keyregion2018, date) %>%
    summarise_at("value", mean, na.rm=T) %>%
    mutate(type="Target")

  t4 <-targetmeans.Q4 %>%
    mutate(date=as.Date("2020-12-31")) %>%
    group_by(region_id=keyregion2018, date) %>%
    summarise_at("value", mean, na.rm=T) %>%
    mutate(type="Target")

  lv <- m %>%
    #filter(date==max(date)) %>%
    filter(date=='2020-10-01') %>%
    select(region_id, date, value, type) %>%
    bind_rows(t4) %>%
    mutate(type="Target")

  (p <- ggplot(m, aes(date, value, col=type, linetype=type)) +
      geom_line(size=1) +
      geom_point(data=bind_rows(t1,t4)) +
      geom_line(data=bind_rows(t1,t4), size=1) +
      geom_line(data=lv, size=1) +
      facet_wrap(~region_id, nrow = nrow) +
      rcrea::theme_crea() +
      scale_x_date(limits=as.Date(c("2020-01-01","2021-04-15")),
                   breaks=seq(as.Date("2020-01-01"), as.Date("2021-04-15"), by="3 month"),
                   date_labels="%b %Y"
      ) +
      #scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0,.05))) +
      theme(legend.position = 'bottom',
            panel.grid.minor.x = element_line(colour = "grey90"),
            axis.text.x = element_text(angle=25, vjust=.5)) +
      scale_linetype_discrete(name='', guide = guide_legend(ncol=2)) +
      scale_color_manual(name='', values=c('black', 'darkred')) +
      labs(title="Are key regions on track?",
           subtitle="Rolling 12-month mean PM2.5 level and path to 2020-2021 winter PM2.5 target",
           y=expression(paste("PM2.5 concentration [",mu,"g/m3]")),
           x=NULL))

  d <- file.path(dir_results_plots, "regional", "EN")
  dir.create(d, showWarnings = F, recursive = T)
  ggsave(file.path(d, "target_regional.png"),
         width=width, height=height, dpi=dpi, ...)
}

plots.targets_col <- function(t.keyregions, nrow=2, width=7.5,height=7.5, dpi=270, ...){

  d <- t.keyregions %>% select(keyregion2018, `Target`=target_reduction, `Quarter-to-Date`=QTD_reduction) %>%
    gather("type","value",-keyregion2018) %>%
    filter(!is.na(keyregion2018))

  ggplot(d) +
    geom_bar(stat="identity", aes(type, value, fill=type)) +
    facet_wrap(~keyregion2018) +
    theme_crea() +
    scale_y_continuous(labels=scales::percent) +
    geom_hline(yintercept=0) +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
    labs("Year-on-year varition of PM2.5 levels in 2020Q4",
         y="Year-on-year change")

  d <- file.path(dir_results_plots, "regional", "EN")
  dir.create(d, showWarnings = F, recursive = T)
  ggsave(file.path(d, "target_regional_cols.png"),
         width=width, height=height, dpi=dpi, ...)

}

plots.targets_yoyts_vs_targets <- function(m.keyregions, t.keyregions, nrow=2, width=7.5,height=7.5, dpi=270, ...){

  poll <- "pm25"

  m <- m.keyregions %>%
    filter(!is.na(region_id),
           date>="2018-10-01",
           poll==!!poll) %>%
    rcrea::utils.running_average(90) %>%
    filter(date>="2019-01-01") %>%
    mutate(year=lubridate::year(date),
           date = `year<-`(date, 0)) %>%
    spread(year, value) %>%
    mutate(value=`2020`/`2019`-1) %>%
    mutate(date=`year<-`(date, 2020),
           type="Observations") %>%
    select(region_id, poll, date, value, type)

  if(poll=="pm25"){
    t <- t.keyregions %>%
      select(region_id=keyregion2018, value=target_reduction) %>%
      mutate(date=as.Date("2020-12-31"),
             type="Target",
             poll="pm25") %>%
      bind_rows(.,
        m %>%
        # filter(date==max(max(m.keyregions$date)))) %>%
        filter(date=='2020-10-01')) %>%
        mutate(type="Target")

    m <- bind_rows(m, t)
  }


  (p <- ggplot(m %>% filter(!is.na(region_id))) +
    geom_line(aes(date, value, col=type, linetype=type), size=0.5) +
    facet_wrap(~region_id, nrow=nrow) +
    geom_hline(yintercept=0) +
    scale_y_continuous(labels=scales::percent) +
    theme_crea() +
    theme(legend.position = 'bottom',
          panel.grid.minor.x = element_line(colour = "grey90"),
          axis.text.x = element_text(angle=25, vjust=.5)) +
    scale_linetype_discrete(name='', guide = guide_legend(ncol=2)) +
    scale_color_manual(name='', values=c('black', 'darkred')) +
    labs(title="Are key regions on track?",
         subtitle=paste("Y-o-y change of 90-day running mean of", poll_str(poll), "levels"),
         x=NULL,
         y="Year-on-year change") +
    rcrea::CREAtheme.scale_fill_crea_d())


  d <- file.path(dir_results_plots, "regional", "EN")
  dir.create(d, showWarnings = F, recursive = T)
  ggsave(file.path(d, paste0("target_regional_90running_", poll,".png")),
         width=width, height=height, dpi=dpi, ...)

}


plots.quarter_anomalies <- function(m.dew.regional, absolute_or_percent="absolute"){

  absolute <- absolute_or_percent == "absolute"
  process_id <- ifelse(absolute, "anomaly", "anomaly_percent")
  ylab <- ifelse(absolute, paste0("Anomaly [",mu,"g/m3]"), "Anomaly [%]")

  m.plot <- m.dew.regional %>%
    filter(process_id==!!process_id,
           date>="2020-01-01") %>%# Anomaly in absolute terms
    mutate(Q=gsub("\\.","Q",as.character(lubridate::quarter(date, with_year = T)))) %>%
    group_by(poll, region_id, process_id, Q) %>%
    summarize_at("value", mean, na.rm=T)

  polls <- unique(m.plot$poll)
  for(poll in polls){
    (p <-  ggplot(m.plot %>% filter(poll==!!poll)) +
      geom_bar(stat="identity", aes(Q, value, fill="a"), show.legend = F) +
      facet_wrap(~region_id) +
      rcrea::CREAtheme.scale_fill_crea_d() +
      theme_crea() +
      labs(
        y=ylab,
        x=NULL,
        title=paste0(rcrea::poll_str(poll), " pollutant levels in 2020"),
        subtitle="Weather-corrected anomalies in 2020 quarters",
        caption="A negative value indicates an air pollution level lower than what would have been expected under observed weather conditions.
Source: CREA based on MEE."
      ))

    d <- file.path(dir_results_plots, "deweathered", "regional")
    dir.create(d, showWarnings = F, recursive = T)
    ggsave(file.path(d, paste0("mee_region_anomaly_qtd_",poll,"_",absolute_or_percent,".png")),
           p,
           width=8,
           height=8)
  }
}

plots.quarter_yoy <- function(m.dew, m.quarterly){


  process_id <- "anomaly_offsetted"
  ylab <- "Year-on-year change [%]"

  m.plot <- m.dew %>%
    filter(process_id=="anomaly_offsetted",
           poll=="pm25") %>%
    mutate(Q=gsub("\\.","Q",as.character(lubridate::quarter(date, with_year = T)))) %>%
    filter(Q %in% c("2019Q4", "2020Q4")) %>%
    group_by(keyregion2018, region_id, Q) %>%
    summarize_at("value", mean, na.rm=T) %>%
    select(keyregion2018, region_id, Q, value) %>%
    mutate_at('Q', make.names) %>% spread(Q, value) %>%
    mutate(QTD_reduction = X2020Q4/X2019Q4-1) %>% select(-starts_with('X')) %>%
    full_join(m.quarterly %>% filter(source=='target', Q==2020.4) %>%
                mutate(Q="2020Q4", region_id=tolower(CityEN)) %>%
                select(region_id, keyregion2018, target_reduction), .) %>%
    filter(!is.na(target_reduction)) %>%
    tidyr::pivot_longer(cols=c("target_reduction", "QTD_reduction"), names_to="indicator")


  (p <-  ggplot(m.plot %>% mutate(region_name=tools::toTitleCase(region_id))) +
       geom_bar(stat="identity", aes(value, region_name, fill=indicator), position="dodge") +
       theme_crea() +
       rcrea::CREAtheme.scale_fill_crea_d() +
       labs(
         y=ylab,
         x=NULL,
         title="PM2.5 year-to-date reductions in 2020Q4"
       ))

    dir.create(d, showWarnings = F, recursive = T)
    ggsave(file.path(d, paste0("mee_region_yoy_qtd_",poll,"_",absolute_or_percent,".png")),
           p,
           width=8,
           height=8)
}
