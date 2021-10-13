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


# Targets -----------------------------------------------------------------


plots.targets_ts <- function(targetmeans.Q1, targetmeans.Q4, m.keyregions, folder=file.path(dir_results_plots, "regional", "EN"), nrow=2, width=7.5,height=7.5, dpi=270, ...){

  m <- m.keyregions %>%
    filter(!is.na(location_id)) %>%
    rcrea::utils.running_average(365) %>%
    mutate(type="Observations") %>%
    select(location_id, date, value, type)

  t1 <-targetmeans.Q1 %>%
    mutate(date=as.Date("2021-03-31")) %>%
    group_by(location_id=keyregion2018, date) %>%
    summarise_at("value", mean, na.rm=T) %>%
    mutate(type="Target")

  t4 <-targetmeans.Q4 %>%
    mutate(date=as.Date("2020-12-31")) %>%
    group_by(location_id=keyregion2018, date) %>%
    summarise_at("value", mean, na.rm=T) %>%
    mutate(type="Target")

  lv <- m %>%
    #filter(date==max(date)) %>%
    filter(date=='2020-10-01') %>%
    select(location_id, date, value, type) %>%
    bind_rows(t4) %>%
    mutate(type="Target")

  (p <- ggplot(m, aes(date, value, col=type, linetype=type)) +
      geom_line(size=1) +
      geom_point(data=bind_rows(t1,t4)) +
      geom_line(data=bind_rows(t1,t4), size=1) +
      geom_line(data=lv, size=1) +
      facet_wrap(~location_id, nrow = nrow) +
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

  dir.create(folder, showWarnings = F, recursive = T)
  ggsave(file.path(folder, "target_regional.png"),
         width=width, height=height, dpi=dpi, ...)
}

plots.targets_col <- function(t.keyregions, folder=file.path(dir_results_plots, "regional", "EN"), nrow=2, width=7.5,height=7.5, dpi=270, ...){

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

  dir.create(folder, showWarnings = F, recursive = T)
  ggsave(file.path(folder, "target_regional_cols.png"),
         width=width, height=height, dpi=dpi, ...)

}


plots.targets_yoyts_vs_targets <- function(m.keyregions, t.keyregions,
                                           en_or_zh="en",
                                           folder=file.path(dir_results_plots, "regional", "EN"),
                                           nrow=2, width=7.5,height=7.5, dpi=270, ...){

  poll <- "pm25"

  lab_obs <- ifelse(en_or_zh=="zh","实际值","Observations")
  lab_tgt <- ifelse(en_or_zh=="zh","达标路线值","Target")


  m <- m.keyregions %>%
    filter(!is.na(location_id),
           date>="2018-10-01",
           poll==!!poll) %>%
    rcrea::utils.running_average(90, group_by_cols = c("location_id","poll")) %>%
    filter(date>="2019-01-01") %>%
    mutate(year=lubridate::year(date),
           date_year = `year<-`(date, 0)) %>%
    group_by(location_id, poll, date_year) %>%
    arrange(year) %>%
    mutate(
      # value=value/lag(value)-1,
      value_1=value/lag(value, n=1)-1,
      value_2=value/lag(value, n=2)-1,
      type=lab_obs
    ) %>%
    mutate(value=ifelse(year==2021, value_2, value_1)) %>%
    filter(!is.na(value) & !is.na(location_id)) %>%
    select(-c(date_year))

  t <- t.keyregions %>%
    select(location_id=keyregion2018, value=target_reduction, Q) %>%
    mutate(date=recode(as.character(Q),
                       "2020.4"=as.Date("2020-12-31"),
                       "2021.1"=as.Date("2021-03-31")),
           type=lab_tgt,
           poll="pm25") %>%
    bind_rows(.,
              m %>%
                # filter(date==max(max(m.keyregions$date)))) %>%
                filter(date=='2020-10-01')) %>%
    mutate(type=lab_tgt)

  # rect.target <- t %>%
  #   filter(type=="Target", !is.na(value), !is.na(location_id)) %>%
  #   arrange(date) %>%
  #   bind_rows(
  #     mutate(., value=ymin) %>% arrange(desc(date)),
  #     .
  #   ) %>%
  #   arrange(location_id)

  m <- bind_rows(m, t) %>%
    filter(!is.na(location_id))

  if(en_or_zh=="zh"){
    m$location_id <- recode(toupper(m$location_id),
                                     "2+26"="京津冀及周边",
                                     "FENWEI"="汾渭平原",
                                     "PRD"="珠三角地区",
                                     "YRD"="长三角地区")
  }


  # scale parameters
  ymin <- min(m$value, na.rm=T) * 1.1
  maxabs <- max(abs(m$value), na.rm=T)
  maxdate <- as.Date(max(m$date, na.rm=T))
  chg_colors <- c("#35416C", "#8CC9D0", "darkgray", "#CC0000", "#990000")


  p <- ggplot() +
    # To force legend
    geom_line(data=m %>% distinct(type, .keep_all = T), aes(date, value, linetype=type)) +
    # geom_polygon(data=rect.target, aes(x=date, y=value,fill=type, alpha=type)) +
    geom_line(data=m %>% filter(type==lab_obs),
              aes(date, value, col=value), linetype="solid", size=0.7) +
    geom_line(data=m %>% filter(type==lab_tgt),
              aes(date, value),col="darkred", linetype="dashed", size=0.7) +
    geom_point(data=m %>% filter(type==lab_tgt, date>today()), aes(date, value),
               shape=1, col='darkred') +

    facet_wrap(~location_id, nrow=nrow) +
    geom_hline(yintercept=0) +
    # geom_point(data=rect.target, aes(date,value,col=type)) +
    theme_crea() +
    theme(legend.position = 'bottom',
          panel.grid.minor.x = element_line(colour = "grey95"),
          panel.grid.major.x = element_line(colour = "grey80"),
          axis.text.x = element_text(angle=25, vjust=.5)) +
    scale_linetype_discrete(name='', guide = guide_legend(ncol=2)) +
    # scale_color_manual(name='', values=c('black', 'darkred')) +
    scale_fill_manual(name='', values=c('darkred')) +
    scale_alpha_manual(name='', values=c(0.4)) +
    scale_x_date(limits=as.Date(c("2020-01-01", maxdate)),
                 minor_breaks =seq(as.Date("2020-01-01"), maxdate, by="1 month"),
                 breaks=seq(as.Date("2020-01-01"), maxdate, by="3 month"),
                 date_labels="%b %Y"
    ) +
    scale_y_continuous(limits=c(ymin, NA), labels=scales::percent, expand=expansion(mult=c(0,.05))) +
    scale_color_gradientn(colors = chg_colors, guide = F, limits=c(-maxabs,maxabs))


  if(tolower(en_or_zh)=="en"){
    p <- p +
      labs(title="Are key regions on track?",
           subtitle=paste("Year-on-year change in", poll_str(poll), "levels and path to targets"),
           x=NULL,
           y="90-day running mean, year-on-year")
  }else{
    p <- p +
      labs(title="重点区域治理进度如何?",
           subtitle=paste0(poll_str(poll), "浓度同比变化及实现2020-2021秋冬季治理目标达标路线"),
           x=NULL,
           y="90天移动平均同比变化")
  }

  if(!is.null(folder)) {
    d <- folder
    dir.create(d, showWarnings = F, recursive = T)
    ggsave(file.path(d, paste0("target_regional_90running_", poll,"_",en_or_zh,".png")),
           plot=p, width=width, height=height, dpi=dpi, ...)
  }

  return(p)
}

plots.observed_vs_targets <- function(m.keyregions,
                                      t.keyregions.abs,
                                      en_or_zh="en",
                                      folder=file.path(dir_results_plots, "regional", "EN"),
                                      nrow=2, width=7.5,height=7.5, dpi=270, ...){

  poll <- "pm25"

  lab_obs <- ifelse(en_or_zh=="zh","实际值","Observations")
  lab_tgt <- ifelse(en_or_zh=="zh","达标路线值","Target")


  m <- m.keyregions %>%
    filter(!is.na(location_id),
           date>="2018-10-01",
           poll==!!poll) %>%
    rcrea::utils.running_average(90, group_by_cols = c("location_id","poll")) %>%
    filter(date>="2019-01-01") %>%
    mutate(type=lab_obs)
    # mutate(year=lubridate::year(date),
    #        date_year = `year<-`(date, 0)) %>%
    # group_by(location_id, poll, date_year) %>%
    # arrange(year) %>%
    # mutate(
    #   value=value/lag(value)-1,
    #   type=lab_obs
    # ) %>%
    # filter(!is.na(value) & !is.na(location_id)) %>%
    # select(-c(date_year))

  t <- t.keyregions.abs %>%
    select(location_id=keyregion2018, value=target, Q) %>%
    mutate(date=recode(as.character(Q),
                       "2020.4"=as.Date("2020-12-31"),
                       "2021.1"=as.Date("2021-03-31")),
           poll="pm25") %>%
    bind_rows(.,
              m %>%
                # filter(date==max(max(m.keyregions$date)))) %>%
                filter(date=='2020-10-01')) %>%
    mutate(type=lab_tgt)

  # rect.target <- t %>%
  #   filter(type=="Target", !is.na(value), !is.na(location_id)) %>%
  #   arrange(date) %>%
  #   bind_rows(
  #     mutate(., value=ymin) %>% arrange(desc(date)),
  #     .
  #   ) %>%
  #   arrange(location_id)

  m <- bind_rows(m, t) %>%
    filter(!is.na(location_id))

  if(en_or_zh=="zh"){
    m$location_id <- recode(toupper(m$location_id),
                            "2+26"="京津冀及周边",
                            "FENWEI"="汾渭平原",
                            "PRD"="珠三角地区",
                            "YRD"="长三角地区")
  }


  # scale parameters
  ymin <- min(m$value, na.rm=T) * 1.1
  maxabs <- max(abs(m$value), na.rm=T)
  maxdate <- as.Date(max(m$date, na.rm=T))
  # chg_colors <- c("#35416C", "#8CC9D0", "darkgray", "#CC0000", "#990000")


  p <- ggplot() +
    # To force legend
    geom_line(data=m %>% distinct(type, .keep_all = T), aes(date, value, linetype=type)) +
    # geom_polygon(data=rect.target, aes(x=date, y=value,fill=type, alpha=type)) +
    geom_line(data=m %>% filter(type==lab_obs),
              aes(date, value, col="1"), linetype="solid", size=0.7, show.legend = F) +
    geom_line(data=m %>% filter(type==lab_tgt),
              aes(date, value), col="darkred", linetype="dashed", size=0.7) +
    geom_point(data=m %>% filter(type==lab_tgt), aes(date, value),
               shape=1, col='darkred') +

    facet_wrap(~location_id, nrow=nrow) +
    # geom_hline(yintercept=0) +
    # geom_point(data=rect.target, aes(date,value,col=type)) +
    theme_crea() +
    theme(legend.position = 'bottom',
          panel.grid.minor.x = element_line(colour = "grey95"),
          panel.grid.major.x = element_line(colour = "grey80"),
          axis.text.x = element_text(angle=25, vjust=.5)) +
    scale_linetype_discrete(name='', guide = guide_legend(ncol=2)) +
    # scale_color_manual(name='', values=c('black', 'darkred')) +
    # scale_fill_manual(name='', values=c('darkred')) +
    # scale_alpha_manual(name='', values=c(0.4)) +
    scale_x_date(limits=as.Date(c("2020-01-01", maxdate)),
                 minor_breaks =seq(as.Date("2020-01-01"), maxdate, by="1 month"),
                 breaks=seq(as.Date("2020-01-01"), maxdate, by="3 month"),
                 date_labels="%b %Y"
    ) +
    scale_y_continuous(limits=c(0, NA),
                       expand=expansion(mult=c(0,.05))) +
    rcrea::scale_color_crea_d()
    # scale_color_gradientn(colors = chg_colors, guide = F, limits=c(-maxabs,maxabs))


  if(tolower(en_or_zh)=="en"){
    p <- p +
      labs(title="Are key regions on track?",
           subtitle=paste(poll_str(poll), "levels and path to targets"),
           x=NULL,
           y="µg/m3")
  }else{
    p <- p +
      labs(title="重点区域治理进度如何?",
           subtitle=paste0(poll_str(poll), "浓度及实现2020-2021秋冬季治理目标达标路线"),
           x=NULL,
           y="µg/m3")
  }

  if(!is.null(folder)) {
    d <- folder
    dir.create(d, showWarnings = F, recursive = T)
    ggsave(file.path(d, paste0("target_regional_abs_", poll,"_",en_or_zh,".png")),
           plot=p, width=width, height=height, dpi=dpi, ...)
  }

  return(p)
}


# Deweathered -------------------------------------------------------------



plots.quarter_anomalies <- function(m.dew.regional,
                                    absolute_or_percent="absolute",
                                    folder=file.path(dir_results_plots, "deweathered", "regional")){

  absolute <- absolute_or_percent == "absolute"
  process_id <- ifelse(absolute, "anomaly", "anomaly_vs_counterfactual")
  ylab <- ifelse(absolute, paste0("Anomaly [",mu,"g/m3]"), "Anomaly [%]")

  m.plot <- m.dew.regional %>%
    filter(process_id==!!process_id,
           date>="2020-01-01") %>%# Anomaly in absolute terms
    mutate(Q=gsub("\\.","Q",as.character(lubridate::quarter(date, with_year = T)))) %>%
    group_by(poll, location_id, process_id, Q) %>%
    summarize_at("value", mean, na.rm=T)

  polls <- unique(m.plot$poll)
  for(poll in polls){
    (p <-  ggplot(m.plot %>% filter(poll==!!poll)) +
       geom_bar(stat="identity", aes(Q, value, fill="a"), show.legend = F) +
       facet_wrap(~location_id) +
       rcrea::scale_color_crea_d() +
       theme_crea() +
       labs(
         y=ylab,
         x=NULL,
         title=paste0(rcrea::poll_str(poll), " pollutant levels in 2020"),
         subtitle="Weather-corrected anomalies in 2020 quarters",
         caption="A negative value indicates an air pollution level lower than what would have been expected under observed weather conditions.
Source: CREA based on MEE."
       ))

    if(!absolute){
      p <- p + scale_y_continuous(labels=scales::percent)
    }

    dir.create(folder, showWarnings = F, recursive = T)
    ggsave(file.path(folder, paste0("mee_region_anomaly_qtd_",poll,"_",absolute_or_percent,".png")),
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
    group_by(keyregion2018, location_id, Q) %>%
    summarize_at("value", mean, na.rm=T) %>%
    select(keyregion2018, location_id, Q, value) %>%
    mutate_at('Q', make.names) %>% spread(Q, value) %>%
    mutate(QTD_reduction = X2020Q4/X2019Q4-1) %>% select(-starts_with('X')) %>%
    full_join(m.quarterly %>% filter(source=='target', Q==2020.4) %>%
                mutate(Q="2020Q4", location_id=tolower(CityEN)) %>%
                select(location_id, keyregion2018, target_reduction), .) %>%
    filter(!is.na(target_reduction)) %>%
    tidyr::pivot_longer(cols=c("target_reduction", "QTD_reduction"), names_to="indicator")


  (p <-  ggplot(m.plot %>% mutate(region_name=tools::toTitleCase(location_id))) +
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
