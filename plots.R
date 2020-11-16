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
