
utils.read_stations <- function(local=T){


  read.csv(ifelse(local, "data/station_key2018.csv","https://raw.githubusercontent.com/energyandcleanair/202011_china_winter_rebound/main/data/station_key2018.csv")) -> stationkey
  read.csv(paste0(ifelse(local,"data/stationZHnames,with Fenwei.csv","https://raw.githubusercontent.com/energyandcleanair/202011_china_winter_rebound/main/data/stationZHnames%2Cwith%20Fenwei.csv")),
           encoding="UTF-8",
           stringsAsFactors=F) %>%
    rename(station_name=station,CityZH=City) -> citynames

  stationkey %>% sel(-station_name, -CityZH) %>%
    right_join(citynames, .)
}

utils.check_cities_unique <- function(cities, stations){

  duplicated_cities <- cities %>%
    left_join(stations) %>%
    distinct(CityEN, Province) %>%
    group_by(CityEN) %>%
    summarise(count=n()) %>%
    filter(count>1) %>%
    pull(CityEN)

  if(length(duplicated_cities)>0){
    warning(paste("Careful, some cities are duplicated:", paste(duplicated_cities, collapse=", ")))
    return(duplicated_cities)
  }else{
    return(T)
  }
}

utils.replace_w_chinese <- function(m, stations){
  m %>% left_join(stations %>%
                    distinct(CityEN, CityZH) %>%
                    mutate(region_id=tolower(CityEN))) %>%
    mutate(
      region_id=CityZH,
      # region_id=stringi::stri_unescape_unicode(gsub(">","", gsub("<U\\+","\\\\u",CityZH))),
      region_name=region_id) %>%
    select(-c(CityEN, CityZH))
}

utils.deweathered_regions <- function(m.deweathered.city, m.deweathered.stations){

}
