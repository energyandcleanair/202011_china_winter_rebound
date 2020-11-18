
utils.read_stations <- function(){
  read.csv("data/station_key2018.csv")
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
      # region_id=gsub(">","", gsub("<U\\+","\\\\u",CityZH)),
      region_id=stringi::stri_unescape_unicode(gsub(">","", gsub("<U\\+","\\\\u",CityZH))),
      region_name=region_id) %>%
    select(-c(CityEN, CityZH))
}
