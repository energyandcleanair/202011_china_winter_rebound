
utils.read_stations <- function(){
  read.csv("data/station_key2018.csv")
}

utils.check_cities_unique <- function(cities){
  if(nrow(
    cities %>% distinct(CityEN) %>%
    left_join(keys %>% distinct(CityEN, Province) %>% filter(!is.na(CityEN)))) != nrow(cities %>% distinct(CityEN))){
    warning("Careful, city English names are not unique")
    return(F)
  }else{
    return(T)
  }
}
