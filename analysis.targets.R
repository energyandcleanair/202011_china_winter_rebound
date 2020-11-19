require(readxl)

m.station.obs = readRDS(file.path(dir_results, "m.station.obs.RDS"))
targets = read_xlsx('data/winter targets 2020-2021.xlsx') %>%
  rename(keyregion2018=keyRegion2018) %>%
  mutate_at(c('target_period', 'base_period'), function(x) x %>% gsub('Q', '\\.', .) %>% as.numeric)

#daily average PM2.5 by station
m.station.obs %>% filter(poll=='pm25', date>='2019-01-01') %>%
  mutate(date=as.Date(date), region_id=toupper(region_id)) %>%
  group_by(stationID=region_id, poll, date, process_id, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T) %>%
  left_join(stations) %>%
  mutate(Q=lubridate::quarter(date, with_year = T)) ->
  daily

m.keyregions <- daily %>%
  group_by(region_id=keyregion2018, process_id, date, poll, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T)

#quarterly averages by city
daily %>% group_by(Province, CityEN, keyregion2018, poll, Q) %>% summarise_at('value', mean, na.rm=T) ->
  m.quarterly

#quarterly averages by key region
daily %>%
  group_by(keyregion2018, poll, Q) %>% summarise_at('value', mean, na.rm=T) %>%
  bind_rows(m.quarterly) %>%
  filter(!is.na(keyregion2018)) %>%
  mutate(source='hourly') -> m.quarterly

#add targets for winter 2020-2021
targets %>% select(keyregion2018, Province, CityEN, Q=target_period, base_period, value=PM25_target) %>%
  mutate(source='target') %>% bind_rows(m.quarterly) -> m.quarterly

#add official monthly averaged data from MEE
targets %>% select(keyregion2018, Province, CityEN, Q=base_period, value=base_PM25) %>%
  mutate(source='monthly') %>% bind_rows(m.quarterly) -> m.quarterly




#calculate Q4 and Q1 target reductions
targets %>% mutate(PM25_target = ifelse(PM25_target==0, base_PM25, PM25_target), #replace 0's with base period values
                   target_reduction = PM25_target / base_PM25 - 1) %>%
  select(CityEN, Province, keyregion2018, Q = target_period, target_reduction) %>% left_join(m.quarterly, .) ->
  m.quarterly

#calculate annual means consistent with targets
m.quarterly %<>% filter(source=='hourly') %>%
  select(CityEN, Province, keyregion2018, base_period=Q, value_base = value) %>%
  left_join(m.quarterly, .) %>%
  mutate(value = ifelse(source == 'target',
                        (1 + target_reduction) * value_base,
                        value))

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.1, 2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4))) %>%
  group_by(CityEN, Province, keyregion2018) %>% summarise_at('value', mean) -> targetmeans.Q4

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4, 2021.1))) %>%
  group_by(CityEN, Province, keyregion2018) %>% summarise_at('value', mean) -> targetmeans.Q1


#One year running mean plots with the target for end of Q4 and Q1 marked as points, and linear path from latest value to targets
# DONE
plots.targets(targetmeans.Q1, targetmeans.Q4, m.keyregions)


m.quarterly %>% filter(source == 'hourly', Q %in% c(2019.4, 2020.4)) %>%
  select(keyregion2018, Province, CityEN, Q, value) %>%
  mutate_at('Q', make.names) %>% spread(Q, value) %>%
  mutate(QTD_reduction = X2020.4/X2019.4-1) %>% select(-starts_with('X')) %>%
  full_join(m.quarterly %>% filter(source=='target', Q==2020.4) %>%
              select(CityEN, Province, keyregion2018, target_reduction), .)

