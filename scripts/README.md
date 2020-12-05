# Scripts
These files are scripts to be run regularly by CREA python engine to generate charts. These should be self-sufficient (i.e. not rely on local files)


```{bash}
gcloud scheduler jobs create app-engine tracker-china \
    --schedule="0 1 * * *" \
    --http-method=POST \
    --attempt-deadline=24h \
    --message-body="{\"config_url\":\"https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/scripts/trackers.china.json\"}" \
    --time-zone="Asia/Hong_Kong"
```
