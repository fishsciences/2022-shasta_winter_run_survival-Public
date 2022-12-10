# Creates weighted averages of the temperature and flow metrics for
# use in the model
# M. Espe
# 2022 Nov 17

spawn_file = "data/Spawning distributions.xlsx"
excel_sheets(spawn_file)

spawn_dist = as.data.frame(read_excel(spawn_file, sheet = "Monthly"))

spawn_prop = apply(spawn_dist[,-1], 2, function(x) x / sum(x))
spawn_prop = as.data.frame(spawn_prop)

# Use average distribution for years missing from data
spawn_prop[,as.character(1996:1999)] =  rowMeans(spawn_prop)
row.names(spawn_prop) = 4:8

spawn_temp_flow = readRDS("data/spawn_temp_flow.rds")
spawn_temp_flow$weight = spawn_prop[cbind(as.character(spawn_temp_flow$spawn_start),
                                          as.character(spawn_temp_flow$year))]

spawn_temp_flow$flow_weighted = spawn_temp_flow$avg_flow * spawn_temp_flow$weight
spawn_temp_flow$ios_surv_weighted = spawn_temp_flow$`Total S (IOS)` * spawn_temp_flow$weight
spawn_temp_flow$martin_surv_weighted = spawn_temp_flow$`Total S (Martin)` * spawn_temp_flow$weight
spawn_temp_flow$temp_raw_weighted = as.numeric(spawn_temp_flow$value.SAC_predicted) * spawn_temp_flow$weight
spawn_temp_flow$ndays_over53F = as.numeric(spawn_temp_flow$ndays_over53F) * spawn_temp_flow$weight
spawn_temp_flow$ndays_over56F = as.numeric(spawn_temp_flow$ndays_over56F) * spawn_temp_flow$weight

temp_flow_avg = aggregate(cbind(temp_raw_weighted, flow_weighted,
                                ios_surv_weighted, martin_surv_weighted,
                                ndays_over53F, ndays_over56F) ~ year, spawn_temp_flow, sum)

saveRDS(temp_flow_avg, file = "data/temp_flow_avg.rds")
