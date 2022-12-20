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
spawn_temp_flow$martin_surv_weighted = spawn_temp_flow$`Total S (Martin)` * spawn_temp_flow$weight



temp_flow_avg = aggregate(cbind(flow_weighted, martin_surv_weighted) ~ year, spawn_temp_flow, sum)

saveRDS(temp_flow_avg, file = "data/temp_flow_avg.rds")
