# Create annual temperature-based survival metric
# M. Espe
# 2022 Nov 17
setwd("E:/Dropbox (Cramer Fish Sciences)/Braden DeMattei/RedBluff/2022/EspeTM/2022-shasta_winter_run_survival-main/")

library(readxl)
temp_file = "data/SacRiverTemperatures_1990_22.xlsx"
excel_sheets(temp_file)
temps = lapply(excel_sheets(temp_file), function(sh)
  as.data.frame(read_excel(temp_file, sheet = sh)))

temps = do.call(rbind, temps)

temps$`mm-dd` = as.character(temps$`mm-dd`)
temps$`mm-dd`= gsub("^[0-9]{4}", "", temps$`mm-dd`)
temps$date = as.Date(paste0(temps$year, temps$`mm-dd`))


library(ggplot2)
ggplot(temps, aes(x = date, y = value, color = location))+
  geom_line() +
  facet_wrap(~location)

tt = reshape(temps[c("date", "location", "value")],
             direction = "wide",
             timevar = "location",
             idvar = "date",
             v.names = "value")

m = lm(value.SAC ~ value.KWK + value.CCR, tt)
summary(m)

tt$value.SAC_predicted = predict(m, newdata = tt)
tt$month = as.integer(format(tt$date, "%m"))
tt$year = as.integer(format(tt$date, "%Y"))

# average for spawning month + 2
# For April -- August
months = 4:8
pad = 2

spawn_temps = lapply(months, function(m){
  tmp = subset(tt, month %in% m:(m+pad))
  ans = aggregate(value.SAC_predicted ~ year, tmp, mean, na.rm = TRUE)
  ans$spawn_start = m
#  browser()
  ans
})

spawn_temps = do.call(rbind, spawn_temps)
spawn_temps$value.SAC_predicted = as.character(round(spawn_temps$value.SAC_predicted, 1))

surv_eq = as.data.frame(read_excel("data/Temp_Survival_Equations.xlsx", skip = 1))

surv_eq$`Temp F` = as.character(surv_eq$`Temp F`)

spawn_temps = merge(spawn_temps, surv_eq[c("Temp F", "Total S (Martin)")],
                    by.x = "value.SAC_predicted", by.y = "Temp F", all.x = TRUE)

spawn_temps$`Total S (Martin)`[i] = 1


# Add flow
flow_file = "data/SacRiverFlows_1990_to_22.xlsx"
excel_sheets(flow_file)
fl = read_excel(flow_file)
fl$`mm-dd` = as.character(fl$`mm-dd`)
fl$`mm-dd`= gsub("^[0-9]{4}", "", fl$`mm-dd`)
fl$date = as.Date(paste0(fl$year, fl$`mm-dd`))
fl$month = as.integer(format(fl$date, "%m"))
fl$year = as.integer(format(fl$date, "%Y"))

# average for spawning month + 3 for flow
# For April -- August
months = 4:8
pad = 3

spawn_flow = lapply(months, function(m){
  tmp = subset(fl, month %in% m:(m+pad))
  ans = aggregate(value ~ year, tmp, mean, na.rm = TRUE)
  ans$spawn_start = m
  ans
})
spawn_flow = do.call(rbind, spawn_flow)

spawn_temp_flow = merge(spawn_temps, spawn_flow, by = c("year", "spawn_start"))
colnames(spawn_temp_flow)[colnames(spawn_temp_flow) == "value"] = "avg_flow"


saveRDS(spawn_temp_flow, file = "data/spawn_temp_flow.rds")


