# Prepares the model data
# M. Espe
# 2022 Nov 17

library(readxl)
temp_flow_avg = readRDS("data/temp_flow_avg.rds")
jpi = as.data.frame(read_excel("data/Winter_run_spawner_and_juvenile_abundance_96_22.xlsx", skip = 1))
covars = read_excel("data/Winter_run_env_covariates.xlsx", skip = 1)

jpi = merge(jpi, temp_flow_avg, by.x = "Year", by.y = "year")
jpi = merge(jpi, covars)
colnames(jpi)[colnames(jpi) == "Mean...5"] = "JPI"
colnames(jpi)[colnames(jpi) == "Mean...9"] = "egg_to_fry_surv"
colnames(jpi)[colnames(jpi) == "Adult Females"] = "adult_females"

vars = c("Year", "adult_females", "Fecundity", "Eggs", "JPI",
      "egg_to_fry_surv", "flow_weighted", "ios_surv_weighted",
      "martin_surv_weighted")
drop = grep("...", colnames(jpi), fixed = TRUE)
jpi = jpi[-drop]
jpi$`LSNFH Smolts` = NULL #drop
jpi[vars] = sapply(jpi[vars], as.numeric)

# Fill in missing data
jpi$egg_to_fry_surv[26] = jpi$JPI[26] / jpi$Eggs[26]

jpi$log_egg_to_fry_surv = log(jpi$egg_to_fry_surv)

colnames(jpi) = gsub(" ", "_", colnames(jpi))

saveRDS(jpi, file = "data/model_data.rds")

