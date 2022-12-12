# Model spawn data
# M. Espe (With Modification and Condensing by B. DeMattei)
# 2022 Nov (2022 Dec)
jpi = readRDS("data/model_data.rds")

library(glmnet)
library(AICcmodavg)
library(caret)
library(MASS)
library(relaimpo)
library(xtable)

# variables to be considered for model
vars = c("Year", "Eggs", "flow_weighted",
         "ios_surv_weighted", "martin_surv_weighted",
         "ndays_over53F", "ndays_over56F",
         "Spawn_Turbidity", "Spawn_DO", "Fry_Temp", "Spawn_Flow",
         "Fry_Flow", "Fry_Low_Flow_Days")

#------------------------------------------------------------------------------#
# Egg to fry survival
# Manual forward stepwise
n1 = lm(log_egg_to_fry_surv ~ Eggs, jpi)

m2d = lm(log_egg_to_fry_surv ~ Eggs + martin_surv_weighted, jpi)

m4a = lm(log_egg_to_fry_surv ~ Eggs + flow_weighted, jpi)

b1 = lm(log_egg_to_fry_surv ~ Eggs + flow_weighted + martin_surv_weighted, jpi) 

b2 = lm(log_egg_to_fry_surv ~ Eggs + ndays_over53F, jpi) 

b2a = lm(log_egg_to_fry_surv ~ Eggs + ndays_over53F + flow_weighted, jpi)

mod_list2 = list(n1, m2d, m4a, b1, b2, b2a)
names(mod_list2) = mod_nm2 = as.character(unlist(sapply(mod_list2, formula)))
aictab(mod_list2, modnames = mod_nm2)

mod_nm2 = gsub("log_egg_to_fry_surv", "log(R/S)", mod_nm2)
mod_nm2 = gsub("_", " ", mod_nm2)
mod_nm2 = gsub("weighted", "(weighted)", mod_nm2)
mod_nm2 = gsub("surv", "surv.", mod_nm2)
mod_nm2 = gsub("martin", "Martin", mod_nm2)
mod_nm2 = gsub("ios", "IOS", mod_nm2)
mod_nm2 = gsub("flow", "Flow", mod_nm2)
mod_nm2 = gsub("ndays over", "N days over ", mod_nm2)

tt2 = aictab(mod_list2, modnames = mod_nm2)
# For latex table in report

print(xtable(tt2), include.rownames = FALSE, auto = TRUE, hline.after = -1:nrow(tt2))

#------------------------------------------------------------------------------#
# plot best model params

jpi2 <- jpi
jpi2 <- jpi2[,c(4,8,10,19)]

names(jpi2) <- c("Eggs", "Flow", "Martin Survival", "log_egg_to_fry_surv")

plot_vars = c("Eggs", "Flow", "Martin Survival")
plot_col = rep("black", nrow(jpi2))
png("docs/single_var.png", width = 7, height = 4, units = "in", res = 300)
par(mfrow = c(1,3))
for(v in plot_vars){
  plot(jpi2[[v]], jpi2$log_egg_to_fry_surv, ylab = "log(R/S)", xlab = v, pch = 16, col = plot_col)
  
  abline(lm(jpi2$log_egg_to_fry_surv ~ jpi2[[v]]), lty = 2, col = "black")
}
dev.off()

#------------------------------------------------------------------------------#
