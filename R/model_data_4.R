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


#------------------------------------------------------------------------------#
# Egg to fry survival
# Manual forward stepwise
n1 = lm(log_egg_to_fry_surv ~ Eggs, jpi)

m2d = lm(log_egg_to_fry_surv ~ Eggs + martin_surv_weighted, jpi)

m4a = lm(log_egg_to_fry_surv ~ Eggs + flow_weighted, jpi)

b1 = lm(log_egg_to_fry_surv ~ Eggs + flow_weighted + martin_surv_weighted, jpi) 

mod_list2 = list(n1, m2d, m4a, b1)
names(mod_list2) = mod_nm2 = as.character(unlist(sapply(mod_list2, formula)))
aictab(mod_list2, modnames = mod_nm2)

mod_nm2 = gsub("log_egg_to_fry_surv", "log(R/S)", mod_nm2)
mod_nm2 = gsub("_", " ", mod_nm2)
mod_nm2 = gsub("weighted", "", mod_nm2)
mod_nm2 = gsub("surv", "surv.", mod_nm2)
mod_nm2 = gsub("martin", "Martin", mod_nm2)
mod_nm2 = gsub("flow", "Flow", mod_nm2)

tt2 = aictab(mod_list2, modnames = mod_nm2)
# For latex table in report

print(xtable(tt2), include.rownames = FALSE, auto = TRUE, hline.after = -1:nrow(tt2))

# top model by AICc
best_model = lm(log_egg_to_fry_surv ~ scale(Eggs) + scale(flow_weighted) + scale(martin_surv_weighted), jpi)

# for report
xtable(summary(best_model))
xtable(anova(best_model))

#Calculating 95% CI
ci95 <- summary(best_model)
ci95 <- ci95$coefficients
g2 <-ci95[c(1:4),2] #isolating std. error
g3<- ci95[c(1:4),1] #isolating coeff

g3+(g2*1.96) #Upper 95CI
#-1.5612472 -0.2023621  0.5946235  0.5584622 # Manually spliced into LaTEX table for RMD
g3-(g2*1.96) #Lower 95CI
#-1.8279432 -0.4898201  0.2196821  0.1983716 # Manually spliced into LaTEX table for RMD

#------------------------------------------------------------------------------#
# plot best model params

jpi2 <- jpi
jpi2 <- jpi2[c("Eggs", "flow_weighted", "martin_surv_weighted", "log_egg_to_fry_surv")]

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

jpi2[["Flow"]]
#------------------------------------------------------------------------------#

subset(jpi, jpi$Year==2022)
