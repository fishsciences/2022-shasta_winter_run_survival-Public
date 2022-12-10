
RSCRIPT = Rscript $<

data: data/model_data.rds

model: R/model_data.R data/model_data.rds
	$(RSCRIPT)

data/model_data.rds: R/prep_model_data.R data/temp_flow_avg.rds data/Winter_run_spawner_and_juvenile_abundance_96_22.xlsx data/Winter_run_env_covariates.xlsx
	$(RSCRIPT)

data/spawn_temp_flow.rds: R/mk_temp_metric.R data/SacRiverTemperatures_1990_22.xlsx data/Temp_Survival_Equations.xlsx data/SacRiverFlows_1990_to_22.xlsx
	$(RSCRIPT)

data/temp_flow_avg.rds: R/weighted_avg.R data/Spawning\ distributions.xlsx data/spawn_temp_flow.rds
	$(RSCRIPT)
