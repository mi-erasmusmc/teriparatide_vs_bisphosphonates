# =============================================================================================================================
# - On treatment analyses
# - Risk strata: hip fracture
# =============================================================================================================================
data/processed/calibrateOverallResults_on_treatment_att_1095_custom_10.rds: code/calibrateOverall.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< on_treatment att 1095_custom_10 ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateOverall_on_treatment_att_1095_custom_10.rds : code/metaCalibrateOverall.R\
	data/processed/calibrateOverallResults_on_treatment_att_1095_custom_10.rds
	$< on_treatment att 1095_custom_10

figures/plotMetaOverall_on_treatment_att.tiff : code/plotMetaOverall.R \
	data/processed/calibrateOverallResults_on_treatment_att_1095_custom_10.rds\
	data/processed/metaCalibrateOverall_on_treatment_att_1095_custom_10.rds 
	$< on_treatment att 1095_custom_10

data/processed/calibrateRiskStratified_on_treatment_att_1095_custom_10.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< on_treatment att 1095_custom_10 ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateRiskStratified_on_treatment_att_1095_custom_10.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_on_treatment_att_1095_custom_10.rds
	$< on_treatment att 1095_custom_10

figures/plotMetaRiskStratified_on_treatment_att_1095_custom_10.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateRiskStratified_on_treatment_att_1095_custom_10.rds\
	data/processed/metaCalibrateRiskStratified_on_treatment_att_1095_custom_10.rds
	$< on_treatment att 1095_custom_10

figures/plotAbsoluteRiskStratified_on_treatment_att_1095_10.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< on_treatment att 1095_custom_10

figures/overallNcPlot_on_treatment_att_1095_custom_10.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< on_treatment att 1095_custom_10

figures/overallCovariateBalance_on_treatment_att_1095_custom_10.tiff : code/PlotCovariateBalance.R
	$< on_treatment att 1095_custom_10

figures/overallPsDensity_on_treatment_att_1095_custom_10.tiff : code/PlotPsDensity.R
	$< on_treatment att 1095_custom_10








figures/PsDensityRiskStratifed_att_q_25_75.tiff : code/PlotPsRiskStratified.R
	$< q_25_75 att 5402 5402 tiff

figures/PsDensityRiskStratifed_att_gl.tiff : code/PlotPsRiskStratified.R
	$< gl att 5402 5402 tiff

figures/plotAbsoluteHip.tiff : code/plotAbsoluteHip.R\
	data/processed/mappedOverallAbsoluteResults.rds
	$<

figures/CombinedAbsolute_age_50_tr_1_gl.tiff : code/CombinedPlots.R
	$< age_50_tr_1_gl major_osteoporotic_fracture

figures/CombinedAbsolute_age_50_tr_1_q_25_75.tiff : code/CombinedPlots.R
	$< age_50_tr_1_q_25_75 major_osteoporotic_fracture

submission/manuscript.pdf : submission/manuscript.rmd\
	submission/references.bib\
	submission/jamia.csl\
	figures/plotMeta.tiff\
	figures/OverallCovariateBalance.tiff\
	figures/OverallPsDensity.tiff\
	figures/OverallNcPlot.tiff
	R -e 'rmarkdown::render("submission/manuscript.rmd", output_format = "all")'

