data/processed/calibrateOverallResults_att.rds: code/calibrateOverall.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< att ccae optum_ehr optum_extended_dod mdcr

data/processed/calibrateOverallResults_ate.rds: code/calibrateOverall.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< ate ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateOverall_att.rds    : code/metaCalibrateOverall.R\
	data/processed/calibrateOverallResults_att.rds
	$< att 

data/processed/metaCalibrateOverall_ate.rds    : code/metaCalibrateOverall.R\
	data/processed/calibrateOverallResults_ate.rds
	$< ate 

data/processed/calibrateRiskStratified_att.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< att ccae optum_ehr optum_extended_dod mdcr

data/processed/calibrateRiskStratified_ate.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< ate ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateRiskStratified_att.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_att.rds
	$< att

data/processed/metaCalibrateRiskStratified_ate.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_ate.rds
	$< ate

data/processed/metaCalibrateRiskStratified_age_50_tr_1_q_25_75.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_age_50_tr_1_q_25_75.rds
	$< age_50_tr_1_q_25_75 

data/processed/calibrateRiskStratified_age_50_tr_1_gl.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< age_50_tr_1_gl optum_ehr optum_extended_dod

data/processed/metaCalibrateRiskStratified_age_50_tr_1_gl.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_age_50_tr_1_gl.rds
	$< age_50_tr_1_gl 

data/processed/hipFractureAbsolute_age_50_tr_1_q_25_75_101_101 : code/extractAbsoluteHip.R\
	data/raw/mappedOverallAbsoluteResults.rds
	$< 101 101 age_50_tr_1_q_25_75

data/processed/hipFractureAbsolute_age_50_tr_1_q_25_75_101_102 : code/extractAbsoluteHip.R\
	data/raw/mappedOverallAbsoluteResults.rds
	$< 101 102 age_50_tr_1_q_25_75

data/processed/hipFractureAbsolute_age_50_tr_1_q_25_75_101_103 : code/extractAbsoluteHip.R\
	data/raw/mappedOverallAbsoluteResults.rds
	$< 101 103 age_50_tr_1_q_25_75

data/processed/hipFractureAbsolute_age_50_tr_1_gl_101_101 : code/extractAbsoluteHip.R\
	data/raw/mappedOverallAbsoluteResults.rds
	$< 101 101 age_50_tr_1_gl

figures/plotMeta.tiff : code/plotMetaOverall.R \
	data/processed/calibrateOverallResults_att.rds\
	data/processed/metaCalibrateOverall_att.rds
	$< att

figures/plotMetaRiskStratified_itt_att_1095_q_25_75_5402.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateOverallResults_att.rds\
	data/processed/metaCalibrateOverall_att.rds
	$< att itt_att_1095_q_25_75 5402

figures/plotMetaRiskStratified_itt_att_1095_gl_5403.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateOverallResults_att.rds\
	data/processed/metaCalibrateOverall_att.rds
	$< att itt_att_1095_gl 5403

figures/plotAbsoluteRiskStratified_itt_att_1095_gl_5403.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< att itt_att_1095_gl 5403

figures/OverallNcPlot.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$<

figures/OverallCovariateBalance.tiff : code/PlotCovariateBalance.R
	$<

figures/OverallPsDensity.tiff : code/PlotPsDensity.R
	$<

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

