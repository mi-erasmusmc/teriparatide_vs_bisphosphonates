# =============================================================================================================================
# ON TREATMENT ANALYSES
# =============================================================================================================================
# -----------------------------------------------------------------------------------------------------------------------------
# Stratification on hip fracture risk
# -----------------------------------------------------------------------------------------------------------------------------
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

figures/plotAbsoluteRiskStratified_on_treatment_att_1095_custom_10.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< on_treatment att 1095_custom_10 tiff

figures/overallNcPlot_on_treatment_att_1095_custom_10.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< on_treatment att 1095_custom_10

figures/overallCovariateBalance_on_treatment_att_1095_custom_10.tiff : code/PlotCovariateBalance.R
	$< on_treatment att 1095_custom_10

figures/overallPsDensity_on_treatment_att_1095_custom_10.tiff : code/PlotPsDensity.R
	$< on_treatment att 1095_custom_10

# -----------------------------------------------------------------------------------------------------------------------------
# Stratification on major osteoporotic fracture risk
# -----------------------------------------------------------------------------------------------------------------------------
data/processed/calibrateOverallResults_on_treatment_att_1095_gl.rds: code/calibrateOverall.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< on_treatment att 1095_gl ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateOverall_on_treatment_att_1095_gl.rds : code/metaCalibrateOverall.R\
	data/processed/calibrateOverallResults_on_treatment_att_1095_gl.rds
	$< on_treatment att 1095_gl

figures/plotMetaOverall_on_treatment_att.tiff : code/plotMetaOverall.R \
	data/processed/calibrateOverallResults_on_treatment_att_1095_gl.rds\
	data/processed/metaCalibrateOverall_on_treatment_att_1095_gl.rds 
	$< on_treatment att 1095_gl

data/processed/calibrateRiskStratified_on_treatment_att_1095_gl.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< on_treatment att 1095_gl ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateRiskStratified_on_treatment_att_1095_gl.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_on_treatment_att_1095_gl.rds
	$< on_treatment att 1095_gl

figures/plotMetaRiskStratified_on_treatment_att_1095_gl.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateRiskStratified_on_treatment_att_1095_gl.rds\
	data/processed/metaCalibrateRiskStratified_on_treatment_att_1095_gl.rds
	$< on_treatment att 1095_gl

figures/plotAbsoluteRiskStratified_on_treatment_att_1095_gl.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< on_treatment att 1095_gl tiff

figures/overallNcPlot_on_treatment_att_1095_gl.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< on_treatment att 1095_gl

figures/overallCovariateBalance_on_treatment_att_1095_gl.tiff : code/PlotCovariateBalance.R
	$< on_treatment att 1095_gl

figures/overallPsDensity_on_treatment_att_1095_gl.tiff : code/PlotPsDensity.R
	$< on_treatment att 1095_gl



# =============================================================================================================================
# RENDER MANUSCRIPT
# =============================================================================================================================
submission/manuscript.pdf : submission/manuscript.rmd\
	submission/references.bib\
	submission/jamia.csl\
	figures/plotMeta.tiff\
	figures/OverallCovariateBalance.tiff\
	figures/OverallPsDensity.tiff\
	figures/OverallNcPlot.tiff
	R -e 'rmarkdown::render("submission/manuscript.rmd", output_format = "all")'

