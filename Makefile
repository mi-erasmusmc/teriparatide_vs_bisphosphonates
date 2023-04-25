# ======================================================================================================
# Stratification on hip fracture risk
# ======================================================================================================


# ------------------------------------------------------------------------------------------------------
# Fetch data
# ------------------------------------------------------------------------------------------------------
data/raw/mappedOverallResultsNegativeControls.rds : code/getRawData.R
	$< mappedOverallResultsNegativeControls.rds

data/raw/mappedOverallResults.rds : code/getRawData.R
	$< mappedOverallResults.rds

data/raw/mappedOverallAbsoluteResults.rds : code/getRawData.R
	$< mappedOverallAbsoluteResults.rds

data/raw/negativeControls.rds : code/getRawData.R
	$< negativeControls.rds

data/raw/mappedOverallRelativeResults.rds : code/getRawData.R
	$< mappedOverallRelativeResults.rds

data/raw/map_outcomes.rds : code/getRawData.R
	$< map_outcomes.rds

data/raw/map_exposures.rds : code/getRawData.R
	$< map_exposures.rds


data/raw/overall_balance_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_balance_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_balance_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_balance_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_balance_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_balance_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_balance_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_balance_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_psDensity_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_psDensity_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_psDensity_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_psDensity_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_psDensity_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_psDensity_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds

data/raw/overall_psDensity_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds : code/getRawData.R
	$< overall_psDensity_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds

# ------------------------------------------------------------------------------------------------------
# Generate processed data
# ------------------------------------------------------------------------------------------------------
data/processed/calibrateOverallResults_itt_att_1095_custom.rds: code/calibrateOverall.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< itt att 1095_custom ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateOverall_itt_att_1095_custom.rds : code/metaCalibrateOverall.R\
	data/processed/calibrateOverallResults_itt_att_1095_custom.rds
	$< itt att 1095_custom

# ------------------------------------------------------------------------------------------------------
# Generate manuscript figures
# ------------------------------------------------------------------------------------------------------
figures/plotMetaOverall_itt_att.tiff : code/plotMetaOverall.R \
	data/processed/calibrateOverallResults_itt_att_1095_custom.rds\
	data/processed/metaCalibrateOverall_itt_att_1095_custom.rds 
	$< itt att 1095_custom

figures/plotMetaOverallUncalibrated_itt_att_1095_custom.tiff : code/plotMetaOverallUncalibrated.R \
	data/raw/mappedOverallResults.rds
	$< itt att 1095_custom

figures/plotMetaRiskStratifiedUncalibrated_itt_att_1095_custom.tiff : code/plotMetaRiskStratifiedUncalibrated.R \
	data/raw/mappedOverallRelativeResults.rds
	$< itt att 1095_custom

data/processed/calibrateRiskStratified_itt_att_1095_custom.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< itt att 1095_custom ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateRiskStratified_itt_att_1095_custom.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_itt_att_1095_custom.rds
	$< itt att 1095_custom

figures/plotMetaRiskStratified_itt_att_1095_custom.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateRiskStratified_itt_att_1095_custom.rds\
	data/processed/metaCalibrateRiskStratified_itt_att_1095_custom.rds
	$< itt att 1095_custom

figures/plotAbsoluteRiskStratified_itt_att_1095_custom.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< itt att 1095_custom tiff

figures/overallNcPlot_itt_att_1095_custom.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< itt att 1095_custom

figures/overallCovariateBalance_itt_att_1095_custom.tiff : code/PlotCovariateBalance.R\
	data/raw/overall_balance_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_balance_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_balance_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_balance_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds
	$< itt att 1095_custom

figures/overallPsDensity_itt_att_1095_custom.tiff : code/PlotPsDensity.R\
	data/raw/overall_psDensity_ccae_itt_ccae_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_psDensity_mdcr_itt_mdcr_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_psDensity_optum_ehr_itt_optum_ehr_itt_att_1095_custom_6889_6888_5402.rds\
	data/raw/overall_psDensity_optum_extended_dod_itt_optum_extended_dod_itt_att_1095_custom_6889_6888_5402.rds
	$< itt att 1095_custom


# ======================================================================================================
# Stratification on major osteoporotic fracture risk
# ======================================================================================================

data/processed/calibrateRiskStratified_itt_att_1095_gl.rds : code/calibrateRiskStratified.R\
	data/raw/negativeControls.rds\
	data/raw/mappedOverallRelativeResults.rds
	$< itt att 1095_gl ccae optum_ehr optum_extended_dod mdcr

data/processed/metaCalibrateRiskStratified_itt_att_1095_gl.rds : code/metaCalibrateRiskStratified.R\
	data/processed/calibrateRiskStratified_itt_att_1095_gl.rds
	$< itt att 1095_gl

figures/plotMetaRiskStratified_itt_att_1095_gl.tiff : code/plotMetaRiskStratified.R\
	data/raw/map_outcomes.rds\
	data/processed/calibrateRiskStratified_itt_att_1095_gl.rds\
	data/processed/metaCalibrateRiskStratified_itt_att_1095_gl.rds
	$< itt att 1095_gl

figures/plotAbsoluteRiskStratified_itt_att_1095_gl.tiff : code/plotAbsoluteRiskStratified.R\
	data/raw/mappedOverallAbsoluteResults.rds\
	data/raw/map_outcomes.rds\
	data/raw/map_exposures.rds
	$< itt att 1095_gl tiff

figures/overallNcPlot_itt_att_1095_gl.tiff : code/PlotNegativeControls.R\
	data/raw/mappedOverallResultsNegativeControls.rds\
	data/raw/mappedOverallResults.rds
	$< itt att 1095_gl

figures/overallCovariateBalance_itt_att_1095_gl.tiff : code/PlotCovariateBalance.R
	$< itt att 1095_gl

figures/overallPsDensity_itt_att_1095_gl.tiff : code/PlotPsDensity.R
	$< itt att 1095_gl


# ======================================================================================================
# RENDER MANUSCRIPT
# ======================================================================================================

submission/manuscript.pdf : submission/manuscript.rmd\
	submission/references.bib\
	submission/jamia.csl\
	figures/plotMeta.tiff\
	figures/OverallCovariateBalance.tiff\
	figures/OverallPsDensity.tiff\
	figures/OverallNcPlot.tiff
	R -e 'rmarkdown::render("submission/manuscript.rmd", output_format = "all")'

