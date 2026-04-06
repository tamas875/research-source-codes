library(foreign)
library(rstudioapi)

dir = sub('renal_resilience_labcorp.R', '', getActiveDocumentContext()$path)
source(paste0(dir, 'data_path.R'))
donors = read.spss(donors_path, to.data.frame = TRUE)
source(paste0(dir, 'preprocess.R'))
source(paste0(dir, 'baseline_analysis.R'))
write.csv(baseline_table, paste0(dir, 'tables/table_1.csv'))
source(paste0(dir, 'models.R'))
write.csv(results, paste0(dir, 'tables/table_2.csv'))
source(paste0(dir, 'interaction_tests.R'))
write.csv(interaction_results, paste0(dir, 'tables/interaction_results.csv'))
source(paste0(dir, 'figures.R'))
source(paste0(dir, '5_10_yrs_mgfr.R'))

rm(list = ls())

