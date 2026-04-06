# Baseline table
library(DescribeData)
donors$tertiles = cut(donors$LABCORP_KETBOD1_screening, 
                      breaks = quantile(donors$LABCORP_KETBOD1_screening, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                      include.lowest = TRUE, 
                      labels = c("T1","T2","T3"))
baseline_table = DescribeData(
  variables = c('LABCORP_KETBOD1_screening', 'LABCORP_BHB1_screening', 'LABCORP_ACAC1_screening', 'LABCORP_ACET1_screening', 
                'skdelta_gfr', 'NP_MGFR_screening', 'ALGLEEFT_screening', 'GESLACHT', 'AVG_WEIGHT_nacontrole', 'AVG_HEIGHT_nacontrole',
                'BMI_screening', 'GLU_BLO_screening', 'HBA1_P_BLO_screening', 'CHOL_BLO_screening', 'HDL_CHOL_BLO_screening', 'TGL_BLO_screening',
                'CRP_BLO_screening', 'ASAT_BLO_screening', 'ALAT_BLO_screening', 'GGT_BLO_screening'),
  normal = c("NP_MGFR_screening", 'skdelta_gfr'),
  group = "tertiles",
  df = donors,
  names = c("Age (years)", "Income ($)", "Education"),
  nmiss = FALSE
)
summary(donors$ALGLEEFT_screening)
summary(donors$GESLACHT)
summary(donors$NP_MGFR_screening)
sd(donors$NP_MGFR_screening)
summary(donors$NP_MGFR_nacontrole)
sd(donors$NP_MGFR_nacontrole)
summary(donors$skdelta_gfr)
sd(donors$skdelta_gfr)
summary(donors$LABCORP_KETBOD1_screening)
summary(donors$LABCORP_ACAC1_screening)
summary(donors$LABCORP_ACET1_screening)
summary(donors$LABCORP_BHB1_screening)