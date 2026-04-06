# Models
extract_model_info = function(model, feature) {
  p_val = round(summary(model)$coefficients[feature,'Pr(>|t|)'], 3)
  estimate = round(summary(model)$coefficients[feature, 'Estimate'], 2)
  ci = round(confint(model)[feature,], 2)
  return(paste0(estimate, ' [', ci['2.5 %'], ' to ', ci['97.5 %'], '], p=', p_val))
}
results = data.frame(
  Model         = paste("Model ", 1:4),
  Ketone_bodies = NA_real_,
  BHB           = NA_real_,
  acac          = NA_real_,
  acetone       = NA_real_
)

# Ketone bodies
model_1_ketones = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening), data = donors)
results[1, 'Ketone_bodies'] = extract_model_info(model_1_ketones, 'scale(LABCORP_KETBOD1_screening)')
model_2_ketones = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[2, 'Ketone_bodies'] = extract_model_info(model_2_ketones, 'scale(LABCORP_KETBOD1_screening)')
model_3_ketones = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[3, 'Ketone_bodies'] = extract_model_info(model_3_ketones, 'scale(LABCORP_KETBOD1_screening)')
model_4_ketones = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening) + scale(GLU_BLO_screening) + scale(HBA1_P_BLO_screening) + scale(BMI_screening), data = donors)
results[4, 'Ketone_bodies'] = extract_model_info(model_4_ketones, 'scale(LABCORP_KETBOD1_screening)')
summary(lm(scale(skdelta_gfr_perc) ~ scale(LABCORP_KETBOD1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening) + scale(GLU_BLO_screening) + scale(HBA1_P_BLO_screening) + scale(BMI_screening), data = donors))
# Diagnostics using 
cd  = cooks.distance(model_4_ketones)
cut = 4 / nrow(donors)
which_infl = which(cd > cut)
length(which_infl)
which_infl
df_noinfl = donors[cd <= cut, ]
fit_noinfl = update(model_4_ketones, data = df_noinfl)
summary(fit_noinfl)

# BHB
model_1_bhb = lm(scale(skdelta_gfr) ~ scale(LABCORP_BHB1_screening), data = donors)
results[1, 'BHB'] = extract_model_info(model_1_bhb, 'scale(LABCORP_BHB1_screening)')
model_2_bhb = lm(scale(skdelta_gfr) ~ scale(LABCORP_BHB1_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[2, 'BHB'] =  extract_model_info(model_2_bhb, 'scale(LABCORP_BHB1_screening)')
model_3_bhb = lm(scale(skdelta_gfr) ~ scale(LABCORP_BHB1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[3, 'BHB'] = extract_model_info(model_3_bhb, 'scale(LABCORP_BHB1_screening)')
model_4_bhb = lm(scale(skdelta_gfr) ~ scale(LABCORP_BHB1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening) + scale(GLU_BLO_screening) + scale(HBA1_P_BLO_screening) + scale(BMI_screening), data = donors)
results[4, 'BHB'] = extract_model_info(model_4_bhb, 'scale(LABCORP_BHB1_screening)')
# AcAc
model_1_acac = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACAC1_screening), data = donors)
results[1, 'acac'] = extract_model_info(model_1_acac, 'scale(LABCORP_ACAC1_screening)')
model_2_acac = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACAC1_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[2, 'acac'] = extract_model_info(model_2_acac, 'scale(LABCORP_ACAC1_screening)')
model_3_acac = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACAC1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[3, 'acac'] = extract_model_info(model_3_acac, 'scale(LABCORP_ACAC1_screening)')
model_4_acac = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACAC1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening) + scale(GLU_BLO_screening) + scale(HBA1_P_BLO_screening) + scale(BMI_screening), data = donors)
results[4, 'acac'] = extract_model_info(model_4_acac, 'scale(LABCORP_ACAC1_screening)')
# Acetone
model_1_acetone = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACET1_screening), data = donors)
results[1, 'acetone'] = extract_model_info(model_1_acetone, 'scale(LABCORP_ACET1_screening)')
model_2_acetone = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACET1_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[2, 'acetone'] = extract_model_info(model_2_acetone, 'scale(LABCORP_ACET1_screening)')
model_3_acetone = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACET1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening), data = donors)
results[3, 'acetone'] = extract_model_info(model_3_acetone, 'scale(LABCORP_ACET1_screening)')
model_4_acetone = lm(scale(skdelta_gfr) ~ scale(LABCORP_ACET1_screening) + scale(NP_MGFR_screening) + GESLACHT + scale(ALGLEEFT_screening) + scale(GLU_BLO_screening) + scale(HBA1_P_BLO_screening) + scale(BMI_screening), data = donors)
results[4, 'acetone'] = extract_model_info(model_4_acetone, 'scale(LABCORP_ACET1_screening)')

#Check model assumptions
pdf(paste0(dir, 'figures/check_assumptions_lm.pdf'), height = 15, width = 15)
oldpar = par(no.readonly = TRUE)
par(mfrow = c(4, 4), mar = c(3, 3, 2, 1), mgp = c(1.6, 0.5, 0), oma = c(0, 0, 2, 0))
plot(model_1_ketones, 2, sub = '')
plot(model_2_ketones, 2, sub = '')
plot(model_3_ketones, 2, sub = '')
plot(model_4_ketones, 2, sub = '')
plot(model_1_acac, 2, sub = '')
plot(model_2_acac, 2, sub = '')
plot(model_3_acac, 2, sub = '')
plot(model_4_acac, 2, sub = '')
plot(model_1_acetone, 2, sub = '')
plot(model_2_acetone, 2, sub = '')
plot(model_3_acetone, 2, sub = '')
plot(model_4_acetone, 2, sub = '')
plot(model_1_bhb, 2, sub = '')
plot(model_2_bhb, 2, sub = '')
plot(model_3_bhb, 2, sub = '')
plot(model_4_bhb, 2, sub = '')
mtext('Checking linear regression assumptions', outer = TRUE, cex = 1.2)
par(oldpar)
dev.off()
