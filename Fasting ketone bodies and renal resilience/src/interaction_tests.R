# Interaction testing
interaction_sex_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + GESLACHT, data = donors)
interaction_sex_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*GESLACHT, data = donors)
interaction_age_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(ALGLEEFT_screening), data = donors)
interaction_age_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*scale(ALGLEEFT_screening), data = donors)
interaction_mgfr_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(NP_MGFR_screening), data = donors)
interaction_mgfr_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*scale(NP_MGFR_screening), data = donors)
interaction_glucose_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(GLU_BLO_screening), data = donors)
interaction_glucose_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*scale(GLU_BLO_screening), data = donors)
interaction_hba1c_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(HBA1_P_BLO_screening), data = donors)
interaction_hba1c_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*scale(HBA1_P_BLO_screening), data = donors)
interaction_bmi_0 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening) + scale(BMI_screening), data = donors)
interaction_bmi_1 = lm(scale(skdelta_gfr) ~ scale(LABCORP_KETBOD1_screening)*scale(BMI_screening), data = donors)

interaction_results = data.frame(variables = c('Sex', 'Age', 'mGFR', 'Glucose', 'HbA1c', 'BMI'),
                                 p_interaction = c(anova(interaction_sex_0, interaction_sex_1)[2,'Pr(>F)'],
                                                   anova(interaction_age_0, interaction_age_1)[2,'Pr(>F)'],
                                                   anova(interaction_mgfr_0, interaction_mgfr_1)[2,'Pr(>F)'],
                                                   anova(interaction_glucose_0, interaction_glucose_1)[2,'Pr(>F)'],
                                                   anova(interaction_hba1c_0, interaction_hba1c_1)[2,'Pr(>F)'],
                                                   anova(interaction_bmi_0, interaction_bmi_1)[2,'Pr(>F)']))
interaction_results$p_interaction = round(interaction_results$p_interaction, 3)