#Add 5 and 10 years mGFR
mgfr_5_10_yrs = read.csv(donors_long_followup_path)
mgfr_5_10_yrs = reshape(mgfr_5_10_yrs,
                        timevar = 'redcap_event_name',
                        idvar = c('dnr_id'),
                        direction = 'wide',
                        sep = '_')
names(mgfr_5_10_yrs)[which(names(mgfr_5_10_yrs) == 'dnr_id')] = 'DNR_ID'
names(mgfr_5_10_yrs)[which(names(mgfr_5_10_yrs) == 'np_mgfr_5_jaar_arm_1')] = 'NP_MGFR_5yrs'
names(mgfr_5_10_yrs)[which(names(mgfr_5_10_yrs) == 'np_mgfr_10_jaar_arm_1')] = 'NP_MGFR_10yrs'
mgfr_5_10_yrs = mgfr_5_10_yrs[which(mgfr_5_10_yrs$DNR_ID %in% donors$DNR_ID),]
donors = merge(donors, mgfr_5_10_yrs, by = 'DNR_ID', all = TRUE)