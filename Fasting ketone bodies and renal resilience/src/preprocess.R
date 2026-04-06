# Pre-process
cols_to_keep = c('MOMENT', 'DNR_ID', 'GESLACHT', 'DONUMCGNR', 
                 'ALGDAT', 'TLTYPE', 'CODE_BIOBANK', 'ALGLEEFT', 'ALGNUCHT', 
                 'LSGEW1', 'LSGEW2', 'LSGEW3', 'LSGEW1', 'LSLEN1', 'LSLEN2', 'LSLEN3',
                 'ASAT_BLO', 'ALAT_BLO', 'GGT_BLO', 'CALC_BLO', 'GLU_BLO', 
                 'HDL_CHOL_BLO', 'CHOL_BLO', 'CRP_BLO', 'TGL_BLO', 'KREA_BLO',
                 'GLUC_BLO', 'HB_BLO', 'HBA1_P_BLO', 'NP_MGFR', 'LABCORP_LPX1',
                 'LABCORP_LPZ1', 'LABCORP_VLTRLP1', 'LABCORP_TRLP1', 
                 'LABCORP_LTRLP1', 'LABCORP_MTRLP1', 'LABCORP_VSTRLP1',
                 'LABCORP_STRLP1', 'LABCORP_CLDLP1', 'LABCORP_LCLDLP1', 
                 'LABCORP_MCLDLP1', 'LABCORP_SCLDLP1', 'LABCORP_CHDLP1', 
                 'LABCORP_LCHDLP1', 'LABCORP_MCHDLP1', 'LABCORP_SCHDLP1', 
                 'LABCORP_H7P1', 'LABCORP_H6P1', 'LABCORP_H5P1', 'LABCORP_H4P1', 
                 'LABCORP_H3P1', 'LABCORP_H2P1', 'LABCORP_H1P1', 
                 'LABCORP_TRLZ1', 'LABCORP_LDLZ1', 'LABCORP_HDLZ1', 
                 'LABCORP_NTG1', 'LABCORP_NTC1', 'LABCORP_NTRLTG1', 
                 'LABCORP_NTRLC1', 'LABCORP_NLDLC1', 'LABCORP_NHDLC1', 
                 'LABCORP_APOB1', 'LABCORP_APOA11', 'LABCORP_VAL1', 
                 'LABCORP_LEU1', 'LABCORP_ISOLEU1', 'LABCORP_ALAN1', 
                 'LABCORP_KETBOD1', 'LABCORP_BHB1', 'LABCORP_ACAC1',
                 'LABCORP_ACET1', 'LABCORP_GLYCA1', 'LABCORP_GLU1')
donors$ALGDAT = as.Date(donors$ALGDAT/86400, origin = "1582-10-14")
donors = donors[,cols_to_keep]
donors = donors[which(donors$TLTYPE == 'Nier Donor' & (donors$MOMENT == 'screening' | donors$MOMENT == 'nacontrole')),]
donors$AVG_WEIGHT = rowMeans(donors[,c('LSGEW1', 'LSGEW2', 'LSGEW3')], na.rm = TRUE)
donors$AVG_HEIGHT = rowMeans(donors[,c('LSLEN1', 'LSLEN2', 'LSLEN3')], na.rm = TRUE)
donors$BMI = donors$AVG_WEIGHT / (donors$AVG_HEIGHT / 100)^2
donors_long = donors
donors_long = donors_long[which(donors_long$ALGNUCHT == 'Ja'),]
donors = reshape(donors,
                 timevar = 'MOMENT',
                 idvar = c('DNR_ID', 'GESLACHT', 'DONUMCGNR', 'TLTYPE'),
                 direction = 'wide',
                 sep = '_')
donors = donors[which(donors$ALGNUCHT_screening == 'Ja'),]
donors$BSA_MGFR_screening = donors$NP_MGFR_screening * (1.73 / (0.007184 * (donors$AVG_WEIGHT_screening**0.425) * ( donors$AVG_HEIGHT_screening**0.725)))
donors$BSA_MGFR_nacontrole = donors$NP_MGFR_nacontrole * (1.73 / (0.007184 * (donors$AVG_WEIGHT_nacontrole**0.425) * ( donors$AVG_HEIGHT_nacontrole**0.725)))
donors$skdelta_gfr_bsa = donors$BSA_MGFR_nacontrole - (donors$BSA_MGFR_screening / 2)
donors$skdelta_gfr = donors$NP_MGFR_nacontrole - (donors$NP_MGFR_screening / 2)
donors$skdelta_gfr_perc = 100 + ((donors$NP_MGFR_nacontrole - (donors$NP_MGFR_screening / 2)) / donors$NP_MGFR_screening) * 100
donors$BSA_screening = 0.007184 * (donors$AVG_WEIGHT_screening**0.425) * ( donors$AVG_HEIGHT_screening**0.725)
donors$total_kidney_capacity = (donors$NP_MGFR_nacontrole*2) * (1.73 / donors$BSA_screening)
donors = donors[which(!is.na(donors$ALGLEEFT_screening) & !is.na(donors$skdelta_gfr) & !is.na(donors$LABCORP_ACAC1_screening)),]