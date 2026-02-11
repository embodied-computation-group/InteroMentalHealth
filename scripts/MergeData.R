
# Create Grand csv of interoception, mental health & demographics 

# Make big merged csv (mental health & intero) ##
demo <- read_tsv("/mnt/raid0/scratch/BIDS/phenotype/demographics/demographics.tsv")
demo <- demo[ ,c("participant_id", "id", "cohort", "age", "gender", "bmi")]

# match and merge the relevant variables of interest
match_merge <- function(data1, data2, subIDs1, subIDs2, labels) {
  reorder_idx <- match(subIDs1, subIDs2)
  data2 <- data2[reorder_idx, ]

  # merge data across dataframes
  merged <- cbind(data1, data2)
  colnames(merged) <- labels
  return(merged)
}

# load HRD
HRD <- read_csv("~/Git/InteroMentalHealth/data/HierarchicalInteroEstimates/HRD/Hierarchical_HRD_Intero.csv")
HRD <- HRD[, c("participant_id", "mean_confidence", "estimated_mean_beta", "estimated_mean_alpha", "Accuracy")]
# make HRD absolute threshold
HRD$absolute_estimated_mean_alpha <- abs(HRD$estimated_mean_alpha)

# List of HDR participant IDs to exclude (extero hrd error so need too exclude for all hrd)
hrd_badid <- read.csv("~/Git/InteroMentalHealth/data/HRD_badid.csv")
exclude_ids <- unique(hrd_badid[hrd_badid$bad_ids==1, 'participant_id'])
HRD <- HRD %>%
  dplyr::filter(!participant_id %in% exclude_ids) # sum(unique(merged_data$participant_id) %in% exclude_ids)

# merge HRD & demographics
alldata <- match_merge(demo, HRD[ ,c("mean_confidence", "estimated_mean_beta", "estimated_mean_alpha", "Accuracy", "absolute_estimated_mean_alpha")], demo$participant_id, HRD$participant_id, c(colnames(demo), 'HRD_conf', 'HRD_slope', 'HRD_thresh', "HRD_accuracy", 'HRD_absthresh'))

# auditory extero HRD control
HRDextero <- read_csv("~/Git/InteroMentalHealth/data/HierarchicalInteroEstimates/HRD/Hierarchical_HRD_Extero.csv")
HRDextero <- HRDextero[, c("participant_id", "mean_confidence", "estimated_mean_beta", "estimated_mean_alpha", "Accuracy")]
# make HRD-extero absolute threshold
HRDextero$absolute_estimated_mean_alpha <- abs(HRDextero$estimated_mean_alpha)

# List of HDR participant IDs to exclude (extero hrd error so need too exclude for all hrd)
hrd_badid <- read.csv("~/Git/InteroMentalHealth/data/HRD_badid.csv")
exclude_ids <- unique(hrd_badid[hrd_badid$bad_ids==1, 'participant_id'])
HRDextero <- HRDextero %>%
  dplyr::filter(!participant_id %in% exclude_ids) # sum(unique(merged_data$participant_id) %in% exclude_ids)

alldata <- match_merge(alldata, HRDextero[ ,2:ncol(HRDextero)], alldata$participant_id, HRDextero$participant_id, c(colnames(alldata), 'HRDextero_conf', 'HRDextero_slope', 'HRDextero_thresh', 'HRDextero_accuracy', 'HRDextero_absthresh'))


# load RRST
RRST <- read_csv("~/Git/InteroMentalHealth/data/HierarchicalInteroEstimates/RRST/Hierarchical_gumbel_n_267_RRST_subjectlevel_estimates.csv")
RRST <- RRST[, c("participant_id", "mean_confidence", "estimated_mean_beta", "estimated_mean_alpha", "Accuracy")]

# List of RRST participant IDs to exclude (exclude from hierarchical rrst modelling as accuracy below 1.5*IQR)
rrst_badid <- read.csv("~/Git/InteroMentalHealth/data/rrst_badid.csv")
exclude_ids <- unique(rrst_badid[rrst_badid$bad_ids==1, 'participant_id'])
RRST <- RRST %>%
  dplyr::filter(!participant_id %in% exclude_ids) # sum(unique(merged_data$participant_id) %in% exclude_ids)

# merge
alldata <- match_merge(alldata, RRST[ ,c("mean_confidence", "estimated_mean_beta", "estimated_mean_alpha", "Accuracy")], alldata$participant_id, RRST$participant_id, c(colnames(alldata), 'RRST_conf', 'RRST_slope', 'RRST_thresh', 'RRST_accuracy'))

# load metacognition
metad_indiv <- read.csv('~/Git/InteroMentalHealth/data/HierarchicalInteroEstimates/metad_indivfits.csv')
# merge
alldata <- match_merge(alldata, metad_indiv[ ,2:ncol(metad_indiv)], alldata$participant_id, metad_indiv$V1, c(colnames(alldata), colnames(metad_indiv[ ,2:ncol(metad_indiv)])))

# # sleep quality & insomnia
# psych_scores <- read_tsv("/mnt/raid0/scratch/BIDS/derivatives/summaries/survey/summary_scores/grand/grand_surveyscores_summary.tsv")
# cols = c("isi", "isi_cutoff", "psqi_subj_sleep_qual", "psqi_sleep_latency", "psqi_sleep_dur", "psqi_sleep_effic", "psqi_sleep_disturb", "psqi_sleep_meds", "psqi_day_dysfunc", "psqi_sum", "maia_notice", "maia_ndistract", "maia_nworry", "maia_attnReg", "maia_EmoAware", "maia_SelfRef", "maia_listen", "maia_trust", "maia_full_mean", "mdi", "mdi_cutoff", "phq9", "phq9_cutoff", "phq15", "phq15_cutoff", "stai", "stai_state", "stai_trait")
# psychdata <- psych_scores[ ,cols]
# alldata <- match_merge(alldata, psychdata, alldata$participant_id, psych_scores$participant_id, c(colnames(alldata), colnames(psychdata)))
# 
# # MDES
# rest_MWprobes <- read.csv("~/Git/body_wandering/data/BodyWanderingData.csv", row.names = 1)[ ,1:23]
# colnames(rest_MWprobes) <- paste0("MDES_", colnames(rest_MWprobes))
# alldata <- match_merge(alldata, rest_MWprobes[ ,2:ncol(rest_MWprobes)], alldata$participant_id, rest_MWprobes$participant_id, c(colnames(alldata), colnames(rest_MWprobes[ ,2:ncol(rest_MWprobes)])))


# mental health model
PsychEFA_multilevel <- read_csv(here::here("data/PsychEFAs", "PsychEFAvss_nfact-lower11-higher2.csv")) #/NoVSS
PsychEFA_multilevel <- PsychEFA_multilevel %>% rename_with(~ paste0("multiEFA_l1_", .), .cols = 1:11)
PsychEFA_multilevel <- PsychEFA_multilevel %>% rename_with(~ paste0("multiEFA_l2_", .), .cols = 12:13)
PsychEFA_3facts <- read_csv(here::here("data/PsychEFAs", "PsychEFAvss_nfact-lower3.csv")) #/NoVSS
PsychEFA_3facts <- PsychEFA_3facts %>% rename_with(~ paste0("3factEFA_", .))
sub_ids <- read_csv(here::here("data/PsychEFAs", "SubIDs_PsychEFA.csv"))
Psychdata <- cbind(sub_ids, PsychEFA_multilevel, PsychEFA_3facts)

alldata <- match_merge(alldata, Psychdata[ ,2:ncol(Psychdata)], alldata$participant_id, Psychdata$x, c(colnames(alldata), colnames(Psychdata[ ,2:ncol(Psychdata)])))

# mental health sumscores
psych_scores <- read_tsv("/mnt/raid0/scratch/BIDS/derivatives/summaries/survey/summary_scores/grand/grand_surveyscores_summary.tsv")
alldata <- match_merge(alldata, psych_scores[ , 5:ncol(psych_scores)], alldata$participant_id, psych_scores$participant_id, c(colnames(alldata), colnames(psych_scores[ , 5:ncol(psych_scores)])))

# change character cols to numerical
alldata[ ,2:ncol(alldata)] <- lapply(alldata[ ,2:ncol(alldata)], function(x) if(is.character(x)) as.numeric(as.character(x)) else x)


write.csv(alldata, file = "~/Git/InteroMentalHealth/data/alldata_acc.csv", row.names = FALSE)

