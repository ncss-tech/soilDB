# all SEKI pedons *CA792*
mk <- fetchNASIS(from='pedons')

# cut down to relevant subset with field notes
mk.all <- read.csv(file='L:/NRCS/MLRAShared/CA792/SEKI_spatial support data/elevational gradient documents/elevational_seki_batch_1.txt', stringsAsFactors=FALSE)

idx <- which(mk$pedon_id %in% mk.all$IDENT)
mk <- mk[idx, ]

mineralKing <- mk

save(mineralKing, file='mineralKing.rda')
