library(soilDB)
library(aqp)
library(daff)

# running on grpname = "%11-IND"
f_l <- fetchNASIS(from = "pedons", rmHzErrors = FALSE)
f_r <- fetchNASIS(from = "pedon_report", rmHzErrors = FALSE)


# check length and peiid
length(f_l) # 5614, local db has 5643
length(f_r) # 5643

all(f_r$peiid %in% f_l$peiid)
all(f_l$peiid %in% f_r$peiid)


# check names
all(siteNames(f_r) %in% siteNames(f_l))
all(siteNames(f_l) %in% siteNames(f_r))

idx <- siteNames(f_l) %in% siteNames(f_r) 
siteNames(f_l)[! idx] # primarily missing surface rock fragments


# check differences
## site data
idx_r <- sort(siteNames(f_r)[siteNames(f_r) %in% siteNames(f_l)]) 
idx_r <- c('agric.horizon', 'albic.horizon', 'albic.materials', 'aquic.conditions', 'argillic.horizon', 'calcic.horizon', 'cambic.horizon', 'classifier', 'classtype', 'coprogenous.earth', 'densic.contact', 'densic.materials', 'drainagecl', 'earthcovkind1', 'earthcovkind2', 'erocl', 'fragic.soil.properties', 'fragipan', 'geomposflats', 'geomposhill', 'geomposmntn', 'geomslopeseg', 'glossic.horizon', 'gypsic.horizon', 'hillslopeprof', 'histic.epipedon', 'horizdatnm', 'labdatadescflag', 'limnic.materials', 'lithic.contact', 'lithologic.discontinuity', 'localphase', 'mollic.epipedon', 'mottles.with.chroma.2.or.less', 'ochric.epipedon', 'osdtypelocflag', 'paralithic.contact', 'paralithic.materials', 'pedlabsampnum', 'pedonpurpose', 'pedontype', 'peiid', 'plantassocnm', 'pmkind', 'pmorigin', 'redox.concentrations', 'redox.depletions.with.chroma.2.or.less', 'sapric.soil.materials', 'seriesstatus', 'shapeacross', 'shapedown', 'siteiid', 'slickensides', 'slopecomplex', 'soiltaxedition', 'strongly.contrasting.particle.size.class', 'taxgrtgroup', 'taxonkind', 'taxonname', 'taxorder', 'taxpartsize', 'taxsubgrp', 'taxsuborder', 'umbric.epipedon', 'utmeasting', 'utmnorthing', 'utmzone', 'weatherable.minerals', 'x', 'x_std', 'y', 'y_std')

site_daff <- diff_data(site(f_l)[idx_r], site(f_r)[idx_r])
render_diff(site_daff) # f_l parent material is all lower change


xtabs(~ f_l$earthcovkind1, droplevels = TRUE) # has less counts, pulling from the siteobs table
xtabs(~ f_r$earthcovkind1, droplevels = TRUE) # has more counts, pulling from the pedon table

test = merge(site(f_l)[c("peiid", "taxsubgrp")], site(f_r)[c("peiid", "taxsubgrp")], by = "peiid", all.x = TRUE)
View(subset(test, taxsubgrp.x != taxsubgrp.y)) # when the dates match f_l is pulling the 'sampled as' instead of the 'correlated' pedon taxonomic history



# rock fragments
vars <- c("peiid", "phiid", "fragvoltot", "total_frags_pct_nopf", "total_frags_pct", "fine_gravel", "gravel", "cobbles", "stones", "boulders", "channers", "flagstones", "parafine_gravel", "paragravel", "paracobbles", "parastones", "paraboulders", "unspecified")

frags_l <- horizons(f_l)[vars]
idx <- table(frags_l$phiid) > 1
idx <- names(idx)[!idx]
frags_l <- frags_l[frags_l$phiid %in% idx, ]
frags_r <- horizons(f_r)[vars]

idx <- frags_l$phiid[frags_l$phiid %in% frags_r$phiid]
idx2 <- frags_l[which(frags_l$phiid %in% idx), ] != frags_r[frags_r$phiid %in% idx, ]
idx2 <- apply(idx2, 1, any)
idx2 <- frags_l$phiid[idx2]
frags_daff <- diff_data(frags_l[frags_l$phiid %in% idx2, ], frags_r[frags_r$phiid %in% idx2, ])
render_diff(frags_daff)
# Out of 5112 records with > 0 frags, 291 or 5% are different. Like the comparison of NASIS-phfrags-testing.R their is some difference in the sorting between classes. The primary differences are that pedon_report is returning frags with the RV size = NULL as unspecified. This is as expected since pedon_report doesn't use the L and H, since NASIS SQL is limited. It also appears that from = "pedons" is misclassifying weakly cemented frags as nonpara. Lots of fragvoltot = 0 even though total_frags_pct != 0, which means fragvoltot has not been calculated. Should write an error check to flag and check when total_frag_pct != fragment modifers (e.g. 16% and non-gravelly). Check could also warn about unspecified rock fragments.
# Overall the rock fragment code is working.
