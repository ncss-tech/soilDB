library(aqp)
library(daff)


### component data ###

# 
load('reference_data/components-CA630-1.8.14.Rda')
old <- components_reference
load('reference_data/components-CA630-1.8.16.Rda')
new <- components_reference

d <- diff_data(site(old), site(new), ids = 'coiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/components-site-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

d <- diff_data(horizons(old), horizons(new), ids = 'chiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/components-hz-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

# 
load('reference_data/comonth-CA630-1.8.14.Rda')
old <- comonth_reference
load('reference_data/comonth-CA630-1.8.16.Rda')
new <- comonth_reference

d <- diff_data(old, new, ids = 'coiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/comonth-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

# 
load('reference_data/correlation-CA630-1.8.14.Rda')
old <- correlation_reference
load('reference_data/correlation-CA630-1.8.16.Rda')
new <- correlation_reference

d <- diff_data(old, new, ids = 'muiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/correlation-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

### component data ###


### pedon data ###

# no differences
load('reference_data/color_data-CA630-1.8.14.Rda')
old <- color_reference
load('reference_data/color_data-CA630-1.8.16.Rda')
new <- color_reference

d <- diff_data(old, new, ids = 'phiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/color-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

#
load('reference_data/pedons-CA630-1.8.14.Rda')
old <- pedons_reference
load('reference_data/pedons-CA630-1.8.16.Rda')
new <- pedons_reference

# differences only in letter case and column names
d <- diff_data(site(old), site(new), ids = 'peiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/pedons-site-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')

# differences only in letter case and column names
d <- diff_data(horizons(old), horizons(new), ids = 'phiid', ordered=FALSE, unchanged_column_context=0, unchanged_context=0)
render_diff(d, file = 'diffs/pedons-hz-diff.html', title = 'soilDB 1.8.14 vs 1.8.16')


