[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/soilDB.svg?branch=master)](https://travis-ci.org/ncss-tech/soilDB)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/soilDB)](https://cran.r-project.org/package=soilDB)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/soilDB)](https://cran.r-project.org/package=soilDB)

# soilDB

Install the stable version from CRAN:

`install.packages('soilDB')`

Install the development version from Github:

`devtools::install_github("ncss-tech/soilDB")`


# TODO Items (move to issues)

- We need to add some queries to accommodate the new ncss_pedon_lab_data tables...
	
- phnaf moved out of phorizon and into phlabresults - need to add a query to the ext_data to pull this in or add a join to the phlabresults table 	to pull it into the phorizon query directly.
	
- Add dsp_comparable_layer_id to the phorizon queries....this is our gen_hz field.

- how should multiple textures be dealt with? (2 rows/hz are currently returned)
 + can this be fixed in SQL ?
 +  NASIS: we are keeping only the first record
 +  PedonPC: texture class is ommited from query

- how should A/B horizons be dealt with when entered as 2 horizons sharing the same depths?

- soil texture plotting / subsetting / flagging helper function or guidance

- finish NASIS component queries and return similar objects as pedon queries

- need to check for presence of STD coordinates vs. DMS/datum coordinates, maybe warn users

- figure out data problems: total_frags_pct vs. fragvoltot

