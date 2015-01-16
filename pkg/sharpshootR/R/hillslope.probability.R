
# 's' is a vector of soil series names, in lower-case
hillslope.probability <- function(s) {	
	# format IN statement
	in.statement <- format_SQL_in_statement(s)
	
	# format query
	q <- paste("
						 SELECT a.compname, hillslopeprof, hillpos_n, total, round(hillpos_n / total, 2) AS p
						 FROM
						 (
						 SELECT LOWER(compname) AS compname, hillslopeprof, CAST(count(hillslopeprof) AS numeric) AS hillpos_n
						 FROM component 
						 LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
						 LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
						 WHERE LOWER(compname) IN ", in.statement, "
						 AND geomftname = 'Landform'
						 AND hillslopeprof IS NOT NULL
						 GROUP BY LOWER(compname), hillslopeprof
						 ) AS a
						 JOIN
						 (
						 SELECT LOWER(compname) AS compname, CAST(count(compname) AS numeric) AS total
						 FROM component
						 LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
						 LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
						 WHERE LOWER(compname) IN ", in.statement, "
						 AND hillslopeprof IS NOT NULL
						 GROUP BY LOWER(compname)
						 ) AS b
						 ON a.compname = b.compname
						 ORDER BY compname, p DESC;", sep='')
	
	# perform query
	x <- SDA_query(q)
	
	# re-level hillslope positions
	x$hillslopeprof <- factor(x$hillslopeprof, levels=c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))
	# convert from long-wide format
	y <- dcast(x, compname ~ hillslopeprof, value.var='p')
	return(y)
}

