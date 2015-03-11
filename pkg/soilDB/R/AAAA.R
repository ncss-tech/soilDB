
# setup a new environment to store error messages, etc.
soilDB.env <- new.env(hash=TRUE, parent = parent.frame())

# define some default options
options(soilDB.verbose=FALSE)