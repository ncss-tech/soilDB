
library(soilDB)
q <- "select top(100000) mukey from mapunit;"
the.keys <- SDA_query(q)

test_fun <- function(n, mukeys) {
  foo <- try(fetchSDA_spatial(mukeys, chunk.size = n))
  good <- !inherits(foo, 'try-error')
  return(list(result = foo, status = good))
}

# how many mukeys total to try?
n.tot <- 120

# calculate a common random index to use
idx <- the.keys[sample(1:nrow(the.keys), n.tot, replace = FALSE),]

# incrementally larger chunk sizes for same "task"
res1 <- test_fun(1, idx) # Done in 4.83 mins; mean/chunk: 2.4 secs; mean/mukey: 2.41 secs.
res2 <- test_fun(2, idx) # Done in 3.61 mins; mean/chunk: 3.6 secs; mean/mukey: 1.81 secs.
res3 <- test_fun(5, idx) # Done in 2.52 mins; mean/chunk: 6.3 secs; mean/mukey: 1.26 secs.
res4 <- test_fun(10, idx) # Done in 2.15 mins; mean/chunk: 10.7 secs; mean/mukey: 1.07 secs.
res5 <- test_fun(20, idx) # Done in 1.88 mins; mean/chunk: 18.8 secs; mean/mukey: 0.94 secs.
res6 <- test_fun(30, idx) # Done in 1.65 mins; mean/chunk: 24.8 secs; mean/mukey: 0.83 secs.
res7 <- test_fun(60, idx) # Done in 1.52 mins; mean/chunk: 45.7 secs; mean/mukey: 0.76 secs.
res9 <- test_fun(80, idx) # Done in 1.4 mins; mean/chunk: 42.1 secs; mean/mukey: 0.7 secs.
res10 <- test_fun(90, idx) # Error
res10 <- test_fun(100, idx) # Error
res8 <- test_fun(120, idx) # Error

# how many mukeys total to try?
n.tot <- 1200

# calculate a common random index to use
idx <- the.keys[sample(1:nrow(the.keys), n.tot, replace = FALSE),]

# incrementally larger chunk sizes for same "task"
res3_2 <- test_fun(60, idx) # Error 2/20th chunk
res2_2 <- test_fun(30, idx) # Done in 20.8 mins; mean/chunk: 31.2 secs; mean/mukey: 1.04 secs.
res1_2 <- test_fun(20, idx) # TODO: fill these in when they finish
res0_2 <- test_fun(10, idx) #        "

#### Setting a baseline with a big set of MUKEYs gathered "naively" and chunked in order
#
# ## try chunk size = 100; JSON limit exceeded on 4/1000th chunk [300:400]
fetchSDA_spatial(res$mukey, chunk.size = 100)
# 
# ## try chunk size = 50; some chunks were _very_ slow -- making the server really think 
# ##                      JSON limit exceeded on 18/2000th chunk [900:950]
fetchSDA_spatial(res$mukey, chunk.size = 50)
# 
# ## try chunk size = 25; JSON limit exceeded on 36/4000th chunk  [900:925]
# ##                      the fact that his happened on ~same mukeys as n=50 suggests that 
# ##                      there is some complicated/big polygon geometry for some of those keys
fetchSDA_spatial(res$mukey, chunk.size = 25)
# 
# ## try 25 again, reverse key order to see if we get farther (or not as far)
# ##              we do not get nearly as far. JSON error on 16/4000 [375:400]
fetchSDA_spatial(rev(res$mukey), chunk.size = 25)
# 
# ## use default value of 10 -- JSON limit exceeded on chunk 38/10000 [380:390] -- same spot again!
fetchSDA_spatial(rev(res$mukey))
