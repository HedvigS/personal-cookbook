start_time <-Sys.time()

###
# PUT YOUR CODE YOU WANT TO TIME HERE #
###

Sys.sleep(time = 5)

###

end_time <- Sys.time()

time <- end_time - start_time
 
 cat(
   paste0("I'm done.\n",
          "It took ", round(time[[1]], 2), " ",  units(time)),
   ".\n"
 )