#read in command line arguments
args = commandArgs(trailingOnly = TRUE)

if(length(args) != 0){
argument <- args[1]
  cat(paste0("I'm being called from the command line and you gave me one argument. It was:\n",
             argument, "\n"))
  } else { #if you're running this script chunkwise in Rstudio or similar instead of via command line, you'll read in the parameters this way:
argument <- "potato"
        cat(paste0("I'm being called from inside Rstudio or from the command line without an argument. The harcoded arugment there is:\n",
               argument, "\n"))
    
}
