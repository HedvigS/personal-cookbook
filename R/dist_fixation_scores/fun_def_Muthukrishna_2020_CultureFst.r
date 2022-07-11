#from Muthukrishna, M., Bell, A. V., Henrich, J., Curtin, C. M., Gedranovich, A., McInerney, J., & Thue, B. (2020). “Beyond Western, Educated, Industrial, Rich, and Democratic (WEIRD) Psychology: Measuring and Mapping Scales of Cultural and Psychological Distance.” Psychological Science, 0956797620916782. Published, 05/21/2020.
#https://journals.sagepub.com/doi/suppl/10.1177/0956797620916782

#Hedvig Skirgård made one minor tweak to the way the function deals with NULL input for the label argument and changed a loop message.

#source("requirements.R")
source("fun_def_h_load.R")
h_load(c("stringi", "utils"))

CultureFst = function( d, loci, type, bootstrap, no.samples, label = NULL, message = NULL ){
  cat(paste0("I've started and there are ", no.samples, " samples to go through.\n"))
	# returns a matrix of pairwise fst values in the lower diagonal
	# and bootstrapped confidence intervals in the upper-diagonal (optional)
	
	# what the data should look like:
		# d: a NxM matrix of N observations for M traits, the first column must consist of population names; columns are given "trait" names, within which are variants of each trait
		# loci: a vector of characters that name the traits for which the fst is to be computed
		# type: is a vector the same length as "loci" indicating what type of trait it is: 0 for discrete (categorical), 1 for quantitative; NOTE the "row names" of type must be the vector loci, i.e. make sure that row.names(type) <- loci
		# bootstrap: a logical, if TRUE, telling the program to compute bootstrapped standard errors and confidence intervals
		# no.samples: numeric, designating the number of resamples in the bootstrap
	
	# label will be part of .rdata file name reporting the results
	
# ------- function to compute an Fst for a single trait ----------
Fst.loci = function( d, l ){
	# d is the data matrix
	# l is the name of the trait	
		# set up vectors for the numerator
		# and denominator for the fst calculation
		fst_num = fst_den = rep(0, dim(pair)[1] )
		# find out the sample size for each population
		sample.size = sapply( pops, function(z) sum( d[ is.na(d[,l])==F,1]==z ) )
		# condition on whether it is a discrete or quantitative character
		if( type[[l]]==1 ){ # quantitative character
			# number of pairs
			npairs <- dim(pair)[1]
		    print( paste( "q trait", l ) )
		# find total variance
		      totalvar = sapply( 1:npairs, function(y){ w = pair[y,2]; yo = pair[y,1]; var( c( d[ d[,1]==yo,l], d[ d[,1]==w, l ] ), na.rm=T )} ) 
		# compute between-group variance
		     # find global mean
		      totalmean = sapply( 1:npairs, function(y){ w = pair[y,2]; yo = pair[y,1]; mean( c( d[ d[,1]==yo,l], d[ d[,1]==w, l ] ), na.rm=T )} )
		      g1mn = sapply( 1:npairs, function(y){ yo = pair[y,1]; mean( d[ d[,1]==yo,l], na.rm=T )} ) # quantitative variance
		        names(g1mn) = pair[,1]
		      g2mn = sapply( 1:npairs, function(y){ w = pair[y,2]; mean( d[ d[,1]==w, l ], na.rm=T )} ) # quantitative variance
		        names(g2mn) = pair[,2]
          bgvar = sapply( 1:npairs, function(y){ yo = pair[y,1]; w = pair[y,2]; ((g1mn[[yo]] - totalmean[[y]])^2 + (g2mn[[w]] - totalmean[[y]])^2 ) } )
        # fst is the between-group variance over the total variance
    		  fst = bgvar / totalvar
		        }else{ # discrete character
		    # count the unique variants    	
        	polymorphs = as.character( unique(d[,l]) ); 
        	polymorphs = polymorphs[is.na(polymorphs)==FALSE]

        # find the frequency of any variant per trait per pair of populations
        	freq.within = lapply( pops, function(z) { 
		    	data = d[ d[,1]==z, ]
		    	freq = rep(0, length(polymorphs) )
		    	for( i in 1:length(polymorphs) ){
			 	freq[i] = sum( data[,l]==polymorphs[i], na.rm=T ) / 	sum( is.na(data[,l])==FALSE )
				  	}
			 	freq
		      	} )
      	names(freq.within) = pops      
        
      	for( i in 1:length(polymorphs) ){
		  # total variance, weighted by sample size	
		  ave.p = sapply( 1:dim(pair)[1], function(y){ w = pair[y,2]; yo = pair[y,1]; (freq.within[[yo]][i] * sample.size[yo] + freq.within[[w]][i] * sample.size[w] ) / ( sample.size[yo] + sample.size[w] ) } ) # weighted by sample size
				
		  # between group variance
		  varpi = sapply( 1:dim(pair)[1], function(y){ yo = pair[y,1]; w = pair[y,2]; ((freq.within[[yo]][i] - ave.p[[y]])^2 + (freq.within[[w]][i] - ave.p[[y]])^2 ) } )
		  
	   if( is.numeric(ave.p)==F ) break
      	# fst for this allele
		  fsti = varpi / ( ave.p*(1-ave.p ) )
		  # fst across alleles up to this point
		  fst_num = ifelse( ave.p>0 , ( ave.p*(1-ave.p ) ) * fsti + fst_num, fst_num )
		  fst_den = ifelse( ave.p>0, ( ave.p*(1-ave.p ) ) + fst_den, fst_den )
			     }
  		  fst = fst_num/fst_den 			     
			 }    
		# return fst for this trait
		fst
		}

# --------- function for calculating Fst means for one or multiple traits -------------
Fst.gen = function( d.a ){	
	
if( length(loci)>1 ){	
		# fst across all traits
		f.loci = sapply( loci, function(z) Fst.loci(d.a, z ) )
		fst.all.loci = lapply( 1:length(loci), function(z) f.loci[,z] ) # put it back in a list
		names(fst.all.loci) = loci		
		# rearrange results by pairs of countries in a symmetric table
		mean.fst = suppressWarnings( sapply( pops, function(w) sapply( pops, function(y) mean( sapply( loci, function(z) fst.all.loci[[z]][ pair[,1]==w & pair[,2]==y ] ), na.rm = TRUE ) ) ) )
		ans = list( fst.all.loci, mean.fst, loci, pops )
		names(ans) = c("fst.loci","mean.fst","loci","pops")
		
		}else{ 
		# case where only one trait
		res.one.loci = Fst.loci(d.a, loci)
		ans = list( sapply( pops, function(y) sapply( pops, function(w) res.one.loci[ pair[,1]==y & pair[,2]==w ] ) ) )
		ans[[1]][upper.tri(ans[[1]], diag=TRUE)] <- NA		
		names(ans) = c( "mean.fst" )
			}
	ans
	}
	
# ----------------------------------------------------------
# Bootstrap function for confidence intervals
bootFst = function(){

  	# function to generate a mean Fst for each sample
	sampleFst = function( i ){

	  cat(paste0("I'm on ", i, " out of ", no.samples, " samples. ", message, "\n"))
	 
	  subpops = subset( pops, sapply( 1:length(pops), function(z) any(pops[z]==pair) ) )
		index.sample = sapply( subpops, function(z){ set = which(d[,1]== z); sample( set, length(set), replace = TRUE ) } )
		index.sample = unlist( index.sample )
		ans = Fst.gen( d[index.sample,] )
		res = ans$mean.fst

		res }
	# resample
	btfst = lapply( 1:no.samples, sampleFst )	

	# rearrange results by population pair, return a vector
	npairs <- dim(pair)[1] # number of pairs
	bootDistr = sapply( 1:npairs, function(w) { 
		a = 0
		for( i in 1:length(btfst) ){ 
			a = c(a, btfst[[i]][pair[w,2],pair[w,1]] )
				} 
			a = unlist(a[-1])
		a } ) 
	 
	# calculate standard errors, means, and quantiles
	Fst.se = sapply( pops, function(w) sapply( pops, function(y){ ifelse( any(pair[,2]==w & pair[,1]==y), sqrt( var( bootDistr[,pair[,2]==w & pair[,1]==y ], na.rm = TRUE ) ), NA ) } ) )
	
	Fst.mean = sapply( pops, function(w) sapply( pops, function(y){ ifelse( any(pair[,2]==w & pair[,1]==y), mean( bootDistr[,pair[,2]==w & pair[,1]==y ], na.rm = TRUE ), NA ) } ) )
	
	Fst.confint = sapply( pops, function(w) sapply( pops, function(y) ifelse( any(pair[,2]==w & pair[,1]==y), paste( format( quantile( bootDistr[,pair[,2]==w & pair[,1]==y ], prob = 0.025, na.rm = TRUE ), digits = 3),", ", format( quantile( bootDistr[,pair[,2]==w & pair[,1]==y ], prob = 0.975, na.rm = TRUE ), digits = 3), sep = "" ), NA )  ) )
		
	ans = list( pair, Fst.se, Fst.mean, Fst.confint, bootDistr )
	names(ans) = c("pairs", "se","mean","quantiles","estimates" )
	ans
	}	
	
# ---------------------------------------------------
# subfunction calls and output	
	library(utils)
	# all pair-wise combinations
	pair = t( combn( as.character( unique(d[,1]) ), 2 ) )
	# population names
	pops = as.character(unique(d[,1]))
	# run fst calculation
	ans = Fst.gen(d)
	
	ans$sample.size = sapply( pops, function(z) sum( d[,1]==z ) )
		
		if( bootstrap==TRUE ){ # arrange output with bootstrap results
			ans$boot = bootFst()
			
			fill.bt = upper.tri( ans$mean.fst, diag = FALSE )
			# mean with standard errors in the upper diagonal
			btse = sapply( 1:length(pops), function(y) sapply( 1:length(pops), function(x) ifelse( fill.bt[x,y]==TRUE, ans$boot$se[x,y], ans$mean.fst[[x,y]] ) ) )
			colnames(btse) = pops; rownames(btse) = pops
			ans$mean.fst = btse
			
			# mean with quantiles in the upper diagonal
			ans$mean.fst.confint = sapply( 1:length(pops), function(y) sapply( 1:length(pops), function(x) ifelse( fill.bt[x,y]==TRUE, ans$boot$quantiles[x,y], ans$mean.fst[x,y] ) ) )
			colnames(ans$mean.fst.confint) = pops; rownames(ans$mean.fst.confint) = pops
			}	
		# save output to .rdata file	
	
	if(!is.null(label)){
		save(ans, file = paste( label, "_Fst.rdata", sep = "" ) ) 
	  }
		if( bootstrap==TRUE ) print( ans$mean.fst.confint ) else print(ans$mean.fst)
		ans
	}