#This is a script which takes a list of glottocodes (be they of the glottolog level dialects, languages or family level) and produces a subtree of a glottolog family tree where they are the tips.

##Local version of glottolog data necessary
#It is necessary that you have a local version of glottolog data. This can either be a clone of the glottolog/glottolog repository or your fork of it, an unzipped released version of Glottolog from GitHub, or an unzipped download of a released version of Glottolog from ZENODO. This is the object that needs to be specified as "glottolog_data" in this script. This sample scripts contains a file path on my machine, you will need to edit this.

#You will  need python3 installed as well as the specific pacakges pyglottolog, newick and pandas.

#list of tips needs to have the relevant glottocodes as its first column

from pyglottolog import Glottolog 
import newick
import pandas

glottolog_data = Glottolog('/Users/skirgard/Dropbox/Git/glottolog') #specify the location where the clone of the Github repos glottolog/glottolog lives (or zipped zenodo file)
top_node = glottolog_data.languoid('ocea1241') #This is where you specify which family we are pruning. This can either by a top-genetic languoid ("aust1307") or a sub-branch ("ocea1241").

lg_list_fn = 'lg_list.tsv' #This is the list of desired tips
lg_list = pandas.read_csv(lg_list_fn, sep='\t')
lg_list = list(lg_list.iloc[:,0]) #Specifying that we are taking column 1 of the table
tree = top_node.newick_node(template='{l.id}') #using the newick package to extract the entire tree
tree.prune_by_names(lg_list, inverse=True) #pruning tree
tree.remove_redundant_nodes() #removing nodes which aren't necessary for this set of tips

#print(tree.ascii_art()) #optional, makes ASII-art illustration of tree 
newick.write(tree, "tree_newick.txt") #printing to file