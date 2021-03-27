from pyglottolog import Glottolog
import newick
import pandas

glottolog_data = Glottolog('/Users/skirgard/Dropbox/Git/glottolog') #specify the location where the clone of the Github repos glottolog/glottolog lives (or zipped zenodo file)
top_node = glottolog_data.languoid('ocea1241')
lg_list_fn = 'lg_list.tsv'
lg_list = pandas.read_csv(lg_list_fn, sep='\t')
lg_list = list(lg_list.iloc[:,0])
tree = top_node.newick_node(template='{l.id}')
tree.prune_by_names(lg_list, inverse=True)
tree.remove_redundant_nodes()

#print(tree.ascii_art())
newick.write(tree, "tree_newick.txt")