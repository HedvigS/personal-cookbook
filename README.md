# cldf-cookbook

This is a collection of code (Python and R) which I use for particular purposes related to glottolog and cldf-data:

* derive a subtree of a glottolog family tree based on a set of tips
* plot points on a Pacific centered map
* make cldf data tables languages and values wide
* exclude language-languoids that aren't languages per se (aren't distinct from all other languages and haven't been used as main communication for a human society) and count the words (strings separated by spaces) in their names
* stripping names of languoids in glottolog to just ascii (so SplitsTree and other packages can manage)
* assigning each languoid to an AUTOTYP area

The scripts are currently written in a style whereby you can run them from the command line, but they do not take command line input. They are also not written as functions which are applied to particular instances, but rather as scripts where you can replace particular parts of it so that it fits your situation. You will need to adapt file locations in some instances and specifics like which subtree to prune to which tips. The r scripts can be run as are, the python script will need to be told where glottolog data lives and necessitates intalling specific packages yourself in your environment (newick, pandas and pyglottolog). The r scripts not only loads packages, but also installs them. If that concerns you, read the script thouroughly before running.

There are six scripts:

* `python/create_tree_bottom_up.py` is a short script which takes a list of glottocodes for a given family in glottolog and outputs the subtree which contains all of those glottocodes as tips and no other 
* `R/Worldmap_plotting.R` is a longer script which plots points on a pacific-centered worldmap. It is exemplified with langoids from glottolog as points colored for MED or Family_ID, but can be adaopted for other purposes 
* `R/make_lang_values_wide.R` fetches the language and values tables from glottolog/glottolog-cldf and combines them and makes them wide.
* `R/Counting_language_names.R` filters the languages and values tables to only "real" languages and counts the words in their names
* `R/stripping_names_to_ascii.R` strips the items in "Name" to two other columsn: one with just ascii and one where spaces have been replaced with "_"
* `R/assigning_AUTOTYP_areas.R` takes languoids which are not assigned to an AUTOTYP-area in AUTOTYP's own tables and assigns it the AUTOTYP-area of the closest neighbour which has an AUTOTYP-area. Showcases general use of fields::rdist.earth() which calcuates distances as the crow flies which takes into account curvature of the earth

To note
* these scripts are meant to be helpful examples, not all purpose code. Copy and adapt if you need these things. I'm not at this stage adapting this to more general functions in a CRAN package
* `python/create_tree_bottom_up.py` relies on there being glottolog data in the style of glottolog/glottolog (either clone or Zenodo version). I prefer Zenodo to glottolog/glottolog since it is a particular version whereas glottolog/glottolog is an ongoing curated dataset
* the R scripts rely on cldf-formatted data and fetch files directly from the web version of glottolog/glottolog-cldf. There are disadvantages and advantages with this. Take what you find useful from these scripts.
