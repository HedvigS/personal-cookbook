# cldf-cookbook

This is a collection of code (Python and R) which I use for particular purposes related to glottolog and cldf-data:

* derive a subtree of a glottolog family tree based on a set of tips
* plot points on a Pacific centered map
* make cldf data tables languages and values wide
* exclude language-languoids that aren't languages per se (aren't distinct from all other languages and haven't been used as main communication for a human society) and count the words (strings separated by spaces) in their names

To that end, there are three scripts:

* `python/create_tree_bottom_up.py` is a short script which takes a list of glottocodes for a given family in glottolog and outputs the subtree which contains all of those glottocodes as tips and no other 
* `R/Worldmap_plotting.R` is a longer script which plots points on a pacific-centered worldmap. It is exemplified with langoids from glottolog as points colored for MED or Family_ID, but can be adaopted for other purposes 
* `R/make_lang_values_wide.R` fetches the language and values tables from glottolog/glottolog-cldf and combines them and makes them wide.
* `R/Counting_language_names.R` filters the languages and values tables to only "real" languages and counts the words in their names

To note
* these scripts are meant to be helpful examples, not all purpose code. Copy and adapt if you need these things
* `python/create_tree_bottom_up.py` relies on there being glottolog data in the style of glottolog/glottolog (either clone or Zenodo version). I prefer Zenodo to glottolog/glottolog since it is a particular version whereas glottolog/glottolog is an ongoing curated dataset
* the R scripts rely on cldf-formatted data and fetch files directly from glottolog/glottolog-cldf. There are disadvantages and advantages with this. Take what you find useful from these scripts.
