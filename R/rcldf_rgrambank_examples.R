library(devtools)

# installing rcldf and rgrambank. the lines below installs the very most recent version available on GitHub. When the package has releases, it will be possible to specify particular versions instead of "the most recent version avaiable on main branch on GitHub".

# devtools::install_github("SimonGreenhill/rcldf", dependencies = TRUE)
# devtools::install_github("grambank/rgrambank", dependencies = TRUE)

library(rcldf) #package for reading in CLDF-datasets
library(rgrambank) #package with functions for common tasks with grambank-data
library(dplyr) # necessary package for rgrambank
library(magrittr) # necessary package for rgrambank
library(reshape2) # not necessary package for grambank, but used below for making glottolog ValueTable wide before joining to Grambank as an example.

#############################################################################
#reading in the data
grambank_cldf_object <- rcldf::cldf(mdpath = "https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip", #The function rcldf::cldf can take a file path to a json meta-data file on your computer in a cldf-dataset folder, or it can take the URL to a Zenodo record. In this case, we use a Zenodo-URL since it doesn't assume that the users has fetched particular files ahead of time. All CLDF-datasets that are released on Zenodo can be fetched with rcldf in this way. Work is underway to provide a lookuptable with URLs for different datasets, see : https://github.com/SimonGreenhill/rcldf/issues/40 .
                        load_bib = FALSE) #rcldf uses the r-package bib2df to read in the bibtex file of references in grambank. If you do not need the references, I recommend setting load_bib to FALSE. Reading in the bib-file takes a bit of time, and because in some cases the year string contains characters (e.g. "N.D") there is a warning which can be a bit annoying. The code doesn't break when load_bib is set to TRUE, it's just a convenience if you don't need the references after all.

#rcldf::cldf makes a special list object of any CLDF-dataset. From here we can extract the ValueTable and other families structures and information.

grambank_ValueTable <- grambank_cldf_object $tables$ValueTable
grambank_LanguageTable <- grambank_cldf_object $tables$LanguageTable
grambank_ParameterTable <- grambank_cldf_object $tables$ParameterTable

#you can fetch the "title" of the cldf object which also contains version number. Note that version numbers can be more detailed than 1 digit but the title only contains the first (i.e. v1.0 instead of v.1.0.3).
version <- grambank_cldf_object$metadata$`dc:title`
cat(paste0("This is dataset is ", version, ".\n"))

#############################################################################
##BINARISING multi-state features in Grambank
# Some Grambank features are not binary. They are all of the type "is the order XY, YX or both (or neither)"? They can be made binary with an rgrambank function. For more details on the arguments, see the helpfile on rgrambank::binarise()
# corresponding script in grambank-analysed: make_wide_binarized.R

grambank_ValueTable_binary <- rgrambank::binarise(ValueTable = grambank_ValueTable,
                                                     drop_multistate = TRUE,
                                                     keep_raw_binary = TRUE,
                                                     trim_to_only_raw_binary = FALSE
                                                    )

#############################################################################
##Combining with other CLDF-datasets

# example glottolog
#for some of the things you want to do with grambank, you may want to have more information about the languages than the grambank-dataset provides. For example, descriptive status of languages, endangerment level or info about the parents of dialects in grambank. If that is the case, you could also fetch glottolog-cldf which contains more meta-data on languages than grambank's LanguageTable. There is currently something wrong with fetching glottolog-cldf via rcldf, see here: https://github.com/SimonGreenhill/rcldf/issues/38 . When it's solved, I'll add in code here for using rcldf::cldf. In the meantime, we can use rgrambank::load_glottolog.
# corresponding script in grambank-analysed make_glottolog-cldf_table.R.

# get glottolog
glottolog_cldf_object <- rgrambank::load_glottolog()

version <- glottolog_cldf_object$metadata$`dc:title`
cat(paste0("This is dataset is ", version, ".\n"))

#Some relevant information on languages are in the glottolog table ValueTable and some in the LanguageTable. For conveniance, we can merge these two to make it easier.

#In the glottolog LanguageTable, there is a column called "Language_ID" which contains the glottocode of the parent of a dialect that itself has the level "language". This can get a bit confusing with the Language_ID column in other CLDF-tables, which maps onto the column "ID" in the LanguageTable. Here we just rename them to make it a bit easier.
glottolog_LanguageTable <-  glottolog_cldf_object$tables$LanguageTable %>%
    dplyr::rename(Language_level_ID = Language_ID) %>%
    dplyr::rename(Language_ID = ID)

#joining glottolog ValueTable and LanguageTable
glottolog_ValueTable_and_LanguageTable <- glottolog_cldf_object$tables$ValueTable %>%
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") %>%  #making long data wide
    full_join(glottolog_LanguageTable, by = "Language_ID")

#If we want to use the Language_level_ID column to pair datasets, it's good to know that this value is only defined if the languoid is a dialect. If the languoid is a language or family, it's missing. We may want to replace those missing values with the glottocode of the language or family if we intend to use the column for matching.
glottolog_ValueTable_and_LanguageTable <- glottolog_ValueTable_and_LanguageTable %>%
                mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID))


#############################################################################
## Combining Grambank and Glottolog.
#Grambank uses glottocodes as Language_IDs. This isn't true of all CLDF-datasets, but since this is also true of glottolog that means we can join on that column directly, we don't need to go via columns called "glottocode" as would be necessary for example with WALS, D-PLACE etc.

grambank_LanguageTable_plus_glottolog <- grambank_ValueTable %>%
    full_join(glottolog_ValueTable_and_LanguageTable, by = "Language_ID")

#############################################################################
##merge dialects and other instances of duplicate language_level_ID glottocodes
# sometimes in Grambank we have coded more than one dialect of the same language. There can also be other reasons that cause entries to have the same glottocode as their "Language_level_ID" (the glottocode of their parent who is a language). We may want to reduce these duplicates to 1 per language, for example for some analysis or for merging with other data-sets that don't contain the exact same dialects but the same languages. What we want to do is drop all but one, and replace the glottocode of that entry with a glottocode that has the level "language" (not "dialect"). To do this, we need to decide on how to drop the others. The rgrambank function has three alternatives, see the help-file for details.
# corresponding script in grambank-analysed: make_wide.R.

grambank_ValueTable_binary_language_level <- rgrambank::language_level_df(
    ValueTable = grambank_ValueTable_binary,
    LanguageTable = glottolog_ValueTable_and_LanguageTable,
    method = "singular_least_missing_data")

# < EXAMPLE CODE TO BE changed IF PR IS MERGED IN. >

##NOTE##
# the function rgrambank::language_level_df() will change name to rgrambank::reduce_ValueTable_to_unique_glottocodes() if this PR is merged in: https://github.com/grambank/rgrambank/pull/25

#############################################################################
# crop away missing data
# if https://github.com/grambank/rgrambank/pull/37 is merged in, rgrambank will have the function crop_missing_data() which takes a takes a ValueTable and given a certain cut-off limit for missing values per feature and per language crops away those features and languages that don't meet the criteria. Defaults to 0.75 and 0.75 like in grambank-analysed, but can be changed.
# corresponding script in grambank-analysed: impute_missing_values.R.

# < EXAMPLE CODE TO BE INSERTED IF PR IS MERGED IN. >

#############################################################################
##make theo scores
# theo scores is a cover term for several measurements based on grambank features such as "fusion", "informativity" etc. There is an rgrambank function that does this given a ValueTable and ParameterTable.
# corresponding scripts in grambank-analysed: make_theo_scores.R and make_theo_scores_fusion.R

grambank_theo_scores <- rgrambank::make_theo_scores(ValueTable = grambank_ValueTable_binary_language_level, ParameterTable = grambank_ParameterTable)

# the warning about NAs introduced by coercion in rgrambank::make_theo_scores() is nothing to worry about, it will be gone when this PR is merged in: https://github.com/grambank/rgrambank/pull/36

#############################################################################
# pruning trees to unique glottocodes
# If https://github.com/grambank/rgrambank/pull/15 is merged in, rgrambank will have the function rgrambank::drop_duplicate_tips_random.R which will take a tree where the tips are glottocodes and trim such that there is only one tip per language-levelled glottocode (c.f. rgrambank::language_level_df()/reduce_ValueTable_to_unique_glottocodes() ).
# corresponding script in grambank-analysed: spatiophylogenetic_modelling/processing/pruning_EDGE_tree.R

# < EXAMPLE CODE TO BE INSERTED IF PR IS MERGED IN. >
