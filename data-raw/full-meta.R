
# load packages
library(infx)
library(magrittr)
library(keyring)

# set up credentials for a user that can actually access to full metadata
user <- key_list()
user <- user[user$service == "openbis", ]$username
pass <- key_get("openbis")

tok <- login_openbis(user, pass)

full <- fetch_meta(tok)
publ <- fetch_meta(tok, type = "public")

# select subset of plates as full is too large
set.seed(2017)
plates <- sample(unique(publ$Barcode), 30)

well <- full$well_annotation[full$well_annotation$PLATE %in%
                                          plates, ]
readr::write_tsv(well,
                 gzfile("../inst/extdata/well_annotation.tsv.gz",
                        compression = 9))

# subset compound info to available well info; also remove sequence info
# due to potential confidentiality
well_catnos <- unique(well$MANUFACTURER_CATALOG_NUMBER)

pools <- full$pool_contained_compound_lookup
pools <- pools[pools$MANUFACTURER_CATALOG_NUMBER %in% well_catnos, ]

sirna <- full$sequence_information_sirna
sirna <- sirna[sirna$MANUFACTURER_CATALOG_NUMBER %in%
                 unique(c(well_catnos,
                          pools$CONTAINED_MANUFACTURER_CATALOG_NUMBER)), ]

pools$CONTAINED_COMPOUND_UNIQUE_IDENTIFIER <- NA
sirna[, grepl("^(SEQUENCE|SEED)_", names(sirna))] <- NA

readr::write_tsv(sirna,
                 gzfile("../inst/extdata/sequence_information_sirna.tsv.gz",
                        compression = 9))
readr::write_tsv(pools,
                 gzfile(paste("../inst/extdata",
                              "pool_contained_compound_lookup.tsv.gz",
                              sep = "/"),
                        compression = 9))

# there are no esiRNA/small molecule compound/miRNA experiments, but still
# include some of this compound info
readr::write_tsv(full$sequence_information_compound,
                 gzfile(paste("../inst/extdata",
                              "sequence_information_compound.tsv.gz",
                              sep = "/"),
                        compression = 9))

esirna <- full$sequence_information_esirna
esirna <- esirna[sample.int(nrow(esirna), 200), ]
esirna[, grepl("^(SEQUENCE|SEED)_", names(esirna))] <- NA

readr::write_tsv(esirna,
                 gzfile(paste("../inst/extdata",
                              "sequence_information_esirna.tsv.gz",
                              sep = "/"),
                        compression = 9))

mirna <- full$sequence_information_mirna
mirna <- mirna[sample.int(nrow(mirna), 200), ]
mirna[, grepl("^(SEQUENCE|SEED)_", names(mirna))] <- NA

readr::write_tsv(mirna,
                 gzfile(paste("../inst/extdata",
                              "sequence_information_mirna.tsv.gz",
                              sep = "/"),
                        compression = 9))

# destroy the login token again
logout_openbis(tok)
