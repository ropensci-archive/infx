
tok <- login_openbis()

projects <- list_projects(tok)

exp_ids <- list_experiment_ids(tok)
experiments <- list_experiments(tok, exp_ids[1:2])

samples <- list_samples(tok, exp_ids[[1]])

plates <- list_plates(tok, exp_ids[[1]])
plate_meta <- list_plate_metadata(tok, plates[1:2])
wells <- list_wells(tok, plates[[1]])

datasets <- list_datasets(tok, samples[[1]])
