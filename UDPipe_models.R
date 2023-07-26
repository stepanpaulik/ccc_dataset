xfun::pkg_attach2("tidyverse","udpipe", "foreach", "doParallel", "furrr")
# #UDPipe model creation for the whole text corpus
# ud_model = udpipe_download_model(language = "czech")
ud_model = udpipe_load_model(file = "czech-pdt-ud-2.5-191206.udpipe")

# Get the number of cores and leave two free for other processes
n.cores = parallel::detectCores() - 2 

# Split the data frame into a list of smaller data.frames
US_texts = readRDS("../data/US_texts.rds") %>% 
  split(., factor(sort(rank(row.names(.))%%(n.cores*3))))

# Run UDPipe in parallel
start = Sys.time()
plan(multisession, workers = n.cores)
data_ud = future_map_dfr(.x = US_texts, ~ udpipe(x = .x, object = "czech"), model_dir = getwd(), .options = furrr_options(seed = 123)) %>%
  as_tibble() %>%
  select(doc_id, paragraph_id, start, end, lemma, upos)
end = Sys.time()

# Time benchmark
end - start

# Save the output
saveRDS(data_ud, file = "models/US_UDmodel.rds")
