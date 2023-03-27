switch_names <- function(judges) {
  judges <- foreach(i = 1:length(judges), .combine = "c") %do% {
  paste0(word(judges[i], 2), " ", word(judges[i], 1))
  }
  return(judges)
}

remove_procedural <- function(data, data_metadata){
  data <- data_metadata %>% select(doc_id, type_verdict) %>% filter(!grepl(pattern = "procesní", x = .$type_verdict)) %>% left_join(data, .)
}

