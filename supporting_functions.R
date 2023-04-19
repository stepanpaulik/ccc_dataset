switch_names <- function(judges) {
  judges <- foreach(i = 1:length(judges), .combine = "c") %do% {
  paste0(word(judges[i], 2), " ", word(judges[i], 1))
  }
  return(judges)
}

remove_procedural <- function(data, data_metadata){
  data <- data_metadata %>% select(doc_id, type_verdict) %>% filter(!grepl(pattern = "procesní", x = .$type_verdict)) %>% left_join(data, .)
}

column_as_Date <- function(data, format = NA) {
  colnames <- colnames(data)
  
  if(is.na(format)) {
    for(i in seq(colnames)) {
      if(grepl("date", colnames[i])) {
        data[[i]] <- as.Date(data[[i]])
      } else (data[[i]] <- as.character(data[[i]]))
    }
  } else {
    for(i in seq(colnames)) {
      if(grepl("date", colnames[i])) {
        data[[i]] <- as.Date(data[[i]], format = format)
      } else (data[[i]] <- as.character(data[[i]]))
    }
  }

  return(data)
}

df_unlist <- function(data) {
  for(i in seq(data)) {
    data[[i]] <- data[[i]] %>% unlist()
    }
  return(data)
}

