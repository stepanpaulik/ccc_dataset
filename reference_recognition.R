xfun::pkg_attach2("tidyverse", "kernlab", "e1071", "ISLR", "RColorBrewer", "word2vec", "rapportools", "foreach", "progress", "doParallel", "jsonlite")

references_annotated <- jsonlite::fromJSON(txt = "data/corpus.json")
references_annotations <- references_annotated$annotations %>% as.data.frame() %>% rename("id" = "_id")
references_documents <- references_annotated$documents %>% as.data.frame() %>% rename("id" = "_id")
references_objects <- references_annotated$objects %>% as.data.frame() %>% rename("id" = "_id")

id_tlf <- references_objectds$attributes[[1]][[1]][[1]][1:length(references_objects$attributes[[1]][[1]][[1]])]

references_documents_sample <- references_annotations %>% filter(id == id_tlf)

yeet <- foreach(i = id_tlf, .combine = "c") %do% {
str_sub(string = references_documents$plainText[references_documents$id == references_annotations$document[references_annotations$id == i]], references_annotations$start[references_annotations$id == i], references_annotations$end[references_annotations$id == i])
}

