xfun::pkg_attach2("tidyverse","udpipe")
# #UDPipe model creation for the whole text corpus
ud_model <- udpipe_download_model(language = "czech")
ud_model <- udpipe_load_model(ud_model$file_model)
data_ud <- data.frame()

for(i in 1:8) {
  data_ud_temp <- udpipe_annotate(ud_model, x = data_texts$text[(i*10000-9999):(i*10000)], doc_id = data_texts$doc_id[(i*10000-9999):(i*10000)]) %>% as.data.frame()
  data_ud_temp <- subset(data_ud_temp, select = -c(4,9:13))
  data_ud <- rbind(data_ud, data_ud_temp)
  print(i)
}
data_ud_temp <- udpipe_annotate(ud_model, x = data_texts$text[80001:length(data_texts$doc_id)], doc_id = data_texts$doc_id[80001:length(data_texts$doc_id)]) %>% as.data.frame()
data_ud_temp <- subset(data_ud_temp, select = -c(4,9:13))
data_ud <- rbind(data_ud, data_ud_temp)


save(data_ud, file = "models/US_UDmodel.RData")
