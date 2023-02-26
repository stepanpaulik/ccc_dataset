xfun::pkg_attach2("httr", "xml2", "stringr", "rvest", "progress", "xmlparsedata", "rapportools")

# Global variables
id_ex_temp <<- read.csv("ID_NSS.csv")
id_ex <- as.list(id_ex_temp[,1])
id_ex <- c()
rozhodnuti_text <- c()
number_decisions <- 68658
current_i <- 1


# IDs rozhodnuti NS S
get_ID_NSS <- function (starting_i, number_decisions) {

  number_pg <- ceiling(number_decisions/20)
  
  pb_ID <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = number_pg, clear = FALSE, width= 60)
  
  request_to_replace <- readLines("Request.txt")

for (i in current_i:number_pg) {

  # Variable prep for the for loop
  id_temp <- c()
  post_cont <- c()
  
  # Replacement of page nr. for the request
  request_replaced <- str_replace (
    string = request_to_replace,
    pattern = "pageNum=1",
    replace = paste0("pageNum=", i)
  )
  
  writeLines(text = request_replaced, con = paste0("Request_temp.txt"))
  filetoupload_temp <- upload_file(paste0("Request_temp.txt"))
  
  # HTTP post to the server with i requests
  POST_req_temp <- POST (
    url = "https://vyhledavac.nssoud.cz/Home/MyResTRowsCont",
    config = list(),
    body = filetoupload_temp,
    handle = NULL,
    content_type("application/x-www-form-urlencoded; charset=UTF-8")
  )
  
  file.remove("Request_temp.txt")
  post_cont <- content(POST_req_temp, "text")
  
  # Concantation to global variable id_ex
  id_temp <- unique(unlist(str_extract_all(post_cont, '/DokumentDetail/Index/[0-9]{5,6}')))
  id_ex <<- c(id_ex, id_temp)
  
  current_i <<- i
  print(i)
  pb_ID$tick()
  Sys.sleep(1 / 100)
  }
}

get_ID_NSS(starting_i = current_i, number_decisions)
write.csv(id_ex,file="ID_NSS.csv",row.names=F)

# Metadata
cislojednaci <- c()
soudsenat <- c()
datumzahajeni <- c()
datumskonceni <- c()
spisovaznacka <- c()
oblastupravy <- c()
druhdokumentu <- c()
vyrokrozhodnuti <- c()
typrizeni <- c()
rozhodnutirizeni <- c()
soudcezpravodaj <- c()
zastupce <- c()
spravniorgan <- c()
datumnapadaneho <- c()
KS <- c()
KSspzn <- c()
KSdatum <- c()
text_zahlavi <- c()
text_vyrok <- c()
text_oduvodneni <- c()
aplikovane_predpisy <- c()

get_metadata_NSS <- function (starting_i = 1, id_ex) {

  pb_metadata <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(id_ex), clear = FALSE, width= 60)
  
  for (i in starting_i:length(id_ex)) {
 
  url <- paste0("https://vyhledavac.nssoud.cz", id_ex[i])
  html_metadata <- read_html(url) %>%
    html_nodes("body")
  
cislojednaci_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Číslo jednací')]/../*[@class='det-textval']")

if (length(cislojednaci_temp) == 0) {
  cislojednaci[i] <<- NA
} else {cislojednaci[i] <<- html_text(cislojednaci_temp)}

soudsenat_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Soud (senát)')]/../*[@class='det-textval']")

if (length(soudsenat_temp) == 0) {
  soudsenat[i] <<- NA
} else {soudsenat[i] <<- html_text(soudsenat_temp)}

datumzahajeni_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Datum zahájení')]/../*[@class='det-textval']")

if (length(datumzahajeni_temp) == 0) {
  datumzahajeni[i] <<- NA
} else {datumzahajeni[i] <<- html_text(datumzahajeni_temp)}

datumskonceni_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Datum skončení')]/../*[@class='det-textval']")

if (length(datumskonceni_temp) == 0) {
  datumskonceni[i] <<- NA
} else {datumskonceni[i] <<- html_text(datumskonceni_temp)}
  
spisovaznacka_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Spisová značka')]/../*[@class='det-textval']")

if (length(spisovaznacka_temp) == 0) {
  spisovaznacka[i] <<- NA
} else {spisovaznacka[i] <<- html_text(spisovaznacka_temp)}

oblastupravy_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Oblast úpravy')]/../*[@class='det-textval']")

if (length(oblastupravy_temp) == 0) {
  oblastupravy[i] <<- NA
} else {oblastupravy[i] <<- html_text(oblastupravy_temp)}

druhdokumentu_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Druh dokumentu')]/../*[@class='det-textval']")

if (length(druhdokumentu_temp) == 0) {
  druhdokumentu[i] <<- NA
} else {druhdokumentu[i] <<- html_text(druhdokumentu_temp)}

vyrokrozhodnuti_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Výrok rozhodnutí NSS')]/../*[@class='det-textval']")

if (length(vyrokrozhodnuti_temp) == 0) {
  vyrokrozhodnuti[i] <<- NA
} else {vyrokrozhodnuti[i] <<- html_text(vyrokrozhodnuti_temp)}

if (length(spisovaznacka_temp) == 0) {
  spisovaznacka[i] <<- NA
} else {spisovaznacka[i] <<- html_text(spisovaznacka_temp)}

typrizeni_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Typ řízení')]/../*[@class='det-textval']")

if (length(typrizeni_temp) == 0) {
  typrizeni[i] <<- NA
} else {typrizeni[i] <<- html_text(typrizeni_temp)}

rozhodnutirizeni_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Rozhodnutí ve vztahu k řízení')]/../*[@class='det-textval']")

if (length(rozhodnutirizeni_temp) == 0) {
  rozhodnutirizeni[i] <<- NA
} else {rozhodnutirizeni[i] <<- html_text(rozhodnutirizeni_temp)}

soudcezpravodaj_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and contains(@title, 'Soudce zpravodaj')]/../*[@class='det-textval']")
  
if (length(soudcezpravodaj_temp) == 0) {
  soudcezpravodaj[i] <<- NA
} else {soudcezpravodaj[i] <<- html_text(soudcezpravodaj_temp)}

zastupce_temp <- xml_find_all(html_metadata, "//*[@class='det-textitle' and @title='Zástupce (zastupce)']/../*[@class='det-textval']")

if (length(zastupce_temp) == 0) {
  zastupce[i] <<- NA
} else {zastupce[i] <<- html_text(zastupce_temp) %>% str_remove(., " hlavní subjekt.*")}

spravniorgan_temp <- xml_find_first(html_metadata, "//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[1]")

if (html_text(spravniorgan_temp) == "") {
  spravniorgan[i] <<- NA
} else {spravniorgan[i] <<- html_text(spravniorgan_temp)}

datumnapadaneho_temp <- xml_find_first(html_metadata, "//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[3]")

if (html_text(datumnapadaneho_temp) == "") {
  datumnapadaneho[i] <<- NA
} else {datumnapadaneho[i] <<- html_text(datumnapadaneho_temp)}

KS_temp <- xml_find_all(html_metadata, "//*[@title='krajskysoud']/../../../tbody/tr/td[1]")

if (length(KS_temp) == 0) {
  KS[i] <<- NA
} else {KS[i] <<- html_text(KS_temp)}

KSspzn_temp <- xml_find_all(html_metadata, "//*[@title='krajskysoud']/../../../tbody/tr/td[2]")

if (length(KSspzn_temp) == 0) {
  KSspzn[i] <<- NA
} else {KSspzn[i] <<- html_text(KSspzn_temp)}

KSdatum_temp <- xml_find_all(html_metadata, "//*[@title='krajskysoud']/../../../tbody/tr/td[8]")

if (length(KSdatum_temp) == 0) {
  KSdatum[i] <<- NA
} else {KSdatum[i] <<- html_text(KSdatum_temp)}

text_zahlavi_temp <- xml_find_all(html_metadata, "//*[@title='hvtcasttextu']/../../../tbody/tr[1]/td[2]")

if (length(text_zahlavi_temp) == 0) {
  text_zahlavi[i] <<- NA
} else {text_zahlavi[i] <<- html_text(text_zahlavi_temp)}

text_vyrok_temp <- xml_find_all(html_metadata, "//*[@title='hvtcasttextu']/../../../tbody/tr[2]/td[2]")

if (length(text_vyrok_temp) == 0) {
  text_vyrok[i] <<- NA
} else {text_vyrok[i] <<- html_text(text_vyrok_temp)}

text_oduvodneni_temp <- xml_find_all(html_metadata, "//*[@title='hvtcasttextu']/../../../tbody/tr[3]/td[2]")

if (length(text_oduvodneni_temp) == 0) {
  text_oduvodneni[i] <<- NA
} else {text_oduvodneni[i] <<- html_text(text_oduvodneni_temp)}

aplikovane_predpisy_temp <- html_elements(x = html_metadata, xpath = "//*[@title='aplikovanepravnipredpisysbcl']/../../../../table")

if (length(aplikovane_predpisy_temp) == 0) {
  aplikovane_predpisy[i] <<- NA
} else {aplikovane_predpisy[i] <<- html_table(aplikovane_predpisy_temp)}


 pb_metadata$tick()
 Sys.sleep(1 / 100)
 current_i <<- i
 print(i)
  }
}

# Functions call
get_metadata_NSS(current_i, id_ex)


    bez_predpisu <- sum(is.na(aplikovane_predpisy))/lenght(aplikovane_predpisy)

# Output
final_data <- data.frame (
  cislo_jednaci = cislojednaci,
  soud = soudsenat,
  datum_zahajeni = datumzahajeni,
  datum_skonceni = datumskonceni,
  oblast_upravy = oblastupravy,
  druh_dokumentu = druhdokumentu,
  vyrok_rozhodnuti = vyrokrozhodnuti,
  typ_rizeni = typrizeni,
  soudce_zpravodaj = soudcezpravodaj,
  zastupce = zastupce,
  spravni_organ = spravniorgan,
  datum_napadani_rozhodnuti = datumnapadaneho,
  krajsky_soud = KS,
  krajsky_soud_spzn = KSspzn,
  krajsky_soud_datum = KSdatum,
  text_zahlavi = text_zahlavi,
  text_vyrok = text_vyrok,
  text_oduvodneni = text_oduvodneni
)

write.csv(final_data,file="rozhodnuti_NSS.csv",row.names=F)
# capture.output(rozhodnuti_text, file = "RozhodnutíNSS.txt")
# write.csv(id_ex,file="ID_NSS.csv",row.names=F)





