xfun::pkg_attach2("stringr", "ggplot2", "tidyverse")


data <- read.csv(file = "Rozhodnuti_US.csv")

data$datum_rozhodnuti <- as.Date(data$datum_rozhodnuti, format = "%d. %m. %y")
data$datum_podani <- as.Date(data$datum_podani, format = "%d. %m. %y")
data$delka_rizeni <- data$datum_rozhodnuti - data$datum_podani

data$delka_rizeni<- data$delka_rizeni %>% 
  str_remove_all(" days") %>% 
  as.numeric()

write.csv(data, "Rozhodnuti_US2.csv")

for (i in seq(data$typ_vyroku)) {
  if(grepl("vyhověno", data$typ_vyroku[i]) == TRUE) {
    data$outcome[i] <- 1
  } else {data$outcome[i] <- 0}
}

log.model <- glm(outcome ~ delka_rizeni, data = data, family = 'binomial')
summary(log.model)

ggplot(data = data, aes(x = delka_rizeni, y = outcome)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) +
  coord_cartesian(xlim = c(0, max(data$delka_rizeni, na.rm = TRUE)), ylim = c(0.035, 0.1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Závislost pravědpodbnosti vyhovění před ÚS na délce řízení.",
    y = "Pravděpodobnost vyhovění",
    x = "Délka řízení (dny)"
  )

data_export <- data %>% select(spisova_znacka, rozhodnuti_texts)
write_csv(data_export, "US_data_export")
