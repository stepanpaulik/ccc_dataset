library(tidyverse)
library(foreach)

judges <- read_csv('/Users/stepanpaulik/Downloads/judges.csv')
registry <- read_csv("https://opendata.czso.cz/data/od_org03/res_data.csv")

judges <- judges %>% summarise(name = paste(Jméno, Příjmení))

registry_filtered <- registry %>% filter(DDATVZN >= '2022-1-1')


# Start the parallelezatin process
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
 
#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

judges_in_registry <- foreach(judge_name = judges$name, .combine = "rbind", .packages = "tidyverse") %dopar% {
  registry_filtered %>% filter(grepl(pattern = judge_name, x = FIRMA))
}

parallel::stopCluster(cl = my.cluster)

write_csv(x = judges_in_registry, file = "data/judges_in_registry.csv")

