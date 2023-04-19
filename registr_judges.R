library(tidyverse)
library(foreach)

judges <- read_csv("https://rejc.justice.cz/;jsessionid=cZqyfAfihEUmZihyCaJT3dPX_DucC26S8fPfO0v0.rejcextapl01?0-IResourceListener-soudceHomePanel-form-downloadLink")
registry <- read_csv("https://opendata.czso.cz/data/od_org03/res_data.csv")

judges <- judges %>% mutate(name = paste(Jméno, Příjmení)) %>% select(name)


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
  registry %>% filter(grepl(pattern = judge_name, x = FIRMA))
}

parallel::stopCluster(cl = my.cluster)
