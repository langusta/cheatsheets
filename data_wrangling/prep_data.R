# Author: Karol Kubicki

dimensions <- list(device = c("komputor", "smartfon", "dumbfon"),
                cluster = 0:5,
                test = c("baza ortogonalna",
                         "megatron30000",
                         "fan",
                         "DataScienceRocks",
                         "CienkiBolek"),
                category = c("SupaHot", "LatestNews", "ZawszeSmieszne"),
                system = c("Windows", "Linux", "Inny"),
                browser = c("Firefox", "Chrome", "Inna"),
                date = c("2016-01-01","2016-01-02","2016-01-03","2016-01-04","2016-01-05","2016-01-06","2016-01-07")
)

data <- expand.grid(dimensions, stringsAsFactors = FALSE)

data$clicks <-sample(500:2000, dim(data)[1], replace = TRUE)
data$views <- data$clicks + sample(500:3000, dim(data)[1], replace = TRUE, prob=500:3000)
data$visits <- ifelse(data$category == "SupaHot"
                     , 3 * data$clicks + sample(1500:3000, dim(data)[1], replace = TRUE, prob=1500:3000)
                     , 0)

data <- as.data.frame(data)

data$clicks[data$test == "CienkiBolek"] <- data$clicks[data$test == "CienkiBolek"]/1.2
data$clicks[data$test == "DataScienceRocks"] <- 1.05*data$clicks[data$test == "DataScienceRocks"]
ll <- sum(data$test == "baza ortogonalna")
data$clicks[data$test == "baza ortogonalna"] <- (1+rnorm(ll, 0, 0.08))*data$clicks[data$test == "baza ortogonalna"]
ll <- sum(data$test == "megatron30000")
data$clicks[data$test == "megatron30000"] <- (0.98+rexp(ll, 20))*data$clicks[data$test == "megatron30000"]
ll <- sum(data$device == "komputor")
data$views[data$device == "komputor"] <- (1.5+rnorm(ll, 0, 0.07))*data$views[data$device == "komputor"]
data$clicks[data$device == "komputor"] <- (1.5+rnorm(ll, 0, 0.07))*data$clicks[data$device == "komputor"]

data$clicks[data$browser == "Inna"] <- data$clicks[data$browser == "Inna"]/1.4
data$clicks[data$system == "Linux"] <- data$clicks[data$system == "Linux"]/1.2

write.csv(data, file = "prep_data.csv", row.names = FALSE)
