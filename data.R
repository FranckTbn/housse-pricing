#=====================================
# transformation de ma base de donnée 
#=====================================


# chargement des données
train <- read.csv("data/train.csv", na.strings = "NA")
test <- read.csv("data/test.csv", na.strings = "NA")


# jointure de train et test
label <- train$SalePrice
Id <- test$Id
a <- train %>% 
  dplyr::select(-SalePrice) %>%
  mutate( type = "train")
b <- test %>%
  mutate( type = "test")
data <- bind_rows(a, b)


# 100% de NA

data <- data %>% 
  dplyr::select( -c(MiscVal, PoolArea))

# ID
#data <- data %>%  dplyr::select( -c(Id) )

#time
data <- data %>% 
  mutate(time = as.Date(paste0(YrSold, "-", MoSold, "-01"), format = "%Y-%m-%d"))  %>%
  select(-c(YrSold, MoSold))


# quantitative 

# Choisir la date de référence
reference_year <- as.numeric(format(min(data$time), "%Y"))
reference_month <- as.numeric(format(min(data$time), "%m"))


# Calculer la variable quantitative
data$quantitative_dates <- (as.numeric(format(data$time, "%Y")) - reference_year) * 12 +
  as.numeric(format(data$time, "%m")) - reference_month + 1

data %>% count(time, quantitative_dates)


data <- read.csv("inputed_data.csv", na.strings = "NA")


#  train ready for model
train <- data[1:nrow(train), ]
train$SalePrice = label







1+1
