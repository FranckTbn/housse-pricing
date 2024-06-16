library(ggridges)
library(DataExplorer)
library(naniar)
library(tidyverse)
library(gt)
library(ggplot2)
library(labelled) 
library(patchwork)
library(forcats) 
library(moments) # skewness and kurtosis fonction

library(rlang) 
library(ggtext) # for adding text on graphique
library(glue) # for adding mean or or html element on graphics with ggtext

library(ggstats) # for proportion in bar plot

library(RColorBrewer) # forbeautifulcolor palette


library(ggpubr) # fonction gghistogram

library(highcharter)



# Couleurs du thème

lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255) # Magenta LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255) # Gris LinkPact
lkp_comp_blue <- grDevices::rgb(0, 113, 148, maxColorValue = 255) # Gris LinkPact
lkp_light_blue  <- grDevices::rgb(35, 95, 221, maxColorValue = 255)  # Bleu LinkPact
lkp_light_green <- grDevices::rgb(0, 227, 166, maxColorValue = 255) # Vert LinkPact

lkp_colors <- c(lkp_comp_blue, lkp_magenta, lkp_green,
                lkp_light_blue, lkp_light_green)














## beau affichage gt() personnalisé

gt_trabi <- function(data){
  data %>%
    gt() %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>% 
    opt_table_lines(extent = "default") %>%
    tab_options(
      column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3)) %>%
    tab_source_note(md( "**@bineneothniel.tra <br> "))
}

# data ----
train <- read.csv("data/train.csv", na.strings = "NA")
test <- read.csv("data/test.csv", na.strings = "NA")
label <- train$SalePrice



a <- train %>% 
  select(-SalePrice) %>%
  mutate( type = "train")

b <- test %>%
  mutate( type = "test")


data <- bind_rows(a, b)


base_data <- read.csv("inputed_data.csv", na.strings = "NA")


# modalités uniques
data <- data %>% 
  select( -Id)

# quasi constante
#data <- data %>% 
#  select( -c(MiscVal, LowQualFinSF, PoolArea, PoolQC, X3SsnPorch)) 

# miscval and miscfeature have practicaly have the same information
data <- data %>% 
    select( -c(MiscVal, PoolArea)) 

# variable avec une modalité dominante dépassant 97%
# data <- data %>% 
#  select( -c(Utilities, Condition2, RoofMatl, Heating)) 


# Convertir les variables à étiquettes en facteurs



data$MSSubClass <- as.character(data$MSSubClass)
data$OverallQual <- as.character(data$OverallQual)
data$OverallCond <- as.character(data$OverallCond)

data$Fireplaces <- as.character(data$Fireplaces)
data$GarageCars <- as.character(data$GarageCars)
data$BsmtFullBath <- as.character(data$BsmtFullBath)
data$BsmtHalfBath <- as.character(data$BsmtHalfBath)

data$FullBath <- as.character(data$FullBath)

data$HalfBath <- as.character(data$HalfBath)
data$BedroomAbvGr <- as.character(data$BedroomAbvGr)
data$KitchenAbvGr <- as.character(data$KitchenAbvGr)

# à retirer

data$TotRmsAbvGrd <- as.character(data$TotRmsAbvGrd)

data$YrSold <- as.character(data$YrSold)
data$MoSold <- as.character(data$MoSold)


# Electrical

median_Electrical <- median(data$Electrical, na.rm = TRUE)
data <- data %>%
  mutate(Electrical = ifelse(is.na(Electrical), median_Electrical, Electrical))




## création de novelles variables

# YearBuilt
data <- data %>%
  mutate(housse_age = 2011 - YearBuilt) %>%
  select(-YearBuilt)

data <- data %>%
  mutate(housse_age = case_when(
    0 <= housse_age & housse_age <= 10 ~ "1",
    10 < housse_age & housse_age <= 20 ~ "2",
    20 < housse_age & housse_age <= 30 ~ "3",
    30 < housse_age & housse_age <= 40 ~ "4",
    40 < housse_age & housse_age <= 65 ~ "5",
    65 < housse_age & housse_age <= 80 ~ "6",
    80 < housse_age  ~ ">80"
  ))


# GarageYrBlt

data <- data %>%
  mutate(garage_age = 2011 - GarageYrBlt) %>%
  select(-GarageYrBlt)

data <- data %>%
  mutate(garage_age = case_when(
    is.na(garage_age) ~ "No_Garage",
    0 <= garage_age & garage_age <= 10 ~ "1",
    10 < garage_age & garage_age <= 20 ~ "2",
    20 < garage_age & garage_age <= 30 ~ "3",
    30 < garage_age & garage_age <= 40 ~ "4",
    40 < garage_age & garage_age <= 50 ~ "5",
    50 < garage_age & garage_age <= 60 ~ "6",
    60 < garage_age  ~ ">60"
  ))

# MasVnrArea




data <- data %>%
  mutate(MasVnrArea = case_when(
    is.na(MasVnrArea) ~ "None",
    MasVnrArea == 0 ~ "None",
    0 <= MasVnrArea & MasVnrArea <= 250 ~ "1",
    250 < MasVnrArea & MasVnrArea <= 500 ~ "2",
    500 < MasVnrArea  ~ ">500"
  ))


# GarageArea

data <- data %>%
  mutate(GarageArea = case_when(
    is.na(GarageArea) ~ "No_Garage",
    0 <= GarageArea & GarageArea <= 400 ~ "1",
    400 < GarageArea & GarageArea <= 600 ~ "2",
    600 < GarageArea & GarageArea <= 800 ~ "3",
    800 < GarageArea  ~ ">800"
  ))

# X1stFlrSF

data <- data %>%
  mutate(X1stFlrSF = case_when(
    is.na(X1stFlrSF) ~ "No.Garage",
    0 <= X1stFlrSF & X1stFlrSF <= 750 ~ "1",
    750 < X1stFlrSF & X1stFlrSF <= 1000 ~ "2",
    1000 < X1stFlrSF & X1stFlrSF <= 1250 ~ "3",
    1250 < X1stFlrSF & X1stFlrSF <= 1500 ~ "4",
    1500 < X1stFlrSF & X1stFlrSF <= 2000 ~ "5",
    2000 < X1stFlrSF  ~ ">2000"
  ))




# YearRemodAdd
data <- data %>%
  mutate(remod_since = 2011 - YearRemodAdd) %>%
  select(-YearRemodAdd)

data <- data %>%
  mutate(remod_since = case_when(
    0 <= remod_since & remod_since <= 10 ~ "1",
    10 <= remod_since & remod_since <= 20 ~ "1",
    20 < remod_since & remod_since <= 40 ~ "2",
    40 < remod_since & remod_since <= 60 ~ "3",
    60 < remod_since  ~ ">60"
  ))
#
##############

data <- data %>%
  mutate(TotalBsmtSF = case_when(
    TotalBsmtSF == 0 ~ "No.Basement",
    0 < TotalBsmtSF & TotalBsmtSF <= 300 ~ "1",
    300 <= TotalBsmtSF & TotalBsmtSF <= 600 ~ "2",
    600 < TotalBsmtSF & TotalBsmtSF <= 900 ~ "3",
    900 < TotalBsmtSF & TotalBsmtSF <= 1200 ~ "4",
    1200 < TotalBsmtSF & TotalBsmtSF <= 1500 ~ "5",
    1500 < TotalBsmtSF  ~ ">1500"
  ))


########################"
data <- data %>%
  mutate(BsmtFinSF1 = case_when(
    BsmtFinSF1 == 0 ~ "No.Basement",
    0 < BsmtFinSF1 & BsmtFinSF1 <= 250 ~ "1",
    250 <= BsmtFinSF1 & BsmtFinSF1 <= 500 ~ "2",
    500 < BsmtFinSF1 & BsmtFinSF1 <= 750 ~ "3",
    750 < BsmtFinSF1 & BsmtFinSF1 <= 1000 ~ "4",
    1000 < BsmtFinSF1 & BsmtFinSF1 <= 1500 ~ "5",
    1500 < BsmtFinSF1  ~ ">1500"
  ))

#################

data <- data %>%
  mutate(BsmtUnfSF = case_when(
    BsmtUnfSF == 0 ~ "No.Basement",
    0 < BsmtUnfSF & BsmtUnfSF <= 500 ~ "1",
    500 < BsmtUnfSF & BsmtUnfSF <= 1000 ~ "2",
    1000 < BsmtUnfSF   ~ ">1000"
  ))

##########################


data <- data %>%
  mutate( OpenPorchSF = ifelse(OpenPorchSF == 0, "No", "Yes"),
          EnclosedPorch = ifelse(EnclosedPorch == 0, "No", "Yes"),
          X3SsnPorch =  ifelse(X3SsnPorch == 0, "No", "Yes"),
          ScreenPorch = ifelse(ScreenPorch == 0, "No", "Yes"),
          LowQualFinSF = ifelse(LowQualFinSF == 0, "No", "Yes"),
          BsmtFinSF2 = ifelse(BsmtFinSF2 == 0, "No", "Yes")
          )








# discretisation ----

# LotArea

# data <- data %>%
#   mutate(LotArea = case_when(
#     0 <= LotArea & LotArea <= 2000 ~ "1",
#     2000 < LotArea & LotArea <= 4000 ~ "2",
#     4000 < LotArea & LotArea <= 6000 ~ "3",
#     6000 < LotArea & LotArea <= 8000 ~ "4",
#     8000 < LotArea & LotArea <= 10000 ~ "5",
#     10000 < LotArea & LotArea <= 12000 ~ "6",
#     12000 < LotArea & LotArea <= 14000 ~ "7",
#     14000 < LotArea & LotArea <= 16000 ~ "8",
#     16000 < LotArea & LotArea <= 18000 ~ "9",
#     18000 < LotArea & LotArea <= 20000 ~ "10",
#     20000 < LotArea & LotArea <= 25000 ~ "11",
#     
#     
#     25000 < LotArea & LotArea <= 50000 ~ "12",
#     50000 < LotArea  ~ ">50000",
#   ))



# GrLivArea
data <- data %>%
  mutate(GrLivArea = case_when(
    0 <= GrLivArea & GrLivArea <= 700 ~ "1",
    700 < GrLivArea & GrLivArea <= 1000 ~ "2",
    1000 < GrLivArea & GrLivArea <= 1300 ~ "3",
    1300 < GrLivArea & GrLivArea <= 1600 ~ "4",
    1600 < GrLivArea & GrLivArea <= 1800 ~ "5",
    1800 < GrLivArea & GrLivArea <= 2100 ~ "6",
    2100 < GrLivArea & GrLivArea <= 2800 ~ "7",
    2800 < GrLivArea  ~ ">2800",
  ))



# LotFrontage





# X2ndFlrSF
data <- data %>%
  mutate(X2ndFlrSF = case_when(
    X2ndFlrSF == 0 ~ "No_2nd_floor",
    0 < X2ndFlrSF & X2ndFlrSF <= 500 ~ "0-500",
    500 < X2ndFlrSF & X2ndFlrSF <= 1000 ~ "500-1000",
    1000 < X2ndFlrSF & X2ndFlrSF <= 1500 ~ "1000-1500",
    1500 < X2ndFlrSF  ~ ">1500",
  ))

# WoodDeckSF

data <- data %>%
  mutate(WoodDeckSF = case_when(
    WoodDeckSF == 0 ~ "No_WoodDeckSF",
    0 < WoodDeckSF & WoodDeckSF <= 200 ~ "0-200",
    200 < WoodDeckSF  ~ ">200",
  ))


## corrélation entre  (àvérifier)

# data <- data %>% 
#  select(- c(BsmtFinSF1, BsmtFinSF2))


#  on remplace les missings
data <- data %>%
  mutate(
    Alley = ifelse(is.na(Alley), "No.alley.access", Alley),
    
    BsmtQual = ifelse(is.na(BsmtQual), "No.Basement", BsmtQual),
    BsmtCond = ifelse(is.na(BsmtCond), "No.Basement", BsmtCond),
    BsmtExposure = ifelse(is.na(BsmtExposure), "No.Basement", BsmtExposure),
    BsmtFinType1 = ifelse(is.na(BsmtFinType1), "No.Basement", BsmtFinType1),
    BsmtFinType2 = ifelse(is.na(BsmtFinType2), "No.Basement", BsmtFinType2),
    
    MiscFeature = ifelse(is.na(MiscFeature), "None", "Yes"),
    Fence = ifelse(is.na(Fence), "None", Fence),
    FireplaceQu = ifelse(is.na(FireplaceQu), "No.Fireplace", FireplaceQu),
    
    GarageType = ifelse(is.na(GarageType), "No.Garage", GarageType),
    GarageFinish = ifelse(is.na(GarageFinish), "No.Garage", GarageFinish),
    GarageQual = ifelse(is.na(GarageQual), "No.Garage", GarageQual),
    GarageCond = ifelse(is.na(GarageCond), "No.Garage", GarageCond),
    
    Pool = ifelse(is.na(PoolQC), "No.Pool", "Yes")
  ) %>%
  select(- PoolQC)



train1 <- data[1:nrow(train), ]

train1$SalePrice = label

vartt <- data %>% select_if(~ is.factor(.) || is.character(.)) %>% as.data.frame()

clean_data <- train1




viz_train_test <- function(data, var){ 
  
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
  ggplot( aes( x = {{var}}, fill = type) ) +
    geom_bar( position = "fill" ) +
    labs(
      title = paste("repartion des moadlités de ", var_name, " entre train et test"),
      x = var_name,
      y = ""
    ) +
    theme_minimal()
  
  }



# variable qualitative ----

var_quali_viz_1 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(mapping = aes(x = fct_infreq({{var}}), y = after_stat(prop), by = 1)) +
    geom_bar(colour = 4, fill = "white", stat = "prop") +
    labs(x = var_name, y = "Frequency") +
    theme(axis.title.x = element_text(size = 15,
                                      color = lkp_colors,
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic")) +
    scale_y_continuous(labels = scales::label_percent())
}



var_quali_viz_2 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  
  data %>%
    mutate({{var}} := as.factor({{var}})) %>%
    ggplot(mapping = aes(
      x = forcats::fct_infreq({{var}}),
      y = after_stat(prop),
      by = 1
    )) +
    geom_bar(colour = "black",
             fill = "#4477AA",
             stat = "prop") +
    labs(title = var_name, y = "Frequency") +
    theme(
      axis.title.x = element_text(
        size = 15,
        color = "blue",
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 10,
        color = "red",
        face = "italic"
      )
    ) +
    geom_text(
      aes(label = after_stat(prop) %>%
            scales::percent(accuracy = .1)),
      stat = "prop",
      nudge_y = .03
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank()
    ) +
    xlab(NULL) + ylab(NULL) 
  
}




var_quali_viz_3 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
  
  data <- data %>%
    count({{class}}) %>%
    mutate(prop = n / sum(n) * 100,
           label = paste0(round(prop, 1), "%"))
  
  ggplot(data, aes(x = "", y = prop, fill = {{class}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL, fill = var_name) +
    theme_void()+
    ggtitle("Répartition par groupe")
}




 # for color in camenber and bar plot

# Fonction de visualisation de camembert
var_quali_viz_4 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
  
  data <- data %>%
    count({{class}}) %>%
    mutate(prop = n / sum(n) * 100,
           label = paste0(round(prop, 1), "%"))
  
  ggplot(data, aes(x = 2, y = prop, fill = {{class}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL, fill = var_name) +
    theme_void()+
    xlim(0.5, 2.5) +
    ggtitle("Répartition par groupe")
}


# variable quantitative ----





var_quanti_viz_1 <- function(data, var){
  
  var_name <- as_label(enquo(var))
  
  # Graphiques basiques
  gghistogram(
    data = data,
    x = var_name,
    fill = "steelblue",
    alpha = 1,
    bins = 15,
    add = "median",
    add_density = TRUE,
    rug = TRUE,
    title = paste("Histogram Plot of", var_name),
    xlab = var_name,
    ylab = "",
  )
  
}




# ajouter le premier quartile, , 3, le min et max

var_quanti_viz_2 <- function(data, var, stat = FALSE) {
  var_name <- as_label(enquo(var))
  
  
  if (!var_name %in% colnames(data)) {
    stop("The variable does not exist in the dataset")
  }
  min_value <- min(data[[var_name]], na.rm = TRUE)
  
  q1_value <- quantile(data[[var_name]], probs = 0.25, na.rm = TRUE)
  
  median_value <- quantile(data[[var_name]], probs = 0.5, na.rm = TRUE)
  
  mean_value <- mean(data[[var_name]], na.rm = TRUE)
  
  q3_value <- quantile(data[[var_name]], probs = 0.75, na.rm = TRUE)
  
  max_value <- max(data[[var_name]], na.rm = TRUE)
  
  # changer le paramètre stat = TRUE uniquement quand il s'agit de la variable cible
  
  if (stat == TRUE){
    median_value <- median(data[[var_name]], na.rm = TRUE)

    skewness_value <- skewness(data[[var_name]], na.rm = TRUE)
    kurtosis_value <- kurtosis(data[[var_name]], na.rm = TRUE)
  }
  
  
  p <- ggplot(data = data, mapping = aes(x = !!enquo(var))) +
    geom_density(
      fill = "steelblue",
      alpha = 0.3,
      color = "lightblue",
      adjust = 0.8
    ) +
    list( if (stat == TRUE) {
      geom_richtext(
        aes(
          x = Inf,
          y = Inf,
          label = glue::glue(
            "<b><span style='color:blue'>mean    :</span></b> {round(mean_value    , 2)} <br> 
           <b><span style='color:red'>mediane :</span></b> {round(median_value  , 2)} <br> 
           <b>Skewness:</b> {round(skewness_value, 2)} <br>
           <b>kurtosis:</b> {round(kurtosis_value, 2)}"
          )
        ),
        hjust = 1.1,
        vjust = 1.1,
        fill = "cornsilk",
        label.color = NA,
        color = "black",
        size = 4
      ) +
        geom_vline(
          aes(xintercept = median_value),
          color = "red",
          linetype = "dotted",
          size = 1
        )
    }, if (stat == FALSE) {
      geom_richtext(
        aes(
          x = Inf,
          y = Inf,
          label = glue::glue(
            "
            <b>Min    :</b> {round(min_value    , 2)}  <br>
            <b>1st Qu    :</b> {round(q1_value     , 2)}  <br>
            <b>Median    :</b> {round(median_value    , 2)}  <br>
            <b><span style='color:blue'>Mean    :</span></b> {round(mean_value    , 2)}  <br>
            <b>3rd Qu    :</b> {round(q3_value    , 2)}  <br>
            <b>Max    :</b> {round(max_value    , 2)}  <br>
            "
          )
        ),
        hjust = 1.1,
        vjust = 1.1,
        fill = "cornsilk",
        label.color = NA,
        color = "black",
        size = 4
      )
    } 
    ) +
    geom_vline(
      aes(xintercept = mean_value),
      color = "blue",
      linetype = "dashed",
      size = 1
    )  +
    labs(
      title = paste("Density Plot of", var_name),
      x = var_name,
      y = "Density"
    ) +
    theme_minimal()
  
  
  p
}


var_quanti_viz_3 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot( aes(x = factor(1)  , y = {{var}})) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.25) +
    geom_boxplot(fill = "dodgerblue1",
                 colour = "black",
                 alpha = 0.7,
                 outlier.colour = "tomato2",
                 outlier.shape = 16,
                 outlier.size = 2) +
    labs(x = var_name , y = "") + 
    theme(axis.title.x = element_text(size = 15,
                                      color = "blue",
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = lkp_blue,
                                      face = "italic")) +
    theme_minimal()
}

# var_quanti_viz_3(data = train, SalePrice)




# variables quantitative / quantitative (à refaire) ----

var_cible_quanti <- function(data, var) {
 
  var_name <- as_label(enquo(var))
  
  # Créer un graphique avec ggplot2
  p <- ggplot(data, aes(x = !!enquo(var), y = SalePrice)) +
    geom_point(color = "black", size = 1.5 ) +                            # Ajouter des points
    geom_smooth(method = "lm", color = "red", se = FALSE ) +
    labs(
      title = paste( var_name, " ~  SalePrice"),
      x = var_name,
      y = "SalePrice"
    ) +
    theme(axis.title.x = element_text(size = 15,
                                      color = lkp_colors,
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic"),
          plot.title = element_text(hjust = 0.5,
                                    face = "bold")
          )
    
  
  # Afficher le graphique
  print(p)
}



# variables qualitative / quantitative ----

var_cible_quali_1 <- function(data = train, var ){
  
  var_name <- as_label(enquo(var))
  
  data %>% 
    group_by({{var}}) %>%
    summarise( moyenne = mean(SalePrice)) %>%
    ggplot(mapping = aes( x = {{var}}, y = moyenne,  ))+
    geom_col(fill = "white",
             colour = 4,) +
    labs(x = var_name, y = "SaleMean") +
    theme(axis.title.x = element_text(size = 15,
                                      color = "blue",
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic"))
  
}

# var_cible_quali_1(var = MSZoning)


var_cible_quali_2 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>%
    ggplot( aes(x = {{var}} , y = SalePrice)) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.25) +
    geom_boxplot(fill = "dodgerblue1",
                 colour = "black",
                 alpha = 0.5,
                 outlier.colour = "tomato2") +
    labs(x = var_name, y = "SalePrice") + 
    theme(axis.title.x = element_text(size = 15,
                                      color = "blue",
                                      face = "bold"),
          axis.title.y = element_text(size = 10,
                                      color = "red",
                                      face = "italic"))
}




var_cible_quali_3 <- function(data = train, var){
  
  var_name <- as_label(enquo(var))
  
  data %>% 
    ggplot( aes(x = SalePrice, y = {{var}}, fill = {{var}})) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none") +
    labs(x = var_name, y = "SalePrice", title = "distribution de prix par zonne") 
}



# missing value ----

missing_viz_1 <- function(data, class){
  
  var_name <- as_label(enquo(class))
  
    data <- data %>%
      select({{class}}) %>%
      mutate(
        observation = ifelse(is.na({{class}}), "missing", "value")
      )
    
    data <- data %>%
      count(observation) %>%
      mutate(prop = n / sum(n) * 100,
             label = paste0(round(prop, 1), "%"))
    
    ggplot(data, aes(x = 2, y = prop, fill = observation)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5
      ) +
      scale_color_brewer(palette = "Dark2") +
      labs(x = NULL, y = NULL) +
      theme_void()+
      xlim(0.5, 2.5) +
      ggtitle( paste0(var_name, " Missing Value") )
    
}

# missing_viz_1(data = train, class = LotFrontage )



missing_loliplot <- function(data){
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter( missing > 0 )
  
  
  missing <- miss_data %>% distinct(variables) %>% as.vector()
  gg_miss_var( data %>% select(missing$variables) )
}



missing_variables <- function(data){
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter( missing > 0 )
  
  miss_data %>%
    gt_trabi()
}



# install.packages("BiocManager")

# BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)



missing_heatmap <- function(data){
  
  miss_data <- data %>%
    questionr::freq.na() %>%
    as.data.frame() %>%
    rownames_to_column("variables") %>%
    filter( missing > 0 )
  
  missing <- miss_data %>% distinct(variables) %>% as.vector()
  
  # Convert data to a matrix
  data_matrix <- as.matrix( data %>% select(missing$variables) )
  
  # Create a logical matrix indicating missing values
  missing_data <- is.na(data_matrix)
  
  # Define a color palette
  col_fun <- colorRamp2(c(FALSE, TRUE), c("lightblue", "tomato"))
  
  # Create the heatmap
  Heatmap(
    missing_data,
    name = "Missing Values",
    col = col_fun,
    show_row_names = TRUE,
    show_column_names = TRUE,
    column_names_side = "top",
    column_names_gp = gpar(fontsize = 10),
    row_names_gp = gpar(fontsize = 10),
    heatmap_legend_param = list(
      title = "Missing",
      at = c(FALSE, TRUE),
      labels = c("value", "missing")
    ),
    cluster_rows = FALSE,
    cluster_columns = FALSE
  )
  
  
}
library(circlize)

library(reshape2)
corelation_heatmap <- function(data){
  
  numeric_train <- data %>% select_if(is.numeric)
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_train)
  
  # Reshape the correlation matrix
  melted_cor_matrix <- melt(cor_matrix)
  
  # Create the heatmap
  ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 5, hjust = 1)) +
    coord_fixed() +
    labs(title = "Correlation Heatmap",
         x = "Variables",
         y = "Variables")
  
}



var_type_viz_1 <- function(data){
  nb_int <- sum(sapply(data, is.integer))
  nb_num <- sum(sapply(data, is.numeric))
  nb_char <- sum(sapply(data, is.character))
  nb_log <- sum(sapply(data, is.logical)) 
  nb_fact <- sum(sapply(data, is.factor))
  
  
  df <- data.frame(
    type = c("numeric", "character", "logical", "factor", "integer" ),
    freq = c(nb_num, nb_char, nb_log, nb_fact, nb_int)
  ) %>%
    arrange(desc(freq))
  
  df %>%
    hchart(
      type = "column", hcaes(x = "type", y = "freq"),
      color = "steelblue"
    ) %>%
    hc_title(
      text = "Bar Plots",
      style = list(fontWeight = "bold", fontSize = "30px"),
      align = "center"
    ) %>% 
    hc_subtitle(
      text = "variables types", 
      style = list(fontWeight = "bold"),
      align = "center"
    ) %>% 
    hc_credits(
      enabled = TRUE, 
      text = "md(Data Source: kaggle: https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques))",
      style = list(fontSize = "10px")
    ) %>%
    hc_xAxis(title = list(text = "type")) %>%
    hc_yAxis(title = list(text = "frequency")) %>%
    hc_add_theme(hc_theme_economist())
  
  
}

data_information <- function(data) {
  if(!is.data.frame(data)) {
    cat("The input is not a data frame.\n")
    return(NULL)
  }
  
  cat("Basic Information about the Dataset:\n")
  cat("-----------------------------\n")
  
  # Dimensions of the dataset
  cat("Dimensions (rows x columns): ", dim(data)[1], " x ", dim(data)[2], "\n\n")
  
  # Column names
  cat("Column Names:\n")
  cat(paste(names(data), collapse = ", "), "\n\n")
  
  # Summary of each column
  cat("Summary of each column:\n")
  for (col in names(data)) {
    cat(col, ":\n")
    cat(paste(capture.output(summary(data[[col]])), collapse = "\n"), "\n\n")
  }
  
  # Number of missing values
  cat("Number of missing values in each column:\n")
  missing_values <- colSums(is.na(data))
  for (col in names(missing_values)) {
    cat(col, ": ", missing_values[col], "\n")
  }
  
  # Data types of each column
  cat("\nData types of each column:\n")
  for (col in names(data)) {
    cat(col, ": ", class(data[[col]]), "\n")
  }
}





