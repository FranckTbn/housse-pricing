
library(dplyr)
  
modele_1 = lm(SalePrice ~ Neighborhood +
                MSSubClass +
                MSZoning +
                LotArea +
                LotConfig +
                Condition1 +
                HouseStyle +
                MasVnrType +
                ExterQual +
                BsmtQual +
                OverallQual +
                OverallCond + 
                ExterQual +
                GarageType +
                GarageQual +
                FireplaceQu +
                KitchenQual +
                housse_age +
                garage_age +
                remod_since,
              data = cleaned_train)


summary(modele_1)

par(mfrow=c(2,2)) 
plot(modele_1) 
# Les résidus doivent être uniformément dispersés
# au-dessus et au-dessous de zéro 





plot(modele_1)

plot.res=function(x,y,titre="")
{
  plot(x,y,col="blue",ylab="Résidus",
       xlab="Valeurs predites",main=titre)
  abline(h=0,col="green")
}

plot.res(predict(modele_1),residuals(modele_1))


pairs(cleaned_train %>% select(where(is.numeric )) )



library(broom)

viz_model_1 <- function(model, n){
  model_summary <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    arrange(p.value) %>%
    head(n)
  ggplot(model_summary, aes(x = term, y = estimate)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "Importance des variables dans le modèle de régression",
         x = "Variables",
         y = "Estimation des coefficients") +
    theme_minimal()
}

viz_model_1(modele_1, 20)







files <- data.frame(
  Id = test$Id,
  SalePrice = predict(modele_1, cleaned_test)
)

write.csv(files, file = "output.csv", row.names = FALSE)
