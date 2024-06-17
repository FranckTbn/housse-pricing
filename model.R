
modele_1 = lm(SalePrice ~ ., data = cleaned_train)
predict(modele_1, cleaned_test)

summary(modele_1)

par(mfrow=c(2,2)) 
plot(modele_1) 
# Les résidus doivent être uniformément dispersés
# au-dessus et au-dessous de zéro 




plot.res=function(modele_1,titre=""){
  plot(predict(modele_1), residuals(modele_1), col="blue",ylab="Résidus",
       xlab="Valeurs predites",main=titre)
  abline(h=0, col="green")
}

plot.res(modele_1)


pairs(cleaned_train %>% select(where(is.numeric )) )



library(broom)

viz_lm_model_1 <- function(model, n){
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

viz_lm_model_1(modele_1, 20)



viz_rf_model_2 <- function(model){
  p1 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = X.IncMSE)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance",
         x = "",
         y = "%IncMSE") +
    theme_minimal()
  
  p2 <- importance(model) %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    ggplot(aes(x = term, y = IncNodePurity)) +
    geom_segment(aes(xend = term, yend = 0), color = "grey") +
    geom_point(size = 2, color = "blue") +
    coord_flip() +
    labs(title = "random forest fearture importance",
         x = "",
         y = "IncNodePurity") +
    theme_minimal()
  
  p1+p2
}


files <- data.frame(
  Id = test$Id,
  SalePrice = predict(modele_1, cleaned_test)
)

write.csv(files, file = "output.csv", row.names = FALSE)
