tel = tel %>% 
  filter(year > 2002) %>% 
  filter(year < 2018) 

cor.test(x = tel$oni, y = tel$pdo, method = "pearson")
cor.test(x = tel$oni, y = tel$pdo, method = "spearman")
summary(lm(oni~pdo, tel))
summary(lm(pdo~oni, tel))

