# a_martini
---
title: "Analise_preliminares_Amanda"
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 


Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
#Importação dados, seleção dados
```{r}
ex <- read.csv("C:/Users/amand/Desktop/Nova_pasta/ex.csv" , sep = ";")
View(ex)
teste <- ex[c(1:27), c(1,7,9,10)]
ex <- teste
```
#Grafico dispersão
```{r}
graphics::plot(PF ~ teor_suplemento , data= ex)
graphics::plot(GMD ~ teor_suplemento, data= ex)
graphics::plot(ECCF ~teor_suplemento, data= ex)
graphics::plot(PF ~GMD, data= ex)
graphics::plot(ECCF ~GMD, data= ex)


jpeg(filename = "dispercaopf.jpg")
graphics::plot(PF ~ teor_suplemento , data= ex)
dev.off()

jpeg(filename = "dispercaogmd.jpg")
graphics::plot(GMD ~ teor_suplemento , data= ex)
dev.off()

jpeg(filename = "dispercaoecc.jpg")
graphics::plot(ECCF ~ teor_suplemento , data= ex)
dev.off()

jpeg(filename = "dispercaoPFGMD.jpg")
graphics::plot(PF ~ GMD , data= ex)
dev.off()

jpeg(filename = "dispercaoPFGMD.jpg")
graphics::plot(ECCF ~ GMD , data= ex)
dev.off()

```

#Histograma com curva normalidade
```{r}

hist(ex$ECCF, main="Histograma do escore de condição corporal", 
     xlab="escore de condição corporal", 
     col="grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$ECCF))

hist(ex$PF, main="Histograma peso corporal final", 
     xlab="peso final", 
     col = "grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$PF))

hist(ex$GMD, main="Histograma do ganho médio diário",
     xlab="ganho médio diário", 
     col="grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$GMD))
```

#Exportando grafico
```{r}
jpeg(filename = "histecc.jpg")
hist(ex$ECCF, main="Histograma do escore de condição corporal", 
     xlab="escore de condição corporal", 
     col="grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$ECCF))
dev.off()

jpeg(filename = "histpf.jpg")
hist(ex$PF, main="Histograma peso corporal final", 
     xlab="peso final", 
     col = "grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$PF))
dev.off()

jpeg(filename = "histgmd.jpg")
hist(ex$GMD, main="Histograma do ganho médio diário",
     xlab="ganho médio diário", 
     col="grey",
     las=1, 
     breaks=15, 
     prob = TRUE)
lines(density(ex$GMD))
dev.off()
```

#Matriz dispersão 
pairs é  funçaõ grafica que forma a matriz de dispersão.lowe. panel= panel.smooth é responsavel por traçãr a linha, pch é a forma dos valores indicados na distribuição,e main é o nome desejado para a matriz.
```{r}
corex<- ex[c(1:27), c(2,3,4)]
pairs(~ECCF+PF+GMD, data=ex,lower.panel=panel.smooth,  pch=20, main="Matriz dispersão desempenho")
cor(corex)

jpeg(filename = "matriz.jpg")
pairs(~ECCF+PF+GMD, data=ex,lower.panel=panel.smooth,  pch=20, main="Matriz dispersão desempenho")
dev.off()
```

#Diagramas de caixa (boxplot)
```{r}
graphics::boxplot(list(ex$ECCF),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Escore de condição corporal final")
means <- c(mean(ex$ECCF, na.rm = T))
graphics::points(means, pch=8) 

graphics::boxplot(list(ex$PF),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Peso corporal final")
means <- c(mean(ex$PF, na.rm = T))
graphics::points(means, pch=8) 

graphics::boxplot(list(ex$GMD),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Ganho médio diário")
means <- c(mean(ex$GMD, na.rm = T))
graphics::points(means, pch=8) 

jpeg(filename = "boxecc.jpg")
graphics::boxplot(list(ex$ECCF),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Escore de condição corporal final")
means <- c(mean(ex$ECCF, na.rm = T))
graphics::points(means, pch=8)
dev.off()

jpeg(filename = "boxpf.jpg")
graphics::boxplot(list(ex$PF),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Peso corporal final")
means <- c(mean(ex$PF, na.rm = T))
graphics::points(means, pch=8)
dev.off()

jpeg(filename = "boxegmd.jpg")
graphics::boxplot(list(ex$GMD),
        range = T, varwidth = F, outline = T,  col =8,
        horizontal = F, pch=20, notch=F,
        main="Ganho médio diário")
means <- c(mean(ex$GMD, na.rm = T))
graphics::points(means, pch=8) 
dev.off()
```

#Normalidade a partir do teste do shapiro
o ultimo comando condensa todos os valores em uma unica tabela
```{r}
dados_norm<- ex[c(1:27), c(2,3,4)]
View(dados_norm)
stats::shapiro.test(ex$ECCF)
stats::shapiro.test(ex$PF)
stats::shapiro.test(ex$GMD)
lshap <- lapply(dados_norm, shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)
```

#Linearidade
```{r}
linear_ecc<- lm(ex$ECCF ~ ex$teor_suplemento)
par(mfrow = c(2, 2))
plot(linear_ecc)
summary(linear_ecc)

linear_pf<- lm(ex$PF ~ ex$teor_suplemento)
par(mfrow = c(2, 2))
plot(linear_pf)
summary(linear_pf)

linear_gmd<- lm(ex$GMD ~ ex$teor_suplemento)
par(mfrow = c(2, 2))
plot(linear_gmd)
summary(linear_gmd)

```






