---
title: "Визуализация биомедицинских данных, домашнее задание №2"
author: "Наталья Шейко"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

##### 1. Загрузите датасет insurance_cost.csv (лежит в папке домашнего задания). Это данные по базовым показателям здоровья индивида и сумме, которую страховая компания заплатила за его лечение в год. Обычно эти данные используют, чтобы потренироваться в предсказании того, как определённые характеристики индивида повышают траты страховой компании (и, соответственно, должны быть заложены в цену страховки).

```{r}
# install.packages('mltools')
library(corrplot)
library(corrr)
library(mltools)
library(data.table)
library(factoextra)
library(cluster)
library(FactoMineR)
library(ggbiplot)
library(dplyr)

data = read.csv('/Users/nataliesheiko/Bioinf/DataViz/DataViz_HT1/insurance_cost.csv', stringsAsFactors = T)
head(data, 5)
str(data)
```

##### 2. Сделайте интерактивный plotly график отношения индекса массы тела и трат на страховку. Раскрасьте его по колонке smoker.

```{r eval=F}
plot_ly(data,
  x = ~ bmi,
  y = ~ charges,
  color = ~ smoker
)   %>%
  layout(
    title = 'Отношение ИМТ и трат на страховку (с учётом статуса курения)',
    yaxis = list(title = 'ИМТ',
                 zeroline = FALSE),  
    xaxis = list(title = 'Расходы, $',
                 zeroline = FALSE))
```

##### 3. Сделайте тоже самое через ggplotly.

```{r eval=F}
plot <- ggplot() +
  geom_point(data = data,
             aes(x = bmi, y = charges, color = smoker),
             size = 1, alpha = 0.7) +
  theme_light() +
  ggtitle('Отношение ИМТ и трат на страховку (с учётом статуса курения)') + 
  labs(x = 'ИМТ', y = 'Расходы, $')
  
ggplotly(plot)
```

##### 4. Кратко сделайте корреляционный анализ данных insurance_cost. Посмотрите документацию пакетов, которые мы проходили на занятии и, исходя из этого, постройте минимум два новых типа графика (которые мы не строили на занятии).

```{r}
# данные не имеют пропусков, 'невозможных' нулевых значений
data_corr <- data %>% 
  select(is.integer | is.numeric) %>% cor()

data_corr
```

```{r warning = F}
corrplot(data_corr, method = 'pie')
corrplot.mixed(data_corr, order = 'AOE')
network_plot(data_corr, min_cor = .01, colors = c('red', 'black'))

x <- data %>% select(where(is.numeric)) %>% 
  correlate() %>% 
  rearrange() %>% 
  shave() 

rplot(x, colors = c('red', 'black'))
```

##### 5. Превратите все номинативные переменные в бинарные/дамми. Т.е. sex и smoker должны стать бинарными (1/0), а каждое уникальное значение region -- отдельной колонкой, где 1 говорит о наличии этого признака для наблюдения, а 0 -- об отсутствии. Создайте новый датафрейм, где вы оставите только нумерические переменные.

```{r}
data_ohe <- one_hot(as.data.table(data))
# датафрейм, построенный таким образом, состоит только из нумерических переменных
head(data_ohe, 5)
```

##### 6. Постройте иерархическую кластеризацию на этом датафрейме из задания 5.

```{r}
# сначала попробуем выполнить кластеризацию "руками"
# подготовка кластеров
data_ohe <- scale(data_ohe)
res.dist <- dist(data_ohe, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")
```

```{r}
# оценка качества кластеризации (два метода)
res.coph <- cophenetic(res.hc)
cat('Correlation between cophenetic distance and the original distance:', cor(res.dist, res.coph))

# визуализация
fviz_dend(res.hc, k = 5,
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE)
```

```{r}
# тот же результат попробуем получить, используя библиотеку clusters 
res.agnes <- agnes(x = data, 
                   stand = TRUE,
                   metric = "euclidean",
                   method = "ward" 
                   )
fviz_dend(res.agnes, cex = 0.6, k = 5)
```

##### 7. Используя документацию или предложенный учебник сделайте ещё несколько возможных графиков по иерархической кластеризации. Попробуйте раскрасить кластеры разными цветами.

```{r}
grp <- cutree(res.hc, k = 5)
fviz_cluster(list(data = data_ohe, cluster = grp),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE)

fviz_dend(res.hc, 
          cex = 0.5, 
          k = 5,
          k_colors = "jco", 
          type = "circular")
```

##### 8. Сделайте одновременный график heatmap и иерархической кластеризации

```{r}
library(pheatmap)
pheatmap(data_ohe)
```

##### 9. Проведите анализ данных полученных в задании 5 методом PCA. Кратко проинтерпретируйте полученные результаты.

```{r}
# избавимся от одного из признаков в парах sex_male/sex women, smoker_no/smoker_yes
# (коэффициент корреляциии для этих пар признаков равен 1 (возникновение dummy trap))
# то же возможно сделать и с одним из регионов - эту колонку вполне можно восстановить из
# оставшихся трёх регионов, однако сначала ознакомимся с тем, как каждый регион влияет на 
# главную компоненту
data_ohe <- subset(data_ohe, select = -c(sex_female, smoker_no))
corrplot(cor(data_ohe), method = 'number')
```

```{r}
data.pca <- prcomp(data_ohe, 
                   scale = T)
summary(data.pca)
fviz_eig(data.pca, 
         addlabels = T, 
         ylim = c(0, 40))

fviz_pca_var(data.pca, col.var = "contrib")

fviz_contrib(data.pca, choice = "var", axes = 1)  
fviz_contrib(data.pca, choice = "var", axes = 2)
```

**Кумулятивный объем объясненной дисперсии** к PC3 \~ 47,5%, к PC5 \~ 71,5%.

**Анализ переменных:** Для графика *Variables-PCA* по оси *x* объяснение вариации 19.4%, по оси *у* - 14.8% наблюдений. В сумме 34,2%, что не очень много - PCA только лишь с двумя главными компонентами показывает невысокую эффективность.

Согласно расстоянию от 0 до периметра круга, наиболее выражен эффект переменных smoker_yes (+ smoker_no), charges, а также двух регионов region_southeast, region_northwest, и в меньшей степени bmi (то же можно наблюдать на графиках *Contribution of variables to Dim-1* и *Contribution of variables to Dim-2*).

В некоторой степени с главной компонентой PC1 скоррелированы следующие из "влиятельных" переменных:

-   region_northwest (отрицательная корреляция),
-   bmi (положительная корреляция),
-   region_southeast (положительная корреляция).

В некоторой степени с главной компонентой PC2 скоррелированы следующие из "влиятельных" переменных:

-   smoker_yes (положительная корреляция),
-   charges (положительная корреляция).

```{r}
ggbiplot(data.pca, 
         scale=0, alpha = 0.1)

ggbiplot(data.pca, 
         scale=0, 
         groups = as.factor(data$smoker), 
         ellipse = T,
         alpha = 0.2)
```

Алгоритм чётко разделил два класса: курящие и некурящие.

Визуально облако точек выраженно изменяется в направлении переменных charges, smoker_yes(smoker_no пойдет в обратном направлении), а также в противоположных сторонах полигона точек находятся жители регионов north_west, northeast по сравнению с southeast. Переменная BMI изменяется примерно в том же направлении. Region_southwest стоит особняком - можно представить что там живут более молодые *(скорее всего) женщины*, несклонные к курению, траты на страховку которых невелики :)

##### 10. В финале вы получили график PCA по наблюдениям и переменным. Сделайте кластеризацию данных на нём по возрастным группам.

```{r}
data$age_group <- case_when(data$age < 35 ~ 'age: 21-34', 
                            data$age > 34 & data$age < 50  ~ 'age: 35-49', 
                            data$age > 49 ~ 'age: 50+')
ggbiplot(data.pca, 
         scale=0, 
         groups = as.factor(data$age_group), 
         ellipse = T,
         alpha = 0.2)
```

##### 11. Подумайте и создайте ещё две номинативные переменные, которые бы гипотетически могли хорошо разбить данные на кластеры. Сделайте две соответствующие визуализации.

Переменная №1: Риск (в зависимости от трат на страховку)

```{r}
data$risk_group <- case_when(data$charges <= 5000 ~ 'low', 
                             data$charges > 5000 & data$charges <= 20000 ~ 'medium',
                             data$charges > 15000 ~ 'high')

ggbiplot(data.pca, 
         scale=0, 
         groups = as.factor(data$risk_group), 
         ellipse = T,
         alpha = 0.2)
```

Переменная №2: ИМТ (в пределах нормы-за пределами нормы).

```{r}
data$bmi_group <- case_when(data$bmi < 18 | data$bmi > 25 ~ 'not normal', 
                            data$bmi >= 18 & data$bmi <= 25 ~ 'normal')

ggbiplot(data.pca, 
         scale=0, 
         groups = as.factor(data$bmi_group), 
         ellipse = T,
         alpha = 0.2)
```

##### 12. Попробуйте самостоятельно поизменять дафрейм -- удалить какие-либо переменные или создать их (создавайте только дамми переменные). Ваша задача -- резко поднять качество вашего анализа PCA (при этом, фактически, оперируя всё теми же данными). Кратко опишите, почему добавление той или иной дамми-переменной так улучшает PCA.

Вносимые изменения: оставим переменную smoker_no, удалим какой-либо регион и колонку children.

```{r}
data_sample <- data_ohe %>% subset(select = -c(region_southwest, children))

data.pca <- prcomp(data_sample, 
                   scale = T) 
summary(data.pca)

fviz_eig(data.pca, 
         addlabels = T, 
         ylim = c(0, 40))
fviz_pca_var(data.pca, col.var = "contrib")

fviz_contrib(data.pca, choice = "var", axes = 1) 
fviz_contrib(data.pca, choice = "var", axes = 2) 
```

Резко увеличилось качество РСА: **кумулятивный объем объясненной дисперсии** к PC3 \~ 59,1%, к PC5 \~ 84,8%. Бинарные переменные сами по себе являютя некоторыми "кластерами", что облегчает алгоритму поиск кластеров. Кроме того, оставляя две дамми-переменные, созданные по одному столбцу, мы заставляем алгоритм "учитывать" этот признак дважды - то есть дополнительно добавляем в PCA то, от чего избавлялись - сильно коррелирующие переменные.
