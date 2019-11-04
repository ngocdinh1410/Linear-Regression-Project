Use reg_model_project.Rmd to read the codes for this project. The project was done through Rstudio. 
The data were obtained from IMDB and Rotten Tomatoes. The data represents 651 randomly sampled movies produced and released before 2016. There are 32 variables about the movies.

The raw data is not a complete list of all movies released prior to 2016. It is a random sample taken from the full data set. We don’t know the sampling method. With random sampling, the results are generalizable to all movies in the range of years released between 1970 and 2014.In observational studies, only associations are shown. Association does not imply causation.

A possible non-independent bias may arise with regard to movie sequels whereby the popularity of a sequel movie may be influenced by that of the previous release.

What we seek to explore with this dataset:
Can we predict a movie’s popularity based on type of movie, genre, runtime, imdb rating, imdb number of votes, critics rating, critics score, audience rating, Oscar awards obtained (actor, actress, director and picture)? We hope to find a model with good predicting power to predict the IMDB rate of a movie. 

---
title: "Modeling and prediction for movies"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
library(statsr)
library(corrplot)
library(GGally)
```

### Load data

```{r load-data}
load("movies.Rdata")
```



* * *

## Part 1: Data

The data were obtained from IMDB and Rotten Tomatoes. The data represents 651 randomly sampled movies produced and released before 2016. There are 32 variables about the movies.

The raw data is not a complete list of all movies released prior to 2016. It is a random sample taken from the full data set. We don’t know the sampling method. With random sampling, the results are generalizable to all movies in the range of years released between 1970 and 2014.In observational studies, only associations are shown. Association does not imply causation.

A possible non-independent bias may arise with regard to movie sequels whereby the popularity of a sequel movie may be influenced by that of the previous release.
* * *

## Part 2: Research question

Can we predict a movie’s popularity based on type of movie, genre, runtime, imdb rating, imdb number of votes, critics rating, critics score, audience rating, Oscar awards obtained (actor, actress, director and picture)?

* * *

## Part 3: Exploratory data analysis


```{r}
str(movies)
```
We can see that 9 columns consisted of character variable 12 columns consisted of factor variables 11 columns consisted of numeric/ interger variables, 6 of which are date-related. We check for missing values * * *


* * *
```{r}
Check<-complete.cases(movies)
dataset<-movies[Check,]
dim(dataset)
```
‘Studio’ has 211 levels, thus it is too granular for the regression model. we will remove it. Column that has to do with actors, directors and actresses and imdb website will be removed (column 25-32)



```{r}
dataset<-dataset[c(1:5,7:9,13,16,18,14:15,17,19:24)]  
str(dataset)
```


```{r}
ggplot(data=dataset, aes(x=genre)) + 
      geom_bar(fill="green") + 
      xlab("Genre Distribution") +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
```
```{r}
ggplot(data=dataset, aes(x=title_type)) + 
      geom_bar(fill="blue") + 
      xlab("Movie Type") +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
```
```{r}
ggplot(data=dataset, aes(x=mpaa_rating)) + 
      geom_bar(fill="yellow") + 
      xlab("MPAA Rating") +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
```
```{r}
ggplot(data=dataset, aes(x=runtime)) + 
      geom_histogram(binwidth=10, fill="black") +
      xlab("Runtime")
```

* * *
## Part 4: Modeling
First, we need to find if there is any collinearity, especially with the numerical explanatory variables. A subset consisted of all numerical explanatory variables was created and the correlation matrix was created to better identify if there is any collinearity.
```{r}
num_expl_var <- dataset[c(4,6:8,10:12)] 
corr<- cor(num_expl_var)
cex.before <- par("cex")
par(cex = 0.55)
col<- colorRampPalette(c("dark red","red","pink", "yellow","light green", "dark green"))(20)
corrplot(corr, method="circle", type="lower", col=col, sig.level = 0.01, tl.col="black")
```
critic score and audience score are highly correlated. This could distort our regression model. To remove one of the two, I plotted the correlation between these two explanatory variables with the response variable.

```{r}
ggpairs(dataset,columns=9:12)
```
higher correlation with the response variables. Thus it is chosen over critic score.


```{r}
model <- dataset[c(2:9,11:20)]
first_model <-lm(imdb_rating~., data=model)
summary(first_model)
```
```{r}
anova(first_model)
```
We start with a relatively high adjusted r squared of 0.7992. We work our way through the set by removing variables with the highest p-value first. So, we will remove 
* * *
```{r}
second_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_month+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating+best_pic_nom+best_actor_win+
                  best_actress_win+best_dir_win, data=model)
summary(second_model)
```
```{r}
anova(second_model)
```
Our adjusted r squared improved slightly from 0.7992 to 0.7997. We wil continue to remove insignificant variable. The variable we will remove this time is the best_actor_win variable.
```{r}
third_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_month+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating+best_pic_nom+
                  best_actress_win+best_dir_win, data=model)
summary(third_model)
```

```{r}
anova(third_model)
```
adjusted r squared increased to 0.8. This time we will remove best_dir_win

```{r}
fourth_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_month+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating+best_pic_nom+
                  best_actress_win, data=model)
summary(fourth_model)
```

```{r}
anova(fourth_model)
```
Adjusted R squared slightly improved to 0.8002. We will remove best_pic_nom this time.



```{r}
fifth_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_month+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating+
                  best_actress_win, data=model)
summary(fifth_model)
```
```{r}
anova(fifth_model)
```
For the sixth model, we will remove best_actress_win. I also started to notice a pattern. We have been removing variables that have to do with oscar prizes ( best actress, best picture nominated, best director…)


```{r}
sixth_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_month+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating, data=model)
summary(sixth_model)
```
```{r}
anova(sixth_model)
```
The R squared decreased slightly. However, the decrease is insignificant. There are still variables with high p-value in the model. There are still variables that are considered insignificant such as thtr_rel_year, thtr_rel_month, thtr_rel_day and mpaa_rating. I will remove them one-by-one to see if the model improves.
```{r}
seventh_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_year+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating, data=model)
summary(seventh_model)
```
```{r}
anova(seventh_model)
```
```{r}
eighth_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+thtr_rel_day+
                  audience_score+imdb_num_votes+critics_rating+audience_rating, data=model)
summary(eighth_model)
```
```{r}
anova(eighth_model)
```
```{r}
ninth_model <-lm(imdb_rating~title_type+genre+runtime+mpaa_rating+audience_score+imdb_num_votes+critics_rating+audience_rating, data=model)
summary(ninth_model)
```
```{r}
anova(ninth_model)
```
```{r}
tenth_model <-lm(imdb_rating~title_type+genre+runtime+audience_score+imdb_num_votes+critics_rating+audience_rating, data=model)
summary(tenth_model)
```
```{r}
anova(tenth_model)
```
After removing all the insignificant variables with high p-value, the r suqared for the 10th model is 0.8007, the highest. Our tenth model is the final model.

Now we need to see if the model fits the following conditions:

the residuals are scattered randomly:


```{r}
par(mfrow = c(1, 3))
plot(tenth_model$residuals~dataset$runtime, ylab="Residuals", xlab="RunTime", main="Residuals vs RunTime")
plot(tenth_model$residuals~dataset$audience_score, ylab="Residuals", xlab="Audience_Score", main="Residuals vs Audience_Score")
plot(tenth_model$residuals~dataset$imdb_num_votes, ylab="Residuals", xlab="IMDB Num Votes", main="Residuals vs IMDB Votes")
```
distributed:


```{r}
par(mfrow = c(1, 2))
hist(tenth_model$residuals, col="blue", main="Histogram-Model Residuals")
qqnorm(tenth_model$residuals)
qqline(tenth_model$residuals)
```
tail but not significant.

the residuals display constant variability

```{r}
par(mfrow = c(1, 2))
plot(tenth_model$residuals~tenth_model$fitted.values, main="Residuals vs Fitted")
plot(abs(tenth_model$residuals)~tenth_model$fitted.values, main="Absolute Residuals vs Fitted")

```
the residuals are independent

```{r}
par(mfrow = c(1, 1))
plot(tenth_model$residuals, main="Residuals Plot")

```
there is no trend overtime for the residuals. * * *
## Part 5: Prediction
We can use it to predict the rating of Deadpool, a movie released in 2016 but was not in the sample.

```{r}
df_deadpool<- data.frame(title_type="Feature Film",genre="Action & Adventure",runtime=98,audience_score=90,imdb_num_votes=764199,critics_rating="Certified Fresh",audience_rating="Upright")
predict(tenth_model, df_deadpool, interval="prediction")

```
The predicted value of 8.1 is very close to the actual imdb of 8.0. With a 95% confidence interval, the lower bound is 7.12 and upper bound is 9.09.

We will now use it to predict the rating of Inside out
```{r}
df_Insideout<- data.frame(title_type="Feature Film",genre="Animation",runtime=94,audience_score=89,imdb_num_votes=499323,critics_rating="Certified Fresh",audience_rating="Upright")
predict(tenth_model, df_Insideout, interval="prediction")
```
The prediction power for inside out was not as strong as that for Deadpool. The actual rating is 8.2 while the predicted value is 7.4.
## Part 6: Conclusion
The model has good predictive power that can be used to predict the imdb rating for a particular movie.

