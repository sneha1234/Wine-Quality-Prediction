library(ggplot2)
library(dplyr)
library(GGally)
library(scales)
library(memisc)
library(reshape)
library(gridExtra)
library(MASS)
library(ISLR)
df <- read.csv('C:/sneha/spring16/Data_Science/mini-project-wine-quality/wineQualityReds.csv')
str(df)
summary(df)

# let's at least explore, clean up, and format the first two points.
# X
df$X = factor(df$X)

# quality
summary(df$quality)
table(df$quality)

# assertion was correct here, so let's ensure the data frame semantically reflects that.
df$quality <- factor(df$quality,ordered = T)
str(df$quality)

# exploratory, quick histogram plots
grid.arrange(qplot(df$fixed.acidity,fill=I("#DB0A5B"), col=I("black")),
qplot(df$volatile.acidity,fill=I("#9A12B3"), col=I("black")),
qplot(df$citric.acid,fill=I("#22313F"), col=I("black")),
qplot(df$residual.sugar,fill=I("#87D37C"), col=I("black")),ncol=2)

grid.arrange(qplot(df$chlorides,fill=I("#03C9A9"), col=I("black")),
qplot(df$free.sulfur.dioxide,fill=I("#F89406"), col=I("black")),
qplot(df$total.sulfur.dioxide,fill=I("#6C7A89"), col=I("black")),
qplot(df$density,fill=I("#E9D460"), col=I("black")),ncol=2)
             
grid.arrange(qplot(df$pH,fill=I("#F22613"), col=I("black")),
qplot(df$sulphates,fill=I("#446CB3"), col=I("black")),
qplot(df$alcohol,fill=I("#019875"), col=I("black")),
qplot(df$quality,fill=I("#D35400"), col=I("black")),ncol=2)
             
ggplot(data = df,
       aes(x = fixed.acidity)) +
  geom_histogram(fill=I("#DB0A5B"), col=I("black")) +
  scale_x_log10()
p1 <- ggplot(data = df, aes(x = residual.sugar)) +
  geom_histogram() +
  scale_x_continuous(lim = c(0, quantile(df$residual.sugar, 0.95))) +
  xlab('residual.sugar, 95th percentile truncated')
plot(p1)

df$TAC.acidity <- df$fixed.acidity + df$volatile.acidity + df$citric.acid
qplot(df$TAC.acidity)

df$rating <- ifelse(df$quality < 5, 'bad', ifelse(
  df$quality < 7, 'average', 'good'))
df$rating <- ordered(df$rating,
                     levels = c('bad', 'average', 'good'))
summary(df$rating)

get_simple_boxplot <- function(column, ylab) {
  return(qplot(data = df, x = 'simple',
               y = column, geom = 'boxplot',
               xlab = '',
               ylab = ylab,fill=I("#F64747")))
}

grid.arrange(get_simple_boxplot(df$fixed.acidity, 'fixed acidity'),
             get_simple_boxplot(df$volatile.acidity, 'volatile acidity'),
             get_simple_boxplot(df$citric.acid, 'citric acid'),
             get_simple_boxplot(df$residual.sugar, 'residual sugar'),
             get_simple_boxplot(df$chlorides, 'chlorides'),
             get_simple_boxplot(df$free.sulfur.dioxide, 'free sulf. dioxide'),
             get_simple_boxplot(df$total.sulfur.dioxide, 'total sulf. dioxide'),
             get_simple_boxplot(df$density, 'density'),
             get_simple_boxplot(df$pH, 'pH'),
             get_simple_boxplot(df$sulphates, 'sulphates'),
             get_simple_boxplot(df$alcohol, 'alcohol'),
             get_simple_boxplot(df$quality, 'quality'),
             ncol = 4)
#bivariate
colr=c("##F64747","#DB0A5B","#F89406","#446CB3","#D35400")
get_bivariate_boxplot <- function(x, y, ylab) {
  return(qplot(data = df, x = x, y = y, geom = 'boxplot', xlab="rating",ylab = ylab,fill=I("#DB0A5B"), col=I("black")))
}

grid.arrange(get_bivariate_boxplot(df$quality, df$fixed.acidity,
                                   'fixed acidity'),
             get_bivariate_boxplot(df$quality, df$volatile.acidity,
                                   'volatile acidity'),
             get_bivariate_boxplot(df$quality, df$citric.acid,
                                   'citric acid'),
             get_bivariate_boxplot(df$quality, log10(df$residual.sugar),
                                   'residual sugar'),
             get_bivariate_boxplot(df$quality, log10(df$chlorides),
                                   'chlorides'),
             get_bivariate_boxplot(df$quality, df$free.sulfur.dioxide,
                                   'free sulf. dioxide'),
             get_bivariate_boxplot(df$quality, df$total.sulfur.dioxide,
                                   'total sulf. dioxide'),
             get_bivariate_boxplot(df$quality, df$density,
                                   'density'),
             get_bivariate_boxplot(df$quality, df$pH,
                                   'pH'),
             get_bivariate_boxplot(df$quality, log10(df$sulphates),
                                   'sulphates'),
             get_bivariate_boxplot(df$quality, df$alcohol,
                                   'alcohol'),
             ncol = 4)

grid.arrange(get_bivariate_boxplot(df$rating, df$fixed.acidity,
                                   'fixed acidity'),
             get_bivariate_boxplot(df$rating, df$volatile.acidity,
                                   'volatile acidity'),
             get_bivariate_boxplot(df$rating, df$citric.acid,
                                   'citric acid'),
             get_bivariate_boxplot(df$rating, log10(df$residual.sugar),
                                   'residual sugar'),
             get_bivariate_boxplot(df$rating, log10(df$chlorides),
                                   'chlorides'),
             get_bivariate_boxplot(df$rating, df$free.sulfur.dioxide,
                                   'free sulf. dioxide'),
             get_bivariate_boxplot(df$rating, df$total.sulfur.dioxide,
                                   'total sulf. dioxide'),
             get_bivariate_boxplot(df$rating, df$density,
                                   'density'),
             get_bivariate_boxplot(df$rating, df$pH,
                                   'pH'),
             get_bivariate_boxplot(df$rating, log10(df$sulphates),
                                   'sulphates'),
             get_bivariate_boxplot(df$rating, df$alcohol,
                                   'alcohol'),
             ncol = 4)
###############correlation########
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(df$fixed.acidity, df$quality),
  simple_cor_test(df$volatile.acidity, df$quality),
  simple_cor_test(df$citric.acid, df$quality),
  simple_cor_test(df$residual.sugar, df$quality),
  simple_cor_test(df$chlorides, df$quality),
  simple_cor_test(df$free.sulfur.dioxide, df$quality),
  simple_cor_test(df$total.sulfur.dioxide, df$quality),
  simple_cor_test(df$density, df$quality),
  simple_cor_test(df$pH, df$quality),
  simple_cor_test(df$sulphates, df$quality),
  simple_cor_test(df$alcohol, df$quality))

names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid','residual.sugar',
                         'chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'sulphates', 'alcohol')
correlations


ggcorr(data = df[,2:13],  nbreaks = 4, palette = "RdGy", size=3.5,layout.exp = 1, hjust = 0.8,legend.position = "bottom", legend.size = 10, label = TRUE, label_alpha = 0.5, label_size = 3, label_color = "white")

ggplot(data = df, aes(x = log10(sulphates), y = alcohol)) +
  facet_wrap(~rating) +
  geom_point()

###effect of acidity

grid.arrange(ggplot(data = df, aes(x = quality, y = fixed.acidity,fill = quality)) + ylab('Fixed Acidity (g/dm^3)') +xlab('Quality') +geom_boxplot(),
ggplot(data = df, aes(x = quality, y = volatile.acidity,fill = quality)) +ylab('Volatile Acidity (g/dm^3)') +xlab('Quality') +geom_boxplot(), 
ggplot(data = df, aes(x = quality, y = citric.acid,fill = quality)) +ylab('Citric Acid (g/dm^3)') +xlab('Quality') +geom_boxplot(), 
ggplot(data = df, aes(x = quality, y = pH,fill = quality)) +ylab('pH') +xlab('Quality') +geom_boxplot())

write.csv(file = "C:/sneha/spring16/Data_Science/mini-project-wine-quality/new.csv",df)

ggplot(data = subset(df, rating != 'average'),
       aes(x = volatile.acidity, y = alcohol,
           color = rating)) +
  geom_point() +
  ggtitle('Alcohol vs. Volatile Acidity and Wine Quality') +
  xlab('citric Acidity (g / dm^3)') +
  ylab('Alcohol (% volume)')


#####SVM
library(e1071)
train.data=df[1:1400,2:14]
train.data<-train.data[,!((names(train.data) %in% c("quality","density")))]
test.data=df[1401:1599,2:12]
test.data<-test.data[,!((names(test.data) %in% c("density")))]
#tune.out = tune(svm,quality~., kernel = "linear",data =train.data,ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
tune.out = tune(svm,rating~., kernel = "linear",data =train.data,ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bmodel =tune.out$best.model
summary (bmodel)

svm.p<-predict(bmodel,test.data)
svm.predict<-data.frame(round(svm.p,0))
View(data.frame(svm.predict,df[1401:1599,14]))
svm.error=svm.predict$round.svm.p..0./ df[1401:1599,14]
mape_svm=mape(svm.error)
###regression
glm.fit<-glm(df$quality~df$alcohol+df$volatile.acidity+df$citric.acid,data=train.data)
glm.fit2<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+density,data=df[1:1300,])           
glm.output<-predict(glm.fit2,df[1301:1599,])
View(round(glm.output,0))
p<-data.frame(round(glm.output,0))
plot(round(df[1301:1599,13],0),(p$round.glm.output..0.))
View(data.frame(round(df[1301:1599,13],0),(p$round.glm.output..0.)))
###error
mape <- function(error)
{
  mean(abs(1-error)*100)
}
#error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error=p$round.glm.output..0./as.factor(df[1301:1599,13])
lm.error<-mape(error)
