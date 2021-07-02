install.packages("plotly")
library(ggplot2)
library(plotly)
library(lubridate)
library(forecast)
library(data.table)

data1 = read.csv("C:/Users/rtbol/Desktop/ProjectRawData.csv", header = TRUE)

head(data1)
head(data)
datanew <- data1[,c(3,1,2,4,5,7,6,8,12,9,13,10,11)]
head(datanew)
head(data)
class(datanew$event_date)
datanew$event_date <- as.Date(datanew$event_date)

data = data[order(data$event_date),]
data = tail(data, -270)


head(datanew)
head(datanew,50)
tail(datanew,5)

datanew = datanew[order(datanew$event_date),]
head(datanew)
tail(datanew,50)
datanew = rbind(datanew, data)

set.seed(0)
split_list = split(datanew, datanew$product_content_id)

data_yüztemizleyici = split_list[[9]]
data_ýslakmendil = split_list[[4]] 
data_kulaklýk = split_list[[6]]
data_süpürge = split_list[[7]]
data_tayt = split_list[[1]]
data_bikini2 = split_list[[8]]
data_fýrça = split_list[[3]]
data_mont = split_list[[5]]
data_bikini1 = split_list[[2]]

## YÜZ TEMÝZLEYÝCÝ

ggplotly(ggplot(data_yüztemizleyici, aes(x=event_date,y=sold_count)) + geom_line())
data_yüztemizleyici = data.table(data_yüztemizleyici)

data_yüztemizleyici = tail(data_yüztemizleyici, -250)


data_yüztemizleyici[event_date=="2020-08-18" | event_date=="2020-08-19" | event_date=="2020-08-20", is_discount:= 1]
data_yüztemizleyici[event_date=="2020-09-08" | event_date=="2020-09-09" | event_date=="2020-09-10", is_discount:= 1]
data_yüztemizleyici[event_date=="2020-10-06" | event_date=="2020-10-07" | event_date=="2020-10-08", is_discount:= 1]
data_yüztemizleyici[event_date=="2020-11-09" | event_date=="2020-11-10" | event_date=="2020-11-11" | event_date=="2020-11-12", is_discount:= 1]
data_yüztemizleyici[event_date=="2020-11-25" | event_date=="2020-11-26" | event_date=="2020-11-27", is_discount:= 1]
data_yüztemizleyici[event_date=="2020-12-21" | event_date=="2020-11-22" | event_date=="2020-11-23", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-01-12" | event_date=="2021-01-13" | event_date=="2021-01-14", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-02-02" | event_date=="2021-02-03" | event_date=="2021-02-04", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_yüztemizleyici[event_date=="2021-05-13" | event_date=="2021-05-14" | event_date=="2021-05-15", is_discount:= 1]
data_yüztemizleyici[is.na(is_discount)==T,is_discount:=0]


temizleyici_reg_discount = lm(sold_count~price+visit_count+basket_count+category_sold+category_visits+category_brand_sold+category_favored+as.factor(is_discount), data_yüztemizleyici)
summary(temizleyici_reg_discount)
checkresiduals(temizleyici_reg_discount)

temizleyici_reg_discount2 = lm(sold_count~price+visit_count+basket_count+category_sold+category_visits+category_favored+as.factor(is_discount), data_yüztemizleyici)
summary(temizleyici_reg_discount2)
checkresiduals(temizleyici_reg_discount2)


temizleyici_reg_discount3 = lm(sold_count~price+basket_count+as.factor(is_discount), data_yüztemizleyici)
summary(temizleyici_reg_discount3)
checkresiduals(temizleyici_reg_discount3)


predictions1 = predict(temizleyici_reg_discount3,data_yüztemizleyici)
data_yüztemizleyici = data_yüztemizleyici[, predictions:=predictions1]
ggplotly(ggplot(data_yüztemizleyici ,aes(x=event_date)) +
  geom_line(aes(y=sold_count,color='real')) +
  geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_yüztemizleyici_price = mean(tail(data_yüztemizleyici$price,5))
nextday_yüztemizleyici_basket = mean(tail(data_yüztemizleyici$basket_count,5))

data_yüztemizleyici=rbind(data_yüztemizleyici,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_yüztemizleyici[event_date=="2021-06-24", is_discount:=0]
data_yüztemizleyici[event_date=="2021-06-24", price := nextday_yüztemizleyici_price]
data_yüztemizleyici[event_date=="2021-06-24", basket_count:= nextday_yüztemizleyici_basket]

predictions1 = predict(temizleyici_reg_discount3,data_yüztemizleyici)
data_yüztemizleyici = data_yüztemizleyici[, predictions:=predictions1]
ggplotly(ggplot(data_yüztemizleyici ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))
predictions1


## ISLAK MENDÝL 

ggplotly(ggplot(data_ýslakmendil, aes(x=event_date,y=sold_count)) + geom_line())
data_ýslakmendil = data.table(data_ýslakmendil)

data_ýslakmendil = tail(data_ýslakmendil, -250)

data_ýslakmendil[event_date=="2020-08-18" | event_date=="2020-08-19" | event_date=="2020-08-20", is_discount:= 1]
data_ýslakmendil[event_date=="2020-09-08" | event_date=="2020-09-09" | event_date=="2020-09-10", is_discount:= 1]
data_ýslakmendil[event_date=="2020-10-06" | event_date=="2020-10-07" | event_date=="2020-10-08", is_discount:= 1]
data_ýslakmendil[event_date=="2020-11-09" | event_date=="2020-11-10" | event_date=="2020-11-11" | event_date=="2020-11-12", is_discount:= 1]
data_ýslakmendil[event_date=="2020-11-25" | event_date=="2020-11-26" | event_date=="2020-11-27", is_discount:= 1]
data_ýslakmendil[event_date=="2020-12-21" | event_date=="2020-11-22" | event_date=="2020-11-23", is_discount:= 1]
data_ýslakmendil[event_date=="2021-02-02" | event_date=="2021-02-03" | event_date=="2021-02-04", is_discount:= 1]
data_ýslakmendil[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_ýslakmendil[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_ýslakmendil[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_ýslakmendil[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_ýslakmendil[is.na(is_discount)==T,is_discount:=0]

ýslakmendil_lm = lm(sold_count~.,data_ýslakmendil)
summary(ýslakmendil_lm)

ýslakmendil_lm2 = lm(sold_count~price+favored_count+basket_count+category_sold+category_visits+category_favored+as.factor(is_discount), data_ýslakmendil)
summary(ýslakmendil_lm2)

ýslakmendil_lm3 = lm(sold_count~basket_count+category_sold+category_visits+category_favored+as.factor(is_discount), data_ýslakmendil)
summary(ýslakmendil_lm3)
checkresiduals(ýslakmendil_lm3)

predictions2 = predict(ýslakmendil_lm3,data_ýslakmendil)
data_ýslakmendil = data_ýslakmendil[, predictions:=predictions2]
ggplotly(ggplot(data_ýslakmendil ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_ýslakmendil_basket = mean(tail(data_ýslakmendil$basket_count,5))
nextday_ýslakmendil_catsold = mean(tail(data_ýslakmendil$category_sold,5))
nextday_ýslakmendil_catvis = mean(tail(data_ýslakmendil$category_visits,5))
nextday_ýslakmendil_catfav = mean(tail(data_ýslakmendil$category_favored,5))


data_ýslakmendil=rbind(data_ýslakmendil,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_ýslakmendil[event_date=="2021-06-24", is_discount:=0]
data_ýslakmendil[event_date=="2021-06-24", basket_count := nextday_ýslakmendil_basket]
data_ýslakmendil[event_date=="2021-06-24", category_sold := nextday_ýslakmendil_catsold]
data_ýslakmendil[event_date=="2021-06-24", category_visits := nextday_ýslakmendil_catvis]
data_ýslakmendil[event_date=="2021-06-24", category_favored := nextday_ýslakmendil_catfav]

predictions2 = predict(ýslakmendil_lm3,data_ýslakmendil)
data_ýslakmendil = data_ýslakmendil[, predictions:=predictions2]
ggplotly(ggplot(data_ýslakmendil ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))


## KULAKLIK

ggplotly(ggplot(data_kulaklýk, aes(x=event_date,y=sold_count)) + geom_line())
data_kulaklýk = data.table(data_kulaklýk)

data_kulaklýk = tail(data_kulaklýk, -250)

data_kulaklýk[event_date=="2020-08-18" | event_date=="2020-08-19" | event_date=="2020-08-20", is_discount:= 1]
data_kulaklýk[event_date=="2020-09-08" | event_date=="2020-09-09" | event_date=="2020-09-10", is_discount:= 1]
data_kulaklýk[event_date=="2020-10-06" | event_date=="2020-10-07" | event_date=="2020-10-08", is_discount:= 1]
data_kulaklýk[event_date=="2020-11-09" | event_date=="2020-11-10" | event_date=="2020-11-11" | event_date=="2020-11-12", is_discount:= 1]
data_kulaklýk[event_date=="2020-11-25" | event_date=="2020-11-26" | event_date=="2020-11-27", is_discount:= 1]
data_kulaklýk[event_date=="2020-12-21" | event_date=="2020-11-22" | event_date=="2020-11-23", is_discount:= 1]
data_kulaklýk[event_date=="2021-01-12" | event_date=="2021-01-13" | event_date=="2021-01-14", is_discount:= 1]
data_kulaklýk[event_date=="2021-02-02" | event_date=="2021-02-03" | event_date=="2021-02-04", is_discount:= 1]
data_kulaklýk[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_kulaklýk[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_kulaklýk[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_kulaklýk[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_kulaklýk[event_date=="2021-05-13" | event_date=="2021-05-14" | event_date=="2021-05-15", is_discount:= 1]
data_kulaklýk[is.na(is_discount)==T,is_discount:=0]

kulaklýk_lm = lm(sold_count~., data_kulaklýk)
summary(kulaklýk_lm)

kulaklýk_lm2 = lm(sold_count~price+visit_count+basket_count+category_sold+category_visits+as.factor(is_discount), data_kulaklýk)
summary(kulaklýk_lm2)

kulaklýk_lm3 = lm(sold_count~visit_count+basket_count+category_sold+category_visits+as.factor(is_discount), data_kulaklýk)
summary(kulaklýk_lm3)

kulaklýk_nointercept = lm(sold_count~-1+visit_count+basket_count+category_sold+category_visits+as.factor(is_discount), data_kulaklýk)
summary(kulaklýk_nointercept)
checkresiduals(kulaklýk_nointercept)

predictions3 = predict(kulaklýk_nointercept,data_kulaklýk)
data_kulaklýk = data_kulaklýk[, predictions:=predictions3]
ggplotly(ggplot(data_kulaklýk ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_kulaklýk_visit = mean(tail(data_kulaklýk$visit_count,5))
nextday_kulaklýk_basket = mean(tail(data_kulaklýk$basket_count,5))
nextday_kulaklýk_catsold = mean(tail(data_kulaklýk$category_sold,5))
nextday_kulaklýk_catvis = mean(tail(data_kulaklýk$category_visits,5))

data_kulaklýk=rbind(data_kulaklýk,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_kulaklýk[event_date=="2021-06-24", is_discount:=0]
data_kulaklýk[event_date=="2021-06-24", visit_count := nextday_kulaklýk_visit]
data_kulaklýk[event_date=="2021-06-24", basket_count := nextday_kulaklýk_basket]
data_kulaklýk[event_date=="2021-06-24", category_sold := nextday_kulaklýk_catsold]
data_kulaklýk[event_date=="2021-06-24", category_visits := nextday_kulaklýk_catvis]

predictions3 = predict(kulaklýk_nointercept,data_kulaklýk)
data_kulaklýk = data_kulaklýk[, predictions:=predictions3]
ggplotly(ggplot(data_kulaklýk ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))


## SÜPÜRGE

ggplotly(ggplot(data_süpürge, aes(x=event_date,y=sold_count)) + geom_line())
data_süpürge = data.table(data_süpürge)

data_süpürge = tail(data_süpürge, -250)

süpürge_lm = lm(sold_count~., data_süpürge)
summary(süpürge_lm)

süpürge_lm2 = lm(sold_count~favored_count+basket_count+category_sold+category_visits+ty_visits, data_süpürge)
summary(süpürge_lm2)
checkresiduals(süpürge_lm2)

süpürge_lm3 = lm(sold_count~-1+favored_count+basket_count+category_sold+category_visits+ty_visits, data_süpürge)
summary(süpürge_lm3)
checkresiduals(süpürge_lm3)

predictions4 = predict(süpürge_lm3,data_süpürge)
data_süpürge = data_süpürge[, predictions:=predictions4]
ggplotly(ggplot(data_süpürge ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions 

nextday_süpürge_favcount = mean(tail(data_süpürge$favored_count,5))
nextday_süpürge_basket = mean(tail(data_süpürge$basket_count,5))
nextday_süpürge_catsold = mean(tail(data_süpürge$category_sold,5))
nextday_süpürge_catvis = mean(tail(data_süpürge$category_visits,5))
nextday_süpürge_tyvis = mean(tail(data_süpürge$ty_visits,5))

data_süpürge=rbind(data_süpürge,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_süpürge[event_date=="2021-06-24", favored_count := nextday_süpürge_favcount]
data_süpürge[event_date=="2021-06-24", basket_count := nextday_süpürge_basket]
data_süpürge[event_date=="2021-06-24", category_sold := nextday_süpürge_catsold]
data_süpürge[event_date=="2021-06-24", category_visits := nextday_süpürge_catvis]
data_süpürge[event_date=="2021-06-24", ty_visits := nextday_süpürge_tyvis]

predictions4 = predict(süpürge_lm3,data_süpürge)
data_süpürge = data_süpürge[, predictions:=predictions4]
ggplotly(ggplot(data_süpürge ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## TAYT

ggplotly(ggplot(data_tayt, aes(x=event_date,y=sold_count)) + geom_line())
data_tayt = data.table(data_tayt)

data_tayt = tail(data_tayt, -250)

data_tayt[event_date=="2021-02-02" | event_date=="2021-02-03" | event_date=="2021-02-04", is_discount:= 1]
data_tayt[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_tayt[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_tayt[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_tayt[is.na(is_discount)==T,is_discount:=0]

tayt_lm = lm(sold_count~.,data_tayt)
summary(tayt_lm)

tayt_lm2 = lm(sold_count~price+visit_count+basket_count+category_sold+category_visits+as.factor(is_discount), data_tayt)
summary(tayt_lm2)
checkresiduals(tayt_lm2)

predictions5 = predict(tayt_lm2,data_tayt)
data_tayt = data_tayt[, predictions:=predictions5]
ggplotly(ggplot(data_tayt ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_tayt_price = mean(tail(data_tayt$price,5))
nextday_tayt_viscount = mean(tail(data_tayt$visit_count,5))
nextday_tayt_basket = mean(tail(data_tayt$basket_count,5))
nextday_tayt_catsold = mean(tail(data_tayt$category_sold,5))
nextday_tayt_catvis = mean(tail(data_tayt$category_visits,5))

data_tayt=rbind(data_tayt,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_tayt[event_date=="2021-06-24", is_discount:=1]
data_tayt[event_date=="2021-06-24", price := nextday_tayt_price]
data_tayt[event_date=="2021-06-24", visit_count := nextday_tayt_viscount]
data_tayt[event_date=="2021-06-24", basket_count := nextday_tayt_basket]
data_tayt[event_date=="2021-06-24", category_sold := nextday_tayt_catsold]
data_tayt[event_date=="2021-06-24", category_visits := nextday_tayt_catvis]

predictions5 = predict(tayt_lm2,data_tayt)
data_tayt = data_tayt[, predictions:=predictions5]
ggplotly(ggplot(data_tayt ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## FIRÇA 

ggplotly(ggplot(data_fýrça, aes(x=event_date,y=sold_count)) + geom_line())
data_fýrça = data.table(data_fýrça)

data_fýrça = tail(data_fýrça, -250)

data_fýrça[event_date=="2020-08-18" | event_date=="2020-08-19" | event_date=="2020-08-20", is_discount:= 1]
data_fýrça[event_date=="2020-09-08" | event_date=="2020-09-09" | event_date=="2020-09-10", is_discount:= 1]
data_fýrça[event_date=="2020-10-06" | event_date=="2020-10-07" | event_date=="2020-10-08", is_discount:= 1]
data_fýrça[event_date=="2020-11-09" | event_date=="2020-11-10" | event_date=="2020-11-11" | event_date=="2020-11-12", is_discount:= 1]
data_fýrça[event_date=="2020-11-25" | event_date=="2020-11-26" | event_date=="2020-11-27", is_discount:= 1]
data_fýrça[event_date=="2020-12-21" | event_date=="2020-11-22" | event_date=="2020-11-23", is_discount:= 1]
data_fýrça[event_date=="2021-01-12" | event_date=="2021-01-13" | event_date=="2021-01-14", is_discount:= 1]
data_fýrça[event_date=="2021-02-02" | event_date=="2021-02-03" | event_date=="2021-02-04", is_discount:= 1]
data_fýrça[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_fýrça[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_fýrça[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_fýrça[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_fýrça[event_date=="2021-05-13" | event_date=="2021-05-14" | event_date=="2021-05-15", is_discount:= 1]
data_fýrça[is.na(is_discount)==T,is_discount:=0]

fýrça_lm = lm(sold_count~., data_fýrça)
summary(fýrça_lm)

fýrça_lm2 = lm(sold_count~price+visit_count+favored_count+basket_count+category_visits+ty_visits+as.factor(is_discount),data_fýrça)
summary(fýrça_lm2)

fýrça_lm3 = lm(sold_count~price+visit_count+favored_count+basket_count+ty_visits+as.factor(is_discount),data_fýrça)
summary(fýrça_lm3)
checkresiduals(fýrça_lm3)

predictions6 = predict(fýrça_lm3,data_fýrça)
data_fýrça = data_fýrça[, predictions:=predictions6]
ggplotly(ggplot(data_fýrça ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_fýrça_price = mean(tail(data_fýrça$price,5))
nextday_fýrça_viscount = mean(tail(data_fýrça$visit_count,5))
nextday_fýrça_favcount = mean(tail(data_fýrça$favored_count,5))
nextday_fýrça_basket = mean(tail(data_fýrça$basket_count,5))
nextday_fýrça_tyvis = mean(tail(data_fýrça$ty_visits,5))

data_fýrça=rbind(data_fýrça,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_fýrça[event_date=="2021-06-24", is_discount:=0]
data_fýrça[event_date=="2021-06-24", price := nextday_fýrça_price]
data_fýrça[event_date=="2021-06-24", visit_count := nextday_fýrça_viscount]
data_fýrça[event_date=="2021-06-24", favored_count := nextday_fýrça_favcount]
data_fýrça[event_date=="2021-06-24", basket_count := nextday_fýrça_basket]
data_fýrça[event_date=="2021-06-24", ty_visits := nextday_fýrça_tyvis]

predictions6 = predict(fýrça_lm3,data_fýrça)
data_fýrça = data_fýrça[, predictions:=predictions6]
ggplotly(ggplot(data_fýrça ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## BÝKÝNÝ ÜSTÜ 1 

ggplotly(ggplot(data_bikini1, aes(x=event_date,y=sold_count)) + geom_line())
data_bikini1 = data.table(data_bikini1)

data_bikini1 = tail(data_bikini1, -271)

data_bikini1[event_date=="2021-03-09" | event_date=="2021-03-10" | event_date=="2021-03-11", is_discount:= 1]
data_bikini1[event_date=="2021-04-05" | event_date=="2021-04-06" | event_date=="2021-04-07", is_discount:= 1]
data_bikini1[event_date=="2021-04-27" | event_date=="2021-04-28" | event_date=="2021-04-29", is_discount:= 1]
data_bikini1[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_bikini1[event_date=="2021-05-13" | event_date=="2021-05-14" | event_date=="2021-05-15", is_discount:= 1]
data_bikini1[is.na(is_discount)==T,is_discount:=0]

bikini1_lm = lm(sold_count~., data_bikini1)
summary(bikini1_lm)

bikini1_lm2 = lm(sold_count~visit_count+basket_count+category_sold+as.factor(is_discount), data_bikini1)
summary(bikini1_lm2)

bikini1_lm3 = lm(sold_count~-1+basket_count+category_sold+as.factor(is_discount),data_bikini1)
summary(bikini1_lm3)
checkresiduals(bikini1_lm3)

predictions7 = predict(bikini1_lm3,data_bikini1)
data_bikini1 = data_bikini1[, predictions:=predictions7]
ggplotly(ggplot(data_bikini1 ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions 

nextday_bikini1_basket = mean(tail(data_bikini1$basket_count,5))
nextday_bikini1_catsold = mean(tail(data_bikini1$category_sold,5))

data_bikini1=rbind(data_bikini1,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_bikini1[event_date=="2021-06-24", is_discount:=0]
data_bikini1[event_date=="2021-06-24", basket_count := nextday_bikini1_basket]
data_bikini1[event_date=="2021-06-24", category_sold := nextday_bikini1_catsold]

predictions7 = predict(bikini1_lm3,data_bikini1)
data_bikini1 = data_bikini1[, predictions:=predictions7]
ggplotly(ggplot(data_bikini1 ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## MONT 

ggplotly(ggplot(data_mont, aes(x=event_date,y=sold_count)) + geom_line())
data_mont = data.table(data_mont)

data_mont = tail(data_mont, -349)

data_mont = data_mont[is.na(price)==1, price:=299.990]

data_mont[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_mont[is.na(is_discount)==T,is_discount:=0]

mont_lm = lm(sold_count~.,data_mont)
summary(mont_lm)

mont_lm2 = lm(sold_count~favored_count+as.factor(is_discount),data_mont)
summary(mont_lm2)
checkresiduals(mont_lm2)

predictions8 = predict(mont_lm2,data_mont)
data_mont = data_mont[, predictions:=predictions8]
ggplotly(ggplot(data_mont ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## Daily Predictions

nextday_mont_favcount = mean(tail(data_mont$favored_count,5))

data_mont=rbind(data_mont,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_mont[event_date=="2021-06-24", is_discount:=0]
data_mont[event_date=="2021-06-24", favored_count := nextday_mont_favcount]

predictions8 = predict(mont_lm2,data_mont)
data_mont = data_mont[, predictions:=predictions8]
ggplotly(ggplot(data_mont ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

## BÝKÝNÝ ÜSTÜ 2

ggplotly(ggplot(data_bikini2, aes(x=event_date,y=sold_count)) + geom_line())
data_bikini2 = data.table(data_bikini2)

data_bikini2 = tail(data_bikini2, -338)

data_bikini2[event_date=="2021-05-07" | event_date=="2021-05-08" | event_date=="2021-05-09", is_discount:= 1]
data_bikini2[event_date=="2021-05-13" | event_date=="2021-05-14" | event_date=="2021-05-15", is_discount:= 1]
data_bikini2[is.na(is_discount)==T,is_discount:=0]

bikini2_lm = lm(sold_count~.,data_bikini2)
summary(bikini2_lm)

bikini2_lm2 = lm(sold_count~basket_count+category_sold+as.factor(is_discount), data_bikini2)
summary(bikini2_lm2)

bikini2_lm3 = lm(sold_count~basket_count+as.factor(is_discount), data_bikini2)
summary(bikini2_lm3)
checkresiduals(bikini2_lm3)

predictions9 = predict(bikini2_lm3,data_bikini2)
data_bikini2 = data_bikini2[, predictions:=predictions9]
ggplotly(ggplot(data_bikini2 ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))

# Daily Predictions

nextday_bikini2_basket = mean(tail(data_bikini2$basket_count,5))
nextday_bikini2_catsold = mean(tail(data_bikini2$category_sold,5))

data_bikini2=rbind(data_bikini2,data.table(event_date=as.Date("2021-06-24")),fill=T)
data_bikini2[event_date=="2021-06-24", is_discount:=0]
data_bikini2[event_date=="2021-06-24", basket_count := nextday_bikini2_basket]
data_bikini2[event_date=="2021-06-24", category_sold := nextday_bikini2_catsold]

predictions9 = predict(bikini2_lm3,data_bikini2)
data_bikini2 = data_bikini2[, predictions:=predictions9]
ggplotly(ggplot(data_bikini2 ,aes(x=event_date)) +
           geom_line(aes(y=sold_count,color='real')) +
           geom_line(aes(y=predictions,color='predictions')))
