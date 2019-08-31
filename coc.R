setwd('c:/Users/USER/Desktop/ICT-CoC Hackathon')
# 데이터 전처리
library(plyr);library(dplyr) ; library(magrittr); library(tidyr)
dir <- ('c:/Users/USER/Desktop/ICT-CoC Hackathon/data')
file_list <- list.files(dir)

data1 <- data.table::fread(file_list[1], encoding = 'UTF-8')
data2 <- data.table::fread(file_list[2], encoding = 'UTF-8')
data3 <- data.table::fread(file_list[3], encoding = 'UTF-8')
data4 <- data.table::fread(file_list[4], encoding = 'UTF-8')
data5 <- data.table::fread(file_list[5], encoding = 'UTF-8')
data6 <- data.table::fread(file_list[6], encoding = 'UTF-8')
data7 <- data.table::fread(file_list[7], encoding = 'UTF-8')
data8 <- data.table::fread(file_list[8], encoding = 'UTF-8')
data9 <- data.table::fread(file_list[9], encoding = 'UTF-8')

colnames(data1) <- gsub("_","",colnames(data1))
colnames(data2) <- gsub("_","",colnames(data2))
colnames(data3) <- gsub("_","",colnames(data3)) ; colnames(data3)[1] <- '기준년코드'
colnames(data4) <- gsub("_","",colnames(data4))
colnames(data5) <- gsub("_","",colnames(data5)) ; colnames(data5)[1] <- '기준년코드' ; colnames(data5)[3] <- '상권구분코드'
colnames(data6) <- gsub("_","",colnames(data6)) ; data6 <- data6[,c(1,26,2:25)]
colnames(data7) <- gsub("_","",colnames(data7))
colnames(data8) <- gsub("_","",colnames(data8)) ;colnames(data8) <- gsub(" ","",colnames(data8))
colnames(data9) <- gsub("_","",colnames(data9)) ; colnames(data9) <- gsub(" ","",colnames(data9))

data1$상권코드명 <- gsub("_","",data1$상권코드명) ; data1$상권코드명 <- gsub(" ","",data1$상권코드명)
data2$상권코드명 <- gsub("_","",data2$상권코드명) ; data2$상권코드명 <- gsub(" ","",data2$상권코드명)
data3$상권코드명 <- gsub("_","",data3$상권코드명) ; data3$상권코드명 <- gsub(" ","",data3$상권코드명)
data4$상권코드명 <- gsub("_","",data4$상권코드명) ; data4$상권코드명 <- gsub(" ","",data4$상권코드명)
data5$상권코드명 <- gsub("_","",data5$상권코드명) ; data5$상권코드명 <- gsub(" ","",data5$상권코드명)
data6$상권코드명 <- gsub("_","",data6$상권코드명) ; data6$상권코드명 <- gsub(" ","",data6$상권코드명)
data7$상권코드명 <- gsub("_","",data7$상권코드명) ; data7$상권코드명 <- gsub(" ","",data7$상권코드명)
data8$상권코드명 <- gsub("_","",data8$상권코드명) ; data8$상권코드명 <- gsub(" ","",data8$상권코드명)
data9$상권코드명 <- gsub("_","",data9$상권코드명) ; data9$상권코드명 <- gsub(" ","",data9$상권코드명)

sum(is.na(data3));sum(is.na(data6))
data2%<>%mutate(기준년월코드=기준년월코드*0.01)%>%separate(col=기준년월코드,into=c('기준년코드','기준분기코드'))
data2%<>%mutate(기준분기코드=revalue(기준분기코드,c('01'='1','02'='1','03'='1','04'='2','05'='2','06'='2','07'='3','08'='3','09'='3','1'='4','11'='4','12'='4')))
data2%<>%mutate(기준분기코드=as.numeric(기준분기코드))
data6[is.na(data6),] <- 0
data1%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data2%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data3%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019) #아파트 쓰레기인듯?
data4%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data5%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data6%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data7%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data8%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)
data9%<>%filter(기준년코드==2017|기준년코드==2018|기준년코드==2019)

sum(is.na(data))
data <- left_join(data7,data8,by=c('기준년코드','기준분기코드','상권코드명'))
data <- left_join(data,data1,by=c('기준년코드','기준분기코드','상권코드명'))
data <- left_join(data,data4,by=c('기준년코드','기준분기코드','상권코드명','서비스업종코드명'))
data <- left_join(data,data6,by=c('기준년코드','기준분기코드','상권코드명'))
data <- left_join(data,data5,by=c('기준년코드','기준분기코드','상권코드명'))
data <- left_join(data,data9,by=c('기준년코드','기준분기코드','상권코드명'))
data <- na.omit(data)
data%<>%select(-ends_with('x'),-ends_with('y'))
data7%<>%select('기준년코드','기준분기코드','상권코드명','서비스업종코드명','점포수')
data <- left_join(data,data7,by=c('기준년코드','기준분기코드','상권코드명','서비스업종코드명'))
colnames(data)
data%<>%mutate(당월매출금액=as.numeric(당월매출금액), 점포당매출금액=당월매출금액/점포수)
data <- data[,-169]
data%<>%data.frame()
#data.table::fwrite(data, '상권분석2.csv')
#write.csv(data,'상권분석.csv',fileEncoding='UTF-8')
data <- data.frame(data.table::fread('상권분석.csv',encoding='UTF-8')) ; data <- data[,-1]
for(k in c(30:52,158:169)){data[,k] <- as.numeric(data[,k])}
data%<>%filter(점포수!=0)
data <- data[,-c(112:131)]
#write.csv(data,'상권분석2.csv',fileEncoding='UTF-8')

# 데이터 시각화
setwd('c:/Users/USER/Desktop/ICT-CoC Hackathon')
library(ggplot2)
data <- data.frame(data.table::fread('상권분석2.csv',encoding='UTF-8')) ; data <- data[,-1]
data%>%group_by(서비스업종코드명)%>%summarise(매출=mean(점포당매출금액))%>%ggplot(aes(x=서비스업종코드명,y=매출, fill=서비스업종코드명))+geom_bar(stat='identity')+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),)+ggtitle('업종별 평균매출액')
data%>%ggplot(aes(x=서비스업종코드명,y=점포당매출금액, fill=서비스업종코드명))+geom_boxplot()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),)
data%<>%filter(점포당매출금액<=300000000)
# 업종별 선형회귀
library(car)
library(lmtest)
library(corrplot)
data%<>%mutate(상권변화지표=as.factor(상권변화지표))
reg <- data%>%select(서비스업종코드명,남성상주인구수,여성상주인구수,아파트가구수,비아파트가구수,상권변화지표,남성직장인구수,여성직장인구수,월평균소득금액)
corrplot(cor(reg[,-c(1,6)]),method = 'number')
model <- with(reg, by(reg, 서비스업종코드명, function(reg) lm(월평균소득금액~., data=reg[,2:9])))
result <- list() ; for(k in 1:45){result[[k]] <- summary(model[[k]])} 
names(result) <- as.vector(sort(unique(reg$서비스업종코드명)))
c <- list()
d <- vector()
for(k in 1:45){c[[k]]<- cooks.distance(model[[k]]) ; d[[k]] <- sum(c[[k]]>=1)}
for(k in 1:45){c[[k]]<- rstudent(model[[k]]) ; d[[k]] <- sum(c[[k]]>=2)}
sum(d)
for(k in 1:45){c[[k]]<- outlierTest(model[[k]])}
a <- data%>%filter(서비스업종코드명=='한식음식점'|서비스업종코드명=='중식음식점'|서비스업종코드명=='일식음식점'|서비스업종코드명=='분식음식점'|서비스업종코드명=='치킨전문점'|서비스업종코드명=='양식음식점'|서비스업종코드명=='패스트푸드점'|서비스업종코드명=='제과점')

# 랜덤포레스트
rfdata<- data%>%select(서비스업종코드명,남성상주인구수,여성상주인구수,아파트가구수,비아파트가구수,상권변화지표,남성직장인구수,여성직장인구수,월평균소득금액,ends_with('매출건수'),점포당매출금액)
rfdata[,1] <- as.factor(rfdata[,1]) ;rfdata[,6] <- as.factor(rfdata[,6])
rfdata%<>%filter(서비스업종코드명=='한식음식점')
corrplot(cor(rfdata[,-c(1,6)]),method = 'number')
library(caret)
train.index <- createDataPartition(rfdata$점포당매출금액,p=0.7,list=F)
train <- rfdata[train.index,]
test <- rfdata[-train.index,]
#write.csv(train, 'train.csv', row.names = F, fileEncoding = 'UTF-8') ; write.csv(test, 'test.csv', row.names = F,fileEncoding = 'UTF-8')
setwd('c:/Users/USER/Desktop/ICT-CoC Hackathon')
train <- data.frame(data.table::fread('train.csv',encoding='UTF-8')) ; test <- data.frame(data.table::fread('test.csv',encoding='UTF-8'))
#train,test set 완성
for(k in c(1,6)){train[,k] <- as.factor(train[,k])}
for(k in c(1,6)){test[,k] <- as.factor(test[,k])}
test_x = test[,-34]
test_y = test[,34]

## parameter tuning(find best mtry)
library(caret)
set.seed(101)
control <- trainControl(method = "cv",number = 5)
rf_1 = NULL
for (i in seq(1,33,by=5)){
  tunegrid = expand.grid(.mtry=i)
  rf = train(점포당매출금액~., data=train, method = 'rf',
                    metric = 'RMSE', tuneGrid = tunegrid,
                    ntree= 100, trControl = control)
  rf_1 = c(rf_1,rf$results[2])
}
result_vec <- unlist(rf_1)
result_df <- data.frame(mtry=seq(1,33,by=4), Accuracy=result_vec)
ggplot(result_df, aes(x=mtry, y=Accuracy))+
  geom_point()+geom_line()

## modeling
library(randomForest)
set.seed(101)
control <- trainControl(method = "cv",number = 5)
model <-  randomForest(점포당매출금액~., data = train, method='rf',
                              metric = 'RMSE', mtry = 6, ntree = 100,
                              trControl = control,importance=T)
rf_predict<- predict(model, test_x)   # train과 test Factor level이 달라서 에러
test <- rbind(train[1, ] , test) # 똑같이 맞춰줌
test <- test[-1,]
rf_predict<- predict(model, test)
RMSE(rf_predict, test_y)



## importance plot
importance(model)
varImpPlot(model,sort=TRUE)
