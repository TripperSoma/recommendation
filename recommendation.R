#########################################
## RankPage Algorithm with Recommendation System
#########################################

#refers to this source code.(동근이와 아형이)

data <-read.csv("sample.csv",head=FALSE, sep=",")
View(data)

install.packages("recommenderlab")

library(recommenderlab)

#데이터형을 realRatingMatrix 변환
r<-(data,"realRatingMatrix")
#바이너리는 산 횟수가 0 1 (샀다 안샀다)리얼 레이팅은 진짜 횟수


trainingData<- sample(4945,4500) # 4950 중 4500 추출
trainingSet<-  r[trainingData]

trainingSet<- trainingSet[rowCounts(trainingSet)>5]
as(trainingSet,"matrix")

#scheme 지정과 표시
# 추천의 평가방식을 저장
# split은 분할, cross는 교차검정
# train 은 모델만드는 데이터 비율 80, 20
# given은 사람당 평가하는 아이템 수
# K는 시뮬레이션 반복횟수
scheme <- evaluationScheme(trainingSet,method ="split",train.8,
                           given=6,
                           goodRating=4,
                           k=3)
#추천의 평가방식을 지정하고 저장 - 중요하다
#교차검정 - 전체 데이터셋을 k개로 나눈다
#train -  모델을 만드는 데이터 비율 - 위 코드는 80%
#given 은 사람당 평가하는 아이템 수
#goodrating = 실제 사용 등급이 4이상이면 모두 평가에 포함
#시뮬레이션 실행할 때마다 결과가바뀜


#추천모델생성
mUBCF <-Recommender(trainingSet,method="UBCF",parameter="Cosine")

mUBCF

mIBCF <- Recommender(trainigSet,method = "IBCF",parameter="Cosine")

mIBCF

recommenderUserList<-r[-trainingData]


#step 3

UBCFlist <-predict(mUBCF,recommenderUserList,n=5)

IBCFlist <- predict(mIBCF,recommenderUserListm,n=5)


#추천받은 리스트 보기
as(UBCFlist, "list")
as(IBCFlist,"list")


#모델 결과표와 그래프 생성
alUBCF<-list("user-based CF_Cosine"=list(name = "UBCF",param= list(method ="Cosine")),
             "user-based CF_Pearson" = list(name="UBCF",
                                            param=list(method = "Pearson")))
alIBCF<-list("user-based CF_Cosine"=list(name = "IBCF",param= list(method ="Cosine")),
             "user-based CF_Pearson" = list(name="IBCF",
                                            param=list(method = "Pearson")))

result1<-evaluate(scheme,alUBCF,n=c(1,3,5))
result1
avg(result1)

result<-evaluate(scheme,alIBCF,n=c(1,3,5))
result
avg(result2)

plot(result1,annotate = TRUE,legend="bottomright")
plot(result1,"prec/rec",annotate = TRUE,legend = "bottomright")

plot(result1,annotate = TRUE,legend="topleft")
plot(result1,"prec/rec",annotate = TRUE,legend = "topleft")

