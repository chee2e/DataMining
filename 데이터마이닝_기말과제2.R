# 데이터 불러오기
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)



# 학습 데이터 및 테스트 데이터 분리과정

library(caret)

rn_data = createDataPartition(y = ucla$admit, p = 0.6, list = F)
training_data = ucla[rn_data, ] # 60%의 학습데이터
str(training_data)

test_data = ucla[-rn_data, ] #40%의 테스트데이터
str(test_data)


#--------------------------- 데이터 모델링 ------------------------------------------------#

# 결정 트리 모델링
decision_tree = rpart(admit~., data=training_data)

# 랜덤포레스트 모델링
rf_50 = randomForest(admit~., data = training_data, ntree = 50)     #트리개수 50개
rf_1000 = randomForest(admit~., data = training_data, ntree = 1000) #트리개수 1000개

# k-NN 모델링
knn_ucla = knn(training_data, test_data, cl = training_data$admit,  k = 15)

# SVM 모델링
svm_radial = svm(admit~., data = training_data, kernel = "radial")          #kernel = radial
svm_polynomial = svm(admit~., data = training_data, kernel = "polynomial")  #kernel = polynomial



#----------------------------- 모델 예측 및 혼동 행렬 출력 --------------------------------#


# 결정트리
# 테스트 데이터로 예측
p_tree = predict(decision_tree, test_data, type = 'class')
#혼동행렬로 출력
table(p_tree, test_data$admit)


#랜덤포레스트
# 테스트 데이터로 예측
rf.p_50 = predict(rf_50, test_data, type = 'class')       #트리개수 50개
rf.p_1000 = predict(rf_1000, test_data, type = 'class')   #트리개수 1000개

#혼동행렬로 출력
table(rf.p_50, test_data$admit)     #트리개수 50개
table(rf.p_1000, test_data$admit)   #트리개수 1000개


# k-NN
# 테스트 데이터로 예측
knn_ucla = knn(training_data, test_data, cl = training_data$admit,  k = 15)
knn_ucla

#혼동행렬로 출력
table(knn_ucla, test_data$admit)



# SVM
# 테스트 데이터로 예측 
p.svm_radial = predict(svm_radial, test_data, type = 'class')           # radial
p.svm_polynomial = predict(svm_polynomial, test_data, type = 'class')   # polynomial

#혼동행렬로 출력
table(p.svm_radial, test_data$admit)      # radial
table(p.svm_polynomial, test_data$admit)  # polynomial



