require(xgboost)

#importing data - 

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
result <- data.frame(PassengerId=test[,1])

# some cleaning data


# tips from here
# https://triangleinequality.wordpress.com/2013/09/08/basic-feature-engineering-with-the-titanic-data/
# http://amunategui.github.io/binary-outcome-modeling/


dataCleaning <- function(df){
  
  df$title <- ifelse(grepl('Mr.',df$Name),'Mr',
                     ifelse(grepl('Mrs ',df$Name),'Mrs',
                            ifelse(grepl('Miss',df$Name),'Miss',
                                   ifelse(grepl('Master',df$Name),'Master',
                                          ifelse(grepl('Dr',df$Name),'Dr',
                                                 ifelse(grepl('Rev',df$Name),'Rev',
                                                        'Nothing'))))))
  # I'm sure have a better way to do this :)
  df$cabCol <- ifelse(grepl('A',df$Cabin),'A',
                      ifelse(grepl('B',df$Cabin),'B',
                             ifelse(grepl('C',df$Cabin),'C',
                                    ifelse(grepl('D',df$Cabin),'D',
                                           ifelse(grepl('E',df$Cabin),'E',
                                                  ifelse(grepl('F',df$Cabin),'F',
                                                         ifelse(grepl('G',df$Cabin),'G',
                                                                '-')))))))
  return(df) 
}


train <- dataCleaning(train)
test <- dataCleaning(test)





data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test


model <- xgboost(data = train$data, label = train$label,
                 nrounds = 2, objective = "binary:logistic")

preds = predict(model, test$data)

cv.res <- xgb.cv(data = train$data, label = train$label, nfold = 5,
                 nrounds = 200, objective = "binary:logistic")