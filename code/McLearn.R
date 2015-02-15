require(rpart)
require(rpart.plot)
require(rattle)
require(caret)
require(Hmisc)
require(randomForest)

# Read data and explore
all = read.csv("pml-training.csv")
#par(ask=T)
#for (i in 2:159) {
  #print(qplot(data=d,d[,i],xlab=names(d)[i]))
#}

# Clean it up a bit
all=all[,-which(names(all) %in% c('X','cvtd_timestamp'))]   # remove the serial # and useless factor variables
all=all[,colSums(!is.na(all))/nrow(all) > .95]              # remove columns with high NA
all=all[,colSums(all!="")/nrow(all) > .95]                  # remove columns with high blanks
cm=cor(all[,!sapply(all,is.factor)])                        # find correlation between features
all=all[,-findCorrelation(cm,.70,verbose = F)]              # remove correlated columns

# Create columns for each factor level for classe
all$A=ifelse(all$classe=='A',1,0)
all$B=ifelse(all$classe=='B',1,0)
all$C=ifelse(all$classe=='C',1,0)
all$D=ifelse(all$classe=='D',1,0)
all$E=ifelse(all$classe=='E',1,0)

# Split the data
split = createDataPartition(all$classe, p=0.4, list=F)
d = all[split,]
v = all[-split,]

# fit a tree
mt = train(classe~.-A-B-C-D-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2,data=d,method="rpart")

# fit a forest
mf = randomForest(classe~.-A-B-C-D-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d)

# fit GLMs
mA = glm(A~.-classe-B-C-D-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d, family="binomial")
mA = step(mA)
mB = glm(B~.-classe-A-C-D-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d, family="binomial")
mB = step(mB)
mC = glm(C~.-classe-A-B-D-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d, family="binomial")
mC = step(mC)
mD = glm(D~.-classe-A-B-C-E-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d, family="binomial")
mD = step(mD)
mE = glm(E~.-classe-A-B-C-D-user_name-raw_timestamp_part_1-raw_timestamp_part_2, data = d, family="binomial")
mE = step(mE)

# Prediction
nd = v

# Tree
confusionMatrix(predict(mt,newdata=nd),nd$classe)

# Forest
confusionMatrix(predict(mf,newdata=nd),nd$classe)

# GLMs
pglm=data.frame(predict(mA,newdata=nd),predict(mB,newdata=nd),predict(mC,newdata=nd),predict(mD,newdata=nd),predict(mE,newdata=nd))
names(pglm)=c("A","B","C","D","E")
pglm$predict=(names(pglm)[max.col(pglm)])
confusionMatrix(pglm$predict,nd$classe)


