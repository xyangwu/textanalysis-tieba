# load in data
teaword <- read.csv('C:/Users/yhy/Desktop/自闭症/原始数据、分词/teaword.csv')
tea <- read.csv('C:/Users/yhy/Desktop/自闭症/原始数据、分词/tea.csv')

# 一、词典导入与处理
### 1.情感正向词，词组+打“+1”-label  
pos <- read.table("C:/Users/yhy/Desktop/自闭症/情感语料包/CNKI词语集/posCN.txt",encoding='UTF-8')  
pos$V1<- trimws(pos$V1, which = "right")
weight <- rep(1, length(pos[,1]))  
pos <- cbind(pos, weight)  

### 2.情感负向词，词组+打“-1”-label  
neg <- read.table("C:/Users/yhy/Desktop/自闭症/情感语料包/CNKI词语集/negCN.txt",encoding='UTF-8')
neg$V1 <- trimws(neg$V1, which = "right")
weight <- rep(-1, length(neg[,1]))  
neg <- cbind(neg, weight)  

### 3.正、负向词组合并  
posneg <- rbind(pos, neg)  #正负词典合并  
names(posneg) <- c("term", "weight")  
posneg <- posneg[!duplicated(posneg$term), ] #`duplicated`函数的作用和`unique`函数比较相似，它返回重复项的位置编号  



# 二、情感得分
### 1.关联情感权重
testtea <- dplyr::left_join(teaword,posneg,by=c('word'='term'))
nonsense <- testtea[is.na(testtea$weight),]
test <- testtea[!is.na(testtea$weight),]

### 2.计算情感指数
dictresult <- aggregate(weight ~ document, data = testtea,sum)
table(dictresult$weight)

### 3.情感得分匹配给tea数据，生成最终进入模型数据result
result <- dplyr::left_join(tea,dictresult,by=c('Id'='document'))
sum(is.na(result$weight))
length(result$Id)
write.csv(result,'C:/Users/yhy/Desktop/自闭症/原始数据、分词/result.csv')

result <- read.csv('C:/Users/yhy/Desktop/自闭症/原始数据、分词/result.csv')



# 方法2：词语表
senti <- openxlsx::read.xlsx('C:/Users/yhy/Desktop/自闭症/情感语料包/情感词汇本体_大连理工大学信息检索/情感词汇本体.xlsx')
### 1.情感类型重新赋值
table(senti$情感分类)
senti$sentitype <- as.numeric(car::recode(senti$情感分类,"'PA'=1;'PE'=1;'PD'=1;
                                'PH'=1;'PG'=1;'PB'=1;'PK'=1;'NF'=-1;
                               'NB'=-1;'NJ'=-1;'NH'=-1;'PF'=-1;
                               'NI'=-1;'NC'=-1;'NG'=-1;'NE'=-1;'ND'=-1;
                               'NN'=-1;'NK'=-1;'NL'=-1;'PC'=-1"))
sum(is.na(senti$sentitype))
table(senti$sentitype)
senti$weight <- senti$sentitype*senti$强度


### 2.关联情感权重
testtea2 <- dplyr::left_join(teaword,senti[c('词语','weight')],by=c('word'='词语'))
nonsense2 <- testtea2[is.na(testtea2$weight),]
test2 <- testtea2[!is.na(testtea2$weight),]

### 3.计算情感指数
dictresult2 <- aggregate(weight ~ document, data = test2,sum)
table(dictresult2$weight)
sum(dictresult2$weight<0)
sum(dictresult2$weight>0)
library(ggplot2)
p1 <- ggplot(aes(x = weight), data = dictresult2) + geom_freqpoly()
p1

### 3.情感得分匹配给tea数据，生成最终进入模型数据result
result2 <- dplyr::left_join(tea,dictresult2,by=c('Id'='document'))
sum(!is.na(result2$weight))
length(result2$Id)
write.csv(result2,'C:/Users/yhy/Desktop/自闭症/原始数据、分词/result2.csv')

