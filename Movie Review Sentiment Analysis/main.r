
j = 1
setwd(paste("split_", j, sep=""))
train = read.table("train.tsv",
                   stringsAsFactors = FALSE,
                   header = TRUE)
train$review = gsub('<.*?>', ' ', train$review)

View(train)

library(text2vec)

stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "the", "us")

it_train = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)

tmp.vocab = create_vocabulary(it_train, 
                              stopwords = stop_words, 
                              ngram = c(1L,4L))

tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                             doc_proportion_max = 0.5,
                             doc_proportion_min = 0.001)

dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))

print(nrow(tmp.vocab))

View(tmp.vocab)

library(glmnet)
set.seed(12345)
tmpfit = glmnet(x = dtm_train, 
                y = train$sentiment, 
                alpha = 1,
                family='binomial')
tmpfit$df
View(tmpfit)


max_2k = which.max((tmpfit$df[tmpfit$df < 2000]))

myvocab = colnames(dtm_train)[which(tmpfit$beta[, max_2k] != 0)]

print(length(myvocab))

vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))

dtm_train = create_dtm(it_train, vectorizer)

test <- read.table("test.tsv", stringsAsFactors = FALSE,
                   header = TRUE)
it_test = itoken(test$review,
                 preprocessor = tolower, 
                 tokenizer = word_tokenizer)

dtm_test = create_dtm(it_test, vectorizer)


View(test)
View(train)
View(dtm_train)

rdfit = glmnet(x = dtm_train, 
                y = train$sentiment, 
                alpha = 0,
                lambda=0.05)
opt_lambda <- rdfit$lambda.min

pred <- predict(rdfit, s = opt_lambda, newx = dtm_test)

output = data.frame(id = test$id, prob = as.vector(pred))

write.table(output, file = "mysubmission.txt", 
            row.names = FALSE, sep='\t')

library(pROC)

test.y <- read.table("test_y.tsv", header = TRUE)
pred <- read.table("mysubmission.txt", header = TRUE)

View(pred)
View(test.y)

pred <- merge(pred, test.y, by="id")
roc_obj <- roc(pred$sentiment, pred$prob)
pROC::auc(roc_obj)

