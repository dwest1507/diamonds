library(tidyverse)
library(randomForest)
library(Metrics)

data <- diamonds

train_index <- sample(1:nrow(data), round(nrow(data)*0.8, digits = 0))

train <- data[train_index,]
test <- data[-train_index,]

# linear model
model_lm <- lm(price ~ ., data = train)

summary(model_lm)
print(model_lm)
plot(model_lm)

predict_lm <- predict(model_lm, test)

test_lm <- test %>% 
  mutate(pred = predict_lm,
         residual = price - pred)

sse_lm <- test_lm %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_lm <- sqrt(sse_lm/nrow(test))

mae(test_lm$price, test_lm$pred)
mse(test_lm$price, test_lm$pred)
rmse(test_lm$price, test_lm$pred)

# linear model 2
model_lm2 <- lm(price ~ carat + cut + color + clarity, data = train)

summary(model_lm2)
print(model_lm2)
plot(model_lm2)

predict_lm2 <- predict(model_lm2, test)

test_lm2 <- test %>% 
  mutate(pred = predict_lm2,
         residual = price - pred)

sse_lm2 <- test_lm2 %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_lm2 <- sqrt(sse_lm2/nrow(test))

mae(test_lm2$price, test_lm2$pred)
mse(test_lm2$price, test_lm2$pred)
rmse(test_lm2$price, test_lm2$pred)

# power model
model_pm <- lm(log(price) ~ log(carat) + cut + color + clarity, data = train)

summary(model_pm)
print(model_pm)
plot(model_pm)

predict_pm <- predict(model_pm, test)

test_pm <- test %>% 
  mutate(pred = predict_pm,
         residual = price - pred)

sse_pm <- test_pm %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_pm <- sqrt(sse_pm/nrow(test))

mae(test_pm$price, test_pm$pred)
mse(test_pm$price, test_pm$pred)
rmse(test_pm$price, test_pm$pred)

# random forest
model_rf <- randomForest(price ~ .,data = train, do.trace=TRUE, ntree = 300)

summary(model_rf)
print(model_rf)
plot(model_rf)
model_rf$importance

predict_rf <- predict(model_rf, test)

test_rf <- test %>% 
  mutate(pred = predict_rf,
         residual = price - pred)

sse_rf <- test_rf %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_rf <- sqrt(sse_rf/nrow(test))

mae(test_rf$price, test_rf$pred)
mse(test_rf$price, test_rf$pred)
rmse(test_rf$price, test_rf$pred)

# Predict
inquiry <- tibble(
  carat = 1.5,
  cut = ordered("Ideal", 
                levels = levels(train$cut)),
  color = ordered("E",
                  levels = levels(train$color)),
  clarity = ordered("VVS2",
                    levels = levels(train$clarity)),
  depth = 61.80,
  table = 57.00,
  x = 5.700,
  y = 5.700,
  z = 3.520
)

predict(model_rf, inquiry)


# random forest 2
model_rf2 <- randomForest(price ~ carat + cut + color + clarity, 
                          data = train, 
                          do.trace=TRUE, 
                          ntree = 300)

print(model_rf2)
plot(model_rf2)
model_rf2$importance

predict_rf2 <- predict(model_rf2, test)

test_rf2 <- test %>% 
  mutate(pred = predict_rf2,
         residual = price - pred)

sse_rf2 <- test_rf2 %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_rf2 <- sqrt(sse_rf2/nrow(test))

mae(test_rf2$price, test_rf2$pred)
mse(test_rf2$price, test_rf2$pred)
rmse(test_rf2$price, test_rf2$pred)


# random forest 3
model_rf3 <- randomForest(price ~ carat + cut + color + clarity + depth + table, 
                          data = train, 
                          do.trace=TRUE, 
                          ntree = 300)

print(model_rf3)
plot(model_rf3)
model_rf3$importance

predict_rf3 <- predict(model_rf3, test)

test_rf3 <- test %>% 
  mutate(pred = predict_rf3,
         residual = price - pred)

sse_rf3 <- test_rf3 %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_rf3 <- sqrt(sse_rf3/nrow(test))

mae(test_rf3$price, test_rf3$pred)
mse(test_rf3$price, test_rf3$pred)
rmse(test_rf3$price, test_rf3$pred)

# random forest 4
model_rf4 <- randomForest(price ~ .,data = train, do.trace=TRUE, ntree = 50)

summary(model_rf4)
print(model_rf4)
plot(model_rf4)
model_rf4$importance

predict_rf4 <- predict(model_rf4, test)

test_rf4 <- test %>% 
  mutate(pred = predict_rf4,
         residual = price - pred)

sse_rf4 <- test_rf4 %>% 
  summarise(SSE = sum(residual^2)) %>% 
  pull()

rmse_rf4 <- sqrt(sse_rf4/nrow(test))

mae(test_rf4$price, test_rf4$pred)
mse(test_rf4$price, test_rf4$pred)
rmse(test_rf4$price, test_rf4$pred)


# Predict
check <- data %>% 
  filter(carat==2.0,
         cut=="Very Good",
         color=="F",
         clarity=="SI1")

check2 <- check %>% 
  summarise(
    mean_depth = mean(depth),
    mean_table = mean(table),
    mean_x = mean(x),
    mean_y = mean(y),
    mean_z = mean(z)
  )

inquiry <- tibble(
  carat = 2.0,
  cut = ordered("Very Good", 
                levels = levels(train$cut)),
  color = ordered("F",
                  levels = levels(train$color)),
  clarity = ordered("SI1",
                    levels = levels(train$clarity)),
  depth = check2$mean_depth,
  table = check2$mean_table,
  x = check2$mean_x,
  y = check2$mean_y,
  z = check2$mean_z
)

mean(check$price)
predict(model_lm, inquiry)
predict(model_lm2, inquiry)
predict(model_pm, inquiry)
predict(model_rf, inquiry)
predict(model_rf2, inquiry)
predict(model_rf3, inquiry)
predict(model_rf4, inquiry)


