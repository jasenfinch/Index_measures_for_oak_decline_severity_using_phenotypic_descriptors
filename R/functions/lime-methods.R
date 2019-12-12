
predict_model.randomForest <- function(x, newdata, type, ...) {
  res <- predict(x, newdata = newdata, ...)
  switch(
    type,
    raw = data.frame(Response = res, stringsAsFactors = FALSE),
    prob = as.data.frame(res, check.names = FALSE)
  )
}

model_type.randomForest <- function(x, ...) x$type
