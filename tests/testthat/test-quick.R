test_that("Quick - lm", {
  m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)

  g <- qg2(m1)

  expect_length(g$x$views, 2)

  m2 <- update(m1, . ~ . + hp)
  m3 <- update(m2, . ~ . + am) 

  g <- qg2(list(m1, m2, m3))

  expect_length(g$x$views, 2)
})

test_that("Quick - survival", {
  data(veteran, package = "survival")

  fit <- survival::survfit(survival::Surv(time, status) ~ trt, data = veteran)

  g <- qg2(fit)
  expect_length(g$x$views, 3)
})

test_that("Quick - acf", {
  cc <- acf(lh, plot = FALSE)

  g <- qg2(cc)
  expect_length(g$x$views, 1)
})

test_that("Quick - xts", {
  options("getSymbols.warning4.0"=FALSE)

  aapl <- quantmod::getSymbols("AAPL", env = NULL)

  g <- aapl %>% 
    head(50) %>% 
    qg2()

  expect_length(g$x$views, 1)
})

test_that("Quick - stl", {
  s <- stl(nottem, "per")

  g <- qg2(s)

  expect_length(g$x$views, 1)
})

test_that("Quick - roc", {
  data(two_class_example, package = "yardstick")

  roc <- yardstick::roc_curve(two_class_example, truth, Class1)

  g <- qg2(roc)

  expect_length(g$x$views, 1)

})

test_that("Quick - forecast", {
  fc <- forecast::forecast(forecast::ets(USAccDeaths))

  g <- qg2(fc)

  expect_length(g$x$views, 4)

})

test_that("Quick - matrix", {
  correl_mat <- cor(mtcars)

  g <- qg2(correl_mat)

  expect_length(g$x$views, 1)
})

test_that("Quick - graph", {
  g <- igraph::erdos.renyi.game(500, 2/500)

  g <- qg2(g)

  expect_length(g$x$views, 2)
})

test_that("Quick - graph", {
  pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

  g <- qg2(pca)

  expect_length(g$x$views, 3)
})
