
##############################################
# Hyper parameter - Random search method     #
##############################################

run <- function(seed, name = paste0("m_", seed), run = TRUE) {
  set.seed(seed)
  
  p <- list(
    Name = name,
    seed = seed,
    depth = sample(1:10, 1),
    l1 = runif(1, 0, .01),
    l2 = runif(1, 0, .01),
    input_dropout = rbeta(1, 1, 12),
    rho = runif(1, .9, .999),
    epochs = sample(c(10,50),1),
    epsilon = runif(1, 1e-10, 1e-4))
  p$neurons <- sample(5:10, p$depth, TRUE)   
  p$hidden_dropout <- rbeta(p$depth, 1.5, 1)/5
  
  if (run) {
    model <- h2o.deeplearning(
      x = colnames(h2odigits.train),
      y = "BoilerEFF",
      training_frame = h2oactivity.train,
      activation = "RectifierWithDropout",
      hidden = p$neurons,
      epochs = p$epochs,
      loss = "Automatic",
      input_dropout_ratio = p$input_dropout,
      hidden_dropout_ratios = p$hidden_dropout,
      l1 = p$l1,
      l2 = p$l2,
      rho = p$rho,
      epsilon = p$epsilon,
      export_weights_and_biases = TRUE,
      model_id = p$Name,
      nfolds = 4, seed = 0xDECAF
    )
    
    ## performance on training data
    p$MSE <- h2o.mse(model)
    p$R2 <- h2o.r2(model)
    p$Logloss <- h2o.logloss(model)
    p$CM <- h2o.confusionMatrix(model)
    
    ## performance on testing data
    perf <- h2o.performance(model,  h2o.rbind(h2odigits.train, h2odigits.test))  # h2o.rbind(train4, test4)�� ����
    p$T.MSE <- h2o.mse(perf)
    p$T.R2 <- h2o.r2(perf)
    p$T.Logloss <- h2o.logloss(perf)
    p$T.CM <- h2o.confusionMatrix(perf)
    
  } else {
    model <- NULL
  }
  
  return(list(
    Params = p,
    Model = model))
}


h2odigits.train <- train.data.frame.h2o
h2odigits.test <- test.data.frame.h2o


use.seeds <- c(403574L, 3237957L, -7531021L, 1148598L, -1945768L, 2,3,5,9,7)

system.time(model.res <- lapply(use.seeds, run)) 


model.res.dat <- do.call(rbind, lapply(model.res,
                                       function(x) with(x$Params,
                                                        data.frame(l1 = l1, l2 = l2,
                                                                   depth = depth, input_dropout = input_dropout,
                                                                   SumNeurons = sum(neurons),
                                                                   MeanHiddenDropout = mean(hidden_dropout),
                                                                   rho = rho, epsilon = epsilon, 
                                                                   epochs = epochs, 
                                                                   #sparsity = sparsity,  
                                                                   MSE = T.MSE))))

str(model.res.dat)


par(mfrow = c(2, 2))
plot(model.res.dat$l1, model.res.dat_200$l2, xlab="L1", ylab="L2")
plot(model.res.dat_200$depth , model.res.dat_200$input_dropout, xlab="depth", ylab="input_dropout")
plot(model.res.dat_200$rho , model.res.dat_200$epsilon , xlab="rho ", ylab="epsilon ")
plot(model.res.dat_200$SumNeurons, model.res.dat_200$MeanHiddenDropout, xlab="SumNeurons", ylab="MeanHiddenDropout")

model.res.dat[which.min(model.res.dat$MSE), ]


plot(sort(model.res.dat$MSE,decreasing = TRUE), type="b", ylab="Mean Square Errors", xlab="The number of model", 
     lwd=2, cex=0.8, col="red", main="N=5")
abline(v=0, lty=2);abline(v=1, lty=2);abline(v=2, lty=2);abline(v=3, lty=2);abline(v=4, lty=2);abline(v=5, lty=2);abline(v=20, lty=2);


library(ggplot2);library(reshape)
p.perf <- ggplot(melt(model.res.dat, id.vars = c("MSE")), aes(value, MSE)) +
  geom_point() +
  stat_smooth(colour = "black") +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  theme_classic()
print(p.perf)


install.packages("mgcv")
library(mgcv)
summary(m.gam <- gam(MSE ~ s(l1, k = 4) +
                       s(l2, k = 4) +
                       s(input_dropout) +
                       s(rho, k = 4) +
                       s(epsilon, k = 4) +
                       s(MeanHiddenDropout, k = 4) +
                       #s(epochs, k = 4) +
                       #s(sparsity, k = 4) +
                       #ti(depth, SumNeurons, k = 4),
                       te(depth, SumNeurons, k = 4),
                     data = model.res.dat))


par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(m.gam, select = i, cex.axis=1.4, cex.lab=1.4)
}


plot(m.gam, select = 9, cex.axis=1.2, cex.lab=1.4, cex=1, pch="*")
plot(m.gam, select = 7)
dev.off()


model.optimized <- h2o.deeplearning(
  x = xnames,
  y = "BoilerEFF",
  training_frame = h2odigits.train ,
  activation = "TanhWithDropout",
  hidden = c(500, 100, 500),
  epochs = 100,
  loss = "CrossEntropy",
  input_dropout_ratio = 0.1745018 ,
  hidden_dropout_ratios = c(0.05944409, 0.05944409, 0.05944409),
  l1 = 0.007450913,
  l2 = 0.002124112,
  rho = 0.9114875,
  epsilon = 1.745123e-05 ,
  export_weights_and_biases = TRUE,
  model_id = "optimized_model"
)

h2o.performance(model.optimized, h2oactivity.test)

