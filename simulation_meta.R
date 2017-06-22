
#
# Metacognitive simulation 2
#

setwd("~/OneDrive/Models/MacGregor/MacGregor_metacognition")
source('files/MacGregor_2.R', echo=F)


# Function to transform criterion into FOW-ratings
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}



### 9 dot

# Simulating 9 dot task; lookahead = 1
sim_FOW_1.2 <- list()
for (i in 1:10){
  sim1 <- sim_macgregor(dots_number = 9, attempts = 6, lookahead = 1, k = 1)
  sig_par <- c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion))
  
  meta_dat <- aggregate(sim1$criterion ~ rep(1:6, each = 4), FUN = mean)
  meta_dat$FOW <- sigmoid(params = c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion)), x = -meta_dat[,2])
  
  sim_FOW_1.2[[i]] <- meta_dat$FOW
  
}


# 9 dot ; lookahead = 2
sim_FOW_2.2 <- list()
for (i in 1:10){
  sim1 <- sim_macgregor(dots_number = 9, attempts = 7, lookahead = 2, k = 1)
  sig_par <- c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion))
  
  meta_dat <- aggregate(sim1$criterion ~ rep(1:7, each = 2), FUN = mean)
  meta_dat$FOW <- sigmoid(params = c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion)), x = -meta_dat[,2])
  
  sim_FOW_2.2[[i]] <- meta_dat$FOW
  
}

# 9 dot ; lookahead = 3
sim_FOW_3.2 <- list()
for (i in 1:10){
  sim1 <- sim_macgregor(dots_number = 9, attempts = 7, lookahead = 3, k = 1)
  sig_par <- c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion))
  
  meta_dat <- aggregate(sim1$criterion ~ rep(1:7, each = 2), FUN = mean)
  meta_dat$FOW <- sigmoid(params = c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion)), x = -meta_dat[,2])
  
  sim_FOW_3.2[[i]] <- meta_dat$FOW
  
}
rowMeans(as.data.frame(sim_FOW_3.2))[1:6]


# 9 dot ; lookahead = 4
sim_9dot_look4.2 <- sim_macgregor(dots_number = 9, attempts = 100, lookahead = 4, k = 1)

r1 = c()
q1 = c()
d1 = c()
sim_dat <- sim_9dot_look4.2
for (i in 6:100){
  if (sim_dat$success[i] == 1 & sum(sim_dat$success[(i-5):(i-1)]) == 0){
    q1 = c(q1, i)
    r1 = c(r1, sim_dat$criterion[(i-5):i])
  }
}

r1 = sigmoid(params = c(10, (max(sim_dat$criterion)-min(sim_dat$criterion))/3, -mean(sim_dat$criterion)), x = -r1)
d1 = (0:(length(r1)-1)) %% 6
d_all_9dots_look4.2 <- aggregate(r1 ~ d1, FUN = mean)$r1

# Combining data into plot
dots9.2 <- data.frame(FOW = c(rowMeans(as.data.frame(sim_FOW_1.2)), rowMeans(as.data.frame(sim_FOW_2.2))[1:6], rowMeans(as.data.frame(sim_FOW_3.2))[1:6], d_all_9dots_look4.2), 
                    lookahead = as.factor(rep(1:4, each = 6)), 
                    trial = rep(-5:0, 4))
dots9_plot.2 <- ggplot(dots9.2, aes(col = lookahead, y = FOW, x = trial)) +  
  geom_line(size = 1) + 
  geom_point(size = 2) +
  theme_classic() + 
  ylim(c(0,10))




### 11 dot

# 11 dot ; lookahead = 1
sim_11dot_look1.2 <- sim_macgregor(dots_number = 11, attempts = 100, lookahead = 1, k = 1)

r1 = c()
q1 = c()
d1 = c()
sim_dat <- sim_11dot_look1.2
for (i in 6:100){
  if (sim_dat$success[i] == 1 & sum(sim_dat$success[(i-5):(i-1)]) == 0){
    q1 = c(q1, i)
    r1 = c(r1, sim_dat$criterion[(i-5):i])
  }
}

r1 = sigmoid(params = c(10, (max(sim_dat$criterion)-min(sim_dat$criterion))/3, -mean(sim_dat$criterion)), x = -r1)
d1 = (0:(length(r1)-1)) %% 6
d_all_11dot_look1.2 <- aggregate(r1 ~ d1, FUN = mean)$r1


# 11 dot ; lookahead = 2
sim_11dot_look2.2 <- sim_macgregor(dots_number = 11, attempts = 100, lookahead = 2, k = 1)

r1 = c()
q1 = c()
d1 = c()
sim_dat <- sim_11dot_look2.2
for (i in 6:100){
  if (sim_dat$success[i] == 1 & sum(sim_dat$success[(i-5):(i-1)]) == 0){
    q1 = c(q1, i)
    r1 = c(r1, sim_dat$criterion[(i-5):i])
  }
}

r1 = sigmoid(params = c(10, (max(sim_dat$criterion)-min(sim_dat$criterion))/3, -mean(sim_dat$criterion)), x = -r1)
d1 = (0:(length(r1)-1)) %% 6
d_all_11dot_look2.2 <- aggregate(r1 ~ d1, FUN = mean)$r1


# 11 dot ; lookahead = 3
sim_11dot_look3.2 <- sim_macgregor(dots_number = 11, attempts = 100, lookahead = 3, k = 1)

r1 = c()
q1 = c()
d1 = c()
sim_dat <- sim_11dot_look3.2
for (i in 6:100){
  if (sim_dat$success[i] == 1 & sum(sim_dat$success[(i-5):(i-1)]) == 0){
    q1 = c(q1, i)
    r1 = c(r1, sim_dat$criterion[(i-5):i])
  }
}

r1 = sigmoid(params = c(10, (max(sim_dat$criterion)-min(sim_dat$criterion))/3, -mean(sim_dat$criterion)), x = -r1)
d1 = (0:(length(r1)-1)) %% 6
d_all_11dot_look3.2 <- aggregate(r1 ~ d1, FUN = mean)$r1


# 11 dot ; lookahead = 4
sim_11dot_look4.2 <- list()
for (i in 1:10){
  sim1 <- sim_macgregor(dots_number = 11, attempts = 6, lookahead = 4, k = 1)
  sig_par <- c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion))
  
  meta_dat <- aggregate(sim1$criterion ~ rep(1:6, each = 1), FUN = mean)
  meta_dat$FOW <- sigmoid(params = c(10, (max(sim1$criterion)-min(sim1$criterion))/3, -mean(sim1$criterion)), x = -meta_dat[,2])
  
  sim_11dot_look4.2[[i]] <- meta_dat$FOW
  
}


# Combining data into plot
dots11.2 <- data.frame(FOW = c(d_all_11dot_look1.2, d_all_11dot_look2.2, d_all_11dot_look3.2, rowMeans(as.data.frame(sim_11dot_look4.2))), 
                     lookahead = as.factor(rep(1:4, each = 6)), 
                     trial = rep(-5:0, 4))
dots11_plot.2 <- ggplot(dots11.2, aes(col = lookahead, y = FOW, x = trial)) +  
  geom_line(size = 1) + 
  geom_point(size = 2) +
  theme_classic() + 
  ylim(c(0,10))

