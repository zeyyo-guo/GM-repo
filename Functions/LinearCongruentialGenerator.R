# Linear Congruential Generator
seed <- 10
random_n <- function(a,c,m){
  out <- (a*seed+c) %% m
  seed <<- out
  return(out)
}

out.length <- 20
outcome <- rep(NA,out.length)
for (i in 1:out.length) {
  outcome[i] <- random_n(5,12,16)
}
outcome

---
# 优化

lcg <- function(seed, a, c, m, n) {
  # 初始化生成器
  state <- seed
  
  # 存储生成的随机数
  random_numbers <- vector(mode = "numeric", length = n)
  
  # 生成随机数
  for (i in 1:n) {
    state <- (a * state + c) %% m
    random_numbers[i] <- state
    # random_numbers[i] <- state/m  可以保证结果在[0,1]内
  }
  
  return(random_numbers)
}
  
## 挖个坑，给定输出序列5201314怎么找到符合条件的初始参数
  