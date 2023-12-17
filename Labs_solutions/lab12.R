fertilizers <-factor(rep(c("F1","F2","F3"),times = 3))
Crops <- factor(rep(c("Corn","Rice","Wheat"),each = 3))
months <- c(6,5,6,4,4.2,5,6,5,5.5)
data <- data.frame(fertilizers,Crops,months)            
analysis  <- aov(months ~ Crops +fertilizers, data = data)
summary(analysis)



rest <- factor(rep(c("r1","r2","r3","r4","r5","r6"),times = 3))
item <- factor(rep(c("item1","item2","item3"),each  = 6))
sales <- c(31,31,45,21,42,32,27,28,29,18,36,17,24,31,46,48,46,40)
data <- data.frame(rest,item,sales)
analysis2 <- aov(sales~rest + item, data = data)
summary(analysis2)


data <- data.frame(
  Tillage = factor(rep(1:5, each = 5)),
  Fertilizer = factor(rep(1:5, times = 5)),
  Seed = factor(c("A", "C", "B", "D", "E",
                  "E", "B", "C", "A", "D",
                  "C", "A", "D", "E", "B",
                  "B", "D", "E", "C", "A",
                  "D", "E", "A", "B", "C")),
  freq = c(
    42, 47, 55, 51, 44,
    45, 54, 52, 44, 50,
    41, 46, 57, 47, 48,
    56, 52, 49, 50, 43,
    47, 49, 45, 54, 46
  )
)
fit3 <- aov(freq ~ Fertilizer + Tillage + Seed, data = data)
summary(fit3) 