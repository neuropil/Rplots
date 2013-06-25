# Simulate data
n.each <- 1000
A1 <- rnorm(n.each, 2, 1)
A2 <- rnorm(n.each, 1.5, 2)
B1 <- rnorm(n.each, 4, 1.5)
B2 <- rnorm(n.each, 0, 1)
values <- c(A1, A2, B1, B2)
treatment <- rep(c("A", "B"), each=n.each*2)
group <- rep(c(1, 2, 1, 2), each=n.each)

par(bty="n")
boxplot(values ~ group*treatment, main="Box plot", col=rep(c("purple", "lightblue"), 2))

require(vioplot)
require(devtools)
require(digest)
source_gist("https://gist.github.com/mbjoseph/5852613")
plot(x=NULL, y=NULL,
     xlim = c(0.5, 2.5), ylim=c(min(values), max(values)),
     type="n", ann=FALSE, axes=F)
axis(1, at=c(1, 2),  labels=c("A", "B"))
axis(2)
for (i in unique(treatment)) {
  for (j in unique(group)){
    vioplot2(values[which(treatment == i & group == j)],
             at = ifelse(i == "A", 1, 2),
             side = ifelse(j == 1, "left", "right"),
             col = ifelse(j == 1, "purple", "lightblue"),
             add = T)
  }
}
title("Violin plot", xlab="Treatment")
legend("bottomright", fill = c("purple", "lightblue"),
       legend = c("Group 1", "Group 2"), box.lty=0)


require(beanplot)
beanplot(values ~ group*treatment, ll = 0.04,
         main = "Bean plot", side = "both", xlab="Treatment",
         col = list("purple", c("lightblue", "black")),
         axes=F)
axis(1, at=c(1, 2),  labels=c("A", "B"))
axis(2)
legend("bottomright", fill = c("purple", "lightblue"),
       legend = c("Group 1", "Group 2"), box.lty=0)
