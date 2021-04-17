library(pwr)

effectSizeFromCohen <- .85/(1-.85)
trialsPerSubject.est <- pwr.f2.test(u = 1, f2=effectSizeFromCohen,  sig.level = 0.05, power = .9)
PowerPerSubject.est <- pwr.f2.test(u = 1, v=119, f2=effectSizeFromCohen,  sig.level = 0.05)

RT.linear <- pwr.f2.test(u = 1, v = 8,  sig.level = 0.05, power = .8)
RT.Quantity <- pwr.f2.test(u = 1, v = 21,  sig.level = 0.05, power = .8)
Value.Quantity <- pwr.f2.test(u = 2, v = 40,  sig.level = 0.05, power = .8)

t.test.power.exp1 <- pwr.t.test(n = 32,  sig.level = 0.05, power = 0.8,
    type = c("one.sample"),
    alternative = c("two.sided"))

t.test.power.exp2 <- pwr.t.test(n = 59,  sig.level = 0.05, power = 0.8,
    type = c("one.sample"),
    alternative = c("two.sided"))

sink ("powerAnalyses.txt")
  cat("Effect size from Cohen and Ahn: ",effectSizeFromCohen, "\n\n" )
  cat("Number of trials needed per subject for 90% power: ",trialsPerSubject.est$v, "\n\n" )
  cat("Power for individual subject, given 120 trials per subject: ",PowerPerSubject.est$power, "\n\n" )
  cat("Effect size needed for 80% power, PVT RT linear analyses: ",RT.linear$f2, "\n\n" )
  cat("Effect size needed for 80% power, PVT RT Quantity analysis: ",RT.Quantity$f2, "\n\n" )
  cat("Effect size needed for 80% power, PVT Value by quantity analysis: ",Value.Quantity$f2, "\n\n" )
  cat("Effect size needed for 80% power, t.test Exp 1: ",t.test.power.exp1$d, "\n\n" )
  cat("Effect size needed for 80% power, t.test Exp 2: ",t.test.power.exp2$d, "\n\n" )
sink (NULL)
