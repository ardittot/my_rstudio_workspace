library(nloptr)
#flight
#hotel

Total_budget <- 685000
CA_flight_target <- 35000
CA_hotel_target <- 27500


B <- Total_budget
CA_h <- CA_hotel_target
CA_f <- CA_flight_target
num_iter <- 3000000
WB <- 1
WH <- 1
WF <- 1

fn1 <- function (b) {
  (b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11]) - B
}

# x <- seq(0,16987.33,100)
# Y <- (453.121408 + (x*0.057847))/(1 + ((3.7653*10^(-6))*x) + ((-3.111693*10^(-11))*(x)^(2)))
# lo <- loess(Y~x)
# plot(x,Y); lines(x,lo$fitted, col='red'); dev.off()

eq <- function(b) {
  c1 <- WF*(CA_f - (
    #android BBM
    #(453.121408 + (b[1]*0.057847))/(1 + ((3.7653*10^(-6))*b[1]) + ((-3.111693*10^(-11))*(b[1])^(2)))
    predict(lo, b[1])
    + #android FB
     b[2]/(17.202514+(b[2]*-0.000024)+((b[2]^(2))*3.2387*10^(-10)))
    + #android Glispa
      (671.991542 + (b[3]*0.058670))/(1 + ((0.000002*10^(-6))*b[3]) + ((-1.75689*10^(-12))*(b[3])^(2)))
    + #android inMobi
      2858.375 + (5995.5028*cos((b[4]*0.000008)-1.780395))
    + #android Iron Source
      (447.386017 + (b[5]*0.055984))/(1 + (0.000001*b[5]) + ((1.647926*10^(-11))*(b[5])^(2)))
    + #android UAC
      1447.325122 + (7358.055424*cos((b[6]*0.000007)-1.611887))
    + #ios BBM
      (378.184676 + (b[7]*0.088344))/(1 + (0.000045*b[7]) + ((-1.20808*10^(-9))*(b[7])^(2)))
    + #ios FB
      (223.218259 + (b[8]*0.048426))/(1 + ((-3.12299*10^(-6))*b[8]) + ((5.920886*10^(-11))*(b[8])^(2)))
    + #ios inMobi
      (241.382860 + (b[9]*0.087999))/(1 + ((5.07153*10^(-5))*b[9]) + ((-1.5769965*10^(-9))*(b[9])^(2)))
    + #ios Mundomedia
      (217.841226 + (b[10]*0.064046))/(1 + ((9.936309*10^(-6))*b[10]) + ((-1.599*10^(-10))*(b[10])^(2)))
    + #ios UAC
      170.551113 + (8477.660869*cos((b[11]*0.000006)-1.524054))
  ))
  c2 <- WH*(CA_h - (
    #android BBM
    (88.648864 + (b[1]*0.028001))/(1 + ((2.48017*10^(-6))*b[1]) + ((-1.385859*10^(-10))*(b[1])^(2)))
    + #android FB
      3956.029242 / (1 + exp(2.120095-(b[2]*0.000041)))
    + #android Glispa
      3953.481290 / (1 + exp(-3.911945-(b[3]*0.000032)))^(1/0.006213)
    + #android inMobi
      4473.958634 / (1 + exp(0.590193-(b[4]*0.000035)))^(1/0.407361)
    + #android Iron Source
      1251.877768 + (1231.611711*cos((b[5]*0.000033)+3.547333))
    + #android UAC 
      4516.475422 / (1 + exp(0.759410-(b[6]*0.000035)))^(1/0.449276)
    + #ios BBM
      976.406768 / (1 + exp(1.818129-(b[7]*0.000150)))
    + #ios FB
      (113.636408 + (b[8]*0.030458))/(1 + ((-2.239577*10^(-6))*b[8]) + ((-4.344317*10^(-11))*(b[8])^(2)))
    + #ios inMobi
      (95.557818 + (b[9]*0.013959))/(1 + ((-2.1660881*10^(-5))*b[9]) + ((-5.43445*10^(-11))*(b[9])^(2)))
    + #ios Mundomedia
      (86.5459 + (b[10]*0.020472))/(1 + ((-3.36426*10^(-6))*b[10]) + ((-4.65890*10^(-11))*(b[10])^(2)))
    + #ios UAC
      3672.482740 / (1 + exp(-1.690423-(b[11]*0.000032)))^(1/0.054051)
  ))
  
  c3 <- WB*((b[1]+b[2]+b[3]+b[4]+b[5]+b[6]+b[7]+b[8]+b[9]+b[10]+b[11]) - B)
  
  return(c(c1,c2,c3))
}

res <- nloptr(x0=c(rep(0,11)),
              eval_f=fn1,
              lb = c(rep(0,11)),
              ub = c(16987.33,241097.7,74782.76,84995.68,45363.65,235207.6,
                     525.441,21842.27,3772.961,30000,53340.3),
              eval_g_ineq = eq,
              opts = list("algorithm"="NLOPT_LN_AUGLAG","maxeval"=num_iter,"local_opts"=list("algorithm"="NLOPT_LN_COBYLA")))
print(res)
#file_save <- ""
#write.csv(res$solution,file_save)
