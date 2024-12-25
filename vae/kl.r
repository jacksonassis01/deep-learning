k=5
lambda=1
x_ini=.01
x_fin=500

P_chi2 = function(x) dchisq(x, df=k)
Q_exp = function(x) dexp(x, rate=lambda)

x_range = seq(x_ini,x_fin,length.out=10000)

P_vals = P_chi2(x_range)
Q_vals = Q_exp(x_range)

kl = sum(P_vals * log(P_vals / Q_vals)) * diff(x_range)[1]

print(kl)

kl_integral = function(x) P_chi2(x) * log(P_chi2(x) / Q_exp(x))

kl_value = integrate(kl_integral, lower=x_ini, upper=x_fin)
print(kl_value$value)
