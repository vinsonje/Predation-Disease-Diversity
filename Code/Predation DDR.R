pode = function(t, y, parms){
  Ns = y["Ns"]; Ni = y["Ni"]; X = y["X"];
  
  b = parms["b"];  bi = parms["bi"]; mn = parms["mn"];  e = parms["e"]; beta = parms["beta"];
  P = parms["P"];  a = parms["a"]; c = parms["c"]; mx = parms["mx"]; ei = parms["ei"];
  
  dNs = b*Ns + bi*Ni - mn*Ns - e*P*Ns - beta*Ns*X;
  dNi = beta*Ns*X - mn*Ni - ei*P*Ni - a*Ni;
  dX = c*(a*Ni+mn*Ni) - mx*X;  
  
  return(list(c(dNs, dNi, dX)))
}

tmax = 1000
dt = 0.1
timevec= seq(0, tmax, dt)

b = 10;  
bi = 1; 
mn = 2;  
e = 0.1; 
ei = 0.3;
beta = 0.1;
P = 30;  
a = 0.5; 
c = 10; 
mx = 1; 

parvec = c(b=b, bi=bi, mn=mn, e=e, ei=ei, beta=beta, P=P, a=a, c=c, mx=mx)

Ns0 = 10
Ni0 = 0
X0 = 100

y0 = c(Ns=Ns0, Ni=Ni0, X=X0)

odeoutput = ode(y0, timevec, pode, parvec)

# plot(odeoutput[,1], odeoutput[,2], type="l", col="blue", ylim=c(0, max(odeoutput[,2:4])))
# lines(odeoutput[,1], odeoutput[,3], col="red")
# lines(odeoutput[,1], odeoutput[,4], col="black")

param.name = "P"

param.seq = seq(0, 30, 1)

param.index = which(param.name == names(parvec))
out = list()
for(i in 1:length(param.seq)) out[[i]] = matrix(0, tmax, length(y0))

for(i in 1:length(param.seq)){
  print(sprintf('Starting Simulation %d of %d ',i, length(param.seq)))
  
  Pars.loop = parvec
  Pars.loop[param.index] = param.seq[i]
  
  odeoutput = ode(y0, timevec, pode, Pars.loop)
  
  out[[i]] = ode(odeoutput[length(timevec),-1], timevec, pode, Pars.loop)[,-1]
}

range.lim = lapply(out, function(x) apply(x, 2, range))
range.lim = apply(do.call("rbind", range.lim), 2, range)
plot.variable = "X"
plot(0, 0, pch="", xlab = param.name, ylab= plot.variable, xlim=range(param.seq), ylim=range.lim[,plot.variable])
for(i in 1:length(param.seq)){
  points(rep(param.seq[i], length(timevec)), out[[i]][,plot.variable])
}

range.lim = lapply(out, function(x) apply(x, 2, range))
range.lim = apply(do.call("rbind", range.lim), 2, range)
plot.variable = "Ns"
plot(0, 0, pch="", xlab = param.name, ylab= plot.variable, xlim=range(param.seq), ylim=range.lim[,plot.variable])
for(i in 1:length(param.seq)){
  points(rep(param.seq[i], length(timevec)), out[[i]][,plot.variable])
}

range.lim = lapply(out, function(x) apply(x, 2, range))
range.lim = apply(do.call("rbind", range.lim), 2, range)
plot.variable = "Ni"
plot(0, 0, pch="", xlab = param.name, ylab= plot.variable, xlim=range(param.seq), ylim=range.lim[,plot.variable])
for(i in 1:length(param.seq)){
  points(rep(param.seq[i], length(timevec)), out[[i]][,plot.variable])
}