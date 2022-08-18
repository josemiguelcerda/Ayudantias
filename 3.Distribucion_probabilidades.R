

# 3. Distribucion de probabilidades ---------------------------------------
#
# Curso: Bioestadistica
#
# Prof.: Javier Lopatin | javier.lopatin@uai.cl
#
###

# Funciones a utilizar ----------------------------------------------------
?dnorm
?qnorm
?cumsum
?curve
?legend
?polygon
?text


# 1. Plot the standard normal distribution  -------------------------------
# (mu=0, delta=1)
 


## alternative 1: doing the math ourselves  --------------------------------

x = seq(-4,4,length=200)
y = 1/sqrt(2*pi)*exp(-x^2/2)
plot(x, y, type = "l", lwd = 4, col = "red", las = 1)


## alternative 2: using dnorm() --------------------------------------------

x = seq(-4,4,length=200)
y = dnorm(x)
plot(x, y, type = "l", lwd = 4, col = "blue", las = 1) # probar distintos argumentos
#https://www.learnbyexample.org/r-plot-function/


## alternative 3: using cruve() --------------------------------------------


curve(dnorm(x), -4, 4, col='green', ylab='y', lwd=4, las=1)


# 2. Plot the accumulative function of a standard normal distribut --------

curve(pnorm(x), -4, 4, col='green', ylab='y', lwd=4, las=1)


# 3. Plot the normal distribution with equal variance and changing --------

curve(dnorm(x, mean=0, sd=1), -4, 4, col='blue', ylab='f(x)', lwd=2, las=1)
curve(dnorm(x, mean=1, sd=1), -4, 4, col='red', lwd=2, add=TRUE)
curve(dnorm(x, mean=2, sd=1), -4, 4, col='green', lwd=2, add=TRUE)
legend('topleft', legend=c('mean=0; sd=1','mean=1; sd=1','mean=2; sd=1'),
       lwd=2, col=c('blue','red','green'), bty='n')

# 4. plot the normal distribution with equal mean and changing var --------

curve(dnorm(x, mean=0, sd=1), -4, 4, col='blue', ylab='f(x)', lwd=2, las=1)
curve(dnorm(x, mean=0, sd=1.5), -4, 4, col='red', lwd=2, add=TRUE)
curve(dnorm(x, mean=0, sd=2), -4, 4, col='green', lwd=2, add=TRUE)
legend('topleft', legend=c('mean=0; sd=1','mean=0; sd=1.5','mean=0; sd=2'),
       lwd=2, col=c('blue','red','green'), bty='n')


# 5. Whats the probability that a sample is located below + 1 sd i --------

# Use always the accumulative (probability) distribution d
# data with mu=0 and delta=1
# P(x <= 1)

prob <- pnorm(1, mean=0, sd=1)
prob # percentage
xmin <- -4
xmax <- 1

x=seq(xmin, xmax, length=200)
y=dnorm(x)

curve(dnorm(x), -4, 4, col='red', ylab='y', lwd=2, las=1, main='Distribucion')

# plot polygon
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(0,0.1, round(prob,2),col="Red")

### 6. mean +- 1 sd = 68% measurements
# P(-1 <= x <= 1)
prob <- pnorm(1)-pnorm(-1)
xmin <- -1
xmax <- 1

x=seq(xmin, xmax, length=200)
y=dnorm(x)

curve(dnorm(x), -4, 4, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin, x, xmax),c(0,y,0),col="gray")
text(0,0.1,round(prob,3),col="Red")

### 7. mean +- 2 sd = 95% measurements (+- 2 sd)
# P(-2 <= x <= 2)
prob <- pnorm(2)-pnorm(-2)

xmin <- -2
xmax <- 2

x=seq(xmin, xmax, length=200)
y=dnorm(x)

curve(dnorm(x), -4, 4, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(0,0.1,round(prob,3),col="Red")

### 8. Whats the probability of having a value over 1.4?
# P(x >= 1.4)
prob <- 1-pnorm(1.4)

xmin <- 1.4
xmax <- 4

x=seq(xmin, xmax, length=200)
y=dnorm(x)

curve(dnorm(x), -4, 4, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(1.8,0.03,round(prob,3),col="Red")

##############
### Tarea! ###
##############


# Usando la distribucion de largos de alas de mariposa que usamos en clases:

W <- c(3.3,3.5,3.6,3.6,3.7,3.8,
       3.8,3.8,3.9,3.9,3.9,4.0,
       4.0,4.0,4.0,4.1,4.1,4.1,
       4.2,4.2,4.3,4.3,4.4,4.5)

hist(W)

# 1. Asumiendo que la muestra es normal, haga un grafico de su distribucion de probabilidad
# 2. Obtenga la probabilidad de que un valor sea menor a 4.2
# 3. Obtenga la probabilidad de que un valor sea menor a 4.1 y mayor a 3.8
# 4. Obtenga la probabilidad de que un valor sea mayor a 3.5


################################################
############################
# Respuestas 

# 1
X <- mean(W)
S <- sd(W)
xminn <- min(W)
xmax <- max(W)


curve(dnorm(x, mean=X, sd=S), xminn, xmax, col='red', ylab='y', lwd=2, las=1)

# 2

prob <- pnorm(4.2, mean=X, sd=S)
xmin <- 3
xmax <- 4.2

x=seq(xmin, xmax, length=200)
y=dnorm(x, mean=X, sd=S)

curve(dnorm(x, mean=X, sd=S), 3, 5, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(4,0.5,round(prob,2),col="Red")

# 3

prob <- pnorm(4.1, mean=X, sd=S) -  pnorm(3.8, mean=X, sd=S)
xmin <- 3.8
xmax <- 4.1

x=seq(xmin, xmax, length=200)
y=dnorm(x, mean=X, sd=S)

curve(dnorm(x, mean=X, sd=S), 3, 5, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(3.95,0.5,round(prob,2),col="Red")

# 4

prob <- 1 -  pnorm(3.5, mean=X, sd=S)
xmin <- 3.5
xmax <- 5

x=seq(xmin, xmax, length=200)
y=dnorm(x, mean=X, sd=S)

curve(dnorm(x, mean=X, sd=S), 3, 5, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(3.95,0.5,round(prob,2),col="Red")





#### Como se puede hacer esto con otras distribuciones no normales?
# mirar requerimientos de la funcion de distribucion

?dt # Distribucion t Student
?dgamma # distribucion Gamma

# Ej., Gamma es una distribucion muy popular para distribuciones con valores solo positivos,
# con valores continuos y con skewness positiva (cola larga a la derecha):

curve(dgamma(x, shape=2, rate=2), 0, 4, lwd=2, col='purple')

# datos reales de peso una poblacion de peces

fishes <- read.table("https://raw.githubusercontent.com/shifteight/R-lang/master/TRB/data/fishes.txt",header=T)
names(fishes)

hist(fishes$mass, breaks=-0.5:16.5, col="green", main="")

# estimar los parametros de funcion (sin entrar en detalle, se puede ver en la descripcion de la funcion)
rate <- mean(fishes$mass)/var(fishes$mass)
shape <- rate*mean(fishes$mass)


curve(dgamma(x,shape,rate), 0, 15)

# plot ambos usando lines()
hist(fishes$mass, breaks=-0.5:16.5, col="green", main="")
lines(seq(0.01,15,0.01),length(fishes$mass)*dgamma(seq(0.01,15,0.01),shape,rate))

# Ahora se puede hace lo mismo que con otras distribuciones
# Cual es P(x <= 5)

prob <- pgamma(5,shape,rate)
xmin <- 0
xmax <- 5

x = seq(xmin, xmax, length=200)
y = dgamma(x,shape,rate)

curve(dgamma(x,shape,rate), 0, 15, col='red', ylab='y', lwd=2, las=1)
polygon(c(xmin,x,xmax),c(0,y,0),col="gray")
text(3,0.08,round(prob,2),col="Red")

