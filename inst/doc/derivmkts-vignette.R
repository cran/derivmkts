## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
rm(list=ls())
library(pander)
library(bookdown)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
##homedir <- '/home/rmcd/tex/d67/Rtutorial/'
options(digits=4)
figsize <- 4.5
opts_chunk$set(size='footnotesize',
               prompt=FALSE,
               comment=NA
              ##,fig.align='center',
              ## fig.width = figsize,
              ## fig.height=figsize,
               ## out.width='3.75in'
               )
opts_knit$set(#eval.after='fig.cap',
              prompt=TRUE,
              #renderer=renderer_latex(document=FALSE),
              size='footnotesize')
curr <- function(amt)  formatC(amt, format='f', digits=2)

## ----echo=FALSE---------------------------------------------------------------
library(derivmkts)
library(mnormt)
library(markdown)

opts_chunk$set(collapse=TRUE)

## -----------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
bscall(s, k, v, r, tt, d)
bsput(s, c(95, 100, 105), v, r, tt, d)


## ----bslist, echo=FALSE, eval=TRUE--------------------------------------------
bstbl <- data.frame(
    Function = c('bscall', 'bsput', 'bsopt', 'assetcall', 'assetput',
                 'cashcall', 'cashput'),
    Description = c('European call', 'European put', 'European call and put and associated Greeks: delta, gamma,
            vega, theta, rho, psi, and elasticity', 'Asset-or-nothing call',
            'Asset-or-nothing put', 'Cash-or-nothing call',
            'Cash-or-nothing put'))
pander(bstbl,
       caption = 'Black-Scholes related option pricing functions\\label{tab:bslist}',
      split.cell = 60, justify = c('center', 'left'))

## -----------------------------------------------------------------------------
H <- 115
bscall(s, c(80, 100, 120), v, r, tt, d)
## Up-and-in call
uicall(s, c(80, 100, 120), v, r, tt, d, H)
bsput(s, c(80, 100, 120), v, r, tt, d)
## Up-and-out put
uoput(s, c(80, 100, 120), v, r, tt, d, H)

## -----------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.04
callperpetual(s, c(95, 100, 105), v, r, d)
callperpetual(s, c(95, 100, 105), v, r, d, showbarrier=TRUE)


## -----------------------------------------------------------------------------
H <- 105
greeks(uicall(s, k, v, r, tt, d, H))


## -----------------------------------------------------------------------------
powercontract <- function(s, v, r, tt, d, a) {
    price <- exp(-r*tt)*s^a* exp((a*(r-d) + 1/2*a*(a-1)*v^2)*tt)
}

## -----------------------------------------------------------------------------
greeks(powercontract(s=40, v=.08, r=0.08, tt=0.25, d=0, a=2))

## -----------------------------------------------------------------------------
bullspread <- function(s, v, r, tt, d, k1, k2) {
    bscall(s, k1, v, r, tt, d) - bscall(s, k2, v, r, tt, d)
}
greeks(bullspread(39:41, .3, .08, 1, 0, k1=40, k2=45))


## ----bullgamma, fig.cap='Gamma for a 40-45 bull spread'-----------------------
sseq <- seq(1, 100, by=0.5)
greeks(bullspread(sseq, .3, .08, 1, 0, k1=40, k2=45),
            initcaps = TRUE, long = TRUE) %>%
    filter(greek == 'Gamma' ) %>% 
    ggplot(aes(x = s, y = value)) + geom_line()

## ----allgreeks, fig.cap='All option Greeks for a call and a put, computed using the greeks function'----
k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
S <- seq(.5, 200, by=.5)
Call <- greeks(bscall(S, k, v, r, tt, d), long = TRUE)
Put <- greeks(bsput(S, k, v, r, tt, d), long = TRUE)
ggplot(rbind(Call, Put), aes(x = s, y = value, color = funcname )) +
    geom_line() + facet_wrap(~ greek, scales = 'free_y') +
    scale_color_discrete(name = 'Option', labels = c('Call','Put' )) +
    scale_x_continuous('Stock', breaks =c(0, 100, 200)  ) +
    scale_y_continuous('Value') 

## -----------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03
binomopt(s, k, v, r, tt, d, nstep=3)
binomopt(s, k, v, r, tt, d, nstep=3, returnparams=TRUE)
binomopt(s, k, v, r, tt, d, nstep=3, putopt=TRUE)
binomopt(s, k, v, r, tt, d, nstep=3, returntrees=TRUE, putopt=TRUE)

## -----------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03; m <- 3
geomavgpricecall(s, 98:102, v, r, tt, d, m)
geomavgpricecall(s, 98:102, v, r, tt, d, m, cont=TRUE)
geomavgstrikecall(s, k, v, r, tt, d, m)


## -----------------------------------------------------------------------------
arithasianmc(s, k, v, r, tt, d, 3, numsim=5000, printsds=TRUE)


## -----------------------------------------------------------------------------
arithavgpricecv(s, k, v, r, tt, d, 3, numsim=5000)


## ---- include=FALSE-----------------------------------------------------------
compound.caption <- 'The timeline for a compound option: a call to buy a  put. The compound option expires at time $t_{1}$ and the  underlying asset is a put option that expires at time  $t_{2}$. At time $t_{1}$, the owner decides whether to pay  $k_{co}$ to buy a put option which has time to expiration $t_{2} - t_{1}$. At time $t_{2}$ the owner decides whether to exercise the put, selling the stock for the strike price of $k_{uo}$.'
#  \label{fig:compoundopt}
#\end{figure}


## -----------------------------------------------------------------------------
s <- 100; kuo <- 95; v <- 0.30; r <-  0.08; t1 <- 0.50; t2 <- 0.75; d <- 0
kco <- 3.50

calloncall(s, kuo, kco, v, r, t1, t2, d, returnscritical=TRUE)

## -----------------------------------------------------------------------------
putoncall(s, kuo, kco, v, r, t1, t2, d, returnscritical=TRUE)
callonput(s, kuo, kco, v, r, t1, t2, d, returnscritical=TRUE)
putonput(s, kuo, kco, v, r, t1, t2, d, returnscritical=TRUE)


## -----------------------------------------------------------------------------
mertonjump(s, k, v, r, tt, d, lambda=0.5, alphaj=-0.2, vj=0.3)
c(bscall(s, k, v, r, tt, d), bsput(s, k, v, r, tt, d))

## -----------------------------------------------------------------------------
coupon <- 8; mat <- 20; yield <- 0.06; principal <- 100; 
modified <- FALSE; freq <- 2
price <- bondpv(coupon, mat, yield, principal, freq)
price
bondyield(price, coupon, mat, principal, freq)
duration(price, coupon, mat, principal, freq, modified)
convexity(price, coupon, mat, principal, freq)


## -----------------------------------------------------------------------------
args(simprice)
simprice(long = TRUE)
simprice(long = FALSE)

## ----fivepaths, fig.cap='Five simulated paths for the same stock, no jumps.'----
s0 <- 100; v <- 0.3; r <- 0.10; d <- 0; tt <- 1
trials <- 5; periods <- 365; set.seed(1)
s <- simprice(s0 = s0, v = v, r = r, tt = tt, d = d, trials = trials,
              periods = periods, jump = FALSE, long = TRUE)
ggplot(s, aes(x = period, y = price, color = trial, group = trial)) +
    geom_line()

## ----fivejumpers, fig.cap='Five simulated paths for the same stock, which can jump.'----
s0 <- 100; v <- 0.3; r <- 0.10; d <- 0; tt <- 1
trials <- 5; periods <- 365; jump <- TRUE; lambda <- 2;
alphaj <- -0.1; vj <- 0.2; set.seed(1)
s <- simprice(s0 = s0, v = v, r = r, tt = tt, d = d, trials = trials,
              periods = periods, jump = jump, alphaj = alphaj,
              lambda = lambda, vj = vj, long = TRUE)
ggplot(s, aes(x = period, y = price, color = trial, group = trial)) +
    geom_line()

## ----negcoorsim, fig.cap='Two stocks for which the returns have a correlation of -.99.'----
set.seed(1)
s0 <- 100; r <- 0.08; tt <- 1;  d <- 0; jump <- FALSE
trials <- 1; periods <- 52;
v <- .3^2*matrix(c(1, -.99, -.99, 1), nrow = 2)
s <- simprice(s0 = s0, v = v, r = r, tt = tt, d = d, trials = trials,
              periods = periods, jump = jump, long = TRUE)
ggplot(s, aes(x = period, y = price, group = asset, color = asset)) +
    geom_line()


## ---- eval=TRUE---------------------------------------------------------------
set.seed(1)
tt <- 2; periods <- tt*365

vc <- vols <- diag(3)
diag(vols) <- c(.6, .2, .45)   ## volatilities
corrs <- c(.4, -.3, .25)
vc[lower.tri(vc)] <- corrs  ## correlations
vc <- t(vc) ## lower triangular becomes upper triangular
vc[lower.tri(vc)] <- corrs
v <- vols %*% vc %*% vols
v

s <- simprice(s0 = s0, v = v, r = r, tt = tt, d = d, trials = 1,
              periods = periods, jump = FALSE, long = TRUE)

threestocks <- s %>%
    filter(trial == 1) %>%
    group_by(asset) %>%
    mutate(ret = log(price/lag(price)),
           row = row_number()) %>%
    select(asset, period, ret) %>%
    spread(key = asset, value = ret )

var(threestocks[2:4], na.rm = TRUE)*365

## ----quincunx, fig.cap='Output from the Quincunx function'--------------------
par(mar=c(2,2,2,2))
quincunx(n=20, numballs=200, delay=0, probright=0.7)

## ----binomplot1, fig.cap='Basic option plot showing stock prices and nodes at which the option is exercised.'----
s0 <- 100; k <- 100; v <- 0.3; r <- 0.08; tt <- 2; d <- 0
binomplot(s0, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE)


## ----binomplot2, fig.cap='Same plot as Figure \\@ref(fig:binomplot1) except that values and arrows are added to the plot.'----
binomplot(s0, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE,
    plotvalues=TRUE, plotarrows=TRUE)

## ----binomplot3, fig.cap="Binomial plot when nstep is 40."--------------------
d <- 0.06
binomplot(s0, k, v, r, tt, d, nstep=40, american=TRUE)

## ----binomplot4, fig.cap="Binomial plot when nstep is 40 using the argument ylimval to focus on a subset."----
d <- 0.06
binomplot(s0, k, v, r, tt, d, nstep=40, american=TRUE, ylimval=c(75, 225))

