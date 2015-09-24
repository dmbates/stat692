
## ----preliminaries,echo=FALSE,results='hide',cache=FALSE-----------------
library(knitr)
library(ggplot2)
opts_chunk$set(fig.align='center',fig.pos="tb",cache=TRUE)


## ----expsamp-------------------------------------------------------------
    N <- 100000
    n <- 9


## ----replicate-----------------------------------------------------------
    set.seed(123)
    system.time(resRepl <- replicate(N, mean(rexp(n))))


## ------------------------------------------------------------------------
    str(resRepl)


## ----resLoop1------------------------------------------------------------
    set.seed(123)
    resLoop1 <- numeric(N)        # preallocate the result of correct size
    system.time(for (i in 1:N) resLoop1[i] <- mean(rexp(n)))


## ------------------------------------------------------------------------
    all.equal(resRepl, resLoop1)


## ----resLoop2------------------------------------------------------------
    set.seed(123)
    resLoop2 <- numeric()                   # empty numeric vector
    system.time(for (i in 1:N) resLoop2[i] <- mean(rexp(n)))


## ------------------------------------------------------------------------
    all.equal(resLoop2, resRepl)


## ----resLoop3------------------------------------------------------------
    set.seed(123)
    resLoop3 <- NULL                        # empty list
    system.time(for (i in 1:N) resLoop3 <- c(resLoop3, mean(rexp(n))))

    str(resLoop3)


## ----matrixsim-----------------------------------------------------------
    set.seed(123)
    str(MM <- matrix(rexp(n*N), nr = n))


## ----matrixsim2----------------------------------------------------------
    set.seed(123)
    system.time(resColMeans <- colMeans(matrix(rexp(n*N), nr = n)))


## ------------------------------------------------------------------------
    all.equal(resColMeans, resRepl)


## ----resApply------------------------------------------------------------
    resApply <- apply(MM, 2, mean)
    all.equal(resApply, resRepl)


## ----resApply2-----------------------------------------------------------
    set.seed(123)
    system.time(resApply <- apply(matrix(rexp(n * N), nr = n), 2, mean))


## ----sapply--------------------------------------------------------------
    set.seed(123)
    system.time(resSapply <- sapply(1:N, function(i) mean(rexp(n))))


## ----lapply--------------------------------------------------------------
    set.seed(123)
    str(unlist(lapply(1:N, function(i) mean(rexp(n)))))


## ------------------------------------------------------------------------
    system.time(unlist(lapply(1:N, function(i) mean(rexp(n)))))


## ----vapply--------------------------------------------------------------
    set.seed(123)
    str(vapply(1:N, function(i) mean(rexp(n)), 1))

    system.time(vapply(1:N, function(i) mean(rexp(n)), 1))


## ----librarybenchmark----------------------------------------------------
    library(rbenchmark)


## ------------------------------------------------------------------------
    fRepl <- function(n, N) replicate(N, mean(rexp(n)))
    fLoopPre <- function(n, N) {
        ans <- numeric(N)
        for(i in seq_len(N)) ans[i] <- mean(rexp(n))
        ans
    }
    fColMeans <- function(n, N) colMeans(matrix(rexp(n * N), nr=n))
    fApply <- function(n, N) apply(matrix(rexp(n * N), nr=n), 2, mean)
    fSapply <- function(n, N) sapply(integer(N), function(...) mean(rexp(n)))
    fLapply <- function(n, N) unlist(lapply(integer(N), function(...) mean(rexp(n))))
    fVapply <- function(n, N) vapply(integer(N), function(...) mean(rexp(n)), 1)
    N <- 1000

    benchmark(fColMeans(n,N),
              fVapply(n, N),
              fRepl(n, N),
              fLoopPre(n, N),
              fApply(n, N),
              fSapply(n, N),
              fLapply(n, N),
              columns = c("test", "elapsed", "relative", "user.self", "sys.self"),
              order = "elapsed")


