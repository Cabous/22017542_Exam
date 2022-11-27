# Optimisation function

optim_foo <- function(type = "mv", mu, Sigma, bvec, Amat, printmsg = TRUE){
    # Check to see if type is correct
    if(type != "mv" & type != "minvol" & type != "maxdecor" & type != "sharpe") stop("Wrong type")
    # Wrap function
    Safe_Optim <- purrr::safely(quadprog::solve.QP)
    # Determine optimisation weights
    if(type == "mv"){
        w.opt <-
            Safe_Optim(Dmat = Sigma,
                       dvec = mu,
                       Amat = Amat,
                       bvec = bvec,
                       meq = meq)
    }
    if(type == "minvol"){
        w.opt <-
            Safe_Optim(Dmat = Sigma,
                       dvec = rep(0, nrow(Sigma)),
                       Amat = Amat,
                       bvec = bvec,
                       meq = meq)
    }
    if(type == "maxdecor"){
        Rho <- cov2cor(Sigma)
        w.opt <-
            Safe_Optim(Dmat = Sigma,
                       dvec = rep(0, nrow(Sigma)),
                       Amat = Amat,
                       bvec = bvec,
                       meq = meq)
    }
    if(type == "sharpe"){
        Amat[,1] <- mu
        w.opt <- Safe_Optim(Dmat = Sigma,
                            dvec = rep(0, nrow(Sigma)),
                            Amat = Amat,
                            bvec = bvec,
                            meq = meq)
    }

    # Create Weights Table output
    if(is.null(w.opt$error)){
        result.QP <- tibble(stocks = colnames(Sigma), weight = w.opt$result$solution) %>%
            rename(!!type := weight)
    }  else {
        result.QP <- tibble(stocks = colnames(Sigma), weight = 1/ncol(Sigma)) %>%
            rename(!!type := weight)
    }
    result.QP
}
