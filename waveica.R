cat("<!--------- waveica --------->\n")
norm_skip = FALSE
pacman::p_load(WaveICA)

if(!norm_skip){

  input_data = t(e_raw)
  colnames(input_data) = paste0("c", 1:ncol(input_data))


  library(WaveICA)
  normFact = function (fact, X, ref, refType, k = 20, t = 0.5, ref2 = NULL,
            refType2 = NULL, t2 = 0.5, alpha, ...)
  {
    if (fact == "stICA") {
      obj = unbiased_stICA(X, k, alpha = alpha)
      B = obj$B
      A = obj$A
    }
    else if (fact == "SVD") {
      obj = svd(X, nu = k, nv = k)
      A = obj$u %*% diag(obj$d[1:k], k)
      B = obj$v
    }
    else {
      stop("Factorization method should be SVD or stICA")
    }
    factR2 = R2(ref, B, refType, pval = T)
    idx = which(factR2$allpv < t)
    if (t < 0 | t > 1) {
      stop("t not in [0 1]")
    }
    if (!is.null(ref2)) {
      if (sum(t2 < 0 | t2 > 1)) {
        stop("t2 not in [0 1]")
      }
      factR2_2 = R2(ref2, B, refType2, pval = T)
      idx_2 = c()
      if (length(t2) != length(refType2)) {
        if (length(t2) == 1) {
          t2 = rep(t2, length(refType2))
        }
        else {
          stop("length(t2) sould be equal to 1 or length(refType2)")
        }
      }
      for (i in 1:length(refType2)) {
        idx_2 = c(idx_2, which(factR2_2$allpv[, i] < t2[i]))
      }
      idx2keep = intersect(idx, idx_2)
      # print(paste("Keeping", length(idx2keep), "cmpts with P value less than t2"))
      idx = setdiff(idx, idx2keep)
    }
    bestcmptA = A[, idx]
    bestcmptB = B[, idx]
    # print(paste("Removing", length(idx), "components with P value less than",t))
    Xn = X - bestcmptA %*% t(bestcmptB)
    R2 = factR2$allR2
    if (!is.null(ref2)) {
      R2 = cbind(R2, factR2_2$allR2)
    }
    return(list(Xn = Xn, R2 = R2, bestSV = bestcmptB, A = A,
                B = B))
  }
  WaveICA = function (data, wf = "haar", batch, group = NULL, K = 20,
            t = 0.05, t2 = 0.05, alpha = 0)
  {
    library(waveslim)
    level <- floor(log(nrow(data), 2))
    if (is.null(colnames(data))) {
      stop("data must have colnames")
    }
    coef <- list()
    for (k in 1:(level + 1)) {
      coef[[k]] <- matrix(NA, nrow(data), ncol(data))
    }
    for (j in 1:ncol(data)) {
      # cat(paste("######Decomposition", j, "########\n"))
      data_temp <- data[, j]
      x_modwt <- modwt(data_temp, wf = wf, n.levels = level)
      for (k in 1:(level + 1)) {
        coef[[k]][, j] <- x_modwt[[k]]
      }
    }
    index <- level + 1
    data_wave_ICA <- list()
    for (i in (1:index)) {
      # cat(paste("######### ICA", i, "#############\n"))
      data_coef <- coef[[i]]
      data_coef_ICA <- normFact(fact = "stICA", X = t(data_coef),
                                ref = batch, refType = "categorical", k = K,
                                t = t, ref2 = group, refType2 = "categorical",
                                t2 = t2, alpha)
      data_wave_ICA[[i]] <- t(data_coef_ICA$Xn)
    }
    index <- ncol(data)
    index1 <- length(data_wave_ICA)
    data_coef <- matrix(NA, nrow(data_wave_ICA[[1]]), index1)
    data_wave <- matrix(NA, nrow(data_wave_ICA[[1]]), ncol(data_wave_ICA[[1]]))
    for (i in 1:index) {
      # cat(paste("######Reconstruction", i, "########\n"))
      for (j in 1:index1) {
        data_coef[, j] <- data_wave_ICA[[j]][, i]
      }
      data_temp <- data[, i]
      data_coef <- as.data.frame(data_coef)
      colnames(data_coef) <- c(paste("d", 1:(index1 -
                                               1), sep = ""), paste("s", (index1 - 1),
                                                                    sep = ""))
      y <- as.list(data_coef)
      attributes(y)$class <- "modwt"
      attributes(y)$wavelet <- wf
      attributes(y)$boundary <- "periodic"
      data_wave[, i] <- imodwt(y) + mean(data_temp)
    }
    rownames(data_wave) <- rownames(data)
    colnames(data_wave) <- colnames(data)
    return(list(data_wave = data_wave))
  }


  WaveICA_result = WaveICA(input_data, batch = p$batch)

  e_norm = t(WaveICA_result$data_wave)

  cat("<!--------- waveica done.--------->\n")

}else{
  cat("<!--------- waveica skipped.--------->\n")
}

