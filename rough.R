fmri.stimulus <- function(scans = 1,
                          onsets = c(1),
                          durations = c(1),
                          TR = 2,
                          times = FALSE,
                          sliceorder = NULL,
                          type = c("canonical", "gamma", "boxcar", "user"),
                          par = NULL,
                          scale = 10,
                          hrf = NULL,
                          verbose = FALSE) {

  ## match the type of HRF function to the defaults
  type <- match.arg(type)
  if ((type == "user") && (class(hrf) != "function"))
    stop("HRF type is user, but specified hrf is not a function!")

  ## is information for slice timing present ?

  ## re-calculate design spec. from Scans to Time
  if (!times) {
    onsets <- onsets*TR
    durations <- durations*TR
  }
  slicetiming <- !is.null(sliceorder)
  if(slicetiming) {
     nslices <- length(sliceorder)
     scale <- max(scale,nslices)
     # resulution should at least reflect slice times
     slicetimes <- (1:nslices-1)[sliceorder]/TR*scale
  }
  ## consider microtime
  onsets <- onsets * scale
  durations <- durations * scale
  scans <- scans * TR * scale
  TR <- TR/scale
  slicetiming <- !is.null(sliceorder)
  if(slicetiming) {
     nslices <- length(sliceorder)
     slicetimes <- ceiling((1:nslices-1)[sliceorder]/nslices*scale)
  }
  ## normalization constant for the user-defined to make stimuli comparable
  if (type == "user") shrf <- sum(hrf(0:(ceiling(scans)-1)/scale))

  ## basic consistency checks for design spec
  no <- length(onsets)
  if (length(durations) == 1) {
    durations <- rep(durations, no)
  } else if (length(durations) != no)  {
    stop("Length of duration vector does not match the number of onsets!")
  }

  ## create boxcar function
  if(slicetiming){
    stimulus <- matrix(0,ceiling(scans),nslices)
    for (j in 1:nslices) for(i in 1:no)
       stimulus[pmax(1,onsets[i]:(onsets[i]+durations[i]-1)-slicetimes[j]),j] <- 1
  } else {
    stimulus <- rep(0, ceiling(scans))
    for (i in 1:no) stimulus[onsets[i]:(onsets[i]+durations[i]-1)] <- 1
  }
  ## define some HRF
  ## t: time in seconds
  .canonicalHRF <- function(t, par = NULL) {
    ttpr <- par[1] * par[3]
    ttpu <- par[2] * par[4]
    (t/ttpr)^par[1] * exp(-(t-ttpr)/par[3]) - par[5] * (t/ttpu)^par[2] * exp(-(t-ttpu)/par[4])
  }

  ## t: time in seconds
  .gammaHRF <- function(t, par = NULL) {
    th <- 0.242 * par[1]
    1/(th*factorial(3)) * (t/th)^3 * exp(-t/th)
  }

  ## prepare parameters for HRF
  if (type == "canonical") {
    if (is.null(par)) par <- c(6, 12, 0.9, 0.9, 0.35)
    if (!is.numeric(par[1:5]) || any(is.na(par[1:5]))) {
      warning("parameter vector c(", paste(par, collapse=", "),
              ") for canonical HRF is not numeric or has unsufficient length (<5)!\nUsing default parameters!",
              paste(par <- c(6, 12, 0.9, 0.9, 0.35), collapse=", "))
    }
  } else if (type =="gamma") {
    if (is.null(par)) par <- 4
    if (!is.numeric(par[1])) {
      warning("parameter ", par[1],
              " for gamma HRF is not numeric!\nUsing default parameter!", par <- 4)
    }
  }

  ## convolve with chosen HRF
  if (verbose) cat("fmriStimulus: Using", type, "HRF for stimulus creation\n")
  y <- switch(type,
              canonical = .canonicalHRF(0:(20*scale)/scale, par)/2.885802,
              gamma = .gammaHRF(0:(28*scale)/scale, par),
              boxcar = scale,
              user = hrf(0:(ceiling(scans)-1)/scale)/shrf)
  if(slicetiming) {
     for(j in 1:nslices) {
      stimulus[,j] <-
        convolve(stimulus[,j], rev(y), type="open")[1:dim(stimulus)[1]]
      }
      ## final operations to get BOLD at scan time
    stimulus <- stimulus[unique((scale:scans)%/%(scale^2*TR))*scale^2*TR,]/(scale^2*TR)
  } else {
     stimulus <- convolve(stimulus, rev(y), type="open")
    ## final operations to get BOLD at scan time
     stimulus <- stimulus[unique((scale:scans)%/%(scale^2*TR))*scale^2*TR]/(scale^2*TR)
  }
  ## return mean corrected stimulus function
  if(slicetiming) sweep(stimulus,2,apply(stimulus,2,mean),"-") else stimulus - mean(stimulus)
}

fmri.design <- function(stimulus,
                        order = 2,
                        cef = NULL,
                        verbose = FALSE) {

  ## create matrices and make consistency checks
  if(is.list(stimulus)){
     nstimulus <- length(stimulus)
     dims <- dim(stimulus[[1]])
     if(!is.null(dims)){
        slicetiming <- TRUE
        nslices <- dims[2]
        scans <- dims[1]
        stims <- array(0,c(scans,nstimulus,nslices))
        for(j in 1:nstimulus){
           if(!all(dim(stimulus[[j]])==dims)) stop("Inconsistent dimensions in stimulus list")
           stims[,j,] <- as.matrix(stimulus[[j]])
        }
     }
  } else {
     slicetiming <- FALSE
     stims <- as.matrix(stimulus)
     dims <- dim(stims)
     nstimulus <- dims[2]
     scans <- dims[1]
     nslices <- 1
     dim(stims) <- c(dims,nslices)
  }
  if (!is.null(cef)) {
    cef <- as.matrix(cef)
    if (dim(stims)[1] != dim(cef)[1])
      stop("Length of stimuli ", dim(stimulus)[1], " does not match the length of confounding effects ", dim(cef)[1])
    neffects <- dim(cef)[2]
  } else {
    neffects <- 0
  }


  ## create empty design matrix and fill first columns with Stimulus

    dz <- c(scans, nstimulus + neffects + order + 1, nslices)
    z <- array(0, dz)
    if (verbose) cat("fmriDesign: Adding stimuli to design matrix\n")
    z[, 1:nstimulus,] <- stims

  ## this is the mean effect
  if (verbose) cat("fmriDesign: Adding mean effect to design matrix\n")
  z[, neffects + nstimulus + 1,] <- 1
  ## now confounding effects
  if (neffects != 0) {
    if (verbose) cat("fmriDesign: Adding", neffects, "confounding effect(s) to design matrix\n")
    z[, (nstimulus + 1):(nstimulus + neffects),] <- cef
}

  ## now the polynomial trend (make orthogonal to stimuli)
  if (order != 0) {
    if (verbose) cat("fmriDesign: Adding polynomial trend of order", order, "to design matrix\n")
    for(k in 1:nslices) {
      ortho <- t(stims[,,k]) %*% stims[,,k]
       hz <- numeric(nstimulus)
       for (i in (neffects + nstimulus + 2):(neffects + nstimulus + order + 1)) {
          z[, i, k] <- (1:scans)^(i - nstimulus - neffects - 1)
       z[, i, k] <- z[, i, k]/mean(z[,i,k])
  }
  }
}
if(dim(z)[3]==1) dim(z) <- dim(z)[1:2]
  ## thats it!
  z
}