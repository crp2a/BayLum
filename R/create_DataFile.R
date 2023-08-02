#'@title Prepare input data for subsequent BayLum Analysis
#'
#'@description The function pre-processes input data from BIN/BINX file, XSYG files
#'or [RLum.Analysis-class] objects for `'BayLum'`. The parameters
#'for the modelling are controlled by a to supplied YAML configuration file.
#'
#'@details
#'The function uses a single configuration file based on the YAML format and operates
#'in two modes:
#'
#'* (1) the YAML file contains the path to the files and the function
#'attempts to import them. In such a case, all files must be thoroughly prepared
#'(e.g., strictly follow the SAR protocol etc.).
#'
#'* (2) Alternatively, the YAML file contains no file paths but the data
#'were imported and processed before `create_DataFile()` was called (recommended).
#'Then the function is searching for objects with the sample name in the global environment.
#'Example: `samp1 <- Luminescence::read_BIN2R(...)` with `samp1` the sample name as specified
#'in the YAML file.
#'
#'For more details, please see the package vignette.
#'
#'@param config_file [character] (**required**): path to YAML configuration file; alternatively
#'the config_file can be a [list] similar to the R representation of the imported YAML file.
#'This enables on-the fly modifications
#'
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl University of Heidelberg (Germany),
#'in parts based on the functions `Generate_DataFile_MG()` and `Generate_DataFile()` by Claire Christophe
#'
#'@returns Returns a [list] that can be processed by the modelling functions of `'BayLum`
#'
#'@seealso [yaml::read_yaml], [Luminescence::read_BIN2R], [Luminescence::read_XSYG2R]
#'
#'@examples
#'##set path to YAML file
#' yaml_file <- system.file("extdata/example.yml", package = "BayLum")
#'
## BIN/BINX files
#' samp1_file <- system.file("extdata/samp1/bin.BIN", package = "BayLum")
#' samp2_file <- system.file("extdata/samp2/bin.BIN", package = "BayLum")
#'
#' ## import BIN files
#' samp1 <<- Luminescence::read_BIN2R(samp1_file, verbose = FALSE) |>
#'  subset(POSITION == 2 & GRAIN == 32)
#' samp2 <<- Luminescence::read_BIN2R(samp2_file, verbose = FALSE) |>
#'  subset(POSITION == 2 & GRAIN == 32)
#'
#' ## create file
#' create_DataFile(yaml_file)
#'
#'@note
#'@export
create_DataFile <- function(
    config_file,
    verbose = TRUE
){

  ## TODO
  ## - adjust vignette

# Import data -------------------------------------------------------------
  ## This function has two modes:
  ## (1) if no file names are provided, the function treats sample names as
  ## objects names that should be available on in the working environment
  ## (2) if file names are present, the function tries to import those files
  ## However, little treatment is done and the files should be correct
  if (verbose) cli::cli_rule("BayLum - [create_DataFile()]")

  ## read YAML file
  if (verbose) cli::cli_progress_step("Load YAML configuration file ... ", spinner = TRUE)
    ## try list
    if(inherits(config_file, "list"))
      config <- config_file
    else
      config <- yaml::read_yaml(file = config_file, readLines.warn = FALSE)

    ## make sure config is always a list
    if(!inherits(config[[1]], "list"))
      config <- list(config)

    ## check structure and names of the YAML file
    for(i in seq_along(config)) {
      ## top level
      t <- names(config[[i]]) %in% c("sample", "files","settings")
      if(!all(t))
        stop(
          paste0(
            "[create_DataFile()] Missing or wrong paramter in YAML file on top level!\n\t-> YAML entry: ", i, "\n\t-> Parameter: ",
               paste(names(config[[i]])[!t], collapse = ", ")),
             call. = FALSE)

      ## settings level
      t <- names(config[[i]]$settings) %in% c("dose_source", "dose_env", "rules")
      if(!all(t))
        stop(
          paste0(
            "[create_DataFile()] Missing or wrong paramter in YAML file on settings level!\n\t-> YAML entry: ", i, "\n\t-> Parameter: ",
                 paste(names(config[[i]]$settings)[!t], collapse = ", ")),
          call. = FALSE)

      ## rules level
      t <- names(config[[i]]$settings$rules) %in% c(
        "beginSignal", "endSignal", "beginBackground", "endBackground",
        "beginTest", "endTest", "beginTestBackground", "endTestBackground",
        "inflatePercent", "nbOfLastCycleToRemove")
      if(!all(t))
        stop(
          paste0(
            "[create_DataFile()] Missing or wrong paramter in YAML file on rules level!\n\t-> YAML entry: ", i, "\n\t-> Parameter: ",
                 paste(names(config[[i]]$settings$rules)[!t], collapse = ", ")),
          call. = FALSE)

    }

  ## determine number of samples
  n_samples <- length(config)

  ## fix sample names ... better safe than a weird error messages
  ## this way the sample names are safe to use (more or less)
  if (verbose) cli::cli_progress_step("Sanitize sample names ... ", spinner = TRUE)
  for (i in seq_len(n_samples)) {
    ## replace all whitespace with "_"
    config[[i]]$sample <- gsub(
      x = config[[i]]$sample,
      pattern = " ",
      replacement = "_",
      fixed = TRUE)

    ## replace all odd characters with nothing
    regmatches(
      x = config[[i]]$sample,
      regexpr(pattern = "[a-zA-Z0-9\\_\\-]+", text = config[[i]]$sample, perl = TRUE),
      invert = TRUE) <- ""

  }

  ## make list of objects; depending on the entry case
  if (verbose) cli::cli_progress_step("Generate object list ... ", spinner = TRUE)
  object_list <- lapply(seq_len(n_samples), function(x) {
    ## use objects -------
    if (is.null(config[[x]]$files)) {
      ## assign sample name if it exists in the working environment
      if (inherits(try(out <- get(x = config[[x]]$sample), silent = TRUE), "try-error"))
        stop(
          paste0("[create_DataFile()] <", config[[x]]$sample,
                 "> is not a valid object in the working environment!"),
          call. = FALSE)

    } else {
      ## import files as specified -------
      out <- lapply(suppressWarnings(normalizePath(tolower(config[[x]]$files))), function(x) {
        if (!file.exists(x))
          stop(paste0("[create_DataFile()] file <", x, "> does not exist!"), call. = FALSE)

        ## select function based on file
        switch(
          strsplit(rev(basename(x)), split = ".", fixed = TRUE)[[1]][1],
          bin = Luminescence::read_BIN2R(x, fastForward = TRUE, verbose = FALSE, zero_data.rm = TRUE),
          xsyg = Luminescence::read_XSYG2R(x, fastForward = TRUE, verbose = FALSE)
        ) |> Luminescence::get_RLum(recordType = c("OSL", "IRSL"), drop = FALSE)

      }) |> unlist()

    }

    ## account for the output object nature, which might be a double list (or not)
    if (!inherits(out, "list"))
        out <- list(out)

    ## check the nature of the files
    ## if Risoe.BINfileData convert automatically
    if (any(vapply(out, function(x) inherits(x, "Risoe.BINfileData"), logical(1)))) {
      if (verbose) cli::cli_alert_warning(paste0("... ", config[[x]]$sample,": Risoe.BINfileData instead of RLum.Analysis detected ... "))
      if (verbose) cli::cli_progress_step(paste0("... ", config[[x]]$sample,": Convert Risoe.BINfileData to RLum.Analysis ... ", spinner = TRUE))
      out <- lapply(out, function(x) {
        if (inherits(x,"Risoe.BINfileData"))
          Luminescence::Risoe.BINfileData2RLum.Analysis(x, txtProgressBar = FALSE)
        else
          x
      }) |> unlist(recursive = FALSE)
    }

    ## now everything must be of type RLum.Analysis ... crash if not
    if ((any(vapply(out, function(x) !inherits(x, "RLum.Analysis"), logical(1)))))
      stop("[create_DataFile()] Wrong object type. All input data must be of type RLum.Analysis!]",
           call. = FALSE)

    ## sometimes datasets are empty due to previous selections
    ## by the user ... remove such datasets
    if (any(vapply(out, length, numeric(1)) == 0)) {
      warning("[create_DataFile()] Empty RLum.Analysis records found and removed ... ",
              call. = FALSE,
              immediate. = FALSE)
      out[sapply(out, length) == 0] <- NULL
    }

    ## remove dose points according to the config
    if(!is.null(config[[x]]$settings$rules$nbOfLastCycleToRemove) &&
       config[[x]]$settings$rules$nbOfLastCycleToRemove > 0) {
        ## how many records?
        rm <- config[[x]]$settings$rules$nbOfLastCycleToRemove * 2

        ## remove the records
        if(verbose) cli::cli_progress_step(paste0(
          "... ", config[[x]]$sample, ": Remove ", rm, " dose points ... "),
          spinner = TRUE)
        out <- lapply(out, function(o) {
           o@records <- o@records[1:(length(o@records) - rm)]
           return(o)
        })

    }

    return(out)
  })

# Additional checks -------------------------------------------------------
  if (verbose) cli::cli_progress_step("Run additional checks ... ", spinner = TRUE)
  ## sample names must be unique
  chk_samples <- vapply(config, function(x) x$sample, character(1))
  if (any(duplicated(chk_samples)))
    stop(
      paste0("[create_DataFile()] Sample names must be unique, found the follwing duplicated entries: ",
             paste(chk_samples[duplicated(chk_samples)], collapse = ", ")),
      call. = FALSE)

  ## check that all curves have the same length
  for (i in seq_along(object_list)) {
    tmp_n_channels <-
      vapply(unlist(Luminescence::get_RLum(object_list[[i]])), function(x) nrow(x@data), numeric(1))

     if (length(unique(tmp_n_channels)) > 1)
       stop(paste0(
         "[create_DataFile()] All curves within one sample must have the same number of channels/data points!\n",
         "\t-> Please check sample <", config[[i]]$sample, "> were I found the following number of channels: ",
         paste(unique(tmp_n_channels), collapse = ", ")),
            call. = FALSE)

     ## check integration limits against the number of channels
     if (max(unlist(config[[i]]$settings$rules[1:4])) > tmp_n_channels[1])
       stop(paste0("[create_DataFile()] Check your integration limit settings for sample <",
                   config[[i]]$sample, ">. Your curves do not have more than ",
                   tmp_n_channels[1], " channels!"),
            call. = FALSE)

  }

# Extract information -----------------------------------------------------
  ## we have to mimic the original format by Claire otherwise everything
  ## else subsequent needs to be modified

  if (verbose) cli::cli_progress_step("Extract information and calculate values ... ", spinner = TRUE)
  ## K -------
  ## get number of regeneration points for all datasets
  K <- vapply(seq_along(object_list), function(x){
    ## get the number of all records in each dataset
    tmp_K <- vapply(object_list[[x]], length, numeric(1))

    ## remove the specified number of last points
    tmp_K <- tmp_K

    ## three integrity checks
    ## (1) less than four? (two records)
    if (any(tmp_K < 4))
      stop("[create_DataFile()] All records must have at least two dose points!", call. = FALSE)

    ## (2) all must be of same length
    if (length(unique(tmp_K)) > 1)
      stop("[create_DataFile()] For one sample all records must have the same number of curves!",
           call. = FALSE)

    ## (3) they must be a multiple of two
    if (any(tmp_K %% 2 > 0))
      stop("[create_DataFile()] Curves must be multiple of two!", call. = FALSE)

    tmp_K[1]/2
  }, numeric(1))

  ## J ---------
  ## the number of records in each object list
  J <- vapply(object_list, length, numeric(1))

  ## dLab --------
  ## the source dose rate of the lab
  dLab <- vapply(config, function(x) unlist(x$settings$dose_source), numeric(2))

  ## ddot_env -------
  ## the environmental dose rate
  ddot_env <- vapply(config, function(x) unlist(x$settings$dose_env), numeric(2))

  ## LT and sLT --------
  ## calculate the LT and sLT values
  LxTx_list <- lapply(seq_along(object_list), function(x){
    ## we use Claire's approach otherwise the risk of mistakes is too high
    rule <- as.numeric(config[[x]]$settings$rules)

    ## calculate multiple of background signal
    prop_Lx <- length(c(rule[3]:rule[4]))/length(c(rule[1]:rule[2]))
    prop_Tx <- length(c(rule[7]:rule[8]))/length(c(rule[5]:rule[6]))

    ## calculate the Lx related value
      ## Signal Lx integral
      Lx <- t(vapply(object_list[[x]], function(y) {
        vapply(seq(1,length(y),2), function(z) sum(y@records[[z]]@data[rule[1]:rule[2],2]), numeric(1))

      }, numeric(K[x])))

      ## Background Lx integral
      Bx <- t(vapply(object_list[[x]], function(y) {
        vapply(seq(1,length(y),2), function(z) sum(y@records[[z]]@data[rule[3]:rule[4],2])/prop_Lx, numeric(1))

      }, numeric(K[x])))

      ## net Lx
      net_Lx <- Lx - Bx

    ## calculate the Tx related value
      ## Tx signal
      Tx <- t(vapply(object_list[[x]], function(y) {
        vapply(seq(2,length(y),2), function(z) sum(y@records[[z]]@data[rule[5]:rule[6],2]), numeric(1))

      }, numeric(K[x])))

      ## Tx background
      BTx <- t(vapply(object_list[[x]], function(y) {
        vapply(seq(2,length(y),2), function(z) sum(y@records[[z]]@data[rule[7]:rule[8],2])/prop_Tx, numeric(1))

      }, numeric(K[x])))

      ## net Tx
      net_Tx <- Tx - BTx

    ## calculate Lx/Tx
    LxTx <- net_Lx/net_Tx

    # calculate error of LxTx which will become sLT
    LxTx_err <- LxTx[,1:ncol(LxTx)] * sqrt(
      ((Lx * (1 + rule[9]^2 * Lx) + Bx * ( 1 + rule[9]^2 * Bx)) /
         (Lx - Bx)^2) + ((Tx * (1 + rule[9]^2 * Tx) + BTx * (1 + rule[9]^2 * BTx))/(Tx - BTx)^2))

    ## returned combined list
    return(list(LT = LxTx, sLT = LxTx_err))

  }) |> unlist(recursive = FALSE)

  ## ITimes -------
  ITimes <- lapply(seq_along(object_list), function(x) {
    ## extract irradiation times
    IRR_TIME <- Luminescence::merge_RLum(
      Luminescence::extract_IrradiationTimes(object_list[[x]]))$irr.times$IRR_TIME

    ## remove natural (0 dose)
    IRR_TIME <- IRR_TIME[c(1, seq(3,length(IRR_TIME),2))]
    IRR_TIME <- IRR_TIME[-seq(1,length(IRR_TIME), K[x])]

    ## create matrix with irradiation
    matrix(IRR_TIME, nrow = J[x], byrow = TRUE)

  })

  ## regDose -------
  regDose <- lapply(seq_along(ITimes), function(x) ITimes[[x]] * dLab[1,x])

# Construct final list ----------------------------------------------------
  if (verbose) cli::cli_progress_step("Combine list ... ", spinner = TRUE)
  l <- list(
    LT = LxTx_list[names(LxTx_list) == "LT"],
    sLT = LxTx_list[names(LxTx_list) == "sLT"],
    ITimes = ITimes,
    dLab = dLab,
    ddot_env = ddot_env,
    regDose = regDose,
    J = J,
    K = K - 1)

  ## add attribute originator
  attr(l, "originator") <- "create_DataFile"

  ## return
  return(l)

}
