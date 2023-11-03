# Read in data ------------------------------------------------------------
read_data_excel <- function(rel_directory, pattern) {
    files <- dir(paste0(dirname(getwd()),"/", rel_directory), pattern = pattern, full.names = FALSE)
    df_list <- vector("list", length(files))
    for (fname in files) {
        df_list[[fname]] <- read_excel(paste0(dirname(getwd()),"/", rel_directory ,fname))
    }
    names(df_list) <- paste0("", gsub(pattern,"",names(df_list)))
    return(df_list)
}


# Mean Median -------------------------------------------------------------

# function example - get measures of central tendency
# and spread for a numeric vector x. The user has a
# choice of measures and whether the results are printed.
mysummary <- function(x,npar=TRUE,print=TRUE) {
    if (!npar) {
        center <- mean(x); spread <- sd(x)
    } else {
        center <- median(x); spread <- mad(x)
    }
    if (print & !npar) {
        cat("Mean=", center, "\n", "SD=", spread, "\n")
    } else if (print & npar) {
        cat("Median=", center, "\n", "MAD=", spread, "\n")
    }
    result <- list(center=center,spread=spread)
    return(result)
}


# Correlation table -------------------------------------------------------

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower", "none"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  else if(removeTriangle[1]=="none"){
    Rnew <- as.matrix(Rnew)
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}


# CSplit ------------------------------------------------------------------

cSplit <- function(indt, splitCols, sep = ",", direction = "wide",
                   makeEqual = NULL, fixed = TRUE, drop = TRUE,
                   stripWhite = FALSE) {
  message("`cSplit` is now part of the 'splitstackshape' package (V1.4.0)")
  ## requires data.table >= 1.8.11
  require(data.table)
  if (!is.data.table(indt)) setDT(indt)
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (any(!vapply(indt[, splitCols, with = FALSE],
                  is.character, logical(1L)))) {
    indt[, eval(splitCols) := lapply(.SD, as.character),
         .SDcols = splitCols]
  }

  if (length(sep) == 1)
    sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) {
    stop("Verify you have entered the correct number of sep")
  }

  if (isTRUE(stripWhite)) {
    indt[, eval(splitCols) := mapply(function(x, y)
      gsub(sprintf("\\s+%s\\s+|\\s+%s|%s\\s+",
                   x, x, x), x, y),
      sep, indt[, splitCols, with = FALSE],
      SIMPLIFY = FALSE)]
  }

  X <- lapply(seq_along(splitCols), function(x) {
    strsplit(indt[[splitCols[x]]], split = sep[x], fixed = fixed)
  })

  if (direction == "long") {
    if (is.null(makeEqual)) {
      IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
      makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                          FALSE, TRUE)
    }
  } else if (direction == "wide") {
    if (!is.null(makeEqual)) {
      if (!isTRUE(makeEqual)) {
        message("makeEqual specified as FALSE but set to TRUE")
        makeEqual <- TRUE
      }
      makeEqual <- TRUE
    } else {
      makeEqual <- TRUE
    }
  }
  if (isTRUE(makeEqual)) {
    SetUp <- lapply(seq_along(X), function(y) {
      A <- vapply(X[[y]], length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(X[[y]]))
    })
    Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]),
                       use.names = FALSE))
    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })
    if (direction == "wide") {
      X <- lapply(seq_along(X), function(x) {
        colnames(X[[x]]) <- paste(splitCols[x],
                                  sequence(ncol(X[[x]])),
                                  sep = "_")
        X[[x]]
      })
      if (isTRUE(drop)) {
        cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
      } else {
        cbind(indt, do.call(cbind, X))
      }
    } else {
      indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
      X <- lapply(X, function(y) as.vector(t(y)))
      indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
    }
  } else {
    Rep <- vapply(X[[1]], length, integer(1L))
    indt <- indt[rep(sequence(nrow(indt)), Rep)]
    indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
  }
}



# harmonic mean -----------------------------------------------------------

## The harmonic mean function
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}



# create network ----------------------------------------------------------

cpc_net <- function(year, data = data) {
  data_cpc_co <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>%
    .[!grepl("\\|", .$`CPC - Current - DWPI`),] %>%
    .[!is.na(.$`CPC - Current - DWPI`),] %>%
    dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d"))
  data_cpc_co$Pub_year <- format(data_cpc_co$`Publication Date`, format = "%Y")
  data_cpc_co <- data_cpc_co %>%
    select(`Publication Number`, `CPC - Current - DWPI`, Pub_year) %>%
    filter(
      if (year == "all") {
        Pub_year == "2015" | Pub_year == "2016" | Pub_year == "2017" | Pub_year == "2018" | Pub_year == "2019" | Pub_year == "2020" | Pub_year == "2021" | Pub_year == "2022"
      } else {
        Pub_year == year
      }
    ) %>%
    mutate(`CPC - Current - DWPI` = sub("^(....)0{1,3}", "\\1", .$`CPC - Current - DWPI`)) %>%
    fastDummies::dummy_cols(., select_columns = "CPC - Current - DWPI") %>%
    select(-`CPC - Current - DWPI`) %>% data.frame(.) %>%
    group_by(Publication.Number) %>%
    summarise_if(is.numeric,sum) %>%
    mutate(sum = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>%
    as.data.frame()
  names(data_cpc_co) <- gsub(".*_","",names(data_cpc_co))
  data_cpc_co_crossprod <- data_cpc_co[,-1] %>%
    select(-sum) %>%
    as.matrix(.) %>%
    crossprod(.)
  diag(data_cpc_co_crossprod) <- 0
  data_cpc_co_crossprod <- data.frame(data_cpc_co_crossprod) %>%
    mutate(Pub_Num = rownames(data_cpc_co_crossprod)) %>%
    pivot_longer(-Pub_Num) %>%
    group_by(Pub_Num) %>%
    #filter(value > 7) %>% #set value for minimum cooccureences
    pivot_wider(names_from = name, values_from=value)

  data_links <- data_cpc_co_crossprod %>%
    pivot_longer(., cols = -Pub_Num) %>%
    filter(., value > 0) %>%
    rename(source = Pub_Num,
           target = name,
           importance = value) %>%
    group_by(source, target) %>%
    dplyr::summarise(importance = sum(importance)) %>%
    as.data.frame() %>%
    .[!duplicated(data.frame(t(apply(.[1:2], 1, sort)))),]
  # remove links between same CPC classes
  data_links$source_letter <- substr(data_links$source, 1, 1)
  data_links$target_letter <- substr(data_links$target, 1, 1)
  data_links <- data_links[!as.character(data_links$source_letter) >= as.character(data_links$target_letter ),]
  nodes <-
    data_links %>%
    select(source, target)
  nodes <-
    data.frame(source = c(nodes[,"source"], nodes[,"target"])) %>%
    mutate(Subclass = substr(.$source, 1, 4)) %>%
    mutate(Subclass = toupper(Subclass)) %>%
    mutate(Subclass = if_else(grepl("A61B",.$Subclass),'(A61B) Diagnostic instruments and processes',
                              if_else(grepl("A61F",.$Subclass),'(A61F) Filters implantable into blood vessels',
                              if_else(grepl("A61H",.$Subclass),'(A61H) Physical therapy apparatus',
                              if_else(grepl("A61K",.$Subclass),'(A61K) Preparations for pharmaceutical products',
                              if_else(grepl("A61M",.$Subclass),'(A61M) Devices for introducing media into or onto the body',
                              if_else(grepl("A61N",.$Subclass),'(A61N) Electrotherapy',
                              if_else(grepl("A61P",.$Subclass),'(A61P) Therapeutic activity of chemical compounds or medicinial preparations',
                              if_else(grepl("A63F",.$Subclass),'(A63F) Gaming',
                              if_else(grepl("A63H",.$Subclass),'(A63H) Toys',
                              if_else(grepl("B25J",.$Subclass),'(B25J) Manipulators',
                              if_else(grepl("C07K",.$Subclass),'(C07K) Peptides',
                              if_else(grepl("C12N",.$Subclass),'(C12N) Microorganisms or enzymes',
                              if_else(grepl("C12Q",.$Subclass),'(C12Q) Measuring or testing processes involving enzymes, nucleic acids, or microorganisms',
                              if_else(grepl("G01N",.$Subclass),'(G01N) Investigating or analyzing materials by determining their physical or chemical properties',
                              if_else(grepl("G01P",.$Subclass),'(G01P) Measuring linear or angular speed, acceleration, or shock',
                              if_else(grepl("G01R",.$Subclass),'(G01R) Measuring electric variables',
                              if_else(grepl("G01V",.$Subclass),'(G01V) Geophysics',
                              if_else(grepl("G05B",.$Subclass),'(G05B) Control or regulating systems in general',
                              if_else(grepl("G06F",.$Subclass),'(G06F) Electronic digital data processing',
                              if_else(grepl("G06K",.$Subclass),'(G06K) Graphical data reading',
                              if_else(grepl("G06N",.$Subclass),'(G06N) Computing arrangements based on computational models',
                              if_else(grepl("G06Q",.$Subclass),'(G06Q) Data processing systems or methods for administrative purposes',
                              if_else(grepl("G06T",.$Subclass),'(G06T) Image data processing or generation',
                              if_else(grepl("G06V",.$Subclass),'(G06V) Image or video recognition or understanding',
                              if_else(grepl("G07F",.$Subclass),'(G07F) Coin sorting',
                              if_else(grepl("G08B",.$Subclass),'(G08B) Signalling or calling systems',
                              if_else(grepl("G09B",.$Subclass),'(G09B) Educational or demonstration appliances',
                              if_else(grepl("G10L",.$Subclass),'(G10L) Speech analysis or synthesis',
                              if_else(grepl("G16C",.$Subclass),'(G16C) Computational chemistry',
                              if_else(grepl("G16H",.$Subclass),'(G16H) Healthcare informatics',
                              if_else(grepl("G16Y",.$Subclass),'(G16Y) ICT for Internet of Things',
                              if_else(grepl("G16Z",.$Subclass),'(G16Z) ICT for specific applications',
                              if_else(grepl("H04B",.$Subclass),'(H04B) Transmission',
                              if_else(grepl("H04L",.$Subclass),'(H04L) Transmission of digital information',
                              if_else(grepl("H04W",.$Subclass),'(H04W) Wireless communication networks',
                              if_else(grepl("Y02A",.$Subclass),'(Y02A) Technologies for adaptation to climate change',
                              if_else(grepl("G16B",.$Subclass),'(G16B) Bioinformatics', 'none')))))))))))))))))))))))))))))))))))))) %>%
    rename(name = source) %>%
    .[!duplicated(.$name), ]
  nodes$Subclass <- gsub('(.{1,24})(\\s|$)', '\\1\n', nodes$Subclass)

  data_cpc_co <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>%
    .[!grepl("\\|", .$`CPC - Current - DWPI`),] %>%
    .[!is.na(.$`CPC - Current - DWPI`),] %>%
    dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d"))
  data_cpc_co$Pub_year <- format(data_cpc_co$`Publication Date`, format = "%Y")
  data_cpc_co <- data_cpc_co %>% select(`Publication Number`, `CPC - Current - DWPI`, Pub_year)
  # Turn it into igraph object
  network <- graph_from_data_frame(d=data_links, vertices=nodes, directed=F)
  len <- length(unique(nodes$Subclass))
  list(network = network, len = len, nodes = unique(nodes$Subclass))
}





# CPC Trends --------------------------------------------------------------
cpc_trends <- function(data = data) {
  data_cpc <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>%
      dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d"))
  data_cpc$year_month <- floor_date(data_cpc$`Publication Date`,  # Create year-month column
                                  "month")
  data_cpc <- data_cpc[!grepl("\\|", data_cpc$`CPC - Current - DWPI`),]
  data_cpc <- data_cpc[!is.na(data_cpc$`CPC - Current - DWPI`),]
  data_cpc$cpc_short <- substr(data_cpc$`CPC - Current - DWPI`, 1, 4)
  # Top CPC codes over all years
  data_cpc_trend <- data_cpc %>%
    count(year_month,cpc_short) %>%
    group_by(cpc_short, year_month) %>%
    mutate(mean_n = mean(n))
  data_cpc_trend <- data_cpc_trend[data_cpc_trend$cpc_short %in% names(which(table(data_cpc_trend$cpc_short) > 9)), ] %>%
    data.frame(.) %>%
    mutate(cpc_short_new =
if_else(grepl("A61B",.$cpc_short),'(A61B) Diagnostic instruments and processes',
if_else(grepl("A61F",.$cpc_short),'(A61F) Filters implantable into blood vessels',
if_else(grepl("A61H",.$cpc_short),'(A61H) Physical therapy apparatus',
if_else(grepl("A61K",.$cpc_short),'(A61K) Preparations for pharmaceutical products',
if_else(grepl("A61M",.$cpc_short),'(A61M) Devices for introducing media into or onto the body',
if_else(grepl("A61N",.$cpc_short),'(A61N) Electrotherapy',
if_else(grepl("A61P",.$cpc_short),'(A61P) Therapeutic activity of chemical compounds or medicinial preparations',
if_else(grepl("A63F",.$cpc_short),'(A63F) Gaming',
if_else(grepl("A63H",.$cpc_short),'(A63H) Toys',
if_else(grepl("B25J",.$cpc_short),'(B25J) Manipulators',
if_else(grepl("C07K",.$cpc_short),'(C07K) Peptides',
if_else(grepl("C12N",.$cpc_short),'(C12N) Microorganisms or enzymes',
if_else(grepl("C12Q",.$cpc_short),'(C12Q) Measuring or testing processes involving enzymes, nucleic acids, or microorganisms',
if_else(grepl("G01N",.$cpc_short),'(G01N) Investigating or analyzing materials by determining their physical or chemical properties',
if_else(grepl("G01P",.$cpc_short),'(G01P) Measuring linear or angular speed, acceleration, or shock',
if_else(grepl("G01R",.$cpc_short),'(G01R) Measuring electric variables',
if_else(grepl("G01V",.$cpc_short),'(G01V) Geophysics',
if_else(grepl("G05B",.$cpc_short),'(G05B) Control or regulating systems in general',
if_else(grepl("G06F",.$cpc_short),'(G06F) Electronic digital data processing',
if_else(grepl("G06K",.$cpc_short),'(G06K) Graphical data reading',
if_else(grepl("G06N",.$cpc_short),'(G06N) Computing arrangements based on computational models',
if_else(grepl("G06Q",.$cpc_short),'(G06Q) Data processing systems or methods for administrative purposes',
if_else(grepl("G06T",.$cpc_short),'(G06T) Image data processing or generation',
if_else(grepl("G06V",.$cpc_short),'(G06V) Image or video recognition or understanding',
if_else(grepl("G07F",.$cpc_short),'(G07F) Coin sorting',
if_else(grepl("G08B",.$cpc_short),'(G08B) Signalling or calling systems',
if_else(grepl("G09B",.$cpc_short),'(G09B) Educational or demonstration appliances',
if_else(grepl("G10L",.$cpc_short),'(G10L) Speech analysis or synthesis',
if_else(grepl("G16C",.$cpc_short),'(G16C) Computational chemistry',
if_else(grepl("G16H",.$cpc_short),'(G16H) Healthcare informatics',
if_else(grepl("G16Y",.$cpc_short),'(G16Y) ICT for Internet of Things',
if_else(grepl("G16Z",.$cpc_short),'(G16Z) ICT for specific applications',
if_else(grepl("H04B",.$cpc_short),'(H04B) Transmission',
if_else(grepl("H04L",.$cpc_short),'(H04L) Transmission of digital information',
if_else(grepl("H04W",.$cpc_short),'(H04W) Wireless communication networks',
if_else(grepl("Y02A",.$cpc_short),'(Y02A) Technologies for adaptation to climate change',
if_else(grepl("G16B",.$cpc_short),'(G16B) Bioinformatics', 'none'))))))))))))))))))))))))))))))))))))))
  data_cpc_trend$cpc_short_new <- gsub('(.{1,24})(\\s|$)', '\\1\n', data_cpc_trend$cpc_short_new)
  data_cpc_trend <- data_cpc_trend %>%  group_by(year_month) %>% mutate(overall_trend = sum(n)) %>%
    mutate(rel_trend = n/overall_trend)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  data_cpc_trend$overall_trend <- range01(data_cpc_trend$overall_trend)
  colors <- unique(data_cpc_trend$cpc_short_new)
  names(colors) <- colors
  color_codes <- colors[intersect(names(dis_pal), names(colors))] <- dis_pal[intersect(names(dis_pal), names(colors))]
  list(cpc_df = data_cpc_trend, color_codes = color_codes)
}



# Create map data ----------------------------------------------------------------

library(mapdeck)

mapping_data <- function(data = data) {

  # improve assignee and inventor columns - to title case
data$Assignee_corrected <-gsub("\\|.*","",data$`Assignee - DWPI`) %>%
  str_to_title()
data$Inventor_corrected <-gsub("\\|.*","",data$`Inventor - DWPI`) %>% str_to_title()

# crate columns with number of patents per assignee and inventor
data_map<- data %>%
  group_by(`Assignee Country`) %>%
  mutate(ass_count_n = as.numeric(n())) %>%
  ungroup(.) %>%
  group_by(Assignee_corrected) %>%
  mutate(ass_n = as.numeric(n())) %>%
  ungroup(.) %>%
  group_by(Inventor_corrected) %>%
  mutate(invent_n = as.numeric(n())) %>%
  ungroup(.)

# remove all rows with < 3 patents per assignee/ inventor
data_map$Assignee_corrected[data_map$ass_n < 3]=NA
data_map$Inventor_corrected[data_map$invent_n < 3]=NA
data_map$ass_n[data_map$ass_n < 3]=NA
data_map$invent_n[data_map$invent_n < 3]=NA

# create lat and lon for assignee location
data_map$assign_city_lat[data_map$Assignee_corrected == "Medtronic Ardian Luxembourg Sarl"] <- 45.069 #finance holding in Luxemburg, but headquater minneapolis
data_map$assign_city_lon[data_map$Assignee_corrected == "Medtronic Ardian Luxembourg Sarl"] <- -93.250

data_map$assign_city_lat[data_map$Assignee_corrected == "Konink Philips Nv"] <- 51.407
data_map$assign_city_lon[data_map$Assignee_corrected == "Konink Philips Nv"] <- 5.459

data_map$assign_city_lat[data_map$Assignee_corrected == "Korea Advanced Sci & Technology Inst"] <- 36.371
data_map$assign_city_lon[data_map$Assignee_corrected == "Korea Advanced Sci & Technology Inst"] <- 127.359

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Hanyang Iucf-Hyu"] <- 37.557
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Hanyang Iucf-Hyu"] <- 127.048

data_map$assign_city_lat[data_map$Assignee_corrected == "Int Business Machines Corp"] <- 41.107
data_map$assign_city_lon[data_map$Assignee_corrected == "Int Business Machines Corp"] <- -73.718

data_map$assign_city_lat[data_map$Assignee_corrected == "X Dev Llc"] <- 37.422
data_map$assign_city_lon[data_map$Assignee_corrected == "X Dev Llc"] <- -122.084

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Leland Stanford Junior "] <- 37.424
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Leland Stanford Junior "] <- -122.169

data_map$assign_city_lat[data_map$Assignee_corrected == "Ginger.io Inc"] <- 37.787
data_map$assign_city_lon[data_map$Assignee_corrected == "Ginger.io Inc"] <- -122.400

data_map$assign_city_lat[data_map$Assignee_corrected == "Blackthorn Therapeutics Inc"] <- 37.664
data_map$assign_city_lon[data_map$Assignee_corrected == "Blackthorn Therapeutics Inc"] <- -122.389

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Third Military Medical Pla Third "] <- 29.535
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Third Military Medical Pla Third "] <- 106.448

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Zhejiang Technology"] <- 30.224
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Zhejiang Technology"] <- 120.048

# rename assignees
data_map$Assignee_corrected <- gsub('Medtronic Ardian Luxembourg Sarl', 'USA: Medtronic', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Konink Philips Nv', 'NL: Koninklijke Philips NV', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Korea Advanced Sci & Technology Inst', 'KR: Advanced Sci. & Tech. Inst.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('X Dev Llc', 'USA: X Development', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Int Business Machines Corp', 'USA: IBM', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Leland Stanford Junior ', 'USA: Stanford Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Ginger.io Inc', 'USA: Ginger.io', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Third Military Medical Pla Third ', 'CN: Third Military Medical Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Blackthorn Therapeutics Inc', 'USA: Blackthorn Therapeutics', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Zhejiang Technology', 'CN: Zhejiang Tech. Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Hanyang Iucf-Hyu', 'KR: Hanyang Univ.', data_map$Assignee_corrected)


# add number of patents behind assignee name
data_map$Assignee_corrected <- paste0(stringr::str_replace_na(data_map$Assignee_corrected, replacement=""), " (", stringr::str_replace_na(data_map$ass_n, replacement=""), ")")

is.na(data_map$Assignee_corrected) <- data_map$Assignee_corrected == " ()"

capitals$`Assignee Country` <- capitals$country

data_map <- merge(data_map,capitals, by  = "Assignee Country", all.x=TRUE)

data_map $capital[data_map$`Assignee Country` == "Czechia"] <- "Prague"
data_map$lat[data_map$`Assignee Country` == "Czechia"] <- 50.07
data_map$lon[data_map$`Assignee Country` == "Czechia"] <- 14.41

data_map$capital[data_map$`Assignee Country` == "Japan"] <- "Tokyo"
data_map$lat[data_map$`Assignee Country` == "Japan"] <- 35.67
data_map$lon[data_map$`Assignee Country` == "Japan"] <- 139.65

data_map$capital[data_map$`Assignee Country` == "Luxemburg"] <- "Luxemburg"
data_map$lat[data_map$`Assignee Country` == "Luxemburg"] <- 49.81
data_map$lon[data_map$`Assignee Country` == "Luxemburg"] <- 6.12

data_map$capital[data_map$`Assignee Country` == "Singapore"] <- "Singapore"
data_map$lat[data_map$`Assignee Country` == "Singapore"] <- 1.35
data_map$lon[data_map$`Assignee Country` == "Singapore"] <- 103.81

data_map$capital[data_map$`Assignee Country` == "South Korea"] <- "Seoul"
data_map$lat[data_map$`Assignee Country` == "South Korea"] <- 37.56
data_map$lon[data_map$`Assignee Country` == "South Korea"] <- 126.97

data_map$capital[data_map$`Assignee Country` == "UK"] <- "London"
data_map$lat[data_map$`Assignee Country` == "UK"] <- 51.50
data_map$lon[data_map$`Assignee Country` == "UK"] <- -0.12

data_map$capital[data_map$`Assignee Country` == "USA"] <- "Washington, D.C."
data_map$lat[data_map$`Assignee Country` == "USA"] <- 38.90
data_map$lon[data_map$`Assignee Country` == "USA"] <- -77.16
data_map
}



# Fit models SEM ----------------------------------------------------------


## Defining function for fitting all competing models to Case study data ##
fit_models <- function(models, data) {
  model_fits <- list()
  for(i in 1:length(models)) {
    model_fit <- sem(models[[i]], data, estimator= "ML", meanstructure=TRUE)
    if(model_fit@Fit@converged)
      model_fits[[length(model_fits)+1]] <- model_fit
    else
      warning(paste0('model: ',model, ' - fit did not converge'))
  }
  return (model_fits)
}



## Defining function for averaging the selected best models ##
average_models <- function(model_fits, sel_models, sel_wt, vars, coefs) {

  intercepts <- paste0(vars,'~1')
  coefs_mat <- matrix(0,nrow=length(sel_models),ncol=length(coefs))
  intercepts_mat <- matrix(0,nrow=length(sel_models),ncol=length(vars))
  colnames(coefs_mat) <- coefs
  colnames(intercepts_mat) <- intercepts
  for(i in 1:length(sel_models)) {
    est <- coef(model_fits[[sel_models[i]]])[coefs]
    est <- est[which(!is.na(est))]
    coefs_mat[i,names(est)] <- est*sel_wt[i]
    intercepts_mat[i,] <- coef(model_fits[[sel_models[i]]])[intercepts]*sel_wt[i]
  }
  coef_avg <- colSums(coefs_mat)
  intercepts_avg <- colSums(intercepts_mat)
  return (list(coef=coef_avg[coef_avg!=0],intercepts=intercepts_avg[intercepts_avg!=0]))
}



## Defining function for ranking competing models based on adjusted-BIC ##
BICc.lavaan <- function(object, second.ord = TRUE, c.hat = 1, return.K = FALSE){
  object <- as.list(fitMeasures(object))
  npar<-object$baseline.df - object$df
  if(return.K==T) return(object$npar)
  if(second.ord==F && c.hat>1) return(object$chisq) #this should be QBIC once defined
  if(second.ord==F) return(object$bic)
  if(c.hat>1) return(object$ntotal) #this should be aQBIC once defined
  object$bic2
}

BICctab.lavaan <- function(cand.set, modnames, sort = TRUE, c.hat = 1, second.ord = TRUE, nobs = NULL){

  if(is.null(modnames)) modnames<-1:length(cand.set)

  # check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
  # if (length(unique(check.resp)) > 1)
  #     stop("You must use the same response variable for all models\n")

  Results <- data.frame(Modnames = modnames)
  Results$K <- unlist(lapply(X = cand.set, FUN = BICc.lavaan,
                             return.K = TRUE, c.hat = c.hat,second.ord = second.ord))

  Results$BIC2 <- unlist(lapply(X = cand.set, FUN = BICc.lavaan,
                                return.K = FALSE, c.hat = c.hat,second.ord = second.ord))

  Results$Delta_BIC2 <- Results$BIC2 - min(Results$BIC2)
  Results$ModelLik <- exp(-0.5 * Results$Delta_BIC2)
  Results$BIC2Wt <- Results$ModelLik/sum(Results$ModelLik)

  if (length(unique(Results$BIC2)) != length(cand.set))
    warning("\nCheck model structure carefully as some models may be redundant\n")

  if (second.ord == TRUE && c.hat == 1) {
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
  }

  if (second.ord == TRUE && c.hat > 1) {
    colnames(Results) <- c("Modnames", "K", "QAICc", "Delta QAICc", "ModelLik", "QAICcWt")
    LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    Results$Quasi.LL <- LL/c.hat
    Results$c_hat <- c.hat #this should be aQBIC once defined
  }

  if (second.ord == FALSE && c.hat == 1) {
    colnames(Results) <- c("Modnames", "K", "BIC", "Delta BIC", "ModelLik", "BICWt")
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
  }

  if (second.ord == FALSE && c.hat > 1) {
    colnames(Results) <- c("Modnames", "K", "QAIC", "Delta QAIC", "ModelLik", "QAICWt")
    LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    Results$Quasi.LL <- LL/c.hat
    Results$c_hat <- c.hat #this should be QBIC once defined
  }

  if (sort) {
    Results <- Results[rev(order(Results[, 6])), ]
    Results$Cum.Wt <- cumsum(Results[, 6])
  }
  else {
    Results$Cum.Wt <- NULL
  }

  class(Results) <- c("aictab", "data.frame")
  return(Results)
}


# Change multiple column names --------------------------------------------

library(data.table)
setnames_across <- function(df, range, name) {
  setnames(df, old = names(data_t31_w1[range]), new = paste(name, seq_along(data_t31_w1[range]), sep = "_"))
}


# Simulate from existing df -----------------------------------------------
library(pgirmess)
rnorm_multi <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0,
                        varnames = NULL, empirical = FALSE,
                        as.matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
    #   # reinstate system seed after simulation
    #   gs <- global_seed(); on.exit(global_seed(gs))
  }

  # error handling ----
  if ( !is.numeric(n) || n %% 1 > 0 || n < 1 ) {
    stop("n must be an integer > 0")
  }

  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }

  # try to guess vars if not set ----
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    }

    if (is.null(vars)) {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }

  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    #mu <- as.matrix(mu) %>% as.vector()
  }

  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    #sd <- as.matrix(sd) %>% as.vector()
  }

  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r # for name-checking later
  } else {
    # get data from mvn ----
    cor_mat <- tryCatch(
      { cormat(r, vars) },
      error = function(e) {
        stop("The correlation you specified is impossible: ", e$message, call. = FALSE)
      }
    )

    sigma <- (sd %*% t(sd)) * cor_mat
    # tryCatch({
    #   mvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
    # }, error = function(e) {
    #   stop("The correlated variables could not be generated. If empirical = TRUE, try increasing the N or setting empirical = FALSE.")
    # })

    err <- "The correlated variables could not be generated."
    if (empirical) err <- paste(err, "Try increasing the N or setting empirical = FALSE.")

    # code from MASS:mvrnorm
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) stop(err)
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -1e-06 * abs(ev[1L]))) stop(paste(err))
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch({
      X <- drop(mu) + eS$vectors %*%
        diag(sqrt(pmax(ev, 0)), p) %*%  t(X)
    }, error = function(e) { stop(err) })

    mvn <- t(X)
  }
  # coerce to matrix if vector when n == 1
  if (n == 1) mvn <- matrix(mvn, nrow = 1)

  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(mvn) <- colnames(cor_mat)
  } else if (!is.null(names(mu))) {
    #use mu names
    colnames(mvn) <- names(mu)
  } else if (!is.null(names(sd))) {
    #use sd names
    colnames(mvn) <- names(sd)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }

  if (as.matrix == TRUE) mvn else data.frame(mvn, check.names = FALSE)
}

sim_df <- function (data, n = 100, within = c(), between = c(),
                    id = "id", dv = "value",
                    empirical = FALSE, long = FALSE, seed = NULL,
                    missing = FALSE, sep = faux_options("sep")) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
    #   # reinstate system seed after simulation
    #   gs <- global_seed(); on.exit(global_seed(gs))
    #   set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }

  # error checking ------
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }

  if (length(within) > 0 & all(within %in% names(data))) {
    # convert long to wide
    data <- long2wide(data = data,
                      within = within,
                      between = between,
                      dv = dv, id = id, sep = sep)
  }

  if (is.numeric(between)) between <- names(data)[between]

  if (length(between) > 0 & !is.character(between)) {
    stop("between must be a numeric or character vector")
  }

  numvars <- setdiff(names(data), c(id, dv, between))
  is_num <- sapply(data[numvars], is.numeric)
  numvars <- numvars[is_num]

  grps <- data[between]
  if (length(grps) == 0) grps <- rep(1, nrow(data))

  simdat <- by(data, grps, function(x) {
    y <- x[numvars]
    z <- rnorm_multi(
      n = n,
      vars = ncol(y),
      mu = sapply(y, mean, na.rm = TRUE),
      sd = sapply(y, sd, na.rm = TRUE),
      r = cor(y, use = "complete.obs"),
      varnames = names(y),
      empirical = empirical
    )

    # simulate missing data pattern
    if (missing) {
      na_cells <- dplyr::mutate_all(y, is.na) %>%
        sim_joint_dist(n = n)

      z <- mapply(function(sim_col, nc_col) {
        sim_col[nc_col] <- NA
        sim_col
      }, z, na_cells, SIMPLIFY = FALSE) %>%
        as.data.frame()
    }

    ## add between vars
    for (b in between) z[b] <- unique(x[[b]])
    z[ , c(between, numvars), drop = FALSE]
  }) %>% do.call(rbind, .)

  # fix names
  nm <- names(simdat)
  simdat[id] <- make_id(nrow(simdat))
  simdat <- simdat[c(id, nm)]
  rownames(simdat) <- c()

  # convert to long
  if (long) {
    simdat <- wide2long(simdat,
                        within_factors = within,
                        within_cols = numvars,
                        dv = dv, id = id, sep = sep)
  }

  return(simdat)
}



# Set names across dailies --------------------------------------------------------

setnames_across_dailies <- function(df) {
  setnames_across(df, 10:12, "w1_engphys")
  setnames_across(df, 13:15, "w1_engemo")
  setnames_across(df, 16:18, "w1_engcog")

  setnames_across(df, 19:21, "w1_fatphys")
  setnames_across(df, 22:24, "w1_fatment")
  setnames_across(df, 25:27, "w1_fatcog")

  setnames_across(df, 28, "w1_jobsat")
  setnames_across(df, 29, "w1_lifesat")

  setnames_across(df, 30:34, "w1_posaff")
  setnames_across(df, 35:39, "w1_negaff")

  setnames_across(df, 40:42, "w1_turnover")

  setnames_across(df, 43:54, "w1_sf12")

  setnames_across(df, 55:57, "w1_quantworkload")

  setnames_across(df, 58, "w1_ineffwork")
  setnames_across(df, 59, "w1_workoutsideresp")

  setnames_across(df, 60:62, "w1_jobaut")

  setnames_across(df, 63:66, "w1_cosupp")
  setnames_across(df, 67:70, "w1_counder")

  setnames_across(df, 71:74, "w1_susupp")
  setnames_across(df, 75:78, "w1_suunder")

  setnames_across(df, 79:83, "w1_ldirect")
  setnames_across(df, 84:88, "w1_lemp")

  setnames_across(df, 89:93, "w1_digi")

  setnames_across(df, 94:98, "w1_flex")

  setnames_across(df, 99:100, "w1_sdtaut")
  setnames_across(df, 101:102, "w1_sdtcomp")
  setnames_across(df, 103:104, "w1_sdtrel")

  setnames_across(df, 105:113, "w1_craft")

  setnames_across(df, 114, "w1_hoposs")
  setnames_across(df, 115, "w1_howork")

  setnames_across(df, 116, "w1_holrel")
  setnames_across(df, 117, "w1_hocrel")
  setnames_across(df, 118, "w1_hoexcl")
  setnames_across(df, 119, "w1_hoopmi")
  setnames_across(df, 120, "w1_hocomi")
  setnames_across(df, 121, "w1_hoinmi")

  setnames_across(df, 122, "w1_hoplac")
  setnames_across(df, 123, "w1_hoplacother")

  setnames_across(df, 124, "w1_hohapp")
  setnames_across(df, 125, "w1_hose")

  setnames_across(df, 126:128, "w1_covid")

  setnames_across(df, 129:133, "w1_leaderself")
  setnames_across(df, 134:146, "w1_leaderother")

  setnames_across(df, 147:155, "w1_workhealth")

}



# Change names diary study ------------------------------------------------------------

change_names_baseline_diary <- function(df) {
  df <- df %>%
    rename_all(~ make.unique(lapply(df, function(x) attributes(x)$label) %>%
                               unlist() %>%
                               as.character()))

  df_new <- df %>% dplyr::rename(ID = "URL-Parameter.1",
                status = "Status (Erwerbstätig?)",
                statusother = "Anderes, und zwar:",
                part = "Teilnahme an Tagebuchstudie",
                age = "Alter",
                gender = "Geschlecht",
                edu = "Bildungsstand",
                eduother = "Anderes, und zwar:.1",
                persinc = "Persönliches Nettoeinkommen",
                housinc = "Haushalts Nettoeinkommen",
                jobteny = "%sJahre",
                jobtenm = "%szusätzliche Monate (oder wenn < 1 Jahr)",
                orgteny = "%sJahre.1",
                orgtenm = "%szusätzliche Monate (oder wenn < 1 Jahr).1",
                leadteny = "%sJahre.2",
                leadtenm = "%szusätzliche Monate (oder wenn < 1 Jahr).2",
                occ = "Tätigkeit",
                indu = "Branche",
                pos = "Position in der Firma",
                leader = "Führungsverantwortung",
                leadernum = "ja -  für %s Mitarbeiterinnen und Mitarbeiter",
                leadery = "%sJahre.3",
                leaderm = "%szusätzliche Monate (oder wenn < 1 Jahr).3",
                worksit = "Arbeitssituation Schicht",
                worksitother = "Andere Arbeitszeitregelung, und zwar:",
                shiftwork = "Schichtarbeit, bitte angeben welche Schichten Sie nächste und übernächste Woche arbeiten:",
                hourscontr = "Anzahl der Stunden pro Woche",
                hoursactual = "Anzahl der Stunden pro Woche.1",
                hourswish = "Anzahl der Stunden pro Woche.2",
                perchome = "Antwortoption",
                tempcontr = "Arbeitsverhältnis un-/befristet",
                country = "Nationalität",
                countryother = "Andere Nationalität/Staatsagehörigkeit, bitte angeben:",
                doublecit = "Doppelte Staatsbürgerschaft: Deutschland und (bitte angeben):",
                migrate = "Migrationshintergrund",
                migratenat = "Ja, mein Migrationshintergrund ist (bitte Nationalität angeben):",
                natminor = "Minderheit",
                extra_1 = "…bin eher zurückhaltend, reserviert.",
                extra_2 = "…bin begeisterungsfähig und kann andere leicht mitreißen.",
                extra_3 = "…bin eher der ^stille Typ^, wortkarg.",
                extra_4 = "…gehe aus mir heraus, bin gesellig.",
                agree_1 = "…neige dazu, andere zu kritisieren.",
                agree_2 = "…schenke anderen leicht Vertrauen, glaube an das Gute im Menschen.",
                agree_3 = "…kann mich kalt und distanziert verhalten.",
                agree_4 = "…kann mich schroff und abweisend anderen gegenüber verhalten.",
                consc_1 = "…erledige Aufgaben gründlich.",
                consc_2 = "…bin bequem, neige zu Faulheit.",
                consc_3 = "…bin tüchtig und arbeite flott.",
                consc_4 = "…mache Pläne und führe sie auch durch.",
                neuro_1 = "…werde leicht deprimiert, niedergeschlagen.",
                neuro_2 = "…bin entspannt, lasse mich durch Stress nicht aus der Ruhe bringen.",
                neuro_3 = "…mache mir viele Sorgen.",
                neuro_4 = "…werde leicht nervös und unsicher.",
                open_1 = "…bin vielseitig interessiert.",
                open_2 = "…bin tiefsinnig, denke gerne über Sachen nach.",
                open_3 = "…habe eine aktive Vorstellungskraft, bin phantasievoll.",
                open_4 = "…habe nur wenig künstlerisches Interesse.",
                open_5 = "…schätze künstlerische und ästhetische Eindrücke.",
                CSES_1 = "Ich bin zuversichtlich, im Leben den Erfolg zu bekommen, den ich verdiene.",
                CSES_2 = "Manchmal bin ich deprimiert.",
                CSES_3 = "Wenn ich mich anstrenge, bin ich im Allgemeinen erfolgreich.",
                CSES_4 = "Wenn ich etwas nicht schaffe, fühle ich mich manchmal wertlos.",
                CSES_5 = "Ich erledige Aufgaben erfolgreich.",
                CSES_6 = "Manchmal habe ich das Gefühl, keine Kontrolle über meine Arbeit zu haben.",
                CSES_7 = "Im Großen und Ganzen bin ich mit mir zufrieden.",
                CSES_8 = "Ich zweifle an meinen Fähigkeiten.",
                CSES_9 = "Ich bestimme, was in meinem Leben geschehen soll.",
                CSES_10 = "Ich habe das Gefühl, den Erfolg meiner Karriere nicht unter Kontrolle zu haben.",
                CSES_11 = "Ich bin in der Lage, die meisten meiner Probleme zu bewältigen.",
                CSES_12 = "Es gibt Zeiten, in denen mir die Dinge ziemlich düster und hoffnungslos erscheinen.",
                proper_1 = "Ich bin ständig auf der Suche nach neuen Wegen, um mein Leben zu verbessern.",
                proper_2 = "Wo immer ich war, war ich eine treibende Kraft für konstruktive Veränderungen.",
                proper_3 = "Nichts ist aufregender, als zu sehen, wie meine Ideen Wirklichkeit werden.",
                proper_4 = "Wenn ich etwas sehe, das mir nicht gefällt, verändere ich es.",
                proper_5 = "Egal wie die Chancen stehen, wenn ich an etwas glaube, mache ich es möglich.",
                proper_6 = "Ich bin gerne ein Verfechter meiner Ideen, auch gegen die Opposition anderer.",
                proper_7 = "Ich bin hervorragend darin, Gelegenheiten zu identifizieren.",
                proper_8 = "Ich bin immer auf der Suche nach besseren Wegen, Dinge zu tun.",
                proper_9 = "Wenn ich an eine Idee glaube, hindert mich nichts daran, sie umzusetzen.",
                proper_10 = "Ich kann eine gute Gelegenheit erkennen, lange bevor andere es können."
                )
  df_new <- df_new[, 1:81] # select only relevant columns

  # recode
  recode_7 <- function(x) {
    x * (-1)+8
  }

  df_recode_7 <- apply(select(df_new, matches("extra_1|extra_3|agree_1|agree_3|agree_4|consc_2|neuro_2|open_4|CSES_2|CSES_4|CSES_6|CSES_8|CSES_10|CSES_12")), 2, recode_7)
  df_new[ , colnames(df_new) %in% colnames(df_recode_7)] <- df_recode_7
  df_new <- data.frame(df_new)

}



# Change names diary study and recode ------------------------------------------------------------

change_names_daily_diary <- function(df) {
  df <- df %>%
    rename_all(~ make.unique(lapply(df, function(x) attributes(x)$label) %>%
                               unlist() %>%
                               as.character()))

  df_new <- df %>% rename(ID = "URL-Parameter",
                          occ = "Erwerbsarbeit",
                          absent = "Absentismus",
                          present = "Präsentismus",
                          workloc = "Arbeitsort",
                          worklocother = "Sonstiges, bitte angeben:",
                          hoursactual = "Tatsächliche Stunden",
                          hourswish = "Gewünschte Stunden",
                          engphy_1 = "...habe ich mich bei der Arbeit sehr stark angestrengt.",
                          engphy_2 = "...habe ich mich sehr bemüht, bei der Arbeit gute Leistung zu zeigen.",
                          engphy_3 = "...habe ich mein Bestes gegeben, um meine Arbeit zu erledigen.",
                          engemo_1 = "...war ich bei der Arbeit enthusiastisch.",
                          engemo_2 = "...habe ich mich bei der Arbeit voller Energie gefühlt.",
                          engemo_3 = "...war ich von meiner Arbeit begeistert.",
                          engcog_1 = "...habe ich meiner Arbeit sehr viel Aufmerksamkeit geschenkt.",
                          engcog_2 = "...habe ich mich sehr stark auf meine Arbeit fokussiert.",
                          engcog_3 = "...war ich von meiner Arbeit stark eingenommen.",
                          fatphy_1 = "...fühlten Sie sich körperlich erschöpft?",
                          fatphy_2 = "...fühlten Sie sich körperlich abgekämpft?",
                          fatphy_3 = "...fühlten Sie sich körperlich ausgelaugt?",
                          fatmen_1 = "...fühlten Sie sich geistig erschöpft?",
                          fatmen_2 = "...fühlten Sie sich geistig abgestumpft?",
                          fatmen_3 = "...fühlten Sie sich geistig leer?",
                          fatemo_1 = "...hatten Sie Schwierigkeiten, Emotionen zu zeigen oder mit Emotionen umzugehen?",
                          fatemo_2 = "...fühlten Sie sich emotional abgestumpft?",
                          fatemo_3 = "...haben Sie versucht, alle emotional anspruchsvollen Dinge zu vermeiden?",
                          jobsat = "Job satisfaction",
                          lifesat = "Life satisfaction",
                          poaff_1 = "angeregt",
                          poaff_2 = "hellwach",
                          poaff_3 = "freudig erregt",
                          poaff_4 = "begeistert",
                          poaff_5 = "entschlossen",
                          negaff_1 = "verärgert",
                          negaff_2 = "ängstlich",
                          negaff_3 = "bedrückt",
                          negaff_4 = "verängstigt",
                          negaff_5 = "nervös",
                          turn_1 = "...plante ich, meinen Job für einen anderen zu kündigen.",
                          turn_2 = "...dachte ich oft daran, meinen Job zu kündigen und mir einen anderen zu suchen.",
                          turn_3 = "...hätte ich gerne meinen Job gekündigt und einen neuen gefunden.",
                          sfgh1 = "Wie würden Sie Ihren Gesundheitszustand am heutigen Tag beschreiben?",
                          sfbp2 = "Inwieweit haben Schmerzen Sie heute bei der Ausübung Ihrer Alltagstätigkeiten zu Hause und im Beruf behindert?",
                          sfpf02 = "Mittelschwere Tätigkeiten, z.B. einen Tisch verschieben, Staubsaugen, Kegeln, Tennis spielen",
                          sfpf04 = "Mehrere Treppenabsätze steigen",
                          sfrp2 = "Ich habe weniger geschafft als ich wollte.",
                          sfrp3 = "Ich konnte nur bestimmte Dinge tun.",
                          sfmh3 = "...ruhig und gelassen?",
                          sfvt2 = "...voller Energie?",
                          sfmh4 = "...entmutigt und traurig?",
                          sfsf2 = "Wie häufig haben Ihre körperliche Gesundheit oder seelischen Probleme am heutigen Tag Ihre Kontakte zu anderen Menschen (Besuche bei Freunden, Verwandten usw.) beeinträchtigt?",
                          sfre2 = "Ich habe weniger geschafft als ich wollte..1",
                          sfre3 = "Ich konnte nicht so sorgfältig wie üblich arbeiten.",
                          load_1 = "Wie oft verlangte es Ihre Arbeit, sehr schnell zu arbeiten?",
                          load_2 = "Wie oft verlangte es Ihre Arbeit, sehr hart zu arbeiten?",
                          load_3 = "Wie oft gab es auf der Arbeit für Sie sehr viel zu tun?",
                          constr_1 = "Ich musste bei der Arbeit Aufgaben erledigen, die unnötig waren, oder die einfacher erledigt hätten werden können, wenn Dinge besser organisiert wären.",
                          constr_2 = "Ich musste bei der Arbeit unsinnige Aufgaben erledigen, die außerhalb meiner Verantwortung lagen und von jemand anderen erledigt werden sollten.",
                          jobaut_1 = "Meine Arbeit ermöglichte es mir, Initiative zu übernehmen und nach eigenem Ermessen zu handeln.",
                          jobaut_2 = "Ich konnte bei meiner Arbeit viele Entscheidungen selbstständig treffen.",
                          jobaut_3 = "Mein Arbeit gewährte mir einen großen Entscheidungsspielraum.",
                          cwsupp_1 = "Meine Kollegen gaben mir nützliche Informationen.",
                          cwsupp_2 = "Meine Kollegen halfen mir, Dinge zu verstehen und zu regeln.",
                          cwsupp_3 = "Meine Kollegen hörten mir zu, wenn ich reden musste.",
                          cwsupp_4 = "Meine Kollegen zeigten, dass sie sich für mich als Mensch interessieren.",
                          cwunder_1 = "Meine Kollegen verhielten sich mir gegenüber unangenehm oder wütend.",
                          cwunder_2 = "Meine Kollegen kritisierten mich.",
                          cwunder_3 = "Meine Kollegen verhielten sich auf eine Art und Weise, die zeigte, dass sie mich nicht mögen.",
                          cwunder_4 = "Meine Kollegen machten mir das Leben schwer.",
                          ssupp_1 = "Mein/e Vorgesetzte/r gab mir nützliche Informationen.",
                          ssupp_2 = "Mein/e Vorgesetzte/r half mir, die Dinge zu verstehen und zu regeln.",
                          ssupp_3 = "Mein/e Vorgesetzte/r hörte mir zu, wenn ich reden musste.",
                          ssupp_4 = "Mein/e Vorgesetzte/r zeigte, dass er/sie sich für mich als Mensch interessiert.",
                          sunder_1 = "Mein/e Vorgesetzte/r verhielt sich mir gegenüber unangenehm oder wütend.",
                          sunder_2 = "Mein/e Vorgesetzte/r kritisierte mich.",
                          sunder_3 = "Mein/e Vorgesetzte/r handelte auf eine Art und Weise, die zeigte, dass er/sie mich nicht mag.",
                          sunder_4 = "Mein/e Vorgesetzte/r machte mir das Leben schwer.",
                          sdirect_1 = "Mein/e Vorgesetzte/r überwachte die Leistung der Mitarbeiter.",
                          sdirect_2 = "Mein/e Vorgesetzte/r überprüfte, ob die Mitarbeiter die richtigen Prozeduren befolgen.",
                          sdirect_3 = "Mein/e Vorgesetzte/r erklärte klar, wie die Arbeit zu erledigen ist.",
                          sdirect_4 = "Mein/e Vorgesetzte/r zeigte den Mitarbeitern, wie sie Probleme lösen können.",
                          sdirect_5 = "Mein/e Vorgesetzte/r korrigierte schlechte Leistung.",
                          semp_1 = "Mein/e Vorgesetzte/r erklärte die allgemeinen Ziele, die wir zu erreichen versuchen.",
                          semp_2 = "Mein/e Vorgesetzte/r brachte den Mitarbeitern bei, wie sie Probleme selbst lösen können.",
                          semp_3 = "Mein/e Vorgesetzte/r teilte wichtige Aufgaben mit seinen/ihren Mitarbeitern.",
                          semp_4 = "Mein/e Vorgesetzte/r gab den Mitarbeitern die Freiheit, selbstständig zu arbeiten.",
                          semp_5 = "Mein/e Vorgesetzte/r ließ die Mitarbeiter wichtige Entscheidungen treffen.",
                          digi_1 = "Zur Erledigung meiner Arbeitsaufgaben war ich abhängig von digitalen Medien (z. B. Internet, Email).",
                          digi_2 = "Ich nutzte von meinem Arbeitgeber bereitgestelltes technisches Equipment (z.B. Smartphone, Laptop, Computer) zur Erledigung meiner Arbeitsaufgaben.",
                          digi_3 = "Ich nutzte während der Arbeit häufig Informationstechnik (z. B. spezifische Software).",
                          digi_4 = "Ich nutzte im Arbeitsalltag Informationstechnik, die eine spezifische Schulung erfordert.",
                          digi_5 = "Ohne die Nutzung von Informationstechnik (z. B. Internet, Software) wäre die Ausübung meiner Tätigkeit unmöglich.",
                          flexi_1 = "Ich konnte mir meine Arbeitszeit flexibel einteilen.",
                          flexi_2 = "Ich arbeitete im Homeoffice.",
                          flexi_3 = "Ich beantwortete berufliche Emails auch außerhalb des Büros (z. B. Zuhause).",
                          flexi_4 = "Von meinem Arbeitgeber wurde kein fester Beginn meines Arbeitstages festgelegt (z. B. durch Gleitzeit).",
                          flexi_5 = "Ich arbeitete während meiner regulären Arbeitszeit außerhalb des Büros (z. B. Zuhause).",
                          needaut_1 = "...basierten meine Entscheidungen auf meinen eigenen Interessen und Werten.",
                          needaut_2 = "...drückten meine Entscheidungen mein ^wahres Selbst^ aus.",
                          needcomp_1 = "...nahm ich schwierige Herausforderungen an und meisterte sie.",
                          needcomp_2 = "...war ich sehr kompetent in dem, was ich getan habe.",
                          needrel_1 = "...fühlte ich mich nah und verbunden mit anderen Menschen, die mir wichtig sind.",
                          needrel_2 = "...hatte ich ein starkes Gefühl der Verbundenheit mit den Menschen, mit denen ich Zeit verbringe.",
                          craft_1 = "...habe ich mir bestimmte Schwerpunkte bei meinen Arbeitsaufgaben gesetzt.",
                          craft_2 = "...habe ich passende Aufgaben übernommen oder mir gesucht.",
                          craft_3 = "...habe ich intensiver an Aufgaben gearbeitet, die zu mir passten.",
                          craft_4 = "...habe ich Kontakt mit Personen bei meiner Arbeit, mit denen ich mich weniger gut verstehe, möglichst kurz gehalten und nur das Nötigste geregelt.",
                          craft_5 = "...habe ich aktiv die Beziehungen zu Personen ausgebaut, mit denen ich mich bei der Arbeit gut verstehe.",
                          craft_6 = "...habe ich Gelegenheiten gesucht, mit den Personen zusammen zu arbeiten, mit denen ich mich gut bei der Arbeit verstehe.",
                          craft_7 = "...habe ich versucht, meine Aufgaben und Zuständigkeiten so zu sehen, dass die Arbeit einen tieferen Sinn ergibt.",
                          craft_8 = "...habe ich meinen Aufgaben und Zuständigkeiten eine persönliche Bedeutung gegeben.",
                          craft_9 = "...habe ich meine Aufgaben und Zuständigkeiten als etwas gesehen, was über meine Arbeit hinausgeht.",
                          hoposs = "Homeoffice1",
                          howork = "Homeoffice2",
                          hocontactsup = "Wie oft hatten Sie heute Kontakt mit Ihrem Vorgesetzten (persönlich, Telefon, Internet)?",
                          hocontactcw = "Wie oft hatten Sie heute Kontakt mit Ihren Kolleginnen und Kollegen (persönlich, Telefon, Internet)?",
                          hoexcl_1 = "Ich fühlte mich aus arbeitsbezogenen Aktivitäten und Treffen ausgeschlossen.",
                          hoexcl_2 = "Ich habe arbeitsbezogene Möglichkeiten verpasst.",
                          hoexcl_3 = "Ich habe den persönlichen Kontakt mit Arbeitskollegen vermisst.",
                          hoexcl_4 = "Ich habe den informellen Austausch mit anderen bei der Arbeit vermisst.",
                          hoplace = "Homeoffice9",
                          hoplaceother = "Sonstiges, bitte angeben:.1",
                          hosat = "Zufrieden Homeoffice",
                          hose = "Selbstwirksam Homeoffice"

  )
  df_new <- data.frame(df_new)

  df_new <- df_new[, 1:125] %>%  # select only relevant columns
    mutate(hoplace = na_if(hoplace, 0)) %>%
    mutate(hosat = na_if(hosat, 0)) %>%
    mutate(hose = na_if(hose, 0))

}


# Centering diary study ---------------------------------------------------------------
library(bmlm)
person_mean_center <- function(df) {
data_center <- df %>% ungroup(.) %>% dplyr::select(matches("^absent|cumsumhowork|^present|^workloc|hoursactual.x|hourswish.x|^eng|^fat|^jobsat|^lifesat|poaff|negaff|^turn|^sf|^load|^constr|^jobaut|^cwsupp|^cwunder|^ssup|^sunder|^sdirec|^semp|^digi|^flexi|^need|^craft|^hoposs$|^howork$|^hoc|hoex|hopl|hosat|hose|^PCS|^MCS")) %>%
  dplyr::select(where(is.numeric))
data_center$ID <- df$ID

data_center <- isolate(data_center, by = "ID",
             value = names(data_center),
             which = "within")
data.frame(data_center) %>% dplyr::select(-ID) %>% dplyr::select(matches("_cw$"))
}




grand_mean_center <- function(df) {
  data_center <- df %>% ungroup(.) %>% dplyr::select(matches("^persinc|cumsumhowork|newindu|Childunder|^turn|^housinc|^jobteny|^jobtenm|orgteny|^orgtenm|^leadteny|^leadtenm|^leadery|^leaderm|^worksit|^shiftwork|^hourscontr|^hoursactual.y|^hourswish.y|^perchome|^tempcontr|^extra|^agree|^consc|^neuro|^open_|^CSES_|proper_|^hoposs$|^howork$|^absent|^present|^workloc|hoursactual.x|hourswish.x|^eng|^fat|^jobsat|^lifesat|poaff|negaff|^turn|^sf|^load|^constr|^jobaut|^cwsupp|^cwunder|^ssup|^sunder|^sdirec|^semp|^digi|^flexi|^need|^craft|^hoposs$|^howork$|^hoc|hoex|hopl|hosat|hose|^PCS|^MCS")) %>%
    dplyr::select(where(is.numeric))
  data_center$ID <- df$ID
  data_center <- isolate(data_center, by = "ID",
                         value = names(data_center),
                         which = "between")
  data.frame(data_center) %>% dplyr::select(-ID) %>% dplyr::select(matches("_cb$"))
}




# Change colnames -----------------------------------------------------

change_colnames <- function(df) {
  # Remove all "_" from column names, except those followed by a number
  new_colnames <- gsub("_(?![0-9])", "", colnames(df), perl = TRUE)

  # Add "_" before the number at the end of the column name
  new_colnames <- gsub("(\\d+)$", "_\\1", new_colnames)
  new_colnames <- gsub("__", "_", new_colnames)

  # Assign the new column names to the data frame
  colnames(df) <- new_colnames

df
}


# Make composites ----------------------------
make_composite <- function(df) {
  data_c <- dplyr::select_if(df, is.numeric) %>% remove_all_labels(.) %>% dplyr::select(matches("_\\d+"))
  data <- df %>% dplyr::select_if(., is.numeric)
  data <- data %>% dplyr::select(str_c(setdiff(colnames(data), colnames(data_c))))
  data_c <- data_c %>% split.default(sub("_.*", "", names(data_c)))
  comp <- purrr::map(data_c, ~ rowMeans(.x, na.rm=T))
  comp_df <- do.call("cbind", comp) %>% as.data.frame(.) %>% cbind(., data)
  comp_df$ID <- df$ID
  comp_df
}

# Make composites ALL VARIABLES ----------------------------
make_composite_all <- function(df) {
  data_comp <- dplyr::select_if(df, is.numeric) %>% remove_all_labels(.)
  # Add "_" before the number at the end of the column name
  new_names <- gsub("(\\d+)$", "_\\1", colnames(data_comp))
  new_names <- gsub("__", "_", new_names)
  new_names <- gsub("_(?![0-9])", "", new_names, perl = TRUE)
  colnames(data_comp) <- new_names
  data_comp <- data_comp %>% split.default(sub("_.*", "", names(data_comp)))
  comp <- purrr::map(data_comp, ~ rowMeans(.x, na.rm=T)) %>% data.frame(.)
  comp
}

# Flatten correlation matrix ----------------------------------------------

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}


# Make alphas -------------------------------------------------------------
make_alphas <- function(df) {
  data_comp <- dplyr::select_if(df, is.numeric) %>% remove_all_labels(.) %>% dplyr::select(matches("\\d+$")) %>% dplyr::select(matches("^T31_|^T32_")) %>% dplyr::select(-matches("status|q26dsf|_SF|covid"))
  data_comp <- data_comp %>% split.default(sub("\\d+$", "", names(.)))
  # Filter nested data frames with more than two columns
  data_comp <- lapply(data_comp, function(x) if(ncol(x) > 2) x)
  # Remove NULL elements
  data_comp <-  Filter(function(x) !is.null(x), data_comp)
  data_comp <- lapply(data_comp, na.omit)
  data_comp <- Filter(function(x) nrow(x) >= 10, data_comp)
  alph <- purrr::map(data_comp, ~ psych::alpha(.x), data = .x) %>%
    purrr::map(~ .x$total)
  alph_df <- do.call("rbind", alph) %>% round(., 2)
  alph_df
}


# Make omegas -------------------------------------------------------------
make_omegas <- function(df) {
  data_comp <- dplyr::select_if(df, is.numeric) %>% remove_all_labels(.) %>% dplyr::select(matches("\\d+$")) %>% dplyr::select(matches("^T31_|^T32_")) %>% dplyr::select(-matches("status|q26dsf|_SF|covid"))
  data_comp <- data_comp %>% split.default(sub("\\d+$", "", names(.)))
  # Filter nested data frames with more than two columns
  data_comp <- lapply(data_comp, function(x) if(ncol(x) > 2) x)
  # Remove NULL elements
  data_comp <-  Filter(function(x) !is.null(x), data_comp)
  data_comp <- lapply(data_comp, na.omit)
  data_comp <- Filter(function(x) nrow(x) >= 10, data_comp)
  omega <- purrr::map(data_comp, ~ psych::omega(.x))
  omega
}



# Create SF 12 Scores -----------------------------------------------------

#' SF12 questionnaire scoring
#'
#' SF12 questionnaire scoring
#' @param X a \code{\link{matrix}} or \code{\link{data.frame}} of 12
#' columns, containing questionnaire items. In order from left to right:
#' gh1, pf02, pf04, rp2, rp3, re2, re3, bp2, mh3, vt2, mh4, sf2.
#' @note
#' This is an R port of SAS algorithm by Apolone and Mosconi found
#' \href{http://crc.marionegri.it/qdv/index.php?page=sf12}{here}.
#'
#' SF-12  is a registered trademark of medical outcomes trust.
#' @examples
#' ## -------------------------
#' ## Algorithm test/validation
#' ## -------------------------
#' (scores <- sf12(sf12sample))
#' ## website data test (printing with many decimals for 10 selected
#' ## questionnaires)
#' web <- c(1,2,4,5,11,27,28,31,37,39)
#' print(scores[web,], digits = 6)
#' ## SF12 Manual checks
#' print(unlist(lapply(scores, mean)), digits = 3)
#' print(unlist(lapply(scores, sd)), digits = 3)
#' print(lapply(scores, range), digits = 3)
#' ## Correlations
#' db <- cbind(sf12sample, scores)
#' var.order <- c(2:5,8,1,10,12,6,7,9,11)
#' cors <- cor(db)[var.order, 13:14]
#' print(cors, digits = 1)
#' ## Fine: reversed item have reverse sign correlation coefficients
#' @export
sf12 <- function( X = NULL ) {

  if((!(is.data.frame(X) | is.matrix(X))) | (ncol(X)!=12) )
    stop("X must be a data.frame (or matrix) with 12 columns")

  X <- as.data.frame(lapply(as.data.frame(X), as.integer))
  names(X) <- c("gh1", "pf02", "pf04", "rp2", "rp3", "re2", "re3", "bp2",
                "mh3", "vt2", "mh4", "sf2" )

  ## *****************************************************************;
  ## ***               STEP 1: DATA CLEANING/REVERSE SCORING       ***;
  ## *****************************************************************;

  ## ARRAY TWOPT RP2 RP3 RE2 RE3;
  ##   DO OVER TWOPT;
  ##   IF TWOPT LT 1 OR TWOPT GT 2 THEN TWOPT = .;
  ## END;
  ## ARRAY THREEPT PF02 PF04;
  ##   DO OVER THREEPT;
  ##   IF THREEPT LT 1 OR THREEPT GT 3 THEN THREEPT = .;
  ## END;
  ## ARRAY FIVEPT GH1 BP2 SF2;
  ##   DO OVER FIVEPT;
  ##   IF FIVEPT LT 1 OR FIVEPT GT 5 THEN FIVEPT = .;
  ## END;
  ## ARRAY SIXPT VT2 MH3 MH4;
  ##   DO OVER SIXPT;
  ##   IF SIXPT LT 1 OR SIXPT GT 6 THEN SIXPT = .;
  ## END;

  twopt <- c("rp2", "rp3", "re2", "re3")
  threept <- c("pf02", "pf04")
  fivept <- c("gh1", "bp2", "sf2")
  sixpt <- c("vt2", "mh3", "mh4")

  ## RBP2=6-BP2;
  ## RGH1=6-GH1;
  ## RVT2=7-VT2;
  ## RMH3=7-MH3;

  X$rbp2  <-  6 - X$bp2
  X$rgh1  <-  6 - X$gh1
  X$rvt2  <-  7 - X$vt2
  X$rmh3  <-  7 - X$mh3

  ## *****************************************************************;
  ## *               STEP 2: CREATE INDICATOR VARIABLES FROM         *
  ## *                       ITEM RESPONSE CHOICES                   *
  ## *****************************************************************;
  ## PF02_1 = .;
  ##   if PF02 = . then PF02_1 = .; else
  ##   if PF02 = 1 then PF02_1 = 1; else PF02_1 = 0;
  ## PF02_2 = .;
  ##   if PF02 = . then PF02_2 = .; else
  ##   if PF02 = 2 then PF02_2 = 1; else PF02_2 = 0;

  X$pf02_1 <- as.numeric(X$pf02 == 1L)
  X$pf02_2 <- as.numeric(X$pf02 == 2L)

  ## PF04_1 = .;
  ##   if PF04 = . then PF04_1 = .; else
  ##   if PF04 = 1 then PF04_1 = 1; else PF04_1 = 0;
  ## PF04_2 = .;
  ##   if PF04 = . then PF04_2 = .; else
  ##   if PF04 = 2 then PF04_2 = 1; else PF04_2 = 0;

  X$pf04_1 <- as.numeric(X$pf04 == 1L)
  X$pf04_2 <- as.numeric(X$pf04 == 2L)

  ## RP2_1 = .;
  ##   if RP2 = . then RP2_1 = .; else
  ##   if RP2 = 1 then RP2_1 = 1; else RP2_1 = 0;

  X$rp2_1 <- as.numeric(X$rp2 == 1L)

  ## RP3_1 = .;
  ##   if RP3 = . then RP3_1 = .; else
  ##   if RP3 = 1 then RP3_1 = 1; else RP3_1 = 0;

  X$rp3_1 <- as.numeric(X$rp3 == 1L)

  ## BP2_1 = .;
  ##   if RBP2 = . then BP2_1 = .; else
  ##   if RBP2 = 1 then BP2_1 = 1; else BP2_1 = 0;
  ## BP2_2 = .;
  ##   if RBP2 = . then BP2_2 = .; else
  ##   if RBP2 = 2 then BP2_2 = 1; else BP2_2 = 0;
  ## BP2_3 = .;
  ##   if RBP2 = . then BP2_3 = .; else
  ##   if RBP2 = 3 then BP2_3 = 1; else BP2_3 = 0;
  ## BP2_4 = .;
  ##   if RBP2 = . then BP2_4 = .; else
  ##   if RBP2 = 4 then BP2_4 = 1; else BP2_4 = 0;

  X$bp2_1 <- as.numeric(X$rbp2 == 1L)
  X$bp2_2 <- as.numeric(X$rbp2 == 2L)
  X$bp2_3 <- as.numeric(X$rbp2 == 3L)
  X$bp2_4 <- as.numeric(X$rbp2 == 4L)

  ## GH1_1 = .;
  ##   if RGH1 = . then GH1_1 = .; else
  ##   if RGH1 = 1 then GH1_1 = 1; else GH1_1 = 0;
  ## GH1_2 = .;
  ##   if RGH1 = . then GH1_2 = .; else
  ##   if RGH1 = 2 then GH1_2 = 1; else GH1_2 = 0;
  ## GH1_3 = .;
  ##   if RGH1 = . then GH1_3 = .; else
  ##   if RGH1 = 3 then GH1_3 = 1; else GH1_3 = 0;
  ## GH1_4 = .;
  ##   if RGH1 = . then GH1_4 = .; else
  ##   if RGH1 = 4 then GH1_4 = 1; else GH1_4 = 0;

  X$gh1_1 <- as.numeric(X$rgh1 == 1L)
  X$gh1_2 <- as.numeric(X$rgh1 == 2L)
  X$gh1_3 <- as.numeric(X$rgh1 == 3L)
  X$gh1_4 <- as.numeric(X$rgh1 == 4L)

  ## VT2_1 = .;
  ##   if RVT2 = . then VT2_1 = .; else
  ##   if RVT2 = 1 then VT2_1 = 1; else VT2_1 = 0;
  ## VT2_2 = .;
  ##   if RVT2 = . then VT2_2 = .; else
  ##   if RVT2 = 2 then VT2_2 = 1; else VT2_2 = 0;
  ## VT2_3 = .;
  ##   if RVT2 = . then VT2_3 = .; else
  ##   if RVT2 = 3 then VT2_3 = 1; else VT2_3 = 0;
  ## VT2_4 = .;
  ##   if RVT2 = . then VT2_4 = .; else
  ##   if RVT2 = 4 then VT2_4 = 1; else VT2_4 = 0;
  ## VT2_5 = .;
  ##   if RVT2 = . then VT2_5 = .; else
  ##   if RVT2 = 5 then VT2_5 = 1; else VT2_5 = 0;

  X$vt2_1 <- as.numeric(X$rvt2 == 1L)
  X$vt2_2 <- as.numeric(X$rvt2 == 2L)
  X$vt2_3 <- as.numeric(X$rvt2 == 3L)
  X$vt2_4 <- as.numeric(X$rvt2 == 4L)
  X$vt2_5 <- as.numeric(X$rvt2 == 5L)

  ## SF2_1 = .;
  ##   if SF2 = . then SF2_1 = .; else
  ##   if SF2 = 1 then SF2_1 = 1; else SF2_1 = 0;
  ## SF2_2 = .;
  ##   if SF2 = . then SF2_2 = .; else
  ##   if SF2 = 2 then SF2_2 = 1; else SF2_2 = 0;
  ## SF2_3 = .;
  ##   if SF2 = . then SF2_3 = .; else
  ##   if SF2 = 3 then SF2_3 = 1; else SF2_3 = 0;
  ## SF2_4 = .;
  ##   if SF2 = . then SF2_4 = .; else
  ##   if SF2 = 4 then SF2_4 = 1; else SF2_4 = 0;

  X$sf2_1 <- as.numeric(X$sf2 == 1L)
  X$sf2_2 <- as.numeric(X$sf2 == 2L)
  X$sf2_3 <- as.numeric(X$sf2 == 3L)
  X$sf2_4 <- as.numeric(X$sf2 == 4L)

  ## RE2_1 = .;
  ##   if RE2 = . then RE2_1 = .; else
  ##   if RE2 = 1 then RE2_1 = 1; else RE2_1 = 0;

  X$re2_1 <- as.numeric(X$re2 == 1L)

  ## RE3_1 = .;
  ##   if RE3 = . then RE3_1 = .; else
  ##   if RE3 = 1 then RE3_1 = 1; else RE3_1 = 0;

  X$re3_1 <- as.numeric(X$re3 == 1L)

  ## MH3_1 = .;
  ##   if RMH3 = . then MH3_1 = .; else
  ##   if RMH3 = 1 then MH3_1 = 1; else MH3_1 = 0;
  ## MH3_2 = .;
  ##   if RMH3 = . then MH3_2 = .; else
  ##   if RMH3 = 2 then MH3_2 = 1; else MH3_2 = 0;
  ## MH3_3 = .;
  ##   if RMH3 = . then MH3_3 = .; else
  ##   if RMH3 = 3 then MH3_3 = 1; else MH3_3 = 0;
  ## MH3_4 = .;
  ##   if RMH3 = . then MH3_4 = .; else
  ##   if RMH3 = 4 then MH3_4 = 1; else MH3_4 = 0;
  ## MH3_5 = .;
  ##   if RMH3 = . then MH3_5 = .; else
  ##   if RMH3 = 5 then MH3_5 = 1; else MH3_5 = 0;

  X$mh3_1 <- as.numeric(X$rmh3 == 1L)
  X$mh3_2 <- as.numeric(X$rmh3 == 2L)
  X$mh3_3 <- as.numeric(X$rmh3 == 3L)
  X$mh3_4 <- as.numeric(X$rmh3 == 4L)
  X$mh3_5 <- as.numeric(X$rmh3 == 5L)

  ## MH4_1 = .;
  ##   if MH4 = . then MH4_1 = .; else
  ##   if MH4 = 1 then MH4_1 = 1; else MH4_1 = 0;
  ## MH4_2 = .;
  ##   if MH4 = . then MH4_2 = .; else
  ##   if MH4 = 2 then MH4_2 = 1; else MH4_2 = 0;
  ## MH4_3 = .;
  ##   if MH4 = . then MH4_3 = .; else
  ##   if MH4 = 3 then MH4_3 = 1; else MH4_3 = 0;
  ## MH4_4 = .;
  ##   if MH4 = . then MH4_4 = .; else
  ##   if MH4 = 4 then MH4_4 = 1; else MH4_4 = 0;
  ## MH4_5 = .;
  ##   if MH4 = . then MH4_5 = .; else
  ##   if MH4 = 5 then MH4_5 = 1; else MH4_5 = 0;

  X$mh4_1 <- as.numeric(X$mh4 == 1L)
  X$mh4_2 <- as.numeric(X$mh4 == 2L)
  X$mh4_3 <- as.numeric(X$mh4 == 3L)
  X$mh4_4 <- as.numeric(X$mh4 == 4L)
  X$mh4_5 <- as.numeric(X$mh4 == 5L)


  ## *****************************************************************;
  ## *               STEP 3: WEIGHTING AND AGGREGATION OF            *
  ## *                       INDICATOR VARIABLES USING               *
  ## *                       PHYSICAL AND MENTAL REGRESSION WEIGHTS  *
  ## *****************************************************************;
  ## RAWPCS12 = (-7.23216*PF02_1) + (-3.45555*PF02_2) +
  ##   (-6.24397*PF04_1) + (-2.73557*PF04_2) + (-4.61617*RP2_1) +
  ##   (-5.51747*RP3_1) + (-11.25544*BP2_1) + (-8.38063*BP2_2) +
  ##   (-6.50522*BP2_3) + (-3.80130*BP2_4) + (-8.37399*GH1_1) +
  ##   (-5.56461*GH1_2) + (-3.02396*GH1_3) + (-1.31872*GH1_4) +
  ##   (-2.44706*VT2_1) + (-2.02168*VT2_2) + (-1.6185*VT2_3) +
  ##   (-1.14387*VT2_4) + (-0.42251*VT2_5) + (-0.33682*SF2_1) +
  ##   (-0.94342*SF2_2) + (-0.18043*SF2_3) + (0.11038*SF2_4) +
  ##   (3.04365*RE2_1) + (2.32091*RE3_1) + (3.46638*MH3_1) +
  ##   (2.90426*MH3_2) + (2.37241*MH3_3) + (1.36689*MH3_4) +
  ##   (0.66514*MH3_5) + (4.61446*MH4_1) + (3.41593*MH4_2) +
  ##   (2.34247*MH4_3) + (1.28044*MH4_4) + (0.41188*MH4_5);

  ## RAWMCS12 = (3.93115*PF02_1) + (1.8684*PF02_2) +
  ##   (2.68282*PF04_1) + (1.43103*PF04_2) + (1.4406*RP2_1) +
  ##   (1.66968*RP3_1) + (1.48619*BP2_1) + (1.76691*BP2_2) +
  ##   (1.49384*BP2_3) + (0.90384*BP2_4) + (-1.71175*GH1_1) +
  ##   (-0.16891*GH1_2) + (0.03482*GH1_3) + (-0.06064*GH1_4) +
  ##   (-6.02409*VT2_1) + (-4.88962*VT2_2) + (-3.29805*VT2_3) +
  ##   (-1.65178*VT2_4) + (-0.92057*VT2_5) + (-6.29724*SF2_1) +
  ##   (-8.26066*SF2_2) + (-5.63286*SF2_3) + (-3.13896*SF2_4) +
  ##   (-6.82672*RE2_1) + (-5.69921*RE3_1) + (-10.19085*MH3_1) +
  ##   (-7.92717*MH3_2) + (-6.31121*MH3_3) + (-4.09842*MH3_4) +
  ##   (-1.94949*MH3_5) + (-16.15395*MH4_1) + (-10.77911*MH4_2) +
  ##   (-8.09914*MH4_3) + (-4.59055*MH4_4) + (-1.95934*MH4_5);

  RAWPCS12 <- with(X,
                   (-7.23216*pf02_1) + (-3.45555*pf02_2) +
                     (-6.24397*pf04_1) + (-2.73557*pf04_2) +
                     (-4.61617*rp2_1) +
                     (-5.51747*rp3_1) +
                     (-11.25544*bp2_1) + (-8.38063*bp2_2) +
                     (-6.50522*bp2_3) + (-3.80130*bp2_4) + (-8.37399*gh1_1) +
                     (-5.56461*gh1_2) + (-3.02396*gh1_3) + (-1.31872*gh1_4) +
                     (-2.44706*vt2_1) + (-2.02168*vt2_2) + (-1.6185*vt2_3) +
                     (-1.14387*vt2_4) + (-0.42251*vt2_5) + (-0.33682*sf2_1) +
                     (-0.94342*sf2_2) + (-0.18043*sf2_3) + (0.11038*sf2_4) +
                     (3.04365*re2_1) + (2.32091*re3_1) + (3.46638*mh3_1) +
                     (2.90426*mh3_2) + (2.37241*mh3_3) + (1.36689*mh3_4) +
                     (0.66514*mh3_5) + (4.61446*mh4_1) + (3.41593*mh4_2) +
                     (2.34247*mh4_3) + (1.28044*mh4_4) + (0.41188*mh4_5))

  RAWMCS12 <- with(X,
                   (3.93115*pf02_1) + (1.8684*pf02_2) +
                     (2.68282*pf04_1) + (1.43103*pf04_2) + (1.4406*rp2_1) +
                     (1.66968*rp3_1) + (1.48619*bp2_1) + (1.76691*bp2_2) +
                     (1.49384*bp2_3) + (0.90384*bp2_4) + (-1.71175*gh1_1) +
                     (-0.16891*gh1_2) + (0.03482*gh1_3) + (-0.06064*gh1_4) +
                     (-6.02409*vt2_1) + (-4.88962*vt2_2) + (-3.29805*vt2_3) +
                     (-1.65178*vt2_4) + (-0.92057*vt2_5) + (-6.29724*sf2_1) +
                     (-8.26066*sf2_2) + (-5.63286*sf2_3) + (-3.13896*sf2_4) +
                     (-6.82672*re2_1) + (-5.69921*re3_1) + (-10.19085*mh3_1) +
                     (-7.92717*mh3_2) + (-6.31121*mh3_3) + (-4.09842*mh3_4) +
                     (-1.94949*mh3_5) + (-16.15395*mh4_1) + (-10.77911*mh4_2) +
                     (-8.09914*mh4_3) + (-4.59055*mh4_4) + (-1.95934*mh4_5))


  ## *****************************************************************;
  ## *               STEP 5: NORM-BASED STANDARDIZATION OF           *
  ## *                       SCALE SCORES                            *
  ## *****************************************************************;

  ## PCS12 = RAWPCS12 + 56.57706;
  ## MCS12 = RAWMCS12 + 60.75781;

  PCS12 <- RAWPCS12 + 56.57706
  MCS12 <- RAWMCS12 + 60.75781

  return(data.frame(PCS12, MCS12))

}


# Multilevel reliabilities ------------------------------------------------

# Multilevel reliabilities - single items
library(lavaan)
library(semTools)

multilevel_rels_eng <- function(df){
  model <- '
    level: 1
        engphy_w  =~  engphy_1 + engphy_2 + engphy_3
        engemo_w =~ engemo_1 + engemo_2 + engemo_3
        engcog_w  =~  engcog_1 + engcog_2 + engcog_3

    level: 2
        engphy_b  =~  engphy_1 + engphy_2 + engphy_3
        engemo_b =~ engemo_1 + engemo_2 + engemo_3
        engcog_b  =~  engcog_1 + engcog_2 + engcog_3
'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}

multilevel_rels_fat <- function(df){
  model <- '
    level: 1
        fatphy_w =~ fatphy_1 + fatphy_2 + fatphy_3
        fatmen_w =~ fatmen_1 + fatmen_2 + fatmen_3
        fatemo_w =~ fatemo_1 + fatemo_2 + fatemo_3

    level: 2
        fatphy_b =~ fatphy_1 + fatphy_2 + fatphy_3
        fatmen_b =~ fatmen_1 + fatmen_2 + fatmen_3
        fatemo_b =~ fatemo_1 + fatemo_2 + fatemo_3
'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}

df <- data
multilevel_rels_aff <- function(df){
  model <- '
    level: 1
        poaff_w =~ poaff_1 + poaff_2 + poaff_3 + poaff_4 + poaff_5
        negaff_w =~ negaff_1 + negaff_2 + negaff_3 + negaff_4 + negaff_5

    level: 2
        poaff_b =~ poaff_1 + poaff_2 + poaff_3 + poaff_4 + poaff_5
        negaff_b =~ negaff_1 + negaff_2 + negaff_3 + negaff_4 + negaff_5
'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}

multilevel_rels_attitudes <- function(df){
  model <- '
    level: 1
        turn_w =~ turn_1 + turn_2 + turn_3
        load_w  =~ load_1 + load_2 + load_3
        jobaut_w  =~ jobaut_1 + jobaut_2 + jobaut_3
    level: 2
        turn_b =~ turn_1 + turn_2 + turn_3
        load_b  =~ load_1 + load_2 + load_3
        jobaut_b  =~ jobaut_1 + jobaut_2 + jobaut_3
'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}

multilevel_rels_digiflexicraft <- function(df){
  model <- '
    level: 1
        digi_w  =~ digi_1 + digi_2 + digi_3 + digi_4 + digi_5
        flexi_w  =~ flexi_1 + flexi_2 + flexi_3 + flexi_4 + flexi_5
        craft_w  =~ craft_1 + craft_2 + craft_3 + craft_4 + craft_5 + craft_6 + craft_7 + craft_8 + craft_9

    level: 2
        digi_b  =~ digi_1 + digi_2 + digi_3 + digi_4 + digi_5
        flexi_b  =~ flexi_1 + flexi_2 + flexi_3 + flexi_4 + flexi_5
        craft_b  =~ craft_1 + craft_2 + craft_3 + craft_4 + craft_5 + craft_6 + craft_7 + craft_8 + craft_9

'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}

multilevel_rels_cosu <- function(df){
  model <- '
    level: 1
        cwsupp_w  =~ cwsupp_1 + cwsupp_2 + cwsupp_3 + cwsupp_4
        cwunder_w  =~ cwunder_1 + cwunder_2 + cwunder_3 + cwunder_4
        ssupp_w  =~ ssupp_1 + ssupp_2 + ssupp_3 + ssupp_4
        sunder_w  =~ sunder_1 + sunder_2 + sunder_3 + sunder_4
        sdirect_w  =~ sdirect_1 + sdirect_2 + sdirect_3 + sdirect_4 + sdirect_5

    level: 2
        cwsupp_b  =~ cwsupp_1 + cwsupp_2 + cwsupp_3 + cwsupp_4
        cwunder_b  =~ cwunder_1 + cwunder_2 + cwunder_3 + cwunder_4
        ssupp_b  =~ ssupp_1 + ssupp_2 + ssupp_3 + ssupp_4
        sunder_b  =~ sunder_1 + sunder_2 + sunder_3 + sunder_4
        sdirect_b  =~ sdirect_1 + sdirect_2 + sdirect_3 + sdirect_4 + sdirect_5
'

  fit <- sem(model, data = df, cluster = "ID", std.lv  = TRUE, verbose = TRUE, estimator="MLR")

  rels_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[1, ] %>% round(., 3)
  rels_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[1, ] %>% round(., 3)
  omega_within <- semTools::reliability(fit, what = c("alpha", "omega"))$within[2, ] %>% round(., 3)
  omega_between <- semTools::reliability(fit, what = c("alpha", "omega"))$ID[2, ] %>% round(., 3)

  rels <- cbind(rels_within, rels_between, omega_within, omega_between)
  rels
}



# Pie2 --------------------------------------------------------------------

pie2 <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45,
          col = NULL, border = NULL, lty = NULL, main = NULL, ...)
{
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col))
    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.30) * P$x, c(1, 1.30) * P$y)
      text(1.4 * P$x, 1.4 * P$y, labels[i], xpd = TRUE,
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}


# Correlation table -------------------------------------------------------

corstars_no_stars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower", "none"),
                             result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R,sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  else if(removeTriangle[1]=="none"){
    Rnew <- as.matrix(Rnew)
    Rnew <- as.data.frame(Rnew)
  }
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}


# Create Table from SEM ---------------------------------------------------

table_sem <- function(lavaan_object) {
  estimates <- parameterEstimates(lavaan_object, se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                     standardized = FALSE) %>% filter(op == "~" | op == ":=") %>%
    unite("Label", lhs:label) %>% mutate(across(c('est', 'se', 'z', 'ci.lower', 'ci.upper'), round, 2)) %>%
    mutate(pvalue = round(pvalue, 3)) %>%  flextable(.)
  estimates
}

