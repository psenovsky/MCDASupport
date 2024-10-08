#' electre1 : ELECTRE I method used to solve multiple criteria decision making
#'
#' @description
#' ELECTRE I method used to solve multiple criteria decision making
#'
#' The acronym ELECTRE stands for: ELimination Et Choix Traduisant la REalite
#' (ELimination and Choice Expressing REality). This method is based on the
#' concept of concordance and discordance.
#'
#' ELECTRE I does not provide rank, but allows the user to identify so called
#' kernel of the decision - the alternatives, which cannot be eleminated from
#' decision making as obviously inferior, thus simplifying the decision
#' problem. This is a core concept for variety of the methods in ELECTRE
#' family, so much so, that the kernel computation procedure is also used
#' in \code{\link{electre2}} and \code{\link{electre1s}} methods, so much
#' so that the procedure was separated into \code{\link{ELECTRE1_Kernel}}
#' function to be utilized by all these methods.
#'
#' Similarly the Concordance and discordance indexes, the outranking for the
#' alternatives is based on are more general concept shared by other methods.
#' Computational procedures for them are available in
#' \code{\link{ELECTRE_ConcordanceMatrix}} and
#' \code{\link{ELECTRE_DiscordanceMatrix}} function.
#'
#' Computation procedure for the method is realized in following steps:
#' \itemize{
#'   \item compute concordance matrix C using
#' \code{\link{ELECTRE_ConcordanceMatrix}}
#'   \item compute discordance matrix D using
#' \code{\link{ELECTRE_DiscordanceMatrix}}
#'   \item compute dominance matrix by comparing values of C to value of
#' concordance threshold and D to discordance threshold
#'   \item compute solution's kernel based on dominance matrix using
#' \code{\link{ELECTRE1_Kernel}} function.
#' }
#'
#' Concordancce matrix (index) measures strength of the statement that
#' alternative a outranks alternative b, while discordance matrix (index)
#' together with discordance threshold (exceeding this threshold) can prevent
#' such outranking.
#'
#' Concoradce matrix C(a,b) is defined as:
#'
#' \mjsdeqn{C(a,b) = \frac{1}{W} \sum_{\forall j: g_j(a) \ge g_j(b)}w_j}
#'
#' where
#'
#' \mjsdeqn{W = \sum_{j=1}^{n}w_j}
#'
#' While discoradce matrix D(a,b) is defined for:
#'
#' \mjsdeqn{g_j(a) \ge g_j(b) \forall j: D(a,b) = 0}
#'
#' and for everything else:
#'
#' \mjsdeqn{D(a,b) = \max_{j} \frac{g_j(b)-g_j(a)}{\delta_j}}
#'
#' where
#'
#' \mjsdeqn{\delta_j = \max_{j} g_j(a)-g_j(b)}
#'
#' Where a, b ... are alternatives to be compared, \mjseqn{g_j(x)} ...
#' performance of alternative x in criterium j, C ... comcordance
#'  matrix consisting of concordance indexes C(a,b), D ...
#'  discordance matrix consisting of discordance indexes D(a,b),
#'  \mjseqn{w_j} ... weight of criterium j and \mjseqn{\delta_j}
#'  is maximal difference of alternatives a and b preformance
#'  across the criteria.
#'
#' Alternative a strongly outranks alternative b if and only in value of their
#'  concordance index is greater or equal to concordance threshold and at same
#'  time discordance index of this comparison is lower or equal do discordance
#'  index. In such case value in dominance matrix will be equal to 1 (aSb).
#'  Otherwise the value will be 0.
#'
#' \mjsdeqn{Dom(a,b) = \left\lbrace\begin{array}{ll} 1 & iff \; C(a,b) \ge c_{thres} \; and \; D(a,b) \le d_{thres} \cr 0 & otherwise\end{array}\right.}
#'
#' Kernel K is defined as a subset of alternatives (nodes in graph) which are
#'  incomparable in terms of stron outranking (aSb), and the alternatives not
#'  contained in K are outranked by at least one alternative belonging to K.
#'  Kernel is being computed from dominance matrix. We compute sums of rows for
#'  the dominance matrix and select the alternatives, where this sum is equal
#'  to 0 as a basis for the kernel estimation. If there is no such alternative,
#'  that there is no clear kernel.
#'
#' Othervise more throughtfull exploration of the dominance relation is needed.
#'
#' Technically the function implements approach authored by M. Balamurali in
#'  pyDecisions (in Python) and reimplements it in R.
#'
#' @references
#' Balamurali, M.: pyDecisions - A Python Library of management decision making
#'  techniques. Avilable on-line from
#'  \url{https://github.com/Valdecy/pyDecisions}
#'
#' Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support:
#'  Methods and Applications in Engineering and Infrastructure investment.
#'  Springer 2000, 208 p., ISBN 978-1-4757-5057-7
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE I
#' @keywords concordance matrix
#' @keywords discordane matrix
#' @keywords kernel
electre1 <- R6Class("electre1",
  public = list(
    #' @field pm_orig original (unmodified) performace matrix
    pm_orig = NULL,

    #' @field pm transformed performance matrix, all criteria will be maximized
    pm = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field minmaxcriteria vector of direction of each of the criteria. "min"
    #' (resp."max") indicates that the criterion has to be minimized
    #' (maximized). Vector canbe replaced by single max or min value if all
    #' criteria are maximized ormimimized.
    minmaxcriteria = NULL,

    #' @field c_threshold concordance theeshold (in interval <0; 1>)
    c_threshold = 1,

    #' @field d_threshold discordance threshold (in interval <0; 1>)
    d_threshold = 0,

    #' @field ConcordanceMatrix Concordance matrix
    ConcordanceMatrix = NULL,

    #' @field DiscordanceMatrix discordance matrix
    DiscordanceMatrix = NULL,

    #' @field PreferenceMatrix preference matrix
    PreferenceMatrix = NULL,

    #' @field GraphResult visualizing overranking relation between the
    #'  alternatives using network diagram
    GraphResult = NULL,

    #' @field Kernel kernel of the solution
    Kernel = NULL,

    #' @field Dominated vector of alternatives identified as dominated
    Dominated = NULL,

    #' @description
    #' public constructor, creates electre1 object.
    #' @param pm Performance matrix
    #' @param w weight vector
    #' @param minmaxcriteria vector of direction of each of the criteria. "min"
    #' (resp."max")indicates that the criterion has to be minimized
    #' (maximized). Vector canbe replaced by single max or min value if all
    #' criteria are maximized or minimized.
    #' @param concordance_threshold concordance threshold (in interval <0; 1>)
    #' @param discordance_threshold discordance threshold (in interval <0; 1>)
    #' @param test Boolean value specifying whether the consistency tests in
    #'  the constructor should be used. FALSE value is usually used during
    #'  sensitivity testing.
    #' @examples
    #' PM <- cbind(
    #'   c(103000,101300,156400,267400,49900,103600,103000,170100,279700,405000),
    #'   c(171.3,205.3,221.7,230.7,122.6,205.1,178.0,226.0,233.8,265.0),
    #'   c(7.65,7.90,7.90,10.50,8.30,8.20,7.20,9.10,10.90,10.30),
    #'   c(352,203,391,419,120,265,419,419,359,265),
    #'   c(11.6,8.4,8.4,8.6,23.7,8.1,11.4,8.1,7.8,6.0),
    #'   c(88.0,78.3,81.5,64.7,74.1,81.7,77.6,74.7,75.5,74.7),
    #'   c(69.7,73.4,69.0,65.6,76.4,73.6,66.2,71.7,70.9,72.0))
    #' rownames(PM) <- c("CBX16","P205G","P405M","P605S","R4GTL",
    #'   "RCLIO","R21TS","R21TU","R25BA","ALPIN")
    #' colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
    #' minmaxcriteria <-c("min","max","min","max","min","min","min")
    #' w <- c(0.3,0.1,0.3,0.2,0.1,0.2,0.1)
    #' M <- electre1$new(PM, w, minmaxcriteria, concordance_threshold = 0.8,
    #'   discordance_threshold = 0.1)
    initialize = function(pm, w,
                          minmaxcriteria = "max",
                          concordance_threshold = 1,
                          discordance_threshold = 0,
                          test = TRUE) {
      # test provided parameters
      if (test) {
        if (!(is.matrix(pm) || (is.data.frame(pm)))) {
          stop("wrong performance matrix, should be a matrix or a data frame")
        }
        if (!is.numeric(unlist(pm))) {
          stop("Only numeric values in performance matrix expected")
        }
        if (!(is.vector(w, mode = "numeric"))) {
          stop("criteriaWeights should be a numeric vector")
        }
        if (ncol(pm) != length(w)) {
          stop("length of criteriaWeights should be checked")
        }
        if (!is.numeric(concordance_threshold) || concordance_threshold < 0
            || concordance_threshold > 1) {
          stop("Concordance threshold out of bounds")
        }
        if (!is.numeric(discordance_threshold) || discordance_threshold < 0
            || discordance_threshold > 1) {
          stop("Discordance threshold out of bounds")
        }
      }
      # initialize class
      self$pm_orig <- pm
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      self$minmaxcriteria <- minmaxcriteria
      self$w <- w
      self$c_threshold <- concordance_threshold
      self$d_threshold <- discordance_threshold
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE I problem. Usually doesn't need to be run manually
    #'  as it is called by class constructor.
    compute = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      alt  <- rownames(self$pm)
      cm <- ELECTRE_ConcordanceMatrix(self$pm, self$w)  #Concordance matrix
      dm <- ELECTRE_DiscordanceMatrix(self$pm) #discordance matrix

      #dominance matrix (preference matrix)
      dom <- matrix(data = 0, nrow = nalt, ncol = nalt)
      #aSb iff C(a,b) >= c_thres and D(a,b) <= d_thres
      dom <- ifelse((cm >= self$c_threshold & dm <= self$d_threshold), 1, dom)
      diag(dom) <- 0
      rownames(dom) <- alt
      colnames(dom) <- alt
      kern <- ELECTRE1_Kernel(dom)

      self$ConcordanceMatrix <- t(cm)
      self$DiscordanceMatrix <- t(dm)
      self$PreferenceMatrix <- dom
      self$Kernel <- kern$kernel
      self$GraphResult <- kern$graph
      self$Dominated <- kern$dominated
    },

    #' @description
    #' summary of the ELECTRE I resutls.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      msg2 <- ""
      kernel_string <- paste(self$Kernel, collapse = ", ")
      cat("Electre 1: \n")
      cat("processed ", nalt, " alternatives in ", ncri, " criteria\n\n")
      cat("identified kernel of the solution: ", kernel_string, "\n\n")
      if (length(self$Dominated) > 0) {
        dom <- paste(self$Dominated, collapse = ", ")
        cat("dominated alternatives: ", dom, "\n")
      }else {
        cat("no dominated alternatives detected\n")
      }
    },

    #' @description
    #' performs sensitivity analysis of the provided solution. It takes present
    #'  solution as a starting point and then changes concordance (c) /
    #'  discordance (d) threshold.
    #'
    #' For concordance threshold (c) two intervals are checked:
    #' \itemize{
    #'   \item from c to 1
    #'   \item from c to d backvard
    #' }
    #'
    #' For discordance threshold (d) also two intervals are checked:
    #' \itemize{
    #'   \item from d to c
    #'   \item from 0 to d
    #' }
    #'
    #' The goal is to identify uper and lower limits of sensitivity of the
    #'  provided solution. Basically we search for last number in provided
    #'  interval, for which the solution does not change.
    #'
    #' @param step iteration step the function uses to generate the testing
    #'  interval
    #'
    #' @return
    #' returns dataframe specifyin upper and lower limits of the solution's
    #'  sensitivity for both thresholds. Limits values can be replaced by
    #'  insens. if no limit has been identified.
    sensitivity = function(step = 0.01) {
      thresholds <- c("concordance", "discordance")
      upper_limit <- lower_limit <- c(0, 0)
      df <- data.frame(thresholds, lower_limit, upper_limit)
      # C upper limit
      c_interval <- seq(from = self$c_threshold + step, to = 1, step = step)
      c_prev <- self$c_threshold
      for (c1 in c_interval) {
        t <- electre1$new(self$pm_orig, self$w, self$minmaxcriteria,
                          concordance_threshold = c1,
                          discordance_threshold = self$d_threshold,
                          test = FALSE)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) { # limit identified
          df[1, 3] <- c_prev
          break
        }
        c_prev <- c1
      }
      if (df[1, 3] == 0) df[1, 3] <- "insens."
      # C lower limit
      c_interval <- seq(from = self$c_threshold - step,
                        to = self$d_threshold + step, step = -step)
      c_prev <- self$c_threshold
      for (c1 in c_interval) {
        t <- electre1$new(self$pm_orig, self$w, self$minmaxcriteria,
                          concordance_threshold = c1,
                          discordance_threshold = self$d_threshold,
                          test = FALSE)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) { # limit identified
          df[1, 2] <- c_prev
          break
        }
        c_prev <- c1
      }
      if (df[1, 2] == 0) df[1, 2] <- "insens."
      # D upper limit
      d_interval <- seq(from = self$d_threshold + step,
                        to = self$c_threshold - step, step = step)
      d_prev <- self$d_threshold
      for (d1 in d_interval) {
        t <- electre1$new(self$pm_orig, self$w, self$minmaxcriteria,
                          concordance_threshold = self$c_threshold,
                          discordance_threshold = d1, test = FALSE)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) { # limit identified
          df[2, 3] <- d_prev
          break
        }
        d_prev <- d1
      }
      if (df[2, 3] == 0) df[2, 3] <- "insens."
      # D lower limit
      d_interval <- seq(from = self$d_threshold - step,
                        to = 0, step = -step)
      d_prev <- self$d_threshold
      for (d1 in d_interval) {
        t <- electre1$new(self$pm_orig, self$w, self$minmaxcriteria,
                          concordance_threshold = self$c_threshold,
                          discordance_threshold = d1, test = FALSE)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) { # limit identified
          df[2, 2] <- d_prev
          break
        }
        d_prev <- d1
      }
      if (df[2, 2] == 0) df[2, 2] <- "insens."
      return(df)
    }
  )
)