#' ELECTRE II method used to solve multiple criteria decision making
#'
#' @description
#' The acronym ELECTRE stands for: ELimination Et Choix Traduisant la REalite
#'  (ELimination and Choice Expressing REality). ELECTRE II method is then
#'  designed for ranking purposes as oposed to ELECTRE I method which is
#'  useful for identification of kernel - as alternatives, which cannot be
#'  eliminated, are not obviously dominated by other alternatives.
#'
#' ELECTRE II uses concordance and discordance indexes to construct two partial
#'  pre-orders by trying do formally describe so called strong and weak
#'  preferences between the alternatives. Strenght is this regard is taken as
#'  strength of belief, that alternative a is better then alternative b.
#'
#' Two partial preorders and then used in aggregation procedure to construct
#'  final patrial preorder.
#'
#' The code is partially inspired by code authored by M. Balamurali in
#'  pyDecisions (in Python) and reimplements it in R, thou some portions of the
#'  function are solved differently in here. For example whole graph
#'  simplification process in here relies on functionality of
#'  iGraph package. This method also differently implements the final
#'  recommendation (total partial order).
#'
#' Considering that the ELECTRE II method is based on ELECTRE I method with
#'  additions to produce ranking, the computation starts with computing
#'  concordance \code{\link{ELECTRE_ConcordanceMatrix}} and discordance
#'  \code{\link{ELECTRE_DiscordanceMatrix}} matrixes. See
#'  \code{\link{electre1}} documentation for mathematical aparatus.
#'
#' Based on concordance and discordance matrixes and comparing its values with
#'  the thresholds, we can establish strong \mjseqn{dom_s} or weak
#'  \mjseqn{dom_w} dominance.
#'
#' \mjsdeqn{dom_s(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; (C(a,b) \ge c^+ \; and \; d(a,b) \le d^+) \; or \; (C(a,b) \ge c^0 \; and \; d(a,b) \le d^-) \cr 0 & otherwise\end{array}\right.}
#'
#' \mjsdeqn{dom_w(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; (C(a,b) \ge c^0 \; and \; d(a,b) \le d^+) \; or \; (C(a,b) \ge c^- \; and \; d(a,b) \le d^-) \cr 0 & otherwise\end{array}\right.}
#'
#' Next preference ratio S(a,b) needs to be computed for \mjseqn{dom_s} and
#'  \mjseqn{dom_w} matrixes. We establish sum of weights of criteria
#'  \mjseqn{w^+}, where a outperforms b and sum of criteria \mjseqn{w^-},
#'  where the opposite applies.
#'
#' \mjsdeqn{w^+ = \sum_{\forall k: PM_k(a)>PM_k(b)} w_k}
#'
#' and
#'
#' \mjsdeqn{w^- = \sum_{\forall k: PM_k(a)<PM_k(b)} w_k}
#'
#' \mjsdeqn{S(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; w^- = 0 \; or \; \frac{w^+}{w^-} \ge 1 \cr 0 & otherwise\end{array}\right.}
#'
#' Technically the computation needs to be performed only on pairs of (a,b)
#'  where either \mjseqn{dom_s} and or \mjseqn{dom_w} showed possibility of a
#'  outranking b, otherwise we can directly establish, that S(a,b) = 0.
#'
#' Strong and weak dominances are used to then to form first and second total
#'  preorder. Both matrixes need to be simplified to remove loops from it first.
#'
#' These preorders aim to measure the haw much alternatives over-rank each
#'  other. The procedure is same for both orders. First we consolidate
#'  information on dominances by
#'
#' \mjsdeqn{dom = 2 \cdot dom_s + dom_w}
#'
#' Then we create from all options not strongly outranked by any other option
#'  the set D. Those options within D that are linked to each other by a weak
#'  outranking relationship constitute the set U. The set B consists of all
#'  options from U not weakly outranked by any other from within U. The single
#'  option or group of equally ranked options is defined by the union of the
#'  sets (D - U) and B. This provides enough information to form full ranking.
#'
#' Second preorder uses same approach described in previous paragraph, but in
#'  reversed order. We achieve this by providing ranking algorithm transposed
#'  dominance matrixes and reversing resulting order.
#'
#' Since we got two separate preorders, agregation procedure process these into
#'  single ranking order. To do that some simple rules are used:
#'
#' \itemize{
#'   \item if a is preferred to b in both pre-orders, then this will also be
#'  the case in final order
#'   \item if a has an equivalent ranking to b in the one of the complete
#'  pre-orders, but is preferred in the other, then a precedes b in the final
#'  order
#'   \item if a is preferred to b in the first complete pre-order, but b is
#'  preferred to a in the second, then the two alternatives are incomparable
#'  in the final order.
#' }
#'
#' @references
#' Balamurali, M.: pyDecisions - A Python Library of management decision
#'  making techniques. Avilable on-line from
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
#' @keywords ELECTRE II
#' @keywords ELECTRE I
#' @keywords strong outranking
#' @keywords weak outranking
#' @keywords concordance matrix
#' @keywords discordance matrix
electre2 <- R6Class("electre2",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix (direction of all criteria is
    #'  maximalize)
    pm = NULL,

    #' @field w weght vector
    w = NULL,

    #' @field minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    minmaxcriteria = "max",

    #' @field c_minus first of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Value is alway between 0-1 and \mjsdeqn{0 \le c^- \le c^0 \le c^+ \le 1}
    #'  default 0.65.
    c_minus = 0.65,

    #' @field c_zero second of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Default value 0.75.
    c_zero = 0.75,

    #' @field c_plus third of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Default calue 0.85.
    c_plus = 0.85,

    #' @field d_minus first of two parameters defining discordance threshold.
    #'  Thershold is defined as range, where \mjsdeqn{0 \le d^- \le d^+ \le 1}
    #' Default value 0.25.
    d_minus = 0.25,

    #' @field d_plus first of two parameters defining discordance threshold.
    #'  Default value 0.5.
    d_plus = 0.5,

    #' @field ConcordanceMatrix concordance matrix for decision problem
    ConcordanceMatrix = NULL,

    #' @field DiscordanceMatrix discordance matrix for decision problem
    DiscordanceMatrix = NULL,

    #' @field StrongOutranking adjacancy matrix for strong outranking
    StrongOutranking = NULL,

    #' @field WeakOutranking adjacancy matrix for weak outranking
    WeakOutranking = NULL,

    #' @field firstTotalPreorder first total preorder
    firstTotalPreorder = NULL,

    #' @field secondTotalPreorder second total preorder
    secondTotalPreorder = NULL,

    #' @field finalPreorderMatrix final preorder matrix
    finalPreorderMatrix = NULL,

    #' @field incomparableAlternatives matrix of incomparable alternatives
    incomparableAlternatives = NULL,

    #' @field graphResult visualization of final preorder matrix
    graphResult = NULL,

    #' @field finalPreorder final preorder vector
    finalPreorder = NULL,

    #' @field finalPreorderSorted sorted final preoder vector of alternatives
    finalPreorderSorted = NULL,

    #' @description
    #' public constructor, creates electre2 object.
    #' @param pm performance matrix
    #' @param w weght vector
    #' @param minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    #' @param c_minus first of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Value is alway between 0-1 and \mjsdeqn{0 \le c^- \le c^0 \le c^+ \le 1}
    #'  default 0.65.
    #' @param c_zero second of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Default value 0.75.
    #' @param c_plus third of three concordance threshold parameters.
    #'  ELECTRE II method implements the concotdance threshold as fuzzy
    #'  triangle. Default calue 0.85.
    #' @param d_minus first of two parameters defining discordance threshold.
    #'  Thershold is defined as range, where \mjsdeqn{0 \le d^- \le d^+ \le 1}
    #' Default value 0.25.
    #' @param d_plus first of two parameters defining discordance threshold.
    #'  Default value 0.5.
    #' @examples
    #' PM <- cbind(
    #'  c(103000,101300,156400,267400,49900,103600,103000,170100,279700,405000),
    #'  c(171.3,205.3,221.7,230.7,122.6,205.1,178.0,226.0,233.8,265.0),
    #'  c(7.65,7.90,7.90,10.50,8.30,8.20,7.20,9.10,10.90,10.30),
    #'  c(352,203,391,419,120,265,419,419,359,265),
    #'  c(11.6,8.4,8.4,8.6,23.7,8.1,11.4,8.1,7.8,6.0),
    #'  c(88.0,78.3,81.5,64.7,74.1,81.7,77.6,74.7,75.5,74.7),
    #'  c(69.7,73.4,69.0,65.6,76.4,73.6,66.2,71.7,70.9,72.0))
    #' rownames(PM) <- c("CBX16","P205G","P405M","P605S",
    #'  "R4GTL","RCLIO","R21TS","R21TU","R25BA","ALPIN")
    #' colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
    #' minmaxcriteria <-c("min","max","min","max","min","min","min")
    #' w <- c(0.3,0.1,0.3,0.2,0.1,0.2,0.1)
    #' t <- electre2$new(PM, w, minmaxcriteria)
    initialize = function(pm, w,
                          minmaxcriteria = "max",
                          c_minus = 0.65, c_zero = 0.75, c_plus = 0.85,
                          d_minus = 0.25, d_plus = 0.5) {
      # validate params
      validation$validate_pm(pm)
      ncri <- ncol(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      thres <- c(d_minus, d_plus, c_minus, c_zero, c_plus)
      validation$validation_vector_in_interval(thres, 0, 1,
                                               "C-, C0, C+, D-, D+")
      validation$validate_vector_progression(thres)
      # end of param validation

      self$pm_orig <- pm
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      self$w <- w
      self$minmaxcriteria <- minmaxcriteria
      self$c_minus <- c_minus
      self$c_zero <- c_zero
      self$c_plus <- c_plus
      self$d_minus <- d_minus
      self$d_plus <- d_plus
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE II problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      nalt  <- nrow(self$pm)  #no. of alternatives
      alt   <- rownames(self$pm)
      cm <- ELECTRE_ConcordanceMatrix(self$pm, self$w)  #Concordance matrix
      dm <- ELECTRE_DiscordanceMatrix(self$pm)  #discordance matrix
      #dominance matrix (preference matrix) - basic
      dominance_s <- matrix(data = 0, nrow = nalt, ncol = nalt)
      dominance_w <- matrix(data = 0, nrow = nalt, ncol = nalt)
      dominance_s <- ifelse((cm >= t(cm) & lower.tri(cm) &
                               ((cm >= self$c_plus & dm <= self$d_plus) |
                                  (cm >= self$c_zero & dm <= self$d_minus))), 1,
                            dominance_s)
      dominance_w <- ifelse((cm >= t(cm) & lower.tri(cm) &
                               ((cm >= self$c_zero & dm <= self$d_plus) |
                                  (cm >= self$c_minus & dm <= self$d_minus))),
                            1, dominance_w)
      diag(dominance_s) <- 0
      diag(dominance_w) <- 0
      # P+/P- ratio evaluation
      s_strong  <- private$preference_ratio(dominance_s, self$pm)
      s_weak    <- private$preference_ratio(dominance_w, self$pm)
      g_strong  <- simplify(graph_from_adjacency_matrix(s_strong)) #remove loops
      am_strong <- Graph2AdjancancyMatrix(g_strong, alt)
      g_weak    <- simplify(graph_from_adjacency_matrix(s_weak))
      am_weak   <- Graph2AdjancancyMatrix(g_weak, alt)
      #ranking
      rank_a   <- private$ranking(am_strong, am_weak, alt) #1st total preorder
      # The second pre-order is obtained using the inverse ranking procedure.
      # The same basic algorithm as above is utilised, with the following
      # modifications.
      # Invert the direction of the strong and weak outranking graphs GF, and
      # Gf, the ranks obtained from the inverted graphs using the direct
      # procedure is adjusted as follows:
      # 'inverse procedure' rank (r2(a»=1+(the number of ranks) (r'2(a)max)
      # -'direct procedure' ranking (r'2(a))
      #2nd total preorder V2
      rank_d   <- rev(private$ranking(t(am_strong), t(am_weak), alt))
      #final partial pre-order
      pref <- matrix(data = 0, nrow = nalt, ncol = nalt)
      incompar <- matrix(data = 0, nrow = nalt, ncol = nalt)
      for (i in 1:nalt) {
        r1_a <- match(alt[i], rank_a)
        r1_d <- match(alt[i], rank_d)
        for (j in 1:nalt) {
          if (i != j) {
            r2_a <- match(alt[j], rank_a)
            r2_d <- match(alt[j], rank_d)
            # if a is preferred to b in 2 complete pre-orders, then this will
            # also be the case in final pre-order
            if (r1_a > r2_a && r1_d > r2_d) pref[i, j] <- 1
            # if a has an equivalent ranking to b in the one of the complete
            # pre-orders, but is preferred to b in the other, then a precedes
            # b in the final pre-order
            if ((r1_a == r2_a && r1_d > r2_d) ||
                  (r1_a > r2_a && r1_d == r2_d)) {
              pref[i, j] <- 1
            }
            # if a is preferred to b in the first complete pre-order, but b is
            # preferred to a in the second, then the 2 options are incomparable
            # in the final pre-order.
            if ((r1_a > r2_a && r1_d < r2_d) || (r1_a < r2_a && r1_d > r2_d)) {
              incompar[i, j] <- 1
            }
          }
        }
      }
      colnames(pref) <- alt
      rownames(pref) <- alt
      colnames(incompar) <- alt
      rownames(incompar) <- alt
      graph <- plot.prefM(pref)
      # does not provide rank but a degree of preference, which can be directly
      # translated to rank
      rank_f <- rowSums(pref)
      names(rank_f) <- alt
      rank_f_sorted <- sort(rank_f, decreasing = TRUE) + 1
      rank_f2 <- rank_f
      # final order
      for (i in 1:nalt) {
        if (i == 1) {
          rank_f2[names(rank_f_sorted[1])] <- 1
          prev_rank <- 1
        } else {
          if (prev_val > rank_f_sorted[i]) prev_rank <- prev_rank + 1
          rank_f2[names(rank_f_sorted[i])] <- prev_rank
        }
        prev_val <- rank_f_sorted[i]
      }
      rank_f_sorted2 <- sort(rank_f2, decreasing = TRUE)
      self$ConcordanceMatrix <- t(cm)
      self$DiscordanceMatrix <- t(dm)
      self$StrongOutranking <- am_strong
      self$WeakOutranking <- am_weak
      self$firstTotalPreorder <- rank_a
      self$secondTotalPreorder <- rank_d
      self$finalPreorderMatrix <- pref
      self$incomparableAlternatives <- incompar
      self$graphResult <- graph
      self$finalPreorder <- rank_f2
      self$finalPreorderSorted <- rank_f_sorted2
    },

    #' @description
    #' summary of the ELECTRE II method resutls.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("ELECTRE II:\n", "processed ", nalt,
                 " alternatives in ", ncri, " criteria\n\n",
                 "Preference matrix:\n"))
      print(self$finalPreorderMatrix, pretty = TRUE)
      cat(paste0("\nIncomparable alternatives:\n"))
      print(self$incomparableAlternatives, pretty = TRUE)
      cat(paste0("\nFinal sorted order:\n"))
      print(self$finalPreorderSorted, pretty = TRUE)
    },

    #' @description
    #' sensitivity analysis for ELECTRE II is performed on values of
    #'  concordance and discordance thresholds. Since these thresholds are
    #'  defined as fuzzy numbers, the analysis is performed on five parameters
    #'  c-, c0 and c+ for concordance thresholds, d- and d+ for discordance
    #'  threshold.
    #'
    #' Analysis always explores changes of a single parametr, while all other
    #'  are held static (non changing).
    #'
    #' @param step iteration step the function uses to generate the testing
    #'  interval
    #'
    #' @return
    #' returns dataframe specifyin upper and lower limits of the solution's
    #'  sensitivity the thresholds. Limits values can be replaced by
    #'  insens. if no limit has been identified.
    sensitivity = function(step = 0.01) {
      df <- data.frame(matrix(0, nrow = 5, ncol = 4))
      colnames(df) <- c("threshold", "from", "default", "to")
      c_min0  <- rev(seq(from = 0, to = self$c_minus, by = step))
      c_min1  <- seq(from = self$c_minus, to = self$c_zero, by = step)
      c_zero0 <- rev(seq(from = self$c_minus, to = self$c_zero, by = step))
      c_zero1 <- seq(from = self$c_zero, to = self$c_plus, by = step)
      c_plus0 <- rev(seq(from = self$c_zero, to = self$c_plus, by = step))
      c_plus1 <- seq(from = self$c_plus, to = 1, by = step)
      d_min0  <- rev(seq(from = 0, to = self$d_minus, by = step))
      d_min1  <- seq(from = self$d_minus, to = self$d_plus, by = step)
      d_plus0 <- rev(seq(from = self$d_minus, to = self$d_plus, by = step))
      d_plus1 <- seq(from = self$d_plus, to = 1, by = step)
      df[1, 1] <- "c-" #c- threshold sensitivity
      df[1, 2] <- private$sens_cminus(c_min0)
      df[1, 3] <- self$c_minus
      df[1, 4] <- private$sens_cminus(c_min1)
      df[2, 1] <- "c0" #c0 threshold sensitivity
      df[2, 2] <- private$sens_czero(c_zero0)
      df[2, 3] <- self$c_zero
      df[2, 4] <- private$sens_czero(c_zero1)
      df[3, 1] <- "c+" #c+ threshold sensitivity
      df[3, 2] <- private$sens_cplus(c_plus0)
      df[3, 3] <- self$c_plus
      df[3, 4] <- private$sens_cplus(c_plus1)
      df[4, 1] <- "d-" #d- threshold sensitivity
      df[4, 2] <- private$sens_dminus(d_min0)
      df[4, 3] <- self$d_minus
      df[4, 4] <- private$sens_dminus(d_min1)
      df[5, 1] <- "d+" #d+ threshold sensitivity
      df[5, 2] <- private$sens_dplus(d_plus0)
      df[5, 3] <- self$d_plus
      df[5, 4] <- private$sens_dplus(d_plus1)
      return(df)
    }
  ),

  private = list(
    # @description
    # computes preference ratio P+/P- and constructs strong or weak outranking
    # matrixes in dominance_s or dominance_w
    #
    # @param dom dominance matrix
    # @param pm performance matrix
    #
    # @returns preference ratio
    preference_ratio = function(dom, pm) {
      nalt <- length(rownames(pm))
      s <- matrix(data = 0, nrow = nalt, ncol = nalt)
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (dom[i, j] == 1) { #P+(a,b)/P-(a,b) >= 1
            w_plus <- sum(ifelse(pm[i, ] > pm[j, ], w, 0)) #P+(a,b)
            w_minus <- sum(ifelse(pm[i, ] < pm[j, ], w, 0)) #P-(a,b)
            if (w_minus == 0 || w_plus / w_minus >= 1) s[i, j] <- 1
          }
        }
      }
      return(s)
    },

    # @description
    # function for ranking
    #
    # @param dom_s simplified graph of strong preferences
    # @param dom_d simplified graph of weak preferences
    # @param alt alternatives
    #
    # @return ramking vector
    ranking = function(dom_s, dom_w, alt) {
      #py code: dominance = np.clip(2*dominance_s + dominance_w, 0, 2)
      #since dom_s and _w should consist of 0/1:
      dominance <- 2 * dom_s + dom_w
      dominance[dominance == 3] <- 2
      colnames(dominance) <- rownames(dominance) <- alt
      y <- alt
      nalt <- length(alt)
      rank <- list()
      while (length(y) > 0) {
        d <- u <- b <- a <- NULL
        for (j in 1:nalt) {
          check_d <- sum(ifelse((dominance[, j] == 2), 1, 0))
          #all options not strongly outranked by any other option form the
          # set D.
          if (check_d == 0 && sum(dominance[, j]) != -nalt) {
            d[length(d) + 1] <- alt[j]
          }
        }
        idx <- match(d, alt)  #get index of alternatives in D set
        for (m in seq_along(d)) {
          for (n in seq_along(d)) {
            if (dominance[idx[m], idx[n]] == 1) {
              # Those options within D that are linked to each other by a weak
              # outranking relationship constitute the set U
              if (!(alt[idx[m]] %in% u)) u[length(u) + 1] <- alt[idx[m]]
              if (!(alt[idx[n]] %in% u)) u[length(u) + 1] <- alt[idx[n]]
            }
          }
        }
        idx <- match(u, alt)
        if (length(idx) > 0) {
          for (m in seq_along(u)) {
            check_b <- 0
            for (n in seq_along(u)) {
              if (dominance[idx[n], idx[m]] == 1) check_b <- check_b + 1
            }
            if (check_b == 0) {
              #The set B consists of all options from U not weakly outranked
              # by any other from within U.
              if (!(alt[idx[m]] %in% b)) b[length(b) + 1] <- alt[idx[m]]
            }
          }
        }
        # The single option or group of equally ranked options is defined
        # by the union of the sets (D - U) and B
        a <- d[!(d %in% u)]
        #TODO review function of ELECTRE II ranking - how it processes results
        #next 2 lines look weird
        if (length(b) > 0) a <- append(a, b)
        if (length(a) > 0) rank <- append(rank, a)
        idx <- match(a, alt)
        y <- y[!(y %in% a)]
        if (length(idx) > 0) {
          for (j in seq_along(idx)) {
            dominance[, idx[j]] <- -1
            dominance[idx[j], ]  <- -1
          }
        }
      }
      return(rank)
    },

    # @description
    # sensitivity testing for c_minus threshold
    #
    # @param c_minus c- value to be tested for sensitivity
    #
    # @return
    # value of c- at which provided solution for the decision problem changes.
    # If no change detected returns insens.
    sens_cminus = function(c_minus) {
      for (i in seq_along(c_minus)) {
        t <- electre2$new(self$pm_orig, self$w, self$minmaxcriteria,
                          c_minus = c_minus[i],
                          c_zero = self$c_zero, c_plus = self$c_plus,
                          d_minus = self$d_minus, d_plus = self$d_plus)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(c_minus[i - 1])
          return(c_minus[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for c_minus threshold
    #
    # @param c_zero c0 value to be tested for sensitivity
    #
    # @return
    # value of c0 at which provided solution for the decision problem changes.
    # If no change detected returns insens.
    sens_czero = function(c_zero) {
      for (i in seq_along(c_zero)) {
        t <- electre2$new(self$pm_orig, self$w, self$minmaxcriteria,
                          c_minus = self$c_minus,
                          c_zero = c_zero[i], c_plus = self$c_plus,
                          d_minus = self$d_minus, d_plus = self$d_plus)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(c_zero[i - 1])
          return(c_zero[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for c_plus threshold
    #
    # @param c_plus c+ value to be tested for sensitivity
    #
    # @return
    # value of c+ at which provided solution for the decision problem changes.
    # If no change detected returns insens.
    sens_cplus = function(c_plus) {
      for (i in seq_along(c_plus)) {
        t <- electre2$new(self$pm_orig, self$w, self$minmaxcriteria,
                          c_minus = self$c_minus,
                          c_zero = self$c_zero, c_plus = c_plus[i],
                          d_minus = self$d_minus, d_plus = self$d_plus)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(c_plus[i - 1])
          return(c_plus[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for d_minus threshold
    #
    # @param d_minus d- value to be tested for sensitivity
    #
    # @return
    # value of d- at which provided solution for the decision problem changes.
    # If no change detected returns insens.
    sens_dminus = function(d_minus) {
      for (i in seq_along(d_minus)) {
        t <- electre2$new(self$pm_orig, self$w, self$minmaxcriteria,
                          c_minus = self$c_minus,
                          c_zero = self$c_zero, c_plus = self$c_plus,
                          d_minus = d_minus[i], d_plus = self$d_plus)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(d_minus[i - 1])
          return(d_minus[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for d_plus threshold
    #
    # @param d_plus d+ value to be tested for sensitivity
    #
    # @return
    # value of d+ at which provided solution for the decision problem changes.
    # If no change detected returns insens.
    sens_dplus = function(d_plus) {
      for (i in seq_along(d_plus)) {
        t <- electre2$new(self$pm_orig, self$w, self$minmaxcriteria,
                          c_minus = self$c_minus,
                          c_zero = self$c_zero, c_plus = self$c_plus,
                          d_minus = self$d_minus, d_plus = d_plus[i])
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(d_plus[i - 1])
          return(d_plus[i])
        }
      }
      return("insens.")
    }
  )
)