#' electre3 : ELECTRE III method for ranking alternatives
#'
#' @description
#' ELECTRE III  method aims to answer the following question: considering a
#'  finite set of actions, A, evaluated on a coherent family of
#'  pseudo-criteria, F, how to make a partition of A in classes of  quivalence
#'  and provide a necessarily complete pre-order expressing the relative
#'  position of these classes? In the first phase, ELECTRE III method involves
#'  the construction of a fuzzy outranking relation.
#'
#' In the second phase, an algorithm is used for making a ranking in a final
#'  partial pre-order, that combines two complete pre-orders.
#'
#' This implementation presents simplified version of the method using single
#'  value alpha, beta parameters. If more complex approach to ELECTRE III
#'  computation is required use OutrankingTools package for R, which provides
#'  variant allowing such computations.
#'
#' The procedure itself is as follows. First we compute Concordance matrix.
#'  Concordancce matrix (index) measures strength of the statement that
#'  alternative a outranks alternative b.
#'
#' To compute concordance matrix, partial concordance matrix \mjseqn{c_j} of
#'  the criteria needs to be computed first. The partial concordance matrix
#'  considers concordance by comparing performances of the alternatives in
#'  criterium \mjseqn{j}.
#'
#' \mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}
#'
#' Based on partial concordance matrixes we compute concordence matrix
#'  \mjseqn{C} as:
#'
#' \mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}
#'
#' Then we compute discordance matrixes \mjseqn{d_j} separately for each
#'  criterion using:
#'
#' \mjsdeqn{d_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(b) > PM_j(a) + V_j(a) \cr 0 & \;if\; PMj(b) \le PM_j(a) + P_j(a) \cr \frac{PM_j(b) - PM_j(a) - P_j(a)}{V_j(a) - P_j(a)} & else\end{array}\right.}
#'
#' where
#'
#' PM ... performance of alternative in criterion, Q ... indifference
#'  threshold, P ... prefference threshold, V ... veto threshold, w ...
#'  weights.
#'
#' Based on discrodance matrixes \mjseqn{d_j} we compute credibility matrix
#'  \mjseqn{S}
#'
#' \mjsdeqn{S(a,b) = \left\lbrace\begin{array}{ll} C(a,b) & \;if\; d_j(a,b) \le C(a,b) \cr C(a,b) \cdot \prod_{\forall j \in J} \frac{1 - d_j(a,b)}{1 - C(a,b)} & \;otherwise\; \end{array}\right.}
#'
#' where J is set of criteria \mjseqn{j} for which \mjseqn{d_j(a,b) > C(a,b)}
#'
#' Finaly we may use the information to derive ranking of the alternatives.To
#'  do that we will perform descending rank distilation
#'  \code{\link{Electre_desc_dist}}, followed by ascending rank distilation
#'  \code{\link{Electre_asc_dist}}, then we agregate both into final rank
#'  \code{\link{finalRanking}}.
#'
#' Preorder for descending rank distilation is achieved by distilling
#'  alternatives from credibility matrix using progresively lower cutoff
#'  thresholds \mjseqn{\lambda}. first value of the \mjseqn{\lambda} is
#'  computed:
#'
#' \mjsdeqn{\lambda_1 = max_{a,b \in A}S(a,b)}
#'
#' where A ... is set of alternatives.
#'
#' Ascending distilation works similarly, but going up from lowest values in
#'  credibility matrix using progressively higher cutoff thresholds
#'  \mjseqn{\lambda}.
#'
#' Since both preorders can be different gathered information needs to be
#'  further processed by creation of pre-order matrix. This matrix provides
#'  comparison between all alternatives by comparing their mutual rankings in
#'  descending and ascending preorders.
#'
#' Suppose we compare alternatives a and b. We already have available its ranks
#'  in descending (\mjseqn{D_a, D_b}) and ascending (\mjseqn{A_a, A_b})
#'  preorders. We will use this information to derive relation between the two
#'  alternatives.
#'
#' If (\mjseqn{D_a < D_b} and at same time \mjseqn{A_a < A_b}) or
#'  (\mjseqn{D_a == D_b} and at same time \mjseqn{A_a < A_b})  or
#'   (\mjseqn{D_a < B_b} and at same time \mjseqn{A_a == A_b}) we can say that
#'  that a is prefered to b (aPb). In other words aPb if it is either preffered
#'  in both preorders or when it is better ranked in one while being evaluated
#'  as equal in other.
#'
#' Since we work with matrix we use [a, b] = P+ and [b, a] = P- notation to
#'  express it.
#'
#' Same rules apply for bPa, leading to [a, b] = P- and [b, a] = P+.
#'
#' We can also say that if the ranks of the alternatives in both preorders are
#'  same, then we are indifferent to them preferantion wise: aIb, leading to
#'  [a, b] = [b, a] = I.
#'
#' Otherwise the alternatives are incomparable: aRb, leading to
#'  [a, b] = [b, a] = R. That can hapen if in one preorder aPb, while in second
#'  one bPa.
#'
#' This information is directly usable for final ranking derivation.
#'
#' Please note, that ELECTRE IV (see \code{\link{Electre_4}}) uses same
#'  procedure for rank derivation, but does not use weights.
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
#' Roy B. : "The outranking approach and the foundations of ELECTRE methods",
#'  Theory and Decision 31, 1991, 49-73.
#'
#' Vallée, D.; Zielniewicz, P. 1994. ELECTRE III-IV, version 3.x, Aspects
#'  Méthodologiques (tome 1), Guide d’utilisation (tome 2). Document du LAMSADE
#'  85 et 85bis, Université Paris Dauphine
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE methods
#' @keywords ELECTRE III
#' @keywords Outranking approaches
#' @keywords preference modelling
#' @keywords multicriteria analysis
#' @keywords pseudo-criterion
#' @keywords Discrimination thresholds
#' @keywords global concordance matrix
#' @keywords credibility matrix
electre3 <- R6Class("electre3",
  public = list(

    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix (direction of all criteria is
    #'  maximalize)
    pm = NULL,

    #' @field w weght vector
    w = NULL,

    #' @field p Preference threshold - Vector containing the preference
    #'  thresholds constraints defined for each criterion.
    p = NULL,

    #' @field q Indifference threshold - Vector containing the indifference
    #'  thresholds constraints defined for each criterion.
    q = NULL,

    #' @field v Veto threshold - Vector containing the veto thresholds
    #'  constraints defined for each criterion.
    v = NULL,

    #' @field minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    minmaxcriteria = "max",

    #' @field alpha alpha and beta coefficients are used in downward and upward
    #'  distilation procedure as wel as final ranking procerude to construct
    #'  orders. Both coef. are used iteratively change thresholds limiting
    #'  evaluation of the outranking relations between the alternatives.
    alpha = 0.3,

    #' @field beta see alpha. Preset values of alpha = 0.3 and beta = 0.15 are
    #'  set as per Vallee and Zielniewicz (1994). This version of method allows
    #'  only to set single value for the purpose.
    beta = 0.15,

    #' @field rank_d Descending distillation ranking - final partial preorder
    #'  orders the alternatives from the best to the worst.
    rank_d = NULL,

    #' @field rank_a Ascending distillation ranking - final partial preorder
    #'  orders the alternatives from worst to best.
    rank_a = NULL,

    #' @field rank_p Pre-order matrix specifying identified relations between
    #'  the alternatives - values are P+ (a prefered to b),
    #'  P- (b prefered to a), I (indifferent), R (incomparable)
    rank_p = NULL,

    #' @field graph processed adajncency matrix into network digram.
    graph = NULL,

    #' @field ConcordanceMatrix Concordance matrix is one of two working
    #'  relations (concordance and discordance) which are subsequently used
    #'  to construct the final dominance relation. For an outranking aSb to
    #'  be validated, a sufficient majority of criteria should be in favor of
    #'  this assertion.
    ConcordanceMatrix = NULL,

    #' @field CredibilityMatrix matrix assessing the strength of the assertion
    #'  that a is at least as good as b.
    CredibilityMatrix = NULL,

    #' @field finalRankingUnsorted named vector of unsorted order of the
    #'  alternatives, added for purposes of sensitivity analysis
    finalRankingUnsorted = NULL,

    #' @field DiscordanceMatrixCriteria Discordance matrix is one of two
    #'  working relations (concordance and discordance) which are subsequently
    #'  used to construct the final dominance relation. The concept of
    #'  discordance is complementary to the one of (concordance and represent
    #'  the discomfort experienced in the choosing of alternative a above
    #'  alternative b. In comparison with ELECTRE I or II methods discordance
    #'  matrix for ELECTRE III is computed separately for every criterion.
    DiscordanceMatrixCriteria = NULL,

    #' @field adjancancyMatrix Adjancency Matrix allows to visualize results as
    #'  network diagram
    adjancancyMatrix = NULL,

    #' @field final_ranking final order of the alternatives.
    final_ranking = NULL,

    #' @description
    #' public constructor, creates electre3 object.
    #'
    #' @param pm performance matrix
    #' @param w weght vector
    #' @param p preference threshold - Vector containing the preference
    #'  thresholds constraints defined for each criterion.
    #' @param q Indifference threshold - Vector containing the indifference
    #'  thresholds constraints defined for each criterion.
    #' @param v Veto threshold - Vector containing the veto thresholds
    #'  constraints defined for each criterion.
    #' @param minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    #' @param alpha alpha and beta coefficients are used in downward and upward
    #'  distilation procedure as wel as final ranking procerude to construct
    #'  orders. Both coef. are used iteratively change thresholds limiting
    #'  evaluation of the outranking relations between the alternatives.
    #' @param beta see alpha. Preset values of alpha = 0.3 and beta = 0.15 are
    #'  set as per Vallee and Zielniewicz (1994). This version of method allows
    #'  only to set single value for the purpose.
    #'
    #' @examples
    #' PM <- cbind(
    #'   c(-14,129,-10,44,-14),
    #'   c(90,100,50,90,100),
    #'   c(40,0,10,5,20),
    #'   c(40,0,10,5,20),
    #'   c(100,0,100,20,40)
    #' )
    #' rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
    #' colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
    #' minmaxcriteria <- 'max'
    #' Q <- c(25,16,0,12,10) #Indifference thresholds
    #' P <- c(50,24,1,24,20) #Preference thresholds
    #' V <- c(100,60,2,48,90) #Veto thresholds
    #' w <- c(1,1,1,1,1) #weights
    #' t <- electre3$new(PM, w, P, Q, V, minmaxcriteria)
    initialize = function(pm, w, p, q, v, minmaxcriteria = "max",
                          alpha = 0.3, beta = 0.15) {

      # common consistency check (common with Electre_3_sensitivity function)
      Electre_3_paramCheck(pm, w, p, q, v, minmaxcriteria, alpha, beta)
      self$pm_orig <- pm
      #validate minmax and invert scales if necessary
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      self$w <- w
      self$p <- p
      self$q <- q
      self$v <- v
      self$minmaxcriteria <- minmaxcriteria
      self$alpha <- alpha
      self$beta <- beta
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE III problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      ncri <- ncol(self$pm)  #no. of criteria
      nalt <- nrow(self$pm)  #no. of alternatives
      alt  <- rownames(self$pm)

      t <- Electre3_ConcordanceIndex(self$pm, self$p, self$q, self$w)
      cm <- t$ConcordanceMatrix
      dj <- list()  #discordance matrix for criteria j
      # template zero matrix of n x n, n = no. of alternatives
      d  <- matrix(data = 0, nrow = nalt, ncol = nalt)
      rownames(d) <- colnames(d) <- alt
      for (k in 1:ncri) {      #index for criteria
        d2 <- d #temp var for dj
        for (i in 1:nalt) {    #index for a
          for (j in 1:nalt) {  #index for b
            #discordance matrix
            # Dj(a,b) = 0 if PMj(b) <= PMj(a) + Pj(a)
            # Dj(a,b) = 1 if PMj(b) > PMj(a) + Vj(a)
            # otherwise Dj(a,b) = (PMj(b)-PMj(a) - Pj(a))/ (Vj(a) - Pj(a))
            if (self$pm[j, k] <= self$pm[i, k] + self$p[k]) {
              d2[i, j] <- 0
            } else if (self$pm[j, k] > self$pm[i, k] + self$v[k]) {
              d2[i, j] <- 1
            } else {
              if (self$v[k] - self$p[k] != 0) {
                d2[i, j] <- (self$pm[j, k] - self$pm[i, k] - self$p[k]) / (self$v[k] - self$p[k])
              } else {
                d2[i, j] <- 1
              }
            }
          }
        }
        dj[[k]]  <- d2
      }
      # degree of credibility
      # S(a,b) = C(a,b) if Dj(a,b) <= C(a,b)
      # otherwise S(a,b) = C(a,b) * PRODUCT{for j in J}((1-Dj(a,b))/(1-C(a,b)))
      # where J is set of criteria for which Dj(a,b) > C(a,b)
      sm <- d # credibility matrix
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          t <- 1 #temporary var. for computation of product part of the equation
          for (k in 1:ncri) {
            dj_temp <- dj[[k]]
            if (dj_temp[i, j] > cm[i, j]) {
              t <- t * ((1 - dj_temp[i, j]) / (1 - cm[i, j]))
            }
          }
          sm[i, j] <- cm[i, j] * t
        }
      }
      distilationdesc <- Electre_desc_dist(sm) #descending distillation
      self$rank_d <- rankDF(distilationdesc)
      distilationasc <- Electre_asc_dist(sm)       #ascending distillation
      self$rank_a <- rankDF(distilationasc)
      self$rank_p <- pre_order_matrix(self$rank_d, self$rank_a, alt)
      t_fr <- finalRanking(alt, self$rank_p)
      final_ranking <- t_fr$final_ranking
      #construct adjancancy matrix
      names_matrank_adj <- final_ranking[, 1]
      matrank_adj <- matrix(data = 0, nrow = nalt, ncol = nalt)
      rownames(matrank_adj) <- names_matrank_adj
      colnames(matrank_adj) <- names_matrank_adj
      pir <- c("P+", "I", "R")
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (final_ranking[j, 3] - final_ranking[i, 3] == 1 &&
                self$rank_p[final_ranking[i, 1],
                            final_ranking[j, 1]] %in% pir) {
            matrank_adj[i, j] <- 1
          }
        }
      }
      self$graph <- plot.prefM(matrank_adj)

      self$ConcordanceMatrix <- cm
      self$DiscordanceMatrixCriteria <- dj
      self$CredibilityMatrix <- sm
      self$adjancancyMatrix <- matrank_adj
      self$final_ranking <- final_ranking
      self$finalRankingUnsorted <- t_fr$finalRankingUnsorted
    },

    #' @description
    #' summary of the ELECTRE III method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("ELECTRE III:\n", "processed ", nalt,
                 " alternatives in ", ncri, " criteria\n\n",
                 "Preference matrix:\n",
                 "legend: P+ (a prefered to b), P- (b prefered to a), I (indifferent), R (incomparable), - (NA)\n"))
      print(self$rank_p, pretty = TRUE)
      cat(paste0("\nFinal sorted order:\n"))
      print(self$final_ranking, pretty = TRUE)
    },

    #' @description
    #' sensitivity analysis for ELECTRE III object documentation) is performed
    #'  on values of preference (p), indefference (q) and veto (v) thresholds.
    #'
    #' Since the thresholds (all of them) are specfied separately for each
    #'  criterion, their sensitivity needs to be also evaluated seprately.
    #'  This is being realized in this function by generating separate
    #'  dataframe for each threshold (sens_p, sens_q, sens_v) with following
    #'  structure:
    #'
    #' criterium; from; default; to
    #'
    #' From and to limits are limist of of solution stability, meaning that
    #'  going under it (from) or over it (to) will produce different result.
    #'
    #' Sensitivity is tested from default value (the value used to produce
    #'  result) going down to 0 for from column and up to maximal performance
    #'  in the criterium j (max(pm[j])). If no change in result is detected,
    #'  value "insens." is inserted into dataframe.
    #'
    #' @param steps how many steps should sensitivity testing take. Interval
    #'  for testing will be split to steps segments. The higher number of steps
    #'  the smaller the step will be, the more granular the testing will be.
    #'  Set to 100 by default
    #'
    #' @return
    #' returns dataframes sens_p, sens_q and sens_v with sensitivity limit for
    #'  the criteria
    sensitivity = function(steps = 100) {
      ncri <- ncol(self$pm)
      cri <- colnames(self$pm)
      sens_p <- data.frame(matrix(0, nrow = ncri, ncol = 4))
      colnames(sens_p) <- c("criterium", "from", "default", "to")
      sens_q <- sens_p
      sens_v <- sens_p
      for (i in 1:ncri) {
        sens_p[i, 1] <- cri[i]
        sens_p[i, 3] <- self$p[i]
        sens_q[i, 1] <- cri[i]
        sens_q[i, 3] <- self$q[i]
        sens_v[i, 1] <- cri[i]
        sens_v[i, 3] <- self$v[i]
        m_i <- max(self$pm[, i])
        hyp_p0 <- rev(seq(from = self$q[i], to = self$p[i],
                          by = (self$p[i] - self$q[i]) / steps))
        step <- (self$v[i] - self$p[i]) / steps
        hyp_p1 <- seq(from = self$p[i], to = (self$v[i] - step), by = step)
        hyp_v0 <- rev(seq(from = self$p[i] + step, to = self$v[i], by = step))
        hyp_v1 <- seq(from = self$v[i], to = m_i,
                      by = (m_i - self$v[i]) / steps)
        hyp_q0 <- rev(seq(from = 0, to = self$q[i], by = self$q[i] / steps))
        hyp_q1 <- seq(from = self$q[i], to = self$p[i],
                      by = (self$p[i] - self$q[i]) / steps)
        sens_p[i, 2] <- private$sens_p(hyp_p0, i)
        sens_p[i, 4] <- private$sens_p(hyp_p1, i)
        sens_q[i, 2] <- private$sens_q(hyp_q0, i)
        sens_q[i, 4] <- private$sens_q(hyp_q1, i)
        sens_v[i, 2] <- private$sens_v(hyp_v0, i)
        sens_v[i, 4] <- private$sens_v(hyp_v1, i)
      }
      t <- list(
        sens_p = sens_p,
        sens_q = sens_q,
        sens_v = sens_v
      )
      return(t)
    }
  ),
  private = list(
    # @description
    # sensitivity testing for preference threshold
    #
    # @param p preference threshold value to be tested for sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of preference threshold at which provided solution for the decision
    # problem changes. If no change detected returns insens.
    sens_p = function(p, j) {
      p2 <- self$p
      for (i in seq_along(p)) {
        p2[j] <- p[i]
        t <- electre3$new(self$pm_orig, w, p2, self$q, self$v,
                          self$minmaxcriteria)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(p[i - 1])
          return(p[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for indifference threshold
    #
    # @param q indifference threshold value to be tested for sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of indifference threshold at which provided solution for the
    #  decision problem changes. If no change detected returns insens.
    sens_q = function(q, j) {
      q2 <- self$q
      for (i in seq_along(q)) {
        q2[j] <- q[i]
        t <- electre3$new(self$pm_orig, w, self$p, q2,
                          self$v, self$minmaxcriteria)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(q[i - 1])
          return(q[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for veto threshold
    #
    # @param q veto threshold value to be tested for sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of veto threshold at which provided solution for the
    #  decision problem changes. If no change detected returns insens.
    sens_v = function(v, j) {
      v2 <- self$v
      for (i in seq_along(v)) {
        v2[j] <- v[i]
        t <- electre3$new(self$pm_orig, w, self$p, self$q, v2,
                          self$minmaxcriteria)
        if (!vector_compare(t$finalPreorder, self$finalPreorder)) {
          if (i != 1) return(v[i - 1])
          return(v[i])
        }
      }
      return("insens.")
    }
  )
)