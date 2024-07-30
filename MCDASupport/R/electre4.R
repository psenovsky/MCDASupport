#' ELECTRE IV method for ranking alternatives
#'
#' @description
#' ELECTRE IV is alternative ranking approach based on ELECTRE III method (see
#'  \code{\link{electre3}}), but without using any weighting of the criteria.
#'  Since weighting is required for concordance and discordance matrixes, these
#'  are not available in ELECTRE IV. Instead method uses more complex system
#'  of outranking relations:
#'
#' \itemize{
#'   \item mp(b,a) - number of criteria for which option b is strictly
#'  preferred to a
#'   \item mq(b,a) - number of criteria for which option b is weakly preferred
#'  to a
#'   \item mj(b,a) - number of criteria for which option b is judged
#'  indifferent to a
#'   \item mo(b,a) = mo(a,b) - number of criteria on which options a and b
#'  perform identically
#' }
#'
#' These are used as a base for computation of credibility matrix.Reamining
#'  computations are same as for ELECTRE III methods including construction of
#'  descending and ascending pre-order and deriving final outranking relation.
#'
#' Please refer further to \code{\link{electre3}} for more in detailed
#'  information on computational aparatus.
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
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE IV
#' @keywords ELECTRE III
#' @keywords Outranking approaches
#' @keywords preference modelling
#' @keywords multicriteria analysis
#' @keywords Discrimination thresholds
#' @keywords credibility matrix
electre4 <- R6Class("electre4",
  public = list(
    #' @field pm_orig performance matrix as inputed into function
    pm_orig = NULL,

    #' @field pm adjusted performance matrix will all criteria transformed
    #'  in max direction
    pm = NULL,

    #' @field p preference threshold
    p = NULL,

    #' @field q indeifference threshold
    q = NULL,

    #' @field v veto threshold
    v = NULL,

    #' @field CredibilityMatrix matrix assessing the strength of the assertion
    #'  that “a is at least as good as b"
    CredibilityMatrix = NULL,

    #' @field rank_d descending distillation ranking - final partial preorder,
    #'  orders the alternatives from the best to the worst
    rank_d = NULL,

    #' @field minmaxcriteria vector of criteria optimization direction, either
    #'  min or max. Can be replaced with single min or max if all criteria are
    #'  optimized in the same direction.
    minmaxcriteria = "max",

    #' @field rank_a ascending distillation ranking - final partial preorder
    #'  orders the alternatives from worst to best
    rank_a = NULL,

    #' @field rank_p re-order matrix specifying identified relations between
    #'  the alternatives - values are P+ (a prefered to b), P-
    #'  (b prefered to a), I (indifferent), R (incomparable)
    rank_p = NULL,

    #' @field adjancancyMatrix Adjancency Matrix of outranking relation between
    #'  the alternatives allows to visualize results as network diagram
    adjancancyMatrix = NULL,

    #' @field graph processed adajncency matrix into network digram
    graph = NULL,

    #' @field final_ranking sorted final order of the alternatives
    final_ranking = NULL,

    #' @field finalRankingUnsorted amed vector of unsorted order of the
    #'  alternatives, added for purposes of sensitivity analysis
    finalRankingUnsorted = NULL,

    #' @description
    #' public constructor, creates electre2 object.
    #'
    #' @param pm performance matrix
    #' @param p preference threshold - Vector containing the preference
    #'  thresholds. Constraints are defined separately for each criterion.
    #' @param q vector of indifference thresholds
    #' @param v vector of veto thresholds
    #' @param minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
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
    #' minmaxcriteria <- "max"
    #' Q <- c(25,16,0,12,10) #Indifference thresholds
    #' P <- c(50,24,1,24,20) #Preference thresholds
    #' V <- c(100,60,2,48,90) #Veto thresholds
    #' t <- electre4:new(PM, P, Q, V, minmaxcriteria)
    initialize = function(pm, p, q, v, minmaxcriteria = "max") {
      # check validity of the objects manipulated by the current function
      if (test) {
        Electre_4_paramCheck(pm = pm, p = p, q = q, v = v,
                             minmaxcriteria = minmaxcriteria)
      }
      self$pm_orig <- pm
      #validate minmax and invert scales if necessary
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      self$p <- p
      self$q <- q
      self$v <- v
      self$minmaxcriteria <- minmaxcriteria
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE IV problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      ncri  <- ncol(self$pm)  #no. of criteria
      nalt <- nrow(self$pm) #no. of alternatives
      alt  <- rownames(self$pm) #names of alternatives

      #count outranking relations
      # template zero matrix of n x n, n = no. of alternatives
      d  <- matrix(data = 0, nrow = nalt, ncol = nalt)
      colnames(d) <- alt
      rownames(d) <- alt
      mp <- d #bFa: number of criteria for which option b is strictly preferred to a
      mq <- d #bSa: number of criteria for which option b is weakly preferred to a
      mi <- d #bIa: number of criteria for which option b is judged indifferent to a
      mo <- d #mo(b,a) = mo(a,b) : the number of criteria on which options a and b perform identically
      veto  <- d
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (i != j) {
            diff <- self$pm[j, ] - self$pm[i, ]
            mp[i, j] <- mp[i, j] + sum(diff > self$p)
            mq[i, j] <- mq[i, j] + sum(diff > self$q & diff <= self$p)
            mi[i, j] <- mi[i, j] + sum(diff >= -self$q & diff <= self$q &
                                         diff > 0)
            mo[i, j] <- mo[i, j] + sum(diff == 0)
            veto[i, j] <- veto[i, j] + sum(diff >= self$v)
          }
        }
      }
      #credibility matrix
      cred_matrix <- d
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (i != j) {
            diff_mp <- mp[i, j]
            diff_mq <- mq[i, j]
            diff_mi <- mi[i, j]
            diff_ji <- mi[j, i] + mq[j, i] + mp[j, i]
            if (diff_mp + diff_mq == 0 && diff_mi < diff_ji) {
              #Quasi-dominance Sq: b outranks a with quasi-dominance:
              # if b Sq a <=> mp(a,b) + mq(a,b) = 0 and mi(a,b) < mi(b,a) +
              # cont. prev. row: + mq(b,a) + mp(b,a)
              cred_matrix[i, j] <- 1.0
            } else if (diff_mp == 0 && diff_mq <= mq[j, i] &&
                         diff_mq + diff_mi <= diff_ji + 1) {
              # Canonical dominance Sc: b outranks a with canonical dominance
              # if b Sc a <=> mp(a,b) = 0, and mq(a,b) <= mq(b,a),
              # and mq(a,b) + mi(a,b) <= mi(b,a) + mq(b,a) + mp(b,a) + 1
              cred_matrix[i, j] <- 0.8
            } else if (diff_mp == 0 && diff_mq <= mq[j, i] + mp[j, i]) {
              # Pseudo-dominance Sp: b outranks a with pseudo-dominance
              # if b Sp a <=> mp(a,b) = 0, and II1q(a,b) S; II1q(b,a) + mp(b,a)
              cred_matrix[i, j] <- 0.6
            } else if (diff_mp == 0) {
              cred_matrix[i, j] <- 0.4
            } else if (diff_mp == 1 && mp[j, i] >= ncri / 2) {
              # Veto dominance Sv: b outranks a with veto-dominance
              # if mp(a,b) = 1, b Sv a: mp(b,a) >= m/2,
              # and gj(b) + vj[gj(b)] >= gj(a)
              le <- all(self$pm[j, ] + self$v >= self$pm[i, ])
              if (le) cred_matrix[i, j] <- 0.2
            } else if (diff_mp == 0) {
              # Veto dominance Sv: b outranks a with veto-dominance
              # if mp(a,b) = 0
              cred_matrix[i, j] <- 0.2
            }
          }
        }
      }

      #descending distillation
      distilationdesc <- Electre_desc_dist(cred_matrix)
      rank_d <- rankDF(distilationdesc)
      distilationasc <- Electre_asc_dist(cred_matrix) #ascending distillation
      rank_a <- rankDF(distilationasc)
      rank_p <- pre_order_matrix(rank_d, rank_a, alt)
      t_fr <- finalRanking(alt, rank_p)
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
                rank_p[final_ranking[i, 1], final_ranking[j, 1]] %in% pir) {
            matrank_adj[i, j] <- 1
          }
        }
      }

      # "publish" computation outputs
      self$CredibilityMatrix <- cred_matrix
      self$rank_d <- rank_d
      self$rank_a <- rank_a
      self$rank_p <- rank_p
      self$adjancancyMatrix <- matrank_adj
      self$graph <- graph
      self$final_ranking <- final_ranking
      self$finalRankingUnsorted <- t_fr$finalRankingUnsorted
    },

    #' @description
    #' summary of the ELECTRE IV method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("ELECTRE IV:\n", "processed ", nalt,
                 " alternatives in ", ncri, " criteria\n\n",
                 "Preference matrix:\n",
                 "legend: P+ (a prefered to b), P- (b prefered to a), I (indifferent), R (incomparable), - (NA)\n"))
      print(self$rank_p, pretty = TRUE)
      cat(paste0("\nFinal sorted order:\n"))
      print(self$final_ranking, pretty = TRUE)
    },

    #' @description
    #' sensitivity analysis for ELECTRE IV object is performed on values of
    #'  preference (p), indefference (q) and veto (v) thresholds.
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
        t <- electre4$new(self$pm_orig, p2, self$q, self$v,
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
        t <- electre4$new(self$pm_orig, self$p, q2,
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
        t <- electre4$new(self$pm_orig, self$p, self$q, v2,
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