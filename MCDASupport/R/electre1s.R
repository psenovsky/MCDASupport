#' electre1s : ELECTRE 1S method used to solve multiple criteria decision
#'  making (experimental)
#'
#' @description
#' Method for supporting multicriteria decision making used to identify so
#'  called kernel of solution as set of alternatives which are not dominated
#'  by any other alternative. Dominated alternatives can be ommited from
#'  decision making as they clearly represent sub-optimal solution for the
#'  problem.
#'
#' Method does not provide ranking of the alternatives.
#'
#' Computationally the method can be seen as a hybrid of ELECTRE III
#'  (\code{\link{electre3}}) and I \code{\link{electre1}} methods. From
#'  ELECTRE III it takes concordance matrix computation as it works with fuzzy
#'  defined preference (P), indifference (Q) and veto (V) thresholds. From
#'  ELECTRE I it takes procedure to identify kernel of the solution.
#'
#' The procedure itself is as follows. First we compute Concordance matrix.
#'  Concordancce matrix (index) measures strength of the statement that
#'  alternative a outranks alternative b.
#'
#' To compute concordance matrix, partial concordance matrix \mjseqn{c_j}
#'  of the criteria needs to be computed first. The partial concordance matrix
#'  considers concordance by comparing performances of the alternatives in
#'  criterium \mjseqn{j}.
#'
#' \mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}
#'
#' Since the procedure is same as for ELECTRE III, the code has been refactored
#'  into separate function \code{\link{ELECTRE_ConcordanceMatrix}} and only
#'  called form this function.
#'
#' Based on partial concordance matrixes we compute concordence matrix
#'  \mjseqn{C} as:
#'
#' \mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}
#'
#' where
#'
#' PM ... performance of alternative in criterion, Q ... indifference
#'  threshold, P ... prefference threshold, w ... weights.
#'
#' Discordance matrix \mjseqn{d} consist of discordance indexes which provide
#'  together with discordance threshold (exceeding this threshold) information
#'  preventing outranking.
#'
#' \mjsdeqn{d[i,j] = \left\lbrace\begin{array}{ll} 1 & \;if\; PM[j,k] - PM[i,k] \ge V_k - Q_k \cdot \frac{1 - C[i,j] - (w_k/\sum w)}{1 - \lambda - (w_k/\sum w)}  \cr 0 & otherwise \end{array}\right.}
#'
#' Where d ... discordance matrix, i ... variable used to iterate over
#'  alternatives, j,k ... variables use to iterate over criteria,
#'  \mjseqn{\lambda} ... cutoff criterium, V ... veto threshold, othervise
#'  same notation as in previous equations has been used.
#'
#' Next credibility matrix needs to be computed. Credibility matrix takes into
#'  account concordance and discordance indexes to evaluate haw credible it is
#'  that a over prerforms b (aSb).
#'
#' \mjsdeqn{cred(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; C(a,b) \ge \lambda \; and \; d(a,b) = 0 \cr 0 & otherwise \end{array}\right.}
#'
#' Finally kernel of solution is computed in same manner as for ELECTRE I
#'  method using \code{\link{ELECTRE1_Kernel}}. We use credibility matrix as
#'  input parameter for the function as it is also adjacancy matrix. The
#'  matrix needs to be simplified by removing loops from it using for example
#'  Johnson's algorithm.
#'
#' The kernel is returned in form of graph, vectors of dominated alternatives
#'  and vector of alternatives forming kernel.
#'
#' @references
#' Balamurali, M.: pyDecisions - A Python Library of management decision making
#'  techniques. Available on-line from
#'  \url{https://github.com/Valdecy/pyDecisions}
#'
#' Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support:
#'  Methods and Applications in Engineering and Infrastructure investment.
#'  Springer 2000, 208 p., ISBN 978-1-4757-5057-7
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support:
#'  Methods and Applications in Engineering and Infrastructure investment.
#'  Springer 2000, 208 p., ISBN 978-1-4757-5057-7
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE I
#' @keywords ELECTRE III
#' @keywords ELECTRE 1S
#' @keywords concordance matrix
#' @keywords discordane matrix
#' @keywords credibility matrix
#' @keywords kernel
electre1s <- R6Class("electre1s",
  public = list(
    #' @field pm_orig original (unmodified) performace matrix
    pm_orig = NULL,

    #' @field pm transformed performance matrix, all criteria will be maximized
    pm = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field q vector of indifference thresholds
    q = NULL,

    #' @field p vector of preference thresholds
    p = NULL,

    #' @field v vector of veto thresholds
    v = NULL,

    #' @field minmaxcriteria criteriaMinMax Vector containing the preference
    #'  direction on each of the criteria. "min" (resp."max") indicates that
    #'  the criterion has to be minimized (maximized).
    minmaxcriteria = "max",

    #' @field lambda lambda parameter defining concordance threshold.
    #'  The default value is 0.5, but the value can be in interval <0.5;1>.
    lambda = 0.5,

    #' @field ConcordanceMatrix concordance matrix
    ConcordanceMatrix = NULL,

    #' @field DiscordanceIndex discordance index (list of discordance matrices
    #'  separate for each criterium)
    DiscordanceIndex = NULL,

    #' @field CredibilityIndex credibility index (list seprate for each
    #'  criterium)
    CredibilityIndex = NULL,

    #' @field Kernel kernel of the solution
    Kernel = NULL,

    #' @field Dominated vector of alternatives identified as dominated
    Dominated = NULL,

    #' @field graph netvork graph visualizing alternative overranking
    graph = NULL,

    #' @description
    #' public constructor, creates electre1s object.
    #'
    #' @param pm Performance matrix
    #' @param w weight vector
    #' @param q vector of indifference thresholds
    #' @param p vector of preference thresholds
    #' @param v vector of veto thresholds
    #' @param minmaxcriteria vector of direction of each of the criteria. "min"
    #' (resp."max")indicates that the criterion has to be minimized
    #' (maximized). Vector canbe replaced by single max or min value if all
    #' criteria are maximized or minimized.
    #' @param lambda lambda parameter defining concordance threshold. The
    #'  default value is 0.5, but the value can be in interval <0.5;1>.
    #'
    #' @examples
    #' PM <- cbind(
    #'   c(-14,129,-10,44,-14),
    #'   c(90,100,50,90,100),
    #'   c(0,0,0,0,0),
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
    #' e1s <- electre1s:new(PM, w, Q, P, V)
    initialize = function(pm, w, q, p, v, minmaxcriteria = "max",
                          lambda = 0.5) {
      # validation of the inputs
      ncri <- ncol(pm)  #no. of criteria
      cri <- colnames(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_value_in_interval(lambda, 0.5, 1, "lambda")
      validation$validate_electre_pqv(p, q, v, cri)
      #end of validation

      self$pm_orig <- pm
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      self$w <- w
      self$q <- q
      self$p <- p
      self$v <- v
      self$minmaxcriteria <- minmaxcriteria
      self$lambda <- lambda
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE IS problem. Usually doesn't need to be run manually
    #'  as it is called by class constructor.
    compute = function() {
      ncri <- ncol(self$pm)  #no. of criteria
      nalt <- nrow(self$pm)  #no. of alternatives
      alt  <- rownames(self$pm)
      #overall concordance index oci
      t <- Electre3_ConcordanceIndex(self$pm, self$p, self$q, self$w)
      oci <- t$ConcordanceMatrix

      # discordance index of the criteria
      w_sum <- 1
      di <- matrix(0, nalt, nalt)
      rownames(di) <- alt
      colnames(di) <- alt
      diag(di) <- 1
      for (i in 1:nalt) {
        dj_ab <- matrix(0, nalt, nalt)
        rownames(dj_ab) <- alt
        colnames(dj_ab) <- alt
        for (j in 1:ncri) {
          for (k in 1:ncri) {
            if (j != k) {
              if (self$w[k] / sum(self$w) != 0) {
                w_sum <- (self$w[k] / sum(self$w))
              }
              if (self$pm[j, k] - self$pm[i, k] >= (self$v[k] - self$q[k] *
                  ((1 - oci[i, j] - (w_sum)) / (1 - self$lambda - (w_sum))))) {
                di[i, j] <- 1
              }
            }
          }
        }
      }

      # credibility matrix
      cred <- matrix(0, nalt, nalt)
      rownames(cred) <- alt
      colnames(cred) <- alt
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (i != j && oci[i, j] >= self$lambda && di[i, j] == 0) {
            cred[i, j] <- 1
          }
        }
      }

      g  <- simplify(graph_from_adjacency_matrix(cred)) #remove loops
      am <- Graph2AdjancancyMatrix(g, alt)

      # establish kernel of the decision
      t <- ELECTRE1_Kernel(am)

      self$ConcordanceMatrix <- oci
      self$DiscordanceIndex <- di
      self$CredibilityIndex <- am
      self$Kernel <- t$kernel
      self$Dominated <- t$dominated
      self$graph <- t$graph
    },

    #' @description
    #' summary of the ELECTRE IS resutls.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      kernel_string <- paste(self$Kernel, collapse = ", ")
      cat("Electre 1S:\nexperimental, do not use in production environment\n\n")
      cat("processed ", nalt, " alternatives in ", ncri, " criteria\n\n")
      cat("identified kernel of the solution: ", kernel_string, "\n\n")
      if (length(self$Dominated) > 0) {
        dom <- paste(self$Dominated, ", ")
        cat("dominated alternatives: ", dom)
      }else {
        cat("no dominated alternatives detected")
      }
    },

    #' @description
    #' performs sensitivity analysis of the provided solution. It takes present
    #'  solution as a starting point and then changes the used thresholds.
    #'
    #' The testing is complicated by the fact, that we are working with three
    #'  separate thresholds preference (p), indifference (q) and veto (v), all
    #'  of these are specified separately for each criterium.
    #'
    #' For the thresholds following equation holds
    #'
    #' \mjsdeqn{0 \le q \le p \le v}
    #'
    #' The veto threshold is problematic, as there is no theoretical upper
    #'  bound for it. The computation provisionaly takes v + 10 % as upper
    #'  bound.
    #'
    #' Which allows us to derive array of parameters to check the sensitivity
    #'  of provided solution.
    #'
    #' @param step iteration step the function uses to generate the testing
    #'  interval
    #'
    #' @return
    #' returns dataframe specifyin upper and lower limits of the solution's
    #'  sensitivity the thresholds. Limits values can be replaced by
    #'  insens. if no limit has been identified.
    #'
    #' @examples
    #' PM <- cbind(
    #'  c(-14,129,-10,44,-14),
    #'  c(90,100,50,90,100),
    #'  c(0,0,0,0,0),
    #'  c(40,0,10,5,20),
    #'  c(100,0,100,20,40)
    #' )
    #' rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
    #' colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
    #' minmaxcriteria <- 'max'
    #' Q <- c(25,16,0,12,10) #Indifference thresholds
    #' P <- c(50,24,1,24,20) #Preference thresholds
    #' V <- c(100,60,2,48,90) #Veto thresholds
    #' w <- c(1,1,1,1,1) #weights
    #' t <- electre1s$new(PM, w, Q, P, V, minmaxcriteria)
    #' t$sensitivity(step = 1)
    sensitivity = function(step = 0.01) {
      ncri <- ncol(self$pm)
      cri <- colnames(self$pm)
      df <- data.frame(matrix(0, nrow = ncri, ncol = 7))
      colnames(df) <- c("criteria", "Q from", "Q to", "P from", "P to",
                        "V from", "V to")
      for (i in 1:ncri) { # for every criterium
        # bounds hyperparameter space
        q_lb <- rev(seq(from = 0, to = self$q[i], by = step))
        q_ub <- seq(from = self$q[i], to = self$p[i], by = step)
        p_lb <- rev(seq(from = self$q[i], to = self$p[i], by = step))
        p_ub <- seq(from = self$p[i], to = self$v[i], by = step)
        v_lb <- rev(seq(from = self$p[i], to = self$v[i], by = step))
        v_ub <- seq(from = self$v[i], to = self$v[i] * 1.1, by = step)
        # perform sensitivity testing
        df[i, 1] <- cri[i]
        df[i, 2] <- private$q_sens(q_lb, i) # Q sensitivity test
        df[i, 3] <- private$q_sens(q_ub, i)
        df[i, 4] <- private$p_sens(p_lb, i) # P sensitivity test
        df[i, 5] <- private$p_sens(p_ub, i)
        df[i, 6] <- private$v_sens(v_lb, i) # V sensitivity test
        df[i, 7] <- private$v_sens(v_ub, i)
      }
      return(df)
    }
  ),

  private = list(
    # @description
    # private function performing exploration of the Q hyperspace to identify
    #  lower or uppoer bound
    #
    # @param interval vector with interval we should use to test sensitivity
    # @param criterium_no column number of the evaluated criterium
    #
    # @return value of the bound
    q_sens = function(interval, criterium_no) {
      q <- self$q
      q_prev <- q[criterium_no]
      for (q1 in interval) {
        q[criterium_no] <- q1
        t <- electre1s$new(self$pm_orig, self$w, q, self$p, self$v,
                           self$minmaxcriteria, self$lambda, test = FALSE)
        # special case when kernel is does not exist.
        if (length(self$Kernel) != length(t$Kernel)) return(q_prev)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) return(q_prev)
        q_prev <- q1
      }
      return("insens.")
    },

    # @description
    # private function performing exploration of the P hyperspace to identify
    #  lower or uppoer bound
    #
    # @param interval vector with interval we should use to test sensitivity
    # @param criterium_no column number of the evaluated criterium
    #
    # @return value of the bound
    p_sens = function(interval, criterium_no) {
      p <- self$p
      p_prev <- p[criterium_no]
      for (p1 in interval) {
        p[criterium_no] <- p1
        t <- electre1s$new(self$pm_orig, self$w, self$q, p, self$v,
                           self$minmaxcriteria, self$lambda, test = FALSE)
        # special case when kernel is does not exist.
        if (length(self$Kernel) != length(t$Kernel)) return(p_prev)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) return(p_prev)
        p_prev <- p1
      }
      return("insens.")
    },

    # @description
    # private function performing exploration of the V hyperspace to identify
    #  lower or uppoer bound
    #
    # @param interval vector with interval we should use to test sensitivity
    # @param criterium_no column number of the evaluated criterium
    #
    # @return value of the bound
    v_sens = function(interval, criterium_no) {
      v <- self$v
      v_prev <- v[criterium_no]
      for (v1 in interval) {
        v[criterium_no] <- v1
        t <- electre1s$new(self$pm_orig, self$w, self$q, self$p, v,
                           self$minmaxcriteria, self$lambda, test = FALSE)
        # special case when kernel is does not exist.
        if (length(self$Kernel) != length(t$Kernel)) return(v_prev)
        t_kern <- all.equal(self$Kernel, t$Kernel)
        if (!t_kern) return(v_prev)
        v_prev <- v1
      }
      return("insens.")
    }
  )

)