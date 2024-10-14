#' ELECTRE TRI method implementation as R6 class
#'
#' @description
#' The ELECTRE TRI is a multiple criteria decision aiding method, designed to
#'  deal with sorting problems. Electre TRI method has been developed by
#'  LAMSADE (Paris-Dauphine University, Paris, France).
#'
#' The method itself is very interesting as it does not directly compare
#'  alternatives against each other. The evaluation is performed against the
#'  "profile". The profile presents scale in which criterion exists and is
#'  divided in the categories. Since we presume imperfect information we are
#'  basically evaluating in which category the performance of the alternative
#'  in criterion is.
#'
#' These categories are then aggregated across the criteria to form rank using
#'  "pessimistic" or "optimistic" agregation procedure. Results of these
#'  procedures is then used to form ranking.
#'
#' As oposed to ELECTRE IV (\code{\link{electre4}}), ELECTRE TRI does not
#'  provide guidance to establish "final" ranking by agregating outcomes of
#'  optimistic and pesimistic procedures.
#'
#' The procedure itself works with alternatives a, and performance profiles
#'  (\mjseqn{b_h}). Partial concordance matrix is derived from comparison
#'  between performance profile and preference or indifference thresholds for
#'  the criteria.
#'
#' \mjsdeqn{c_j(a,b_h) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) \ge PM_j(b_h) - Q_j \cr 0 & \;if\; PM_j(a) < PM_j(b_h) - P_j \cr \frac{PM_j(a) + P_j - PM_j(b_h)}{P_j - Q_j} & \;otherwise\;\end{array}\right.}
#'
#' Where P ... preference threshold, Q ... indifference threshold,
#'  \mjseqn{PM_j(a)} ... performance of alternative in criterium j,
#'  \mjseqn{PM_j(b_h)} ... performance profile in criterium j.
#'
#' Comprehensive concordance index is then computed by aggregating weighted
#'  concordance indexes:
#'
#' \mjseqn{c(a, b_h) = \sum_{j \in J} w_j c_j(a, b_h)}
#'
#' Similarly discordance index for the criterie is derived from comparing
#'  alternative's performance against performace profiles with applied veto or
#'  preference thresholds:
#'
#' \mjsdeqn{d_j(a,b_h) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) < PM_j(b_h) - V_j \cr 0 & \;if\; PM_j(a) \ge PM_j(b_h) - P_j \cr \frac{PM_j(b_h) - P_j - PM_j(b_h)}{V_j - P_j} & \;otherwise\;\end{array}\right.}
#'
#' To construct preference relations, credibility of the outranking relation
#'  (aSbh - a is at least as good as bh) needs to be computed. First
#'  credibility index \mjseqn{\rho} needs to be constructed.
#'
#' \mjsdeqn{\rho(a,b_h) = \left\lbrace\begin{array}{ll} c(a,b_h) \prod_{j \in V} \frac{1 - d_j(a,b_h)}{1 - c(a, b_h)} & \;if\; V \ne \;NULL\; \cr c(a, b_h) & \;otherwise\;\end{array}\right.}
#'
#' Where \mjseqn{V = \{ j \in J: d_j(a, b_h) > c(a, b_h)\}}.
#'
#' Based on it we can explore outranking relations by comparing the index to
#'  cut-off criterium \mjseqn{\lambda}. \mjseqn{\lambda} should be in interval
#'  of <0,5; 1>, value \mjseqn{\lambda = 0,75} is used as default.
#'
#' \itemize{
#'   \item \mjseqn{\rho(a,b_h) \ge \lambda} and
#'  \mjseqn{\rho(b_h, a) \ge \lambda}: a is indifferent to bh
#'   \item \mjseqn{\rho(a,b_h) \ge \lambda} and
#'  \mjseqn{\rho(b_h, a) < \lambda}: a is preferred to bh (weakly or strongly)
#'  - aPbh
#'   \item \mjseqn{\rho(a,b_h) < \lambda} and \mjseqn{\rho(b_h,a) \ge \lambda}:
#'  bh is preferred to a (weakly or strongly)
#'   \item \mjseqn{\rho(a,b_h) < \lambda} and \mjseqn{\rho(b_h,a) < \lambda}:
#'  a is incomparable to bh
#' }
#'
#' This information is then used to derive the ranking using pessimistic and
#'  optimistic procedure.
#'
#' In Pessimistic procedure the alternative is assigned best category for which
#'  aPbh. The categories for profiles are evaluated in order
#'  \mjseqn{b_k, b_{k-1}, ..., b_0}.
#'
#' Optimistic procedure evaluates in inversed order
#'  \mjseqn{b_0, b_1, ..., b_{k-1}, b_k}. Alternative is assigned warst
#'  category (first for which bhPa).
#'
#' Overral agregation procedure is not provided by the method.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support:
#'  Methods and Applications in Engineering and Infrastructure investment.
#'  Springer 2000, 208 p., ISBN 978-1-4757-5057-7
#'
#' FIGUEIRA, J.R., Greco, S., Roy, B., Slowinski, R. ELECTRE Methods : Main
#'  Features and Recent Developments. Laboratoire d'Analyse et Modélisation
#'  de Systèmes pour l'Aide à la Décision, Paris: 2010, 34 pp., available from
#'  \url{https://hal.archives-ouvertes.fr/hal-00876980/document}.
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' Shofade, Olanrewaju Joseph Soniran. Considering hierarchical structure of
#'  criteria in ELECTRE decision aiding methods. Universitat Rovira i Virgili,
#'  Escola Tecnica Superior d'Enginyeria, Tarragona: 2011
#'  \url{https://deim.urv.cat/~itaka/itaka2/PDF/acabats/ThesisJoseph-ELECTRE-H.pdf}
#'
#' @keywords ELECTRE methods
#' @keywords ELECTRE TRI
#' @keywords ELECTRE IV
#' @keywords Outranking approaches
#' @keywords preference modelling
#'
#' @seealso \code{\link{electre4}}
#' @seealso \code{\link{Electre_4_paramCheck}}
electretri <- R6Class("electretri",
  public = list(
    #' @field pm_orig original performance matrix as presented to class
    #'  constructor
    pm_orig = NULL,

    #' @field pm performance matrix defined as matrix or data frame containing
    #'  the performance table. Each row corresponds to an alternative, and each
    #'  column to a criterion. Rows (resp. columns) must be named according to
    #'  the IDs of the alternatives (resp. criteria). This performance matrix is
    #'  transformed in such way, that all the criteria are being maximized.
    pm = NULL,

    #' @field profiles Matrix containing, in each row, the lower profiles of
    #'  the categories. The columns are named according to the criteria, and
    #'  the rows are named according to the categories. The index of the row in
    #'  the matrix corresponds to the rank of the category.
    profiles = NULL,

    #' @field profiles_names vector containing profiles' names
    profiles_names = NULL,

    #' @field w vector containing the weights of the criteria
    w = NULL,

    #' @field q vector containing the indifference thresholds constraints
    #'  defined for each criterion.
    q = NULL,

    #' @field p vector containing the preference thresholds constraints defined
    #'  for each criterion
    p = NULL,

    #' @field v vector containing the veto thresholds constraints defined for
    #'  each criterion
    v = NULL,

    #' @field minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    minmaxcriteria = "max",

    #' @field lambda parameter is used as cut-off criterion for outranking
    #'  evaluation. Should be in range of 0.5 and 1.0. Default value=0.75
    lambda = 0.75,

    #' @field partialConcordanceIndex Partial concordance index indicates the
    #'  relative dominance of one option over profile aSbh where bh is profile
    #'  value. Partial concordance index is being constructed separately for
    #'  each criterion.
    partialConcordanceIndex = NULL,

    #' @field partialConcordanceIndexInverse Same as partialConcordanceIndex
    #'  but evaluates bhSa: dominance of profile value over performance of the
    #'  alternative in criterion.
    partialConcordanceIndexInverse = NULL,

    #' @field overallConcordanceIndex Consolidates information from partial
    #'  concordance indexes into single matrix describing dominance of the
    #'  alternative over the profile aSbh across the criteria
    overallConcordanceIndex = NULL,

    #' @field overallConcordanceIndexInverse same as overallConcordanceIndex,
    #'  but describes bhSa instead, again across all criteria
    overallConcordanceIndexInverse = NULL,

    #' @field discordanceIndex discordance index is oposite to concordance
    #'  index. It is a value (matrix of values) used to establish that a!Sbh
    #'  (a does not dominate bh). Discordance index is being computed
    #'  separately for each criterion.
    discordanceIndex = NULL,

    #' @field discordanceIndexInverse Same as discordanceIndex but helps
    #'  evaluate bhSa.
    discordanceIndexInverse = NULL,

    #' @field credibilityIndex represents a degree of credibility of the
    #'  assertion that a outranks bh (aSbh).
    credibilityIndex = NULL,

    #' @field credibilityIndexInverse represents a degree of credibility of the
    #'  assertion that bh outranks a (bhSa).
    credibilityIndexInverse = NULL,

    #' @field preferenceRelation determination of preference situation between
    #'  alternatives (aSbh = ">", bhSa = "<", I = indifferent, R =
    #'  incomparable).
    preferenceRelation = NULL,

    #' @field pesimistic The direction of the ranking obtained from the
    #'  pesimistic procedure is from best to worst.
    pesimistic = NULL,

    #' @field pesimisticUnsorted ranked results of pesimistic ranking procedure
    #'  unsorted (sorted by alternatives)
    pesimisticUnsorted = NULL,

    #' @field optimistic the direction of the ranking obtained from optimistic
    #'  procedure goes from worst to best.
    optimistic = NULL,

    #' @field optimisticUnsorted ranked results of optimistic ranking procedure
    #'  unsorted (sorted by alternatives)
    optimisticUnsorted = NULL,

    #' public constructor of the class
    #'
    #' @description
    #' creates instance of electretri class. First it validates provided input
    #'  parameters, next it computes the decision problem defined by these
    #'  parameters.
    #'
    #' Parameter checking part is identical with ELECTRE IV method, so it
    #'  utilizes \code{\link{Electre_4_paramCheck}} function.
    #'
    #' @param pm performance matrix defined as matrix or data frame containing
    #'  the performance table. Each row corresponds to an alternative, and each
    #'  column to a criterion. Rows (resp. columns) must be named according to
    #'  the IDs of the alternatives (resp. criteria)
    #' @param profiles Matrix containing, in each row, the lower profiles of
    #'  the categories. The columns are named according to the criteria, and
    #'  the rows are named according to the categories. The index of the row in
    #'  the matrix corresponds to the rank of the category.
    #' @param profiles_names vector containing profiles' names
    #' @param w vector containing the weights of the criteria
    #' @param q vector containing the indifference thresholds constraints
    #'  defined for each criterion.
    #' @param p vector containing the preference thresholds constraints defined
    #'  for each criterion
    #' @param v vector containing the veto thresholds constraints defined for
    #'  each criterion
    #' @param minmaxcriteria vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    #' @param lambda parameter is used as cut-off criterion for outranking
    #'  evaluation. Should be in range of 0.5 and 1.0. Default value=0.75
    #'
    #' @return instance od electretri class with solution to the decision
    #'  problem
    #'
    #' @examples
    #' PM <- cbind(
    #'   c(-120.0,-150.0,-100.0,-60,-30.0,-80,-45.0),
    #'   c(-284.0,-269.0,-413.0,-596,-1321.0,-734,-982.0),
    #'   c(5.0,2.0,4.0,6,8.0,5,7.0),
    #'   c(3.5,4.5,5.5,8,7.5,4,8.5),
    #'   c(18.0,24.0,17.0,20,16.0,21,13.0)
    #' )
    #' # names of alternatives
    #' rownames(PM) <- c("a1","a2","a3","a4","a5","a6","a7")
    #' # names of criteria
    #' colnames(PM) <- c( "g1","g2","g3","g4","g5")
    #' w <- c(0.25,0.45,0.10,0.12,0.08) # criteria weights
    #' # all criteria maxed - ommiting minmaxcriteria parameter
    #' # lambda = 0.75 - ommiting lambda parameter
    #' # Matrix containing the profiles.
    #' profiles <- cbind(c(-100,-50), c(-1000,-500),
    #'   c(4,7),c(4,7),c(15,20))
    #' #  vector defining profiles' names
    #' profiles_names <-c("b1","b2")
    #' # thresholds vector
    #' I <- c(15,80,1,0.5,1) # indifference threshold
    #' P <- c(40,350,3,3.5,5) # prefference threshold
    #' V <- c(100,850,5,4.5,8) # veto threshold
    #' t<- electretri$new(PM,
    #'                    profiles, profiles_names,
    #'                    w, I, P, V)
    initialize = function(pm, profiles, profiles_names, w, q, p, v,
                          minmaxcriteria = "max", lambda = 0.75) {
      # common consistency check (common with Electre_3_sensitivity function)
      ncri <- ncol(pm)
      cri <- colnames(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_electre_pqv(p, q, v, cri)
      minmaxcriteria <- validation$validate_minmax(minmaxcriteria, ncri)
      validation$validate_value_in_interval(lambda, 0, 1, "lambda")
      self$pm_orig <- pm
      #validate minmax and invert scales if necessary
      self$pm <- util_pm_minmax(pm, minmaxcriteria)
      ncri <- ncol(pm)  #no. of criteria
      nalt <- nrow(pm)  #no. of alternatives
      alt  <- rownames(pm)
      if (!(is.matrix(profiles) || is.data.frame(profiles))) {
        stop("wrong profiles, should be a matrix or a data frame")
      }
      if (!is.vector(profiles_names)) stop("profiles_names should be a vector")
      npr <- length(profiles_names) # number of profiles
      if (npr != nrow(profiles)) {
        stop("Number of stated profile names must correspond to number 
              of rows in profile matrix")
      }
      if (!is.vector(w, mode = "numeric")) {
        stop("criteria weights should be a vector")
      }
      if (ncri != length(w)) {
        stop("length of criteria weights should be checked")
      }
      if (!is.numeric(lambda)) {
        stop("if lambda is set, it must be numeric value")
      }
      if (lambda < 0.5 || lambda > 1) {
        stop("Lambda value must be in interval <0.5;1>")
      }
      # end of consistency check
      self$w <- w
      self$p <- p
      self$q <- q
      self$v <- v
      self$minmaxcriteria <- minmaxcriteria
      self$lambda <- lambda
      self$profiles <- profiles
      self$profiles_names <- profiles_names
      self$compute()
      self
    },

    #' @description
    #' computes the ELECTRE TRI problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      ncri <- ncol(self$pm)  #no. of criteria
      nalt <- nrow(self$pm)  #no. of alternatives
      alt  <- rownames(self$pm)
      npr <- length(self$profiles_names) # number of profiles
      cj     <- list() # list of partial conc. indexes for profiles cj(a,bh)
      cj_inv <- list() # inverse list of partial concordance
      #                  indexes for profiles: cj(bh,a)
      dj     <- list() # list of discordance indexes: dj(a,bh)
      dj_inv <- list() # inverse list of discordance indexes: dj(bh,a)
      # template zero matrix of n x m, n =
      # no. of alternatives, m = number of profiles
      d  <- matrix(data = 0, nrow = nalt, ncol = npr)
      colnames(d) <- self$profiles_names
      rownames(d) <- alt

      for (j in 1:ncri) {
        cj_ab <- d
        cj_ba <- d
        dj_ab <- d
        dj_ba <- d
        diff_pj <- self$p[j] - self$q[j]
        diff_vp <- self$v[j] - self$p[j]
        for (i in 1:nalt) {
          for (k in 1:npr) {
            diff_ab <- self$profiles[k, j] - self$pm[i, j]
            diff_ba <- self$pm[i, j] - self$profiles[k, j]
            cj_ab[i, k] <- ifelse(diff_ab >= self$p[j], 0, ifelse(diff_ab <= self$q[j], 1, (self$p[j] + self$pm[i, j] - self$profiles[k, j]) / diff_pj))
            cj_ba[i, k] <- ifelse(diff_ba >= self$p[j], 0, ifelse(diff_ba <= self$q[j], 1, (self$p[j] + self$profiles[k, j] - self$pm[i, j]) / diff_pj))
            dj_ab[i, k] <- min(1, max(0, (diff_ab - self$p[j]) / diff_vp))
            dj_ba[i, k] <- min(1, max(0, (diff_ba - self$p[j]) / diff_vp))
          }
        }
        cj[[j]] <- cj_ab
        cj_inv[[j]] <- cj_ba
        dj[[j]] <- dj_ab
        dj_inv[[j]] <- dj_ba
      }

      # compute the overall concordance index c_abh
      # c(a,bh) = sum for all j (kj * cj(a,bh) / sum for all j kj
      oci <- d     #overall concordance index: c(a,bh)
      oci_inv <- d #inverse overall concordance index: c(bh,a)
      rownames(oci) <- alt
      colnames(oci) <- self$profiles_names
      rownames(oci_inv) <- alt
      colnames(oci_inv) <- self$profiles_names
      for (j in 1:ncri) {
        oci <- oci + self$w[j] * cj[[j]]
        oci_inv <- oci_inv + self$w[j] * cj_inv[[j]]
      }
      oci <- oci / sum(self$w)
      oci_inv <- oci_inv / sum(self$w)

      # partial credibility index
      # cr(a,b) = c(a,bh) * PRODUCT of all j in Fr (1 - dj(a,bh))/(1 - c(a,bh))
      # where Fr are all criteria for which dj(a,bh) > c(a,bh)
      cr <- oci         # partial credibility index: cr(a,bh)
      cr_inv <- oci_inv # partial inverse credibility index: cr(bh, a)
      for (j in 1:ncri) {
        dj_ab <- dj[[j]]
        dj_ba <- dj_inv[[j]]
        for (i in 1:nalt) {
          for (k in 1:npr) {
            if (dj_ab[i, k] > oci[i, k]) {
              cr[i, k] <- cr[i, k] * (1 - dj_ab[i, k]) / (1 - oci[i, k])
            }
            if (dj_ba[i, k] > oci_inv[i, k]) {
              cr_inv[i, k] <- cr_inv[i, k] * (1 - dj_ba[i, k]) / (1 - oci_inv[i, k])
            }
          }
        }
      }

      # preference relations
      pref <- matrix(0, nalt, npr)
      rownames(pref) <- alt
      colnames(pref) <- self$profiles_names
      for (i in 1:nalt) {
        for (k in 1:npr) {
          if (cr[i, k] >= self$lambda && cr_inv[i, k] >= self$lambda) {
            # cr(a,bh) >= lambda and cr(bh,a) >= lambda => aSbh and bhSa => a is
            # indifferent to bh
            pref[i, k] <- "I" # aIbh
          } else if (cr[i, k] >= self$lambda && cr_inv[i, k] < self$lambda) {
            # cr(a,bh) >= lambda and cr(bh,a) < lambda => aSbh and not bhSa
            # => a > bh, i.e. a is preferred to bh (weakly or strongly)
            pref[i, k] <- ">" # aPbh
          } else if (cr[i, k] < self$lambda && cr_inv[i, k] >= self$lambda) {
            # cr(a,bh) < lambda and cr(bh,a) >= lambda => not aSbh and bhSa
            # => bh > a, i.e. bh is preferred to a (weakly or strongly)
            pref[i, k] <- "<" # bhPa
          } else if (cr[i, k] < self$lambda && cr_inv[i, k] < self$lambda) {
            # cr(a,bh) < lambda and cr(bh,a) < lambda => not aSbh and not
            # bhSa => aRbh, i.e. a is incomparable to bh
            pref[i, k] <- "R" # aRbh
          }
        }
      }

      # pessimistic procedure
      # a) compare alternatives ("a") successively to "b(i)", for i=p,p-1,...,0,
      # b) let "b(h)" = the first profile such that "a" outranks "b(h).",
      # c) assign "a" to the category C(h+1).
      # The direction of the ranking obtained from the pessimistic procedure is
      # from best to worst.
      #
      # optimistic procedure
      # a) compare a successively to bi, for i = 1, 2, ....,p,
      # b) bh being the first profile such that bh > a,
      # c) assign a to category C(h)
      # The direction from worst to best
      pessimistic <- rep(0, times = nalt)
      names(pessimistic) <- alt
      optimistic <- rep(0, times = nalt)
      names(optimistic) <- alt
      for (i in 1:nalt) {
        t_pes <- FALSE
        t_opt <- FALSE
        for (k in npr:1) {
          if (!t_pes && pref[i, k] == ">") {
            pessimistic[i] <- k + 1
            t_pes <- TRUE
          }
          if (!t_opt && pref[i, k] == "<") {
            optimistic[i] <- k
            t_opt <- TRUE
          }
          if (t_pes && t_opt) break
        }
      }
      pesimisticUnsorted <- pessimistic
      pesimistic <- sort(pessimistic)
      optimisticUnsorted <- optimistic
      optimistic <- sort(optimistic)

      # aggregation procedure
      # - No option can be indifferent to more than one reference option.
      # - Each option must be assigned to one reference category only
      #   (uniqueness/unicity).
      # - The assignment of any option to its allotted category is not dependent
      #   on the assignment of any of the other options (independence).
      # - The procedure for assigning options to categories must be entirely
      #   consistent with the design of the reference options themselves
      #   (conformity).
      # - When two options have the same outranking relationship with a given
      #   reference option, they must be assigned to the same category
      #   (homogeneity).
      # - If option a' outranks a, then a' must be assigned to a category at
      #   least as good as the one to which a is assigned (monotonicity).
      # - The grouping together of two neighbouring categories must not cause
      #   the alteration of options to categories not affected by the alteration
      #   (stability)

      self$partialConcordanceIndex        <- cj
      self$partialConcordanceIndexInverse <- cj_inv
      self$overallConcordanceIndex        <- oci
      self$overallConcordanceIndexInverse <- oci_inv
      self$discordanceIndex               <- dj
      self$discordanceIndexInverse        <- dj_inv
      self$credibilityIndex               <- cr
      self$credibilityIndexInverse        <- cr_inv
      self$preferenceRelation             <- pref
      self$pesimistic                     <- pesimistic
      self$pesimisticUnsorted             <- pesimisticUnsorted
      self$optimistic                     <- optimistic
      self$optimisticUnsorted             <- optimisticUnsorted
    },

    #' @description
    #' summary of the ELECTRE TRI method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("ELECTRE TRI:\n", "processed ", nalt,
                 " alternatives in ", ncri, " criteria\n\n",
                 "Preference matrix:\n",
                 "legend: aSbh = >, bhSa = <, I = indifferent, R = incomparable\n"))
      print(self$preferenceRelation, pretty = TRUE)
      cat(paste0("\nPesimistic sorted preorder:\n"))
      print(self$pesimistic, pretty = TRUE)
      cat(paste0("\nOptimistic sorted preorder:\n"))
      print(self$optimistic, pretty = TRUE)
    },

    #' Sensitivity testing
    #'
    #' @description
    #' Sensitivity analysis for ELECTRE TRI object is performed on values of
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
    #' Please note that the ELECTRE TRI uses two computation procedures
    #'  optimistic and pesimistic, which means that there are two versions of
    #'  sens_* matrix. For example sens_p_optimistic and sens_p_pesimistic.
    #'
    #' Sensitivity analysis also doesn't test for changes in lambda parameter
    #'  at this time.
    #'
    #' @param steps how many steps should sensitivity testing take. Interval
    #'  for testing will be split to steps segments. The higher number of steps
    #'  the smaller the step will be, the more granular the testing will be.
    #'  Set to 100 by default
    #'
    #' @return
    #' returns dataframes sens_p, sens_q and sens_v in optimistic and pesimistic
    #'  variant with sensitivity limit for the criteria
    sensitivity = function(steps = 100) {
      if (!is.numeric(steps) || steps < 0) {
        stop("Steps parameter must be numeber greater then 0.")
      }
      ncri <- ncol(self$pm)
      cri <- colnames(self$pm)
      sens_p_optimistic <- data.frame(matrix(0, nrow = ncri, ncol = 4))
      colnames(sens_p_optimistic) <- c("criterium", "from", "default", "to")
      sens_p_pesimistic <- sens_p_optimistic
      sens_q_optimistic <- sens_p_optimistic
      sens_q_pesimistic <- sens_p_optimistic
      sens_v_optimistic <- sens_p_optimistic
      sens_v_pesimistic <- sens_p_optimistic
      for (i in 1:ncri) {
        sens_p_optimistic[i, 1] <- cri[i]
        sens_p_pesimistic[i, 1] <- cri[i]
        sens_p_optimistic[i, 3] <- self$p[i]
        sens_p_pesimistic[i, 3] <- self$p[i]
        sens_q_optimistic[i, 1] <- cri[i]
        sens_q_pesimistic[i, 1] <- cri[i]
        sens_q_optimistic[i, 3] <- self$q[i]
        sens_q_pesimistic[i, 3] <- self$q[i]
        sens_v_optimistic[i, 1] <- cri[i]
        sens_v_pesimistic[i, 1] <- cri[i]
        sens_v_optimistic[i, 3] <- self$v[i]
        sens_v_pesimistic[i, 3] <- self$v[i]
        m_i <- max(self$pm[, i])
        hyp_p0 <- rev(seq(from = self$q[i], to = self$p[i],
                          by = (self$p[i] - self$q[i]) / steps))
        step <- (self$v[i] - self$p[i]) / steps
        hyp_p1 <- seq(from = self$p[i], to = (self$v[i] - step), by = step)
        hyp_v0 <- rev(seq(from = self$p[i] + step, to = self$v[i], by = step))
        if (self$v[i] >= m_i) {
          hyp_v1 <- seq(from = self$v[i], to = 2 * self$v[i],
                        by = self$v[i] / steps)
        } else {
          hyp_v1 <- seq(from = self$v[i], to = m_i,
                        by = (m_i - self$v[i]) / steps)
        }
        hyp_q0 <- rev(seq(from = 0, to = self$q[i], by = self$q[i] / steps))
        hyp_q1 <- seq(from = self$q[i], to = self$p[i],
                      by = (self$p[i] - self$q[i]) / steps)
        t <- private$sens_p(hyp_p0, i)
        sens_p_optimistic[i, 2] <- t$optimistic
        sens_p_pesimistic[i, 2] <- t$pesimistic
        t <- private$sens_p(hyp_p1, i)
        sens_p_optimistic[i, 4] <- t$optimistic
        sens_p_pesimistic[i, 4] <- t$pesimistic
        t <- private$sens_q(hyp_q0, i)
        sens_q_optimistic[i, 2] <- t$optimistic
        sens_q_pesimistic[i, 2] <- t$pesimistic
        t <- private$sens_q(hyp_q1, i)
        sens_q_optimistic[i, 4] <- t$optimistic
        sens_q_pesimistic[i, 4] <- t$pesimistic
        t <- private$sens_v(hyp_v0, i)
        sens_v_optimistic[i, 2] <- t$optimistic
        sens_v_pesimistic[i, 2] <- t$pesimistic
        t <- private$sens_v(hyp_v1, i)
        sens_v_optimistic[i, 4] <- t$optimistic
        sens_v_pesimistic[i, 4] <- t$pesimistic
      }
      # TODO doplnit testování sensitivity na změny lambda parametru
      t <- list(
        sens_p_optimistic = sens_p_optimistic,
        sens_p_pesimistic = sens_p_pesimistic,
        sens_q_optimistic = sens_q_optimistic,
        sens_q_pesimistic = sens_q_pesimistic,
        sens_v_optimistic = sens_v_optimistic,
        sens_v_pesimistic = sens_v_pesimistic
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
      t_opt <- FALSE
      t_pes <- FALSE
      t_opt_val <- NULL
      t_pes_val <- NULL
      for (i in seq_along(p)) {
        p2[j] <- p[i]
        t <- electretri$new(self$pm_orig, self$profiles, self$profiles_names,
                            self$w, self$q, p2, self$v, self$minmaxcriteria,
                            self$lambda)
        if (!t_opt &&
              !vector_compare(t$optimisticUnsorted, self$optimisticUnsorted)) {
          if (i != 1) {
            t_opt_val <- p[i - 1]
          } else {
            t_opt_val <- p[i]
          }
          t_opt <- TRUE
        }
        if (!t_pes &&
              !vector_compare(t$pesimisticUnsorted, self$pesimisticUnsorted)) {
          if (i != 1) {
            t_pes_val <- p[i - 1]
          } else {
            t_pes_val <- p[i]
          }
          t_pes <- TRUE
        }
        if (t_opt && t_pes) break
      }
      if (is.null(t_opt_val)) t_opt_val <- "insens."
      if (is.null(t_pes_val)) t_pes_val <- "insens."
      t <- list(
        optimistic = t_opt_val,
        pesimistic = t_pes_val
      )
      return(t)
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
      t_opt <- FALSE
      t_pes <- FALSE
      t_opt_val <- NULL
      t_pes_val <- NULL
      for (i in seq_along(q)) {
        q2[j] <- q[i]
        t <- electretri$new(self$pm_orig, self$profiles, self$profiles_names,
                            self$w, q2, self$p, self$v, self$minmaxcriteria,
                            self$lambda)
        if (!t_opt &&
              !vector_compare(t$optimisticUnsorted, self$optimisticUnsorted)) {
          if (i != 1) {
            t_opt_val <- q[i - 1]
          } else {
            t_opt_val <- q[i]
          }
          t_opt <- TRUE
        }
        if (!t_pes &&
              !vector_compare(t$pesimisticUnsorted, self$pesimisticUnsorted)) {
          if (i != 1) {
            t_pes_val <- q[i - 1]
          } else {
            t_pes_val <- q[i]
          }
          t_pes <- TRUE
        }
        if (t_opt && t_pes) break
      }
      if (is.null(t_opt_val)) t_opt_val <- "insens."
      if (is.null(t_pes_val)) t_pes_val <- "insens."
      t <- list(
        optimistic = t_opt_val,
        pesimistic = t_pes_val
      )
      return(t)
    },

    # @description
    # sensitivity testing for veto threshold
    #
    # @param q veto threshold value to be tested for sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of veto threshold at which provided solution for the
    #   decision problem changes. If no change detected returns insens.
    sens_v = function(v, j) {
      v2 <- self$v
      t_opt <- FALSE
      t_pes <- FALSE
      t_opt_val <- NULL
      t_pes_val <- NULL
      for (i in seq_along(v)) {
        v2[j] <- v[i]
        t <- electretri$new(self$pm_orig, self$profiles, self$profiles_names,
                            self$w, self$q, self$p, v2, self$minmaxcriteria,
                            self$lambda)
        if (!t_opt &&
              !vector_compare(t$optimisticUnsorted, self$optimisticUnsorted)) {
          if (i != 1) {
            t_opt_val <- v[i - 1]
          } else {
            t_opt_val <- v[i]
          }
          t_opt <- TRUE
        }
        if (!t_pes &&
              !vector_compare(t$pesimisticUnsorted, self$pesimisticUnsorted)) {
          if (i != 1) {
            t_pes_val <- v[i - 1]
          } else {
            t_pes_val <- v[i]
          }
          t_pes <- TRUE
        }
        if (t_opt && t_pes) break
      }
      if (is.null(t_opt_val)) t_opt_val <- "insens."
      if (is.null(t_pes_val)) t_pes_val <- "insens."
      t <- list(
        optimistic = t_opt_val,
        pesimistic = t_pes_val
      )
      return(t)
    }
  )
)