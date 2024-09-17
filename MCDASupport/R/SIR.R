#' Superiority and inferiority ranking (SIR) method
#'
#' @description
#' SIR stands for Superiority and inferiority ranking (SIR) method.The method
#'  is interesting as it is computationwise closely related to PROMETHEE and
#'  TOPSIS methods. Similarly to them it computes preference based on flows.
#'
#' The flows are computed based on S and I (superiority and inferiority)
#'  matrixes. Based on transformation of preference P(A, A2) from natural units
#'  to 0-1 according to 6 function (in same way as PROMETHEE computes them). S
#'  and I matrixes are defined:
#'
#' \mjsdeqn{S_j(A_i) = \sum_{k=1}^m P_j(A_i, A_k) = \sum_{k=1}^m f_j(g_j(A_i) - g_j(A_k))}
#'
#' and
#'
#' \mjsdeqn{I_j(A_i) = \sum_{k=1}^m P_j(A_k, A_i) = \sum_{k=1}^m f_j(g_j(A_k) - g_j(A_i))}
#'
#' Where m is the number of alternatives, fj is transformed value using
#'  function d.
#'
#' Results in S and I matrixes need to be agregated.The method supports two
#'  agregation methods SIR-SAW or SIR-TOPSIS.
#'
#' For SIR-SAW:
#'
#' \mjsdeqn{SFlow(A_i) = \sum_{j=1}^n w_jS_j(A_i)}
#'
#' \mjsdeqn{IFlow(A_i) = \sum_{j=1}^n w_jI_j(A_i)}
#'
#' SIR-TOPSIS on the other hand uses computation of ideal and antiideal
#'  solution and computes S- and I- flows from it. See TOPSIS documentation for
#'  details.
#'
#' This approach is used separately for both S and I matrixes. For computation
#'  of ideal and anti-ideal solution min and max functions are switched.
#'
#' Directly from these flow complete rankings can be derived, or simple
#'  aggregation of them as net flow (diference between S and I flows) and
#'  relative flow as a closeness to addition of the S and I flows.
#'
#' @references
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p., ISBN
#'  978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SIR SAW SIR-SAW SIR-TOPSIS superiority inferiority net relative
#' @keywords flow
sir <- R6Class("sir",
  public = list(

    #' @field pm_orig original (not transformed) performance matrix of
    #'  alternatives in criteria
    pm_orig = NULL,

    #' @field pm performance matrix of alternatives in criteria (all criteria
    #'  are maximized)
    pm = NULL,

    #' @field pref_function vector, specifies type of function used to compute
    #'  preferences. Need to be set for each criterion. Possible values are:
    #'  'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice
    #'  of function type will decide on what type of threshold (if any) is
    #'  required for computation. Each criterion can use different preference
    #'  function.
    pref_function = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field i_threshold vector of indifference thresholds
    i_threshold = NULL,

    #' @field p_threshold vector of proference thresholds
    p_threshold = NULL,

    #' @field im_threshold vector containing intermetiate thresholds for
    #'  criteria. only Gaussian type performance functions rewuire this type of
    #'  threshold. If prefference and indifference thresholds are present, the
    #'  PROMETHEE function will try to 'gues' intermediate threshold as value
    #'  right in the middle between these thresholds.
    im_threshold = NULL,

    #' @field SAW implicit TRUE, if set TRUE will aggregate S and I matrixes
    #'  using SIR-SAW method, otherwise it will use SIR-TOPSIS.
    SAW = NULL,

    #' @field superiorityFlow vector of the alternatives derived from S matrix
    superiorityFlow = NULL,

    #' @field inferiorityFlow vector of the alternatives derived from I matrix
    inferiorityFlow = NULL,

    #' @field rankSFlow vector of alternatives with assigned ranks (is possible
    #'  to directly derive from superiorityFlow)
    rankSFlow = NULL,

    #' @field rankIFlow vector of alternatives with assigned ranks (is possible
    #'  to directly derive from inferiorityFlow)
    rankIFlow = NULL,

    #' @field partialRanking matrix providing partial order of alternatives
    #'  with P-prefered, I-indifferent and R-incomparable values
    partialRanking = NULL,

    #' @field netFlow differencce between superiority and inferiority flows
    netFlow = NULL,

    #' @field relativeFlow expresses closeness to addition of the S and I flows
    relativeFlow = NULL,

    #' @field flows matrix with all types of flows in one place
    flows = NULL,

    #' @description
    #' public constructor allowing the user to construct SIR decision analysis
    #'  problem and compute it using either SAW or TOPSIS approach.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expercted. Rows and columns are expected to be named.
    #' @param w vector, specifies type of function used to compute preferences.
    #'  Need to be set for each criterion. Possible values are: 'default',
    #'  'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice of function
    #'  type will decide on what type of threshold (if any) is required for
    #'  computation. Each criterion can use different preference function.
    #' @param d vector containing the weights of the criteria. Values need to
    #'  \mjseqn{0 \le w_i \le 1, \sum w_i = 1}
    #' @param minmax value or vector of values 'min' or 'max' specifying
    #'  optimization direction for the criterium
    #' @param i_threshold vector containing indifference threshods for
    #'  criteria. Not all types of performance functions require it. The
    #'  parameter must be used if there is at least one criterion, for which it
    #'  is required. Values for all other criteria should be 0 (and will not be
    #'  used during computations). Only 'U-shape', 'level', 'linear' functions
    #'  need this threshold.
    #' @param p_threshold vector containing prefference threshods for criteria.
    #'  Not all types of performance functions require it. The parameter must
    #'  be used if there is at least one criterion, for which it is required.
    #'  Values for all other criteria should be 0 (and will not be used during
    #'  computations). Only 'V-shape', 'level', 'linear' functions need this
    #'  threshold.
    #' @param im_threshold vector containing intermetiate thresholds for
    #'  criteria. only Gaussian type performance functions rewuire this type of
    #'  threshold. If prefference and indifference thresholds are present, the
    #'  PROMETHEE function will try to 'gues' intermediate threshold as value
    #'  right in the middle between these thresholds.
    #' @param SAW implicit TRUE, if set TRUE will aggregate S and I matrixes
    #'  using SIR-SAW method, otherwise it will use SIR-TOPSIS.
    #'
    #' @return initialized R6 class with computed results for SIR
    #'
    #' @examples
    #' # Example from the book (see references)
    #' PM <- rbind(
    #'    c(8,7,2,1),
    #'    c(5,3,7,5),
    #'    c(7,5,6,4),
    #'    c(9,9,7,3),
    #'    c(11,10,3,7),
    #'    c(6,9,5,4)
    #' )
    #' rownames(PM) <- c('Site 1', 'Site 2', 'Site 3', 'Site 4', 'Site 5',
    #'  'Site 6')
    #' colnames(PM) <- c('Investment costs (million EUR)',
    #'                   'Employment needs (hundred employees)',
    #'                   'Social impact (1-7)',
    #'                   'Environmental impact (1-7)'
    #' )
    #' minmax <- 'max'
    #' w <- c(0.4, 0.3, 0.1, 0.2)
    #' shape <- c('linear', 'linear', 'linear', 'linear')
    #' q <- c(1, 1, 1, 1)
    #' p <- c(2,2,2,2)
    #' s <- c(0,0,0,0)
    #' result <- SIR(PM, w, shape, minmax, q, p, s, SAW = TRUE)
    initialize = function(pm, w, d, minmax = "max", i_threshold = NULL, p_threshold = NULL,
                          im_threshold = NULL, SAW = TRUE) {

      ## check validity of the objects manipulated by the current function
      # with < 2 criteria or 2 alternatives, there is no MCDA problem
      self$pm_orig <- pm
      #validate minmax and invert scales if neccessary
      self$pm <- util_pm_minmax(pm, minmax)
      self$pref_function <- d
      t <- promethee_param_check(pm, self$pref_function, w, i_threshold,
                                 p_threshold, im_threshold)
      ## End of checking the validity of the "inputs"

      self$w <- w
      self$minmax <- minmax
      self$i_threshold <- i_threshold
      self$p_threshold <- p_threshold
      self$im_threshold <- t
      self$SAW <- SAW
      self$compute()
      self
    },

    #' @description
    #' Computes SIR model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      alt  <- rownames(self$pm)
      cri  <- colnames(self$pm)
      qj   <- self$i_threshold
      pj   <- self$p_threshold
      sj   <- self$im_threshold

      #pairwaise comparison
      DK <- lapply(1:ncri, function(k) {
        DKf <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) self$pm[i, k] - self$pm[j, k]))
        rownames(DKf) <- alt
        colnames(DKf) <- alt
        DKf # return value for the function
      })
      names(DK) <- cri

      #preference degree (validated in PROMETHEE)
      Pj <- preferenceDegree(nalt, ncri, DK, self$pref_function, qj, pj, sj,
                             alt, cri)

      #superiority (Si) and inferiority (Ii) index
      si <- sapply(1:ncri, function(i) rowSums(Pj[[i]]))
      ii <- sapply(1:ncri, function(i) colSums(Pj[[i]]))
      rownames(si) <- alt
      rownames(ii) <- alt
      colnames(si) <- cri
      colnames(ii) <- cri

      #S-flow a I-flow
      if (self$SAW) {
        #SIR SAW agregation method
        s_flow <- rowSums(sweep(si, MARGIN = 2, w, `*`))
        i_flow <- rowSums(sweep(ii, MARGIN = 2, w, `*`))
      } else {
        #SIR TOPSIS agregation method
        # as oposed to TOPSIS the procedure works separately with Si and Ii
        # matrixes to derive S and I flow
        # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti)
        # Solutions, as max or minimums in the criteria
        si <- as.data.frame(si)
        ii <- as.data.frame(ii)
        t <- topsis_ideal(si)
        s_flow <- t$closenes

        # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti)
        # Solutions as max or minimums in the criteria.
        # Note that for Ii max and min are switched - min for ideal and max for
        # anti ideal solution
        t <- topsis_ideal(ii, inferiority = TRUE)
        i_flow <- t$closenes
      }

      # first and second complete ranking (according to S and I flows)
      net_flow <- s_flow - i_flow
      relative_flow <- s_flow / (s_flow + i_flow)
      flows <- cbind(s_flow, i_flow, net_flow, relative_flow)
      rownames(flows) <- alt
      colnames(flows) <- c("S-flow", "I-flow", "n-flow", "r-flow")
      names(s_flow) <- alt
      names(i_flow) <- alt
      names(net_flow) <- alt
      names(relative_flow) <- alt
      s_flow <- sort(s_flow, decreasing = TRUE)
      i_flow <- sort(i_flow, decreasing = FALSE)
      net_flow <- sort(net_flow, decreasing = TRUE)
      relative_flow <- sort(relative_flow, decreasing = TRUE)
      rank_s_flow <- cumsum(c(1, diff(s_flow) != 0))
      rank_i_flow <- cumsum(c(1, diff(i_flow) != 0))
      names(rank_s_flow) <- names(s_flow)
      names(rank_i_flow) <- names(i_flow)

      # partial ranking
      partial_ranking <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
        if (i == j) {
          return(0) #no meaning evaluation alternative with itself
        } else if ((rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] < rank_i_flow[alt[j]]) ||
                   (rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] == rank_i_flow[alt[j]]) ||
                   (rank_s_flow[alt[i]] == rank_s_flow[alt[j]] && rank_i_flow[alt[i]] < rank_i_flow[alt[j]])) {
          return("P") #preference
        } else if (rank_s_flow[alt[i]] == rank_s_flow[alt[j]] && rank_i_flow[alt[i]] == rank_i_flow[alt[j]]) {
          return("I") #indifference
        }else if (rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] > rank_i_flow[alt[j]]) {
          return("R") #incomparable
        }
      }))
      rownames(partial_ranking) <- alt
      colnames(partial_ranking) <- alt

      self$superiorityFlow <- s_flow
      self$inferiorityFlow <- i_flow
      self$rankSFlow <- rank_s_flow
      self$rankIFlow <- rank_i_flow
      self$partialRanking <- partial_ranking
      self$netFlow <- net_flow
      self$relativeFlow <- relative_flow
      self$flows <- flows
    },

    #' @description
    #' summary of the SIR method resutls.
    #' 
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("SIR:\n", "processed ", nalt,
                 " alternatives in ", ncri, " criteria\nused "))
      if (self$SAW) {
        cat(paste(" SIR-SAW procedure\n"))
      } else {
        cat(paste(" SIR-TOPSIS procedure\n"))
      }
      cat(paste("FLows: \n"))
      print(self$flows, pretty = TRUE)
    },

    #' @description
    #' test sensitivity of the model to changes in the thresholds.
    #'
    #' Provides sens_i (for indifference threshold), sens_p (for prefference
    #'  threshold) and sens_im (for intermediate treshold) dataframes in
    #'  structure:
    #'
    #' \itemize{
    #'   \item criterium
    #'   \item from - lower bound of sensitivity
    #'   \item default - value computed by original model
    #'   \item to - upper bound of sensitivity
    #'   \item function - type of preference function for criterium
    #' }
    #'
    #' Dataframes do have a numeric value with lower/upper bound of
    #'  sensitivity identifying last value of the threshold for which the
    #'  result still doesn't change.
    #'
    #' If no such value is identified "insens." is provided.
    #'
    #' Also note that PROMETHEE function is complex, because of existence of
    #'  "preference" functions, which are stated separately for each criterium
    #'  and each of these has a different requirements on types of thresholds
    #'  it uses. For example level and linear functions use both preference and
    #'  indifference thresholds (but not intermediate). V-shape function uses
    #'  prefference threshold only, U-shape uses indifference threshold only.
    #'
    #' PROMETHEE function is being utilized only in SIR-TOPSIS, so if SIR-SAW
    #'  procedure is used the function only sneds an error message to terminal.
    #'
    #' Gaussian function uses intermediate threshold only, but if preference
    #'  andindifference thresholds are provided, the sensitivity is being
    #'  tested on in interval between these two.
    #'
    #' @param step number of steps to divide threshold testing interval
    #'  (default: 100)
    #'
    #' @return dataframes sens_i, sens_p and sens_im with sensitivity limit for
    #'  the criteria
    sensitivity = function(step = 100) {
      if (self$SAW) {
        print("The sensitivity analysis in SIR is supported only using SIR-TOPSIS procedure.")
      } else {
        return(sensitivity_p12(self, step))
      }
    }
  )
)