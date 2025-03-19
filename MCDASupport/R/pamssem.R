#' PAMSSEM
#'
#' @description
#' Introduced by Martel, Kiss and Rousseau in 1996. The method uses concordance
#'  and discordance matrices like ELECTRE family of methods and measures of
#'  flows similar to PROMETHEE family of methods. Both ELECTRE and PROMETHEE
#'  also use concept of indifference (q), preference (p) and veto (v)
#'  thresholds the PAMSSEM utilizes also.
#'
#' Method starts with local outranking index. For ordinal attributes we do that
#'  by
#'
#' \mjsdeqn{\delta(A_i, A_j) = \sum_{A_j} \sum_{A_i} d_j(A_i, A_j)}
#'
#' Please note that original implementation also works with probability
#' distributions fj(Ai) and fj(Aj) which usually are assumed equal to 1. This
#' implementation doesn't support these two function.
#'
#' \mjsdeqn{d_j(A_i, A_j) = \left\lbrace\begin{array}{ll} 0 & \;if\; \Delta_j \le -p_j \cr \frac{\Delta_j - p_j}{p_j - q_j} & \;if\; -p_j < \Delta_j - q_j \cr 1 & \;if\; \Delta_j \ge -q_i \end{array}\right.} 
#'
#' \mjsdeqn{\Delta_j = k_j(A_i) - k_j(A_j)}
#'
#' where kj(A) is performance of alternative in crtierium i.
#'
#' For cardinal attributes the computation is:
#'
#' \mjsdeqn{d_j(A_i, A_j) = \left\lbrace\begin{array}{ll} 0 & \;if\; \Delta_j \le -1 \cr 0.5 & \;if\; -1 \le \Delta_j < 0 \cr 1 & \;if\; \Delta_j \ge 0 \end{array}\right.}
#'
#' Delta j represent deffirence between the levels in this case.
#'
#' Next concordance index is computed
#'
#' \mjsdeqn{C(A_i, A_j) = \sum_{j = 1}^n \delta_j (A_i, A_j) \cdot w_j}
#'
#' Local discoredance index is computed
#'
#' \mjsdeqn{D(A_i, A_j) = \sum_{A_i} \sum_{A_j} \overline{D_j}(A_i, A_j)}
#'
#' for ordinal attributes
#'
#' \mjsdeqn{\overline{D_j}(A_i, A_j) = \left\lbrace\begin{array}{ll} 1 & \;if\; \Delta_j \le -v_j \cr - \frac{\Delta_j + p_j}{v_j - p_j} & \;if\; -v_j < \Delta_j < -p_j \cr 0 & \;if\; \Delta_j \ge -p_j \end{array}\right.}
#'
#' for cardinal attributes
#'
#' \mjsdeqn{\overline{D_j}(A_i, A_j) = \left\lbrace\begin{array}{ll} min(1, \xi(w_j)\Delta_j + \frac{\gamma_j + 1}{2} & \;if\; \Delta_j < -\frac{\gamma_j + 1}{2} \cr 0 & \;if\; \Delta_j \ge -\frac{\gamma_j + 1}{2} \end{array}\right.}
#'
#' where gamma is the number of measurement scale levels in the used scale for
#'  the j-th criterium.
#'
#' \mjsdeqn{\xi(A_i, A_j) = 0.2 (1 + \frac{w_j}{2})}
#'
#' Outranking degree
#'
#' \mjsdeqn{\varphi(A_i, A_j) = C(A_i, A_j) \cdot \prod_{j = 1}^n 1 - D_j^3(A_i, A_j)}
#'
#' Finally from outranking degree we compute the flows. Entering flow:
#'
#' \mjsdeqn{\varphi^+(A_i) = \sum_{A_i \in A} \varphi(A_i, A_j)}
#'
#' loaving flow
#'
#' \mjsdeqn{\varphi^-(A_i) = \sum_{A_i \in A} \varphi(A_j, A_i)}
#'
#' and the net flow
#'
#' \mjsdeqn{\varphi(A_i) = \varphi^+(A_i) - \varphi^-(A_i)}
#'
#' Values of net flow are directly usable for ranking purposes ordered from
#'  highest to lowest. This is the approach PAMSSEM II method uses for ranking.
#'
#' PAMSSEM I provides only partial ranking with following rules for proving that
#'  Ai is preffered to Aj.
#'
#' \mjsdeqn{A_j \;P\; A_j \;if\; \left\lbrace\begin{array}{l} A_i \;P^+\; A_j \;and\; A_i \;P^-\; A_j \cr A_i \;P^+\; A_j \;and\; A_i \;I^-\; A_j \cr A_i \;I^+\; A_j \;and\; A_i \;P^-\; A_j \end{array}\right.}
#'
#' We can say that Ai is indifferent to Aj if Ai I Aj and Aj I Ai.
#'
#' Plese note that while support for PAMSSEM I is coded in the results are not
#'  validated and thus the implementation may include some errors.
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PAMSSEM
pamssem <- R6Class("pamssem",
  public = list(
    #' @field pm_orig original performance matrix (as used in constructor)
    pm_orig = NULL,

    #' @field pm performance matrix with all criteria converted to maximize
    pm = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field minmax vector of min/max (set direction of optimization for
    #'  criteria)
    minmax = NULL,

    #' @field type vector of the types of the critera used, either "C" -
    #'  cardinal, or "O" - ordinal.
    type = NULL,

    #' @field q vector of indifference thresholds
    q = NULL,

    #' @field p vector of prefference thresholds
    p = NULL,

    #' @field v vector of veto thresholds
    v = NULL,

    #' @field flows data frame of flows
    flows = NULL,

    #' @field pamssem_ii ranking based on net flow (PAMSSEM II method)
    pamssem_ii = NULL,

    #' @field pamssem_i final ranking using PAMSSEM I method (not
    #'   validated)
    pamssem_i = NULL,

    #' @description
    #' Public constructor. Valides the inputs and computes the method based on
    #'  them.
    #'
    #' @param pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential.
    #' @param w weights vector
    #' @param minmax vector of min/max (set direction of optimization for
    #'  criteria)
    #' @param type vector of the types of the critera used, either "C" -
    #'  cardinal, or "O" - ordinal.
    #' @param q vector of indifference thresholds
    #' @param p vector of prefference thresholds
    #' @param v vector of veto thresholds
    #'
    #' @examples
    #' alt <- c("A1", "A2", "A3")
    #' cri <- c("C1", "C2", "C3")
    #' pm <- rbind(
    #'   c(80, 90, 5),
    #'   c(65, 58, 2),
    #'   c(83, 60, 7)
    #' )
    #' rownames(pm) <- alt
    #' colnames(pm) <- cri
    #' w <- c(1, 1, 1) # weights equal
    #' minmax <- c("min", "max", "max")
    #' type <- c("C", "C", "O")
    #' q <- c(5, 15, 1)
    #' p <- c(12, 25, 2)
    #' v <- c(18, 32, 3)
    #' scale_level <- c(3, 3, 3)
    #' t <- pamssem$new(pm, w, minmax, type, q, p, v)
    initialize = function(pm, w, minmax = "max", type, q, p, v) {
      # params consistency check centralized in generalized PROMETHEE function
      # here is only minmax evaluation
      # validate minmax and invert scales if neccessary
      self$pm_orig <- pm
      self$pm <- util_pm_minmax(pm, minmax)
      validation$validate_pm(pm)
      ncri <- ncol(pm) #no. of criteria
      cri <- colnames(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_no_elements_vs_cri(type, ncri, "types o criteria",
                                             FALSE)
      validation$validate_invalid_val(crit_type, c("C", "O"), "criteria type")
      validation$validate_electre_pqv(p, q, v, cri)
      #end of parameter consistency check

      self$w <- w / sum(w)
      self$type <- type
      self$q <- q
      self$p <- p
      self$v <- v
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' computes ORESTE model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)

      # local outranking index (loi)
      t <- matrix(0, nrow = nalt, ncol = nalt)
      rownames(t) <- colnames(t) <- alt
      loi_list <- list()
      loi_wlist <- list() # weighted for concordance index
      ldi_list <- list() # local discordance index
      ldi_3list <- list()
      # number of measurement scale levels of the jth attribute (gamma)
      gamma <- apply(self$pm, 2, private$count_unique)
      for (i in 1:ncri) {
        loi <- t
        ldi <- t
        w <- 0.2 * (1 + self$w[i] / 2)
        for (j in 1:nalt) {
          for (k in 1:nalt) {
            d <- self$pm[j, i] - self$pm[k, i]
            if (self$type[i] == "O") { # ordinal criteria
              if (d < -1) { # concordance
                loi[j, k] <- 0
              } else if (d >= -1 && d < 0) {
                loi[j, k] <- 0.5
              } else {
                loi[j, k] <- 1
              }
              if (d <= -self$v[i]) { # discordance
                ldi[j, k] <- 1
              } else if (-self$v[j] < d && d < -self$p[j]) {
                ldi[j, k] <- -(d + self$p[i]) / (self$v[i] - self$p[i])
              } else {
                ldi[j, k] <- 0
              }
            } else { # cardinal criteria
              if (d <= -self$p[i]) { # concordance
                loi[j, k] <- 0
              } else if (-self$p[i] < d - self$q[i]) {
                loi[j, k] <- - (d - self$p[i]) / (self$p[i] - self$q[i])
              } else {
                loi[j, k] <- 1
              }
              if (d < - (gamma[i] + 1) / 2) {
                ldi[j, k] <- min(1, (w * d + (gamma[i] + 1) / 2))
              } else {
                ldi[j, k] <- 0
              }
            }
          }
        }
        diag(loi) <- NA
        diag(ldi) <- NA
        loi_w <- loi * self$w[i]
        loi_list[[i]] <- loi
        loi_wlist[[i]] <- loi_w
        ldi_list[[i]] <- ldi
        ldi_3list[[i]] <- 1 - ldi^3
      }
      loi_fin <- Reduce("+", loi_list)
      diag(loi_fin) <- NA
      c <- Reduce("+", loi_wlist) # Concordance index (c)
      d <- Reduce("+", ldi_list) # Discordance index (d)
      od <- c * Reduce("*", ldi_3list) # outranking degree (od)
      # flows
      f_entering <- rowSums(od, na.rm = TRUE)
      f_leaving <- colSums(od, na.rm = TRUE)
      f_net <- f_entering - f_leaving
      flows <- data.frame(f_entering, f_leaving, f_net)
      colnames(flows) <- c("entering flow", "leaving flow", "net flow")
      self$flows <- flows
      self$pamssem_ii <- rank(-f_net, ties.method = "max")
      # PAMSSEM I 
      pam1 <- t
      od2 <- od
      diag(od2) <- 0
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          if (od2[i, j] > od2[j, i] |
                (od2[i, j] > 0 & od2[j, i] == 0) |
                (od2[i, j] == 0 & od2[j, i] < 0)) {
            pam1[i, j] <- "P"
          } else if (od2[i, j] == 0 & od2[j, i] == 0) {
            pam1[i, j] <- "I"
          }
        }
      }
      self$pamssem_i <- pam1
    },
    
    #' @description
    #' prepares summary of the PAMSSEM method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("PAMSSEM results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nPAMSSEM I (not validated):\n"))
      print(self$pamssem_i, pretty = TRUE)
      cat(paste("\nPAMSSEM II:\n"))
      print(self$pamssem_ii, pretty = TRUE)
      cat(paste("\nFlows:\n"))
      print(self$flows, pretty = TRUE)
    }
  ),
  private = list(
    # @description
    # count unique values in the vector
    #
    # @param v vector to count unique values
    # @return number of unique values in vector
    count_unique = function(v) {
      return(length(unique(v)))
    }

  )
)