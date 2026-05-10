#' Méthode d'ELimination et de CHoix Incluant les relations d'ORdre
#'
#' @description
#' Method is based on specification of preferences on alternatives in criteria.
#'  Methods works with 3 types of preference: 1) weak Preference (Q) in situation
#'  when thre is good reson to prefer one alternative, but not to be strictly
#'  prefering it. 2) strict preference (P) - it is justified to prefer one
#'  alternative. 3) Veto (V): value of performance difference gj(b)-gj(a), from
#'  which the proposition aSb will not be accepted.
#'
#' For computational point of view also indifference threshold qj and a preference
#'  threshold pj  with pj > qj >= 0.
#'
#' Outranking relationship is established by followwing the rules:
#'
#' \tabular{llc}{
#'    \bold{relationship} \tab \bold{condition} \cr
#'    aPj+b \tab If gj(a) > gj(b) + pj \cr
#'    aQj+b \tab If gj(a) > gj(b) + pj and gj(a) > gj(b) + qj \cr
#'    aIj+b \tab If gj(a) > gj(b) + qj and gj(a) > gj(b) \cr
#'    aEjb \tab If gj(a) = gj(b) \cr
#'    aPj-b \tab If bPj+a \cr
#'    aQj-b \tab If bQj+a \cr
#'    aIj-b \tab If bIj+a
#' }
#'
#' For string outranking (SF), there are no criteria for which b is strictly
#'  preferable to a; Criterion i for which b is weakly preferable to a must be
#'  masked by more important criteria for which a enjoys strict preference.
#'
#' Weak outranking (Sf) it is necessary that the criteria i for which b has the
#'  advantage must be masked by criteria j at least as important in favor of a.
#'
#' @references
#' COSTA, Igor Pinheiro de Araújo et al. Algorithm Selection for Machine Learning
#' Classification:An Application of the MELCHIOR Multicriteria Method. In: Modern
#'  Management based on Big Data II and Machine Learning and Intelligent Systems
#'  III, IOS Press, 2021, pp. 154-161, doi:10.3233/FAIA210243
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MELCHIOR outranking
melchior <- R6Class(
  "melchior",
  public = list(
    #' @field pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential.
    pm = NULL,

    #' @field minmax vector of optimization direction for criteria (min/max)
    minmax = NULL,

    #' @field p preference threshold
    p = NULL,

    #' @field q indifference threshold
    q = NULL,

    #' @field v veto threshold
    v = NULL,

    #' @field outranking data frame with outranking relationship
    outranking = NULL,

    #' @field pairwise_outranking criteria paiwise comparison to establish
    #'  outraniking relationship
    pairwise_outranking = NULL,

    #' @field result dataframe with final score and rank
    result = NULL,

    #' @field outranking_graph visualization of the network of strong and weak
    #'  outranking connections
    outranking_graph = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns
    #' @param minmax vector of optimization direction (min/max, max is default)
    #' @param p preference threshold
    #' @param q indifference threshold
    #' @param v veto threshold
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' pm <- rbind(
    #'   c(39.6, 19.8, 4.5),
    #'   c(71.5, 34.6, 7.5),
    #'   c(67.2, 62.6, 5.2),
    #'   c(66.3, 61, 4.7)
    #' )
    #' alt <- c(paste0("A", 1:4))
    #' cri <- c(paste0("C", 1:3))
    #' colnames(pm) <- cri
    #' rownames(pm) <- alt
    #' minmax <- c("max", "max", "min")
    #' p <- c(10, 13, 0.5)
    #' q <- c(20, 20, 1)
    #' v <- c(40, 50, 4)
    #' t <- melchior$new(pm, minmax, p, q, v)
    initialize = function(pm, minmax = "max", p, q, v) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(
        p,
        ncri,
        "preference threshold",
        TRUE
      )
      validation$validate_no_elements_vs_cri(
        q,
        ncri,
        "indifference threshold",
        TRUE
      )
      validation$validate_no_elements_vs_cri(v, ncri, "veto threshold", TRUE)
      for (i in 1:ncri) {
        if (p[i] <= q[i]) stop("q must be lower p")
      }
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$pm <- pm
      self$p <- p
      self$q <- q
      self$v <- v
      self$compute()
      self
    },

    #' @description
    #' computes MELCHIOR model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)

      pr <- rep(0, times = (ncri + 8))
      e <- as.data.frame(t(pr))
      #                 1    2    3      4      5     6     7     8    9+
      colnames(e) <- c("a", "b", "alt", "rel", "p+", "p-", "q+", "q-", cri)
      pref <- as.data.frame(matrix(0, nrow = nalt, ncol = nalt))
      rownames(pref) <- colnames(pref) <- alt
      l <- 0
      for (i in 1:(nalt - 1)) {
        for (j in (i + 1):nalt) {
          l <- l + 1
          t <- rep(0, times = (ncri + 8))
          cp_plus <- 0
          cp_minus <- 0
          cq_plus <- 0
          cq_minus <- 0
          for (k in 1:ncri) {
            t[1] <- i
            t[2] <- j

            val_i <- self$pm[i, k]
            val_j <- self$pm[j, k]
            diff <- 0
            if (self$minmax[k] == "max") {
              diff <- val_i - val_j
            } else {
              diff <- val_j - val_i
            }

            if (diff > self$p[k]) {
              t[k + 8] <- "P+"
              cp_plus <- cp_plus + 1
            } else if (diff >= self$q[k]) {
              t[k + 8] <- "Q+"
              cq_plus <- cq_plus + 1
            } else if (diff > 0) {
              t[k + 8] <- "I+"
            } else if (diff == 0) {
              t[k + 8] <- "E"
            } else if (diff > -self$q[k]) {
              #  -q <= diff < 0
              t[k + 8] <- "I-"
            } else if (diff >= -self$p[k]) {
              #  -p <= diff < -q
              t[k + 8] <- "Q-"
              cq_minus <- cq_minus + 1
            } else {
              # diff < -p
              t[k + 8] <- "P-"
              cp_minus <- cp_minus + 1
            }
          }
          t[5] <- cp_plus
          t[6] <- cp_minus
          t[7] <- cq_plus
          t[8] <- cq_minus
          # strong outranking
          if (cp_plus > 0 && cp_minus == 0 && cp_plus + cq_plus > cq_minus) {
            t[3] <- "a"
            t[4] <- "SF"
          } else if (
            cp_minus > 0 && cp_plus == 0 && cp_minus + cq_minus > cq_plus
          ) {
            t[3] <- "b"
            t[4] <- "SF"
          } else if (
            (cp_plus > cp_minus && cq_plus >= cq_minus) ||
              (cp_plus >= cp_minus && cq_plus > cq_minus)
          ) {
            # weak outranking
            t[3] <- "a"
            t[4] <- "Sf"
          } else if (
            (cp_minus > cp_plus && cq_minus >= cq_plus) ||
              (cp_minus >= cp_plus && cq_minus > cq_plus)
          ) {
            t[3] <- "b"
            t[4] <- "Sf"
          }
          e[l, ] <- t
          if (t[3] == "a") {
            pref[i, j] <- t[4]
            pref[j, i] <- 0
          } else {
            pref[j, i] <- t[4]
            pref[i, j] <- 0
          }
        }
      }
      self$pairwise_outranking <- e
      self$outranking <- pref

      SF_mat <- (self$outranking == "SF") * 1
      Sf_mat <- (self$outranking == "Sf") * 1
      phi_SF <- rowSums(SF_mat) - colSums(SF_mat)
      phi_Sf <- rowSums(Sf_mat) - colSums(Sf_mat)
      final_score <- phi_SF + (0.5 * phi_Sf)
      self$result <- data.frame(
        final_score,
        rank(-final_score, ties.method = "max")
      )
      colnames(self$result) <- c("score", "rank")
      rownames(self$result) <- rownames(self$pm)
      self$outranking_graph <- self$graph()
    },

    #' @description
    #' summary of the MELCHIOR method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm) #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "MELCHIOR:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
      self$outranking_graph
    },

    #' @description
    #' functions prepares network diagram of outranking relations
    #' @return plot_ly network graph with outranking relations
    graph = function() {
      alt <- rownames(self$pm)
      n <- length(alt)
      theta <- seq(0, 2 * pi, length.out = n + 1)[1:n]
      nodes_x <- cos(theta)
      nodes_y <- sin(theta)

      SF_mat <- (self$outranking == "SF") * 1
      Sf_mat <- (self$outranking == "Sf") * 1
      p <- plot_ly()
      p <- private$add_preference_edges(
        p,
        Sf_mat,
        "gray",
        1.5,
        "dash",
        "Weak preference (Sf)",
        n,
        nodes_x,
        nodes_y
      )
      p <- private$add_preference_edges(
        p,
        SF_mat,
        "#1f77b4",
        2.5,
        "solid",
        "Strong preference (SF)",
        n,
        nodes_x,
        nodes_y
      )
      p <- p %>%
        add_markers(
          x = nodes_x,
          y = nodes_y,
          marker = list(
            size = 40,
            color = "white",
            line = list(color = "black", width = 2)
          ),
          text = alt,
          hoverinfo = "text"
        ) %>%
        add_annotations(
          x = nodes_x,
          y = nodes_y,
          text = alt,
          showarrow = FALSE,
          font = list(size = 14, weight = "bold")
        )
      p <- p %>%
        layout(
          title = list(text = "Preference structure (MELICHOR)", y = 0.95),
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          showlegend = TRUE
        )
      return(p)
    }
  ),
  private = list(
    # @description
    # Helper function for graphing
    #
    # @param p plot_ly object
    # @param m matrix (Sf or SF)
    # @param color color of the edge
    # @param width width of the edge
    # @param style style of the edge
    # @param name name of the edge
    # @param n number of nodes
    # @param nodes_x x coordinates
    # @param nodes_y y coordinates
    #
    # @return plot_ly object with added edges
    add_preference_edges = function(
      p,
      m,
      color,
      width,
      style,
      name,
      n,
      nodes_x,
      nodes_y
    ) {
      for (i in 1:n) {
        for (j in 1:n) {
          if (m[i, j] == 1) {
            p <- p %>%
              add_segments(
                x = nodes_x[i],
                xend = nodes_x[j],
                y = nodes_y[i],
                yend = nodes_y[j],
                line = list(color = color, width = width, dash = style),
                name = name,
                hoverinfo = "none",
                showlegend = FALSE
              )
          }
        }
      }
      return(p)
    }
  )
)